# Lightweight tests (base R only). Run from the repo root: Rscript tests/run_tests.R
for (f in c("config", "utils", "data_prep", "preprocess", "models", "evaluation", "demo_data")) {
  source(file.path("R", paste0(f, ".R")))
}
load_packages()

passed <- 0L
check <- function(desc, cond) {
  if (!isTRUE(cond)) stop("FAILED: ", desc, call. = FALSE)
  passed <<- passed + 1L
  cat("ok -", desc, "\n")
}

raw <- make_demo_ico_data(n = 600, seed = 1)

# standardise_binary ---------------------------------------------------------------
check("binary encodings map to 1/0/NA",
      identical(standardise_binary(c("Yes", " no ", "TRUE", "f", "maybe", NA)),
                c(1L, 0L, 1L, 0L, NA_integer_, NA_integer_)))

# assert_columns / loader ------------------------------------------------------------
check("missing columns raise an error",
      inherits(try(assert_columns(raw[, -1], cfg$required_cols), silent = TRUE), "try-error"))
check("missing data file raises an error",
      inherits(try(load_ico_data("does/not/exist.csv"), silent = TRUE), "try-error"))

# clean_ico_data ---------------------------------------------------------------------
ico <- clean_ico_data(raw)
check("leakage column sold_tokens is dropped", !"sold_tokens" %in% names(ico))
check("target is a Fail/Success factor", identical(levels(ico$goal), c("Fail", "Success")))
check("binary flags are 0/1/NA", all(ico$bonus_bin %in% c(0L, 1L, NA)))
check("digital_presence_score within 0-4", all(ico$digital_presence_score %in% 0:4))
check("compliance_score within 0-2", all(ico$compliance_score %in% 0:2))
check("ico_duration within plausible range", all(ico$ico_duration >= 0 & ico$ico_duration <= 500, na.rm = TRUE))
check("distributed_in_ico rescaled to <= 1", all(ico$distributed_in_ico <= 1, na.rm = TRUE))
check("presale_duration is zero without presale", all(ico$presale_duration[ico$has_presale == 0] == 0))
check("country_group has the four expected levels",
      identical(levels(ico$country_group), c("Asia", "Crypto_Europe", "English", "Other")))

# split + prep_features (leakage guard) ---------------------------------------------------
parts <- split_ico(ico)
check("split keeps class balance within 2pp",
      abs(mean(parts$train$goal == "Success") - mean(parts$test$goal == "Success")) < 0.02)
check("train/test are disjoint", length(intersect(rownames(parts$train), rownames(parts$test))) == 0)

p <- prep_features(parts$train, parts$test)
check("no missing values after imputation", !anyNA(p$train) && !anyNA(p$test))
check("final feature set matches config", identical(names(p$train), cfg$features))

# Test rows must be imputed with TRAIN medians, not their own.
tr <- parts$train; te <- parts$test
te$rating[1] <- NA
med_train <- stats::median(tr$rating, na.rm = TRUE)
te$rating[2:nrow(te)] <- 5   # make the test median differ from the train median
check("test NAs filled with the training median",
      prep_features(tr, te)$test$rating[1] == med_train)

# make_model_inputs --------------------------------------------------------------------------
inp <- make_model_inputs(p$train, p$test)
check("min-max inputs lie in [0,1] on train", all(inp$mm_train >= 0 & inp$mm_train <= 1))
check("z-scored train columns have mean ~0", all(abs(colMeans(inp$z_train)) < 1e-8))
check("train and test share the same columns", identical(colnames(inp$z_train), colnames(inp$z_test)))

# Every model runs end to end and returns well-formed output ------------------------------------
small_train <- parts$train[seq_len(300), ]
small_test  <- parts$test
for (m in names(cfg$models)) {
  set.seed(cfg$seed)
  res <- fit_predict(m, small_train, small_test, k = 5, probs = TRUE, want_train = TRUE)
  check(sprintf("%s: predictions are a two-level factor of the right length", m),
        is.factor(res$pred) && length(res$pred) == nrow(small_test) &&
          identical(levels(res$pred), c("Fail", "Success")))
  check(sprintf("%s: probabilities are in [0,1]", m), all(res$prob >= 0 & res$prob <= 1))
}

# Folds / k-selection ------------------------------------------------------------------------------
cfg_fast <- modifyList(cfg, list(cv_folds = 3L, k_grid = c(3, 7)))
folds <- make_folds(small_train$goal, cfg_fast)
check("CV folds partition the training rows", identical(sort(unname(unlist(folds))), seq_len(nrow(small_train))))
tuned <- tune_knn_k(small_train, folds, cfg_fast)
check("best k comes from the grid", tuned$best_k %in% cfg_fast$k_grid)

# Metrics ---------------------------------------------------------------------------------------------
hm <- holdout_metrics(res$pred, small_test$goal, res$prob)
check("hold-out metrics are within [0,1]", all(unlist(hm$table) >= 0 & unlist(hm$table) <= 1, na.rm = TRUE))
check("baseline accuracy equals the majority share",
      abs(baseline_metrics(small_test)$Accuracy - mean(small_test$goal == "Fail")) < 1e-12)

cat(sprintf("\nAll %d checks passed.\n", passed))
