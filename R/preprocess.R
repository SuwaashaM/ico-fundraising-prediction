# Data-dependent preprocessing. Every function here is *fitted on training rows
# and applied to held-out rows*, and is called once per CV fold as well as once
# for the final hold-out split, so no fold ever sees its own validation data.

# Median imputation + log transforms + feature selection ---------------------
prep_features <- function(train, test, config = cfg) {
  cols <- intersect(config$impute_cols, names(train))
  med  <- vapply(train[cols], stats::median, numeric(1), na.rm = TRUE)

  finish <- function(df) {
    for (nm in names(med)) {
      df[[nm]][is.na(df[[nm]])] <- med[[nm]]
    }
    for (v in config$log_vars) df[[paste0("log_", v)]] <- log1p(df[[v]])
    df[, config$features, drop = FALSE]
  }
  list(train = finish(train), test = finish(test))
}

# Dummy-encode, then min-max (KNN) and z-score (NB, SVM) using training stats -
minmax_apply <- function(m, lo, hi) {
  span <- hi - lo
  const <- span == 0
  span[const] <- 1
  out <- sweep(sweep(m, 2, lo, "-"), 2, span, "/")
  out[, const] <- 0
  out
}

zscore_apply <- function(m, mu, sdv) {
  sdv[sdv == 0] <- 1
  sweep(sweep(m, 2, mu, "-"), 2, sdv, "/")
}

make_model_inputs <- function(train, test, target = cfg$target) {
  x_tr <- train[, setdiff(names(train), target), drop = FALSE]
  x_te <- test[,  names(x_tr), drop = FALSE]

  enc   <- caret::dummyVars(~ ., data = x_tr, fullRank = TRUE)
  tr_dm <- predict(enc, x_tr)
  te_dm <- predict(enc, x_te)

  lo <- apply(tr_dm, 2, min); hi <- apply(tr_dm, 2, max)
  mu <- apply(tr_dm, 2, mean); sdv <- apply(tr_dm, 2, stats::sd)

  list(
    mm_train = minmax_apply(tr_dm, lo, hi), mm_test = minmax_apply(te_dm, lo, hi),
    z_train  = zscore_apply(tr_dm, mu, sdv), z_test  = zscore_apply(te_dm, mu, sdv)
  )
}
