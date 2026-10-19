# One entry point for all five classifiers so the hold-out evaluation, k-selection
# and cross-validation all run exactly the same pipeline.

p_success <- function(prob_matrix) as.numeric(prob_matrix[, cfg$positive])

# Fit on `train`, predict `test`. Both are cleaned-but-not-imputed frames.
# Returns list(pred, prob, pred_train, model, data) - `prob`/`pred_train` only if requested.
fit_predict <- function(model, train, test, k = NULL, probs = TRUE, want_train = FALSE,
                        config = cfg) {
  prepped <- prep_features(train, test, config)
  tr <- prepped$train; te <- prepped$test
  y  <- tr[[config$target]]
  out <- list(prob = NULL, pred_train = NULL, model = NULL, data = prepped)

  if (model == "dt") {
    m <- C50::C5.0(goal ~ ., data = tr, trials = config$c5_trials)
    out$pred <- predict(m, te)
    if (probs) out$prob <- p_success(predict(m, te, type = "prob"))
    if (want_train) out$pred_train <- predict(m, tr)
    out$model <- m
    return(out)
  }

  inp <- make_model_inputs(tr, te, config$target)

  if (model == "knn") {
    stopifnot(!is.null(k))
    fit <- class::knn(inp$mm_train, inp$mm_test, cl = y, k = k, prob = TRUE)
    out$pred <- fit
    win <- attr(fit, "prob")
    out$prob <- ifelse(fit == config$positive, win, 1 - win)
    if (want_train) out$pred_train <- class::knn(inp$mm_train, inp$mm_train, cl = y, k = k)
  } else if (model == "nb") {
    m <- naivebayes::naive_bayes(inp$z_train, y)
    out$pred <- predict(m, inp$z_test)
    if (probs) out$prob <- p_success(predict(m, inp$z_test, type = "prob"))
    if (want_train) out$pred_train <- predict(m, inp$z_train)
    out$model <- m
  } else if (model %in% c("svm_lin", "svm_rbf")) {
    kern <- if (model == "svm_lin") "vanilladot" else "rbfdot"
    utils::capture.output(
      m <- kernlab::ksvm(inp$z_train, y, kernel = kern, C = config$svm_C, prob.model = probs)
    )
    out$pred <- predict(m, inp$z_test)
    if (probs) out$prob <- p_success(predict(m, inp$z_test, type = "probabilities"))
    if (want_train) out$pred_train <- predict(m, inp$z_train)
    out$model <- m
  } else {
    stop("Unknown model: ", model)
  }
  out
}

# Cross-validation ------------------------------------------------------------
make_folds <- function(y, config = cfg) {
  set.seed(config$seed)
  caret::createFolds(y, k = config$cv_folds)
}

fold_scores <- function(model, train, folds, k = NULL, config = cfg) {
  set.seed(config$seed)
  do.call(rbind, lapply(seq_along(folds), function(i) {
    idx <- folds[[i]]
    res <- fit_predict(model, train[-idx, ], train[idx, ], k = k, probs = FALSE, config = config)
    cm  <- caret::confusionMatrix(res$pred, train[[config$target]][idx], positive = config$positive)
    data.frame(Fold = i, Accuracy = unname(cm$overall["Accuracy"]), F1 = unname(cm$byClass["F1"]))
  }))
}

# Choose k by mean CV F1 on the training set (never on the hold-out test set).
tune_knn_k <- function(train, folds, config = cfg) {
  grid <- do.call(rbind, lapply(config$k_grid, function(k) {
    s <- fold_scores("knn", train, folds, k = k, config = config)
    data.frame(K = k, CV_F1 = mean(s$F1, na.rm = TRUE), CV_Accuracy = mean(s$Accuracy))
  }))
  list(grid = grid, best_k = grid$K[which.max(grid$CV_F1)])
}
