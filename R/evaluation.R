holdout_metrics <- function(pred, actual, prob = NULL, config = cfg) {
  cm <- caret::confusionMatrix(pred, actual, positive = config$positive)
  roc_obj <- if (!is.null(prob)) pROC::roc(actual, prob, levels = c("Fail", config$positive),
                                           quiet = TRUE) else NULL
  list(
    table = data.frame(
      Accuracy          = unname(cm$overall["Accuracy"]),
      Sensitivity       = unname(cm$byClass["Sensitivity"]),
      Specificity       = unname(cm$byClass["Specificity"]),
      Precision         = unname(cm$byClass["Pos Pred Value"]),
      F1                = unname(cm$byClass["F1"]),
      Balanced_Accuracy = unname(cm$byClass["Balanced Accuracy"]),
      AUC               = if (!is.null(roc_obj)) as.numeric(pROC::auc(roc_obj)) else NA_real_
    ),
    roc = roc_obj
  )
}

# Evaluate every model on the hold-out set and on training CV.
evaluate_models <- function(train, test, folds, best_k, config = cfg) {
  actual_te <- test[[config$target]]
  actual_tr <- train[[config$target]]
  results <- list()

  for (m in names(config$models)) {
    label <- if (m == "knn") sprintf("KNN (k=%d)", best_k) else config$models[[m]]
    log_msg("Evaluating %s", label)
    set.seed(config$seed)
    fit <- fit_predict(m, train, test, k = best_k, probs = TRUE, want_train = TRUE, config = config)
    hm  <- holdout_metrics(fit$pred, actual_te, fit$prob, config)

    train_acc <- mean(fit$pred_train == fit$data$train[[config$target]])
    test_acc  <- mean(fit$pred == actual_te)
    cv <- fold_scores(m, train, folds, k = best_k, config = config)

    results[[m]] <- list(
      label = label, fit = fit, metrics = hm$table, roc = hm$roc,
      overfit = data.frame(Model = label, Train_Acc = train_acc, Test_Acc = test_acc,
                           Gap = train_acc - test_acc),
      cv = cbind(Model = label, cv)
    )
  }
  results
}

# Majority-class baseline: what accuracy do you get by always predicting "Fail"?
baseline_metrics <- function(test, config = cfg) {
  actual <- test[[config$target]]
  pred <- factor(rep("Fail", length(actual)), levels = levels(actual))
  data.frame(Model = "Majority class", Accuracy = mean(pred == actual), Sensitivity = 0,
             Specificity = 1, Precision = NA_real_, F1 = NA_real_, Balanced_Accuracy = 0.5, AUC = 0.5)
}

comparison_table <- function(results, test, config = cfg) {
  tab <- do.call(rbind, lapply(results, function(r) cbind(Model = r$label, r$metrics)))
  tab <- tab[order(-tab$F1), ]
  rownames(tab) <- NULL
  rbind(tab, baseline_metrics(test, config)[, names(tab)])
}

cv_table <- function(results) {
  do.call(rbind, lapply(results, function(r) {
    data.frame(Model = r$label,
               Mean_Acc = mean(r$cv$Accuracy), SD_Acc = stats::sd(r$cv$Accuracy),
               Mean_F1  = mean(r$cv$F1, na.rm = TRUE), SD_F1 = stats::sd(r$cv$F1, na.rm = TRUE))
  }))
}

variable_importance <- function(dt_model) {
  imp <- C50::C5imp(dt_model)
  out <- data.frame(Feature = rownames(imp), Importance = imp$Overall)
  out[order(-out$Importance), ]
}
