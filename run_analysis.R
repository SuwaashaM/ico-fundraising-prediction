#!/usr/bin/env Rscript
# End-to-end ICO fundraising-success pipeline.
#
#   Rscript run_analysis.R          # real data (data/raw/...csv or ICO_DATA_PATH)
#   Rscript run_analysis.R --demo   # synthetic data, to try the pipeline
#
# Run from the repository root.

if (!file.exists("R/config.R")) stop("Run this script from the repository root.", call. = FALSE)
for (f in c("config", "utils", "data_prep", "preprocess", "models", "evaluation", "plots", "demo_data")) {
  source(file.path("R", paste0(f, ".R")))
}
load_packages()

args <- commandArgs(trailingOnly = TRUE)
demo <- "--demo" %in% args
if (demo) cfg$out_dir <- "outputs_demo"

t0 <- Sys.time()
fig_dir <- ensure_dir(cfg$out_dir, "figures")
tab_dir <- ensure_dir(cfg$out_dir, "tables")
set.seed(cfg$seed)

# 1. Data -----------------------------------------------------------------------
raw <- if (demo) make_demo_ico_data() else load_ico_data()
if (demo) log_msg("DEMO MODE: synthetic data; metrics are not meaningful.")
log_msg("Loaded %d rows x %d columns", nrow(raw), ncol(raw))
assert_columns(raw, cfg$required_cols, "ICO dataset")
print(round(prop.table(table(raw[[cfg$target]])) * 100, 1))

# 2. Clean, split, prepare ---------------------------------------------------------
ico   <- clean_ico_data(raw)
parts <- split_ico(ico)
train <- parts$train; test <- parts$test
log_msg("Train: %d | Test: %d", nrow(train), nrow(test))
prepped <- prep_features(train, test)   # imputed + logged copy, used for EDA figures only

# 3. EDA ------------------------------------------------------------------------
save_fig(plot_target(raw),                          fig_dir, "fig1_target_distribution.png", 7, 5)
save_fig(plot_missing(raw),                         fig_dir, "fig2_missing_values.png", 9, 5)
save_fig(plot_distributions(prepped$train),         fig_dir, "fig3_feature_distributions.png", 14, 8)
save_fig(plot_class_conditional(prepped$train),     fig_dir, "fig4_class_conditional.png", 12, 8)
save_fig(plot_correlation(prepped$train),           fig_dir, "fig5_correlation.png", 14, 12)

# 4. Model selection (CV on the training set only) --------------------------------------
folds <- make_folds(train[[cfg$target]])
log_msg("Selecting k for KNN by %d-fold CV", cfg$cv_folds)
knn_tuning <- tune_knn_k(train, folds)
log_msg("Best k = %d", knn_tuning$best_k)
write.csv(knn_tuning$grid, file.path(tab_dir, "knn_k_selection.csv"), row.names = FALSE)
save_fig(plot_knn_tuning(knn_tuning$grid, knn_tuning$best_k), fig_dir, "fig7_knn_k_selection.png", 8, 5)

# 5. Evaluate all models ---------------------------------------------------------------
results    <- evaluate_models(train, test, folds, knn_tuning$best_k)
comparison <- comparison_table(results, test)
cv_summary <- cv_table(results)
overfit    <- do.call(rbind, lapply(results, `[[`, "overfit"))
importance <- variable_importance(results$dt$fit$model)

print(comparison, digits = 3, row.names = FALSE)
print(cv_summary, digits = 3, row.names = FALSE)
print(overfit,    digits = 3, row.names = FALSE)

write.csv(comparison, file.path(tab_dir, "model_comparison.csv"),   row.names = FALSE)
write.csv(cv_summary, file.path(tab_dir, "cv_results.csv"),         row.names = FALSE)
write.csv(overfit,    file.path(tab_dir, "overfitting_gap.csv"),    row.names = FALSE)
write.csv(importance, file.path(tab_dir, "variable_importance.csv"), row.names = FALSE)

# 6. Result figures -------------------------------------------------------------------------
save_fig(plot_importance(importance),          fig_dir, "fig6_variable_importance.png", 10, 7)
save_fig(plot_roc(results),                    fig_dir, "fig8_roc_curves.png", 8, 7.5)
save_fig(plot_comparison(comparison),          fig_dir, "fig9_model_comparison.png", 12, 6)
save_fig(plot_confusion(results, test),        fig_dir, "appB1_confusion_matrices.png", 14, 9)
save_fig(plot_sens_spec(comparison),           fig_dir, "appB2_sensitivity_specificity.png", 10, 7)
save_fig(plot_cv_stability(results),           fig_dir, "appB3_cv_stability.png", 13, 7)

writeLines(capture.output(sessionInfo()), file.path(cfg$out_dir, "session_info.txt"))
log_msg("Done in %.1f minutes. Outputs in '%s/'.", as.numeric(difftime(Sys.time(), t0, units = "mins")), cfg$out_dir)
