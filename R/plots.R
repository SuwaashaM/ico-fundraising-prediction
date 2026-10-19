BLUE <- "#2166AC"; RED <- "#D6604D"; GREEN <- "#4DAC26"; PURPLE <- "#762A83"; ORANGE <- "#FF7F00"
MODEL_COLOURS <- c(BLUE, GREEN, PURPLE, RED, ORANGE)

save_fig <- function(plot, dir, name, width, height) {
  ggplot2::ggsave(file.path(dir, name), plot, width = width, height = height, dpi = 200, bg = "white")
}

plot_target <- function(raw, config = cfg) {
  tab <- table(raw[[config$target]])
  df <- data.frame(Class = c("Fail (0)", "Success (1)"), Count = as.numeric(tab),
                   Pct = round(as.numeric(prop.table(tab)) * 100, 1))
  ggplot(df, aes(Class, Count, fill = Class)) +
    geom_col(width = 0.55) +
    geom_text(aes(label = paste0(Count, "\n(", Pct, "%)")), vjust = -0.3, size = 4.5, fontface = "bold") +
    scale_fill_manual(values = c(RED, BLUE)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
    labs(title = "Figure 1: Target Variable Distribution", subtitle = "ICO fundraising outcome",
         x = NULL, y = "Number of campaigns") +
    theme_bw(base_size = 12) + theme(legend.position = "none")
}

plot_missing <- function(raw) {
  pct <- colMeans(is.na(raw)) * 100
  pct <- pct[pct > 0]
  df <- data.frame(Variable = names(pct), Missing = as.numeric(pct))
  ggplot(df, aes(Missing, reorder(Variable, Missing))) +
    geom_col(fill = BLUE, alpha = 0.85) +
    geom_text(aes(label = paste0(round(Missing, 1), "%")), hjust = -0.1, size = 3.5) +
    geom_vline(xintercept = 30, colour = RED, linetype = 2, alpha = 0.7) +
    labs(title = "Figure 2: Missing Value Profile", subtitle = "Red dashed line = 30% threshold",
         x = "Missing (%)", y = NULL) +
    xlim(0, 105) + theme_bw(base_size = 11)
}

plot_distributions <- function(train) {
  vars <- c("rating", "ico_duration", "distributed_in_ico", "digital_presence_score",
            "compliance_score", "presale_duration")
  df <- reshape2::melt(train[, vars], measure.vars = vars)
  ggplot(df, aes(value)) +
    geom_histogram(bins = 30, fill = BLUE, alpha = 0.8, colour = "white") +
    facet_wrap(~ variable, scales = "free", nrow = 2) +
    labs(title = "Figure 3: Numeric Feature Distributions (training set)", x = NULL, y = "Count") +
    theme_bw(base_size = 11) + theme(strip.text = element_text(face = "bold"))
}

plot_class_conditional <- function(train, config = cfg) {
  vars <- c("rating", "ico_duration", "digital_presence_score", "compliance_score")
  df <- reshape2::melt(train[, c(config$target, vars)], id.vars = config$target)
  ggplot(df, aes(value, fill = goal, colour = goal)) +
    geom_density(alpha = 0.45) +
    scale_fill_manual(values = c(RED, BLUE)) + scale_colour_manual(values = c(RED, BLUE)) +
    facet_wrap(~ variable, scales = "free", nrow = 2) +
    labs(title = "Figure 4: Class-Conditional Feature Distributions", x = NULL, y = "Density",
         fill = "Outcome", colour = "Outcome") +
    theme_bw(base_size = 11) + theme(strip.text = element_text(face = "bold"))
}

plot_correlation <- function(train) {
  num <- train[, vapply(train, is.numeric, logical(1)), drop = FALSE]
  long <- reshape2::melt(stats::cor(num, use = "pairwise.complete.obs"))
  names(long) <- c("Feature1", "Feature2", "Correlation")
  long <- long[long$Feature1 != long$Feature2, ]
  ggplot(long, aes(Feature2, Feature1, fill = Correlation)) +
    geom_tile(colour = "white", linewidth = 0.4) +
    geom_text(aes(label = sprintf("%.2f", Correlation)), size = 2.8) +
    scale_fill_gradient2(low = "#B22222", mid = "white", high = "#08306B", midpoint = 0, limits = c(-1, 1)) +
    scale_x_discrete(position = "top") + coord_fixed() +
    labs(title = "Figure 5: Feature Correlation Matrix (training set)") +
    theme_minimal(base_size = 10) +
    theme(axis.text.x = element_text(angle = 45, hjust = 0, size = 8), axis.text.y = element_text(size = 8),
          axis.title = element_blank(), plot.title = element_text(face = "bold", size = 13, hjust = 0.5))
}

plot_importance <- function(imp) {
  ggplot(imp, aes(Importance, reorder(Feature, Importance))) +
    geom_col(aes(fill = Importance > 5), show.legend = FALSE) +
    scale_fill_manual(values = c("grey70", BLUE)) +
    geom_vline(xintercept = 5, colour = RED, linetype = 2, alpha = 0.7) +
    labs(title = "Figure 6: Decision Tree Variable Importance", x = "Importance (%)", y = NULL) +
    theme_bw(base_size = 11)
}

plot_knn_tuning <- function(grid, best_k) {
  long <- reshape2::melt(grid, id.vars = "K", variable.name = "Metric")
  ggplot(long, aes(K, value, colour = Metric)) +
    geom_line(linewidth = 1.1) + geom_point(size = 3) +
    scale_colour_manual(values = c(BLUE, RED)) +
    geom_vline(xintercept = best_k, linetype = 2, colour = "grey50") +
    labs(title = "Figure 7: KNN k-Selection (10-fold CV on training set)",
         subtitle = "Dashed line = selected k (highest CV F1)", x = "k", y = "Mean CV score") +
    theme_bw(base_size = 11)
}

plot_roc <- function(results) {
  rocs <- lapply(results, `[[`, "roc")
  names(rocs) <- vapply(results, function(r) sprintf("%s (AUC=%.3f)", r$label, as.numeric(pROC::auc(r$roc))), "")
  pROC::ggroc(rocs, legacy.axes = TRUE, linewidth = 1.1) +
    geom_abline(linetype = 2, colour = "grey60") +
    scale_colour_manual(values = MODEL_COLOURS, name = NULL) +
    labs(title = "Figure 8: ROC Curves, All Models (hold-out test set)",
         x = "1 - Specificity", y = "Sensitivity") +
    coord_equal() + theme_bw(base_size = 12) + theme(legend.position = "bottom") +
    guides(colour = guide_legend(nrow = 2))
}

plot_comparison <- function(comparison) {
  df <- comparison[comparison$Model != "Majority class", c("Model", "F1", "Balanced_Accuracy", "AUC")]
  long <- reshape2::melt(df, id.vars = "Model", variable.name = "Metric")
  ggplot(long, aes(reorder(Model, -value), value, fill = Metric)) +
    geom_col(position = "dodge", width = 0.7) +
    geom_text(aes(label = sprintf("%.3f", value)), position = position_dodge(0.7), vjust = -0.4, size = 3) +
    scale_fill_manual(values = c(BLUE, RED, GREEN)) + ylim(0, 1.05) +
    labs(title = "Figure 9: Model Comparison (hold-out test set)", x = NULL, y = "Metric value") +
    theme_bw(base_size = 11) + theme(axis.text.x = element_text(angle = 20, hjust = 1))
}

plot_confusion <- function(results, test, config = cfg) {
  actual <- test[[config$target]]
  df <- do.call(rbind, lapply(results, function(r) {
    tab <- as.data.frame(table(Actual = actual, Predicted = r$fit$pred))
    tab$Pct <- round(tab$Freq / sum(tab$Freq) * 100, 1)
    tab$Label <- paste0(tab$Freq, "\n(", tab$Pct, "%)")
    tab$Model <- r$label
    tab
  }))
  df$Model <- factor(df$Model, levels = vapply(results, `[[`, "", "label"))
  ggplot(df, aes(Predicted, Actual, fill = Freq)) +
    geom_tile(colour = "white", linewidth = 1.2) +
    geom_text(aes(label = Label), size = 4.2, fontface = "bold") +
    scale_fill_gradient(low = "#EEF4FF", high = BLUE, name = "Count") +
    facet_wrap(~ Model, nrow = 2) +
    labs(title = "Figure B1: Confusion Matrices, All Models",
         subtitle = sprintf("Hold-out test set (n = %s)", format(length(actual), big.mark = ","))) +
    theme_bw(base_size = 13) +
    theme(strip.text = element_text(face = "bold"), strip.background = element_rect(fill = "#EEF4FF"),
          panel.grid = element_blank())
}

plot_sens_spec <- function(comparison) {
  df <- comparison[comparison$Model != "Majority class", ]
  ggplot(df, aes(Specificity, Sensitivity, colour = Model, label = Model)) +
    annotate("rect", xmin = 0.5, xmax = 1.05, ymin = 0.5, ymax = 1.05, fill = GREEN, alpha = 0.07) +
    geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "grey70") +
    geom_point(size = 6) +
    ggrepel::geom_text_repel(size = 4, fontface = "bold", box.padding = 0.6, show.legend = FALSE) +
    scale_colour_manual(values = MODEL_COLOURS) +
    labs(title = "Figure B2: Sensitivity-Specificity Trade-off",
         subtitle = "Upper-right quadrant = strong on both",
         x = "Specificity (true negative rate)", y = "Sensitivity (true positive rate)") +
    theme_bw(base_size = 13) + theme(legend.position = "none")
}

plot_cv_stability <- function(results) {
  df <- do.call(rbind, lapply(results, `[[`, "cv"))
  stats <- aggregate(F1 ~ Model, df, function(v) c(m = mean(v, na.rm = TRUE), s = stats::sd(v, na.rm = TRUE)))
  lab <- setNames(sprintf("%s (mean=%.3f, sd=%.3f)", stats$Model, stats$F1[, "m"], stats$F1[, "s"]), stats$Model)
  df$Legend <- factor(lab[df$Model], levels = lab[vapply(results, `[[`, "", "label")])
  ggplot(df, aes(Fold, F1, colour = Legend, group = Legend)) +
    geom_line(linewidth = 1.4) + geom_point(size = 3.5) +
    scale_colour_manual(values = setNames(MODEL_COLOURS, levels(df$Legend)), name = NULL) +
    scale_x_continuous(breaks = 1:10) +
    labs(title = "Figure B3: CV F1 Stability Across Folds", subtitle = "Flatter lines = more stable",
         x = "Fold", y = "F1") +
    theme_bw(base_size = 13) + theme(legend.position = "bottom") + guides(colour = guide_legend(nrow = 2))
}
