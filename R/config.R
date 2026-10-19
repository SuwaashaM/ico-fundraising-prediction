# Central configuration. Override paths with environment variables, e.g.
#   ICO_DATA_PATH=/path/to/data.csv ICO_OUT_DIR=results Rscript run_analysis.R

cfg <- list(
  seed        = 1234L,
  data_path   = Sys.getenv("ICO_DATA_PATH", file.path("data", "raw", "LUBS5990M_courseworkData_202526.csv")),
  out_dir     = Sys.getenv("ICO_OUT_DIR", "outputs"),

  train_prop  = 0.80,                  # stratified hold-out split
  cv_folds    = 10L,                   # folds for k-selection and model comparison
  k_grid      = c(3, 5, 7, 9, 11),     # KNN candidates, chosen by CV F1 on the training set
  c5_trials   = 10L,                   # boosted C5.0 trials
  svm_C       = 1,

  target      = "goal",
  positive    = "Success",
  leakage_cols = "sold_tokens",        # only known after the ICO closes

  # Columns that must exist in the raw file
  required_cols = c(
    "goal", "sold_tokens", "whitelist", "kyc", "bonus", "ERC20", "mvp",
    "link_white_paper", "linkedin_link", "github_link", "website",
    "ico_start", "ico_end", "pre_ico_start", "pre_ico_end", "country",
    "distributed_in_ico", "price_usd", "rating", "teamsize", "token_for_sale"
  ),

  # Numeric columns imputed with training-set medians
  impute_cols = c(
    "rating", "price_usd", "teamsize", "token_for_sale", "distributed_in_ico",
    "ico_duration", "presale_duration", "has_presale", "ico_year",
    "bonus_bin", "ERC20_bin", "mvp_bin", "digital_presence_score", "compliance_score"
  ),

  # Skewed variables replaced by log1p() versions
  log_vars    = c("price_usd", "teamsize", "token_for_sale"),

  # Final modelling features (target first)
  features = c(
    "goal",
    "rating", "log_price_usd", "log_teamsize", "log_token_for_sale",
    "distributed_in_ico", "ico_duration", "presale_duration",
    "has_presale", "ico_year",
    "bonus_bin", "ERC20_bin", "mvp_bin",
    "digital_presence_score", "compliance_score",
    "country_group"
  ),

  models = c(dt = "Decision Tree", knn = "KNN", nb = "Naive Bayes",
             svm_lin = "SVM (Linear)", svm_rbf = "SVM (RBF)")
)
