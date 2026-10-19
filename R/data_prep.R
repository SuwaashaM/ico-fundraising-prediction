# Row-level cleaning and feature engineering.
# Nothing in this file learns from the data (no medians, no scaling), so it can
# safely run before the train/test split. Anything that does learn from data
# lives in preprocess.R and is fitted on training rows only.

load_ico_data <- function(path = cfg$data_path) {
  if (!file.exists(path)) {
    stop(sprintf(
      "Dataset not found at '%s'.\nPlace the coursework CSV there (see data/README.md) or run with --demo.",
      path), call. = FALSE)
  }
  utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8",
                  na.strings = c("", "NA", "N/A"))
}

group_country <- function(country) {
  english_markets <- c("USA", "UK", "Canada", "Australia")
  asia_pacific    <- c("Singapore", "Japan", "South Korea", "China", "India", "Hong Kong")
  crypto_europe   <- c("Switzerland", "Estonia", "Malta")
  x <- trimws(country)
  factor(
    dplyr::case_when(
      x %in% english_markets ~ "English",
      x %in% asia_pacific    ~ "Asia",
      x %in% crypto_europe   ~ "Crypto_Europe",
      TRUE                   ~ "Other"
    ),
    levels = c("Asia", "Crypto_Europe", "English", "Other")
  )
}

clean_ico_data <- function(raw, config = cfg) {
  assert_columns(raw, config$required_cols, "ICO dataset")
  ico <- raw

  # Leakage: variables only observable after the campaign closes
  ico <- ico[, !(names(ico) %in% config$leakage_cols), drop = FALSE]

  # Binary flags
  for (col in c("whitelist", "kyc", "bonus", "ERC20")) {
    ico[[paste0(col, "_bin")]] <- standardise_binary(ico[[col]])
    ico[[col]] <- NULL
  }
  mvp <- trimws(tolower(ico$mvp))
  ico$mvp_bin <- ifelse(is.na(ico$mvp), 0L,
                        ifelse(mvp %in% c("available", "yes", "present"), 1L, NA_integer_))
  ico$mvp <- NULL

  # URL presence
  for (col in c("link_white_paper", "linkedin_link", "github_link", "website")) {
    ico[[paste0(col, "_present")]] <- as.integer(!is.na(ico[[col]]))
    ico[[col]] <- NULL
  }

  # Dates -> durations
  dt <- lapply(ico[c("ico_start", "ico_end", "pre_ico_start", "pre_ico_end")],
               lubridate::dmy, quiet = TRUE)
  dur <- as.numeric(dt$ico_end - dt$ico_start)
  ico$ico_duration <- ifelse(dur < 0 | dur > 500, NA, dur)   # implausible -> missing

  ico$has_presale <- as.integer(!is.na(dt$pre_ico_start))
  pre <- as.numeric(dt$pre_ico_end - dt$pre_ico_start)
  pre <- ifelse(ico$has_presale == 1L, pre, 0)
  ico$presale_duration <- ifelse(!is.na(pre) & pre < 0, 0, pre)
  ico$ico_year <- lubridate::year(dt$ico_start)
  ico[c("ico_start", "ico_end", "pre_ico_start", "pre_ico_end")] <- NULL

  ico$country_group <- group_country(ico$country)
  ico$country <- NULL

  # Numeric corrections
  ico$distributed_in_ico <- ifelse(ico$distributed_in_ico > 1,
                                   ico$distributed_in_ico / 100, ico$distributed_in_ico)
  ico$price_usd <- ifelse(ico$price_usd < 0.0001, NA, ico$price_usd)

  # Composite features
  ico$digital_presence_score <- ico$github_link_present + ico$linkedin_link_present +
    ico$website_present + ico$link_white_paper_present
  ico$compliance_score <- ifelse(is.na(ico$whitelist_bin), 0, ico$whitelist_bin) +
    ifelse(is.na(ico$kyc_bin), 0, ico$kyc_bin)

  ico[[config$target]] <- factor(ico[[config$target]], levels = c(0, 1),
                                 labels = c("Fail", config$positive))
  ico
}

# Stratified hold-out split. Returns list(train, test) of cleaned (not yet imputed) rows.
split_ico <- function(ico, config = cfg) {
  set.seed(config$seed)
  idx <- caret::createDataPartition(ico[[config$target]], p = config$train_prop, list = FALSE)
  list(train = ico[idx, ], test = ico[-idx, ])
}
