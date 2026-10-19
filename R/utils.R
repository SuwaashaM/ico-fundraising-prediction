required_packages <- c(
  "dplyr", "lubridate", "caret", "C50", "class", "naivebayes", "kernlab",
  "pROC", "ggplot2", "reshape2", "ggrepel"
)

# Fail early with a clear message instead of installing packages mid-run.
load_packages <- function(pkgs = required_packages) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop("Missing R packages: ", paste(missing, collapse = ", "),
         ".\nRun: Rscript install_dependencies.R", call. = FALSE)
  }
  suppressPackageStartupMessages(
    for (p in c("caret", "C50", "class", "naivebayes", "kernlab", "pROC", "ggplot2")) {
      library(p, character.only = TRUE)
    }
  )
  invisible(TRUE)
}

assert_columns <- function(df, cols, what = "data") {
  absent <- setdiff(cols, names(df))
  if (length(absent) > 0) {
    stop(sprintf("%s is missing required column(s): %s", what, paste(absent, collapse = ", ")),
         call. = FALSE)
  }
  invisible(TRUE)
}

log_msg <- function(...) cat(format(Sys.time(), "[%H:%M:%S] "), sprintf(...), "\n", sep = "")

ensure_dir <- function(...) {
  d <- file.path(...)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

# Map assorted yes/no encodings to 1/0; anything unrecognised becomes NA.
standardise_binary <- function(x) {
  yes_vals <- c("yes", "y", "true", "t", "1", "1.0", "available", "present")
  no_vals  <- c("no", "n", "false", "f", "0", "0.0")
  v <- trimws(tolower(as.character(x)))
  ifelse(v %in% yes_vals, 1L, ifelse(v %in% no_vals, 0L, NA_integer_))
}
