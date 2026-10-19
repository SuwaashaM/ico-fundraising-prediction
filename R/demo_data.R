# Synthetic stand-in for the coursework dataset (which cannot be redistributed).
# It mimics the raw schema, messy encodings and missingness so the whole pipeline
# can be run and tested end to end. Results on it are NOT meaningful.

make_demo_ico_data <- function(n = 1200, seed = 42) {
  set.seed(seed)
  pick <- function(x, p = NULL) sample(x, n, replace = TRUE, prob = p)
  na_at <- function(x, rate) { x[stats::runif(n) < rate] <- NA; x }

  start <- as.Date("2016-01-01") + sample(0:1400, n, replace = TRUE)
  dur   <- sample(5:120, n, replace = TRUE)
  has_pre <- stats::runif(n) < 0.5
  pre_start <- start - sample(20:90, n, replace = TRUE)
  fmt <- function(d) format(d, "%d/%m/%Y")

  rating   <- round(stats::runif(n, 1, 5), 1)
  signal   <- scale(rating)[, 1] + stats::rnorm(n, sd = 1.4)
  goal     <- as.integer(stats::plogis(signal - 0.7) > stats::runif(n))

  data.frame(
    goal = goal,
    sold_tokens = round(stats::rlnorm(n, 15, 2)),
    whitelist = pick(c("yes", "no", "Y", "N", "TRUE", NA)),
    kyc = pick(c("yes", "no", "1", "0", NA)),
    bonus = pick(c("yes", "no", "t", "f")),
    ERC20 = pick(c("yes", "no", "1.0", "0.0")),
    mvp = na_at(pick(c("Available", "Yes")), 0.5),
    link_white_paper = na_at(rep("https://example.org/wp.pdf", n), 0.15),
    linkedin_link = na_at(rep("https://linkedin.com/company/x", n), 0.35),
    github_link = na_at(rep("https://github.com/x", n), 0.5),
    website = na_at(rep("https://example.org", n), 0.05),
    ico_start = fmt(start),
    ico_end = fmt(start + dur),
    pre_ico_start = ifelse(has_pre, fmt(pre_start), NA),
    pre_ico_end = ifelse(has_pre, fmt(pre_start + sample(5:40, n, replace = TRUE)), NA),
    country = pick(c("USA", "UK", "Singapore", "Switzerland", "Estonia", "Russia", "Germany", "India", NA)),
    distributed_in_ico = na_at(ifelse(stats::runif(n) < 0.3, stats::runif(n, 20, 90), stats::runif(n)), 0.1),
    price_usd = na_at(round(stats::rlnorm(n, -1.5, 1.2), 4), 0.2),
    rating = na_at(rating, 0.05),
    teamsize = na_at(sample(2:40, n, replace = TRUE), 0.15),
    token_for_sale = na_at(round(stats::rlnorm(n, 17, 2)), 0.25),
    stringsAsFactors = FALSE
  )
}
