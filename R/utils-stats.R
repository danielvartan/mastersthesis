# library(cli)
# library(prettycheck) # https://github.com/danielvartan/prettycheck

extract_from_summary <- function(x, element = "r.squared") {
  prettycheck:::assert_list(x)
  prettycheck:::assert_string(element)

  summary <- summary(x)

  if (is.null(names(summary))) {
    cli::cli_abort(paste0(
      "{.strong {cli::col_red('summary(x)')}} has no names."
    ))
  }

  if (!element %in% names(summary)) {
    cli::cli_abort(paste0(
      "There is no element named {.strong {cli::col_red(element)}} in ",
      "{.strong summary(x)}."
    ))
  }

  summary(x)[[element]]
}

r_squared <- function(x) extract_from_summary(x, "r.squared")
adj_r_squared <- function(x) extract_from_summary(x, "adj.r.squared")
f_statistic <- function(x) extract_from_summary(x, "fstatistic")

# library(stats)
# library(prettycheck) # https://github.com/danielvartan/prettycheck

std_error <- function(x){
  prettycheck:::assert_numeric(x)

  stats::sd(x, na.rm = TRUE) / sqrt(length(x))
}

# library(dplyr)
# library(prettycheck) # https://github.com/danielvartan/prettycheck
# library(stats)

# TODO: Move to `rutils`.
is_outlier <- function( # Change name to `test_outlier`
    x,
    method = "iqr",
    iqr_mult = 1.5,
    sd_mult = 3
) {
  prettycheck:::assert_numeric(x)
  prettycheck:::assert_choice(method, c("iqr", "sd"))
  prettycheck:::assert_number(iqr_mult)
  prettycheck:::assert_number(sd_mult)

  if (method == "iqr") {
    iqr <- stats::IQR(x, na.rm = TRUE)
    min <- stats::quantile(x, 0.25, na.rm = TRUE) - (iqr_mult * iqr)
    max <- stats::quantile(x, 0.75, na.rm = TRUE) + (iqr_mult * iqr)
  } else if (method == "sd") {
    min <- mean(x, na.rm = TRUE) - (sd_mult * stats::sd(x, na.rm = TRUE))
    max <- mean(x, na.rm = TRUE) + (sd_mult * stats::sd(x, na.rm = TRUE))
  }

  dplyr::if_else(x >= min & x <= max, FALSE, TRUE, missing = FALSE)
}

# library(prettycheck) # https://github.com/danielvartan/prettycheck

# TODO: Move to `rutils`.
remove_outliers <- function(
    x,
    method = "iqr",
    iqr_mult = 1.5,
    sd_mult = 3
) {
  prettycheck:::assert_numeric(x)
  prettycheck:::assert_choice(method, c("iqr", "sd"))
  prettycheck:::assert_number(iqr_mult, lower = 1)
  prettycheck:::assert_number(sd_mult, lower = 0)

  x |>
    test_outlier(
      method = method,
      iqr_mult = iqr_mult,
      sd_mult = sd_mult
    ) %>%
    `!`() %>%
    magrittr::extract(x, .)
}
