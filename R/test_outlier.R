# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(stats)

test_outlier <- function(
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

  dplyr::if_else(x > min & x < max, FALSE, TRUE, missing = FALSE)
}
