# Based on:
#
# Frey, B. B. (Ed.). (2022). The SAGE encyclopedia of research design (2. ed.).
# SAGE Publications. https://doi.org/10.4135/9781071812082
cohens_d <- function(x, y, t = NULL, abs = TRUE) {
  checkmate::assert_numeric(x)
  checkmate::assert_numeric(y)
  checkmate::assert_number(t, null.ok = TRUE)
  checkmate::assert_flag(abs)

  x_n <- length(x)
  y_n <- length(y)
  df <- x_n + y_n - 2

  if (!is.null(t)) {
    # Frey (2022) | Equation 9
    out <- (t * (x_n + y_n)) / (sqrt(df) * sqrt(x_n * y_n))
  } else {
    # Frey (2022) | Equation 7
    # Only when sample sizes are equal.
    sd_pooled <- sqrt(
      (stats::var(x, na.rm = TRUE) + stats::var(y, na.rm = TRUE)) / 2
      )

    out <- (mean(x, na.rm = FALSE) - mean(y, na.rm = FALSE)) / sd_pooled
  }

  if (isTRUE(abs)) {
    abs(out)
  } else {
    out
  }
}
