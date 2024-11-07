# library(here)
# library(hms)
# library(lubridate)
# library(moments)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(purrr)
# library(rutils) # github.com/danielvartan/rutils
# library(stats)
# library(tidyr)

source(here::here("R/utils.R"))
source(here::here("R/utils-stats.R"))

stats_sum <- function(x,
                      threshold = hms::parse_hms("12:00:00"),
                      na_rm = TRUE,
                      remove_outliers = FALSE,
                      iqr_mult = 1.5,
                      hms_format = TRUE,
                      print = TRUE) {
  prettycheck:::assert_atomic(x)
  prettycheck:::assert_hms(
    threshold, lower = hms::hms(0), upper = hms::parse_hms("23:59:59"),
    null_ok = TRUE
  )
  prettycheck:::assert_flag(na_rm)
  prettycheck:::assert_flag(remove_outliers)
  prettycheck:::assert_number(iqr_mult)
  prettycheck:::assert_flag(hms_format)
  prettycheck:::assert_flag(print)

  is_temporal <- x |> prettycheck:::test_temporal()
  tz <- ifelse(lubridate::is.POSIXt(x), lubridate::tz(x), "UTC")

  if (prettycheck:::test_temporal(x)) {
    if (lubridate::is.POSIXt(x)) {
      x <- x |> as.numeric()
    } else {
      x <- x |> transform_time(threshold = threshold)
    }
  }

  if (isTRUE(remove_outliers)) {
    x <- x |> remove_outliers(method = "iqr", iqr_mult = iqr_mult)
  }

  out <- list(
    n = length(x),
    n_rm_na = length(x[!is.na(x)]),
    n_na = length(x[is.na(x)])
  )

  if (is.numeric(x)) {
    out <-
      out |>
      append(list(
        mean = mean(x, na.rm = na_rm),
        var = stats::var(x, na.rm = na_rm),
        sd = stats::sd(x, na.rm = na_rm),
        min = rutils:::clear_names(stats::quantile(x, 0, na.rm = na_rm)),
        q_1 = rutils:::clear_names(stats::quantile(x, 0.25, na.rm = na_rm)),
        median = rutils:::clear_names(stats::quantile(x, 0.5,
                                                      na.rm = na_rm)),
        q_3 = rutils:::clear_names(stats::quantile(x, 0.75, na.rm = na_rm)),
        max = rutils:::clear_names(stats::quantile(x, 1, na.rm = na_rm)),
        iqr = IQR(x, na.rm = na_rm),
        skewness = moments::skewness(x, na.rm = na_rm),
        kurtosis = moments::kurtosis(x, na.rm = na_rm)
      ))
  }

  if (is_temporal && isTRUE(hms_format)) {
    if (test_timeline_link(x)) {
      out <- purrr::map(out, ~ hms::as_hms(lubridate::as_datetime(.x, tz = tz)))
    } else {
      out <- purrr::map(out, hms::hms)
    }

    out$n <- length(x)
    out$n_rm_na <- length(x[!is.na(x)])
    out$n_na <- length(x[is.na(x)])
    out$skewness <- moments::skewness(x, na.rm = na_rm)
    out$kurtosis <- moments::kurtosis(x, na.rm = na_rm)
  }

  if (isTRUE(print)) {
    out |>
      list_as_tibble() |>
      print()
  }

  if (!is.numeric(x) && !is_temporal) {
    out <-
      out |>
      append(list(
        count = dplyr::tibble(col = x) |> dplyr::count(col)
      ))

    if (print == TRUE) {
      cli::cat_line()
      out$count |> print()
    }
  }

  invisible(out)
}
