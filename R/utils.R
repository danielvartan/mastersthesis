# TODO: Refactor and move to `lubritime`.
transform_time <- function(x, threshold = hms::parse_hms("12:00:00")) {
  checkmate::assert_atomic(x)
  gutils:::assert_hms(
    threshold, lower = hms::hms(0), upper = hms::parse_hms("23:59:59"),
    null.ok = TRUE
  )

  classes <- c("Duration", "difftime", "hms", "POSIXt", "Interval")

  if (hms::is_hms(x) && !is.null(threshold)) {
    x %>%
      lubritime:::link_to_timeline(threshold = threshold) %>%
      as.numeric()
  } else if (checkmate::test_multi_class(x, classes)) {
    x %>% lubritime:::extract_seconds()
  } else {
    x
  }
}

# TODO: Try to integrate this with `lubritime:::link_to_timeline()`.
midday_trigger <- function(x, trigger = hms::parse_hm("22:00")) {
  if (hms::is_hms(x) && any(x > trigger, na.rm = TRUE)) {
    lubritime:::link_to_timeline(
      x, threshold = hms::parse_hms("12:00:00")
    )
  } else if (hms::is_hms(x) && !any(x > trigger, na.rm = TRUE)) {
    lubridate::as_datetime(x, tz = "UTC")
  } else {
    x
  }
}

# TODO: Move to `gutils`.
is_outlier <- function(x, method = "iqr", iqr_mult = 1.5, sd_mult = 3) {
  checkmate::assert_numeric(x)
  checkmate::assert_choice(method, c("iqr", "sd"))
  checkmate::assert_number(iqr_mult)
  checkmate::assert_number(sd_mult)

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

# TODO: Move to `gutils`.
remove_outliers <- function(x, method = "iqr", iqr_mult = 1.5, sd_mult = 3) {
  checkmate::assert_numeric(x)
  checkmate::assert_choice(method, c("iqr", "sd"))
  checkmate::assert_number(iqr_mult)
  checkmate::assert_number(sd_mult)

  x[!is_outlier(x, method = method, iqr_mult = iqr_mult, sd_mult = sd_mult)]
}

# TODO: Move to `lubritime`.
test_timeline_link <- function(x, tz = "UTC") {
  checkmate::assert_multi_class(x, c("numeric", "POSIXt"))
  checkmate::assert_choice(tz, OlsonNames())

  x <- x %>% gutils:::drop_na()

  if (is.numeric(x)) x <- x %>% lubridate::as_datetime(tz = tz)

  dates <- x %>%
    lubridate::date() %>%
    unique()

  if (((lubridate::as_date("1970-01-01") %in% dates) &&
       length(dates) == 2) ||
      ((lubridate::as_date("1970-01-02") %in% dates) &&
       length(dates) == 1)) {
    TRUE
  } else {
    FALSE
  }
}

# TODO: Move to `gutils`.
count_not_na <- function(x) {
  checkmate::assert_atomic(x)

  length(which(!is.na(x)))
}

# TODO: Move to `gutils`.
drop_inf <- function(x) {
  checkmate::assert_atomic(x)

  x[!(x == -Inf | x == Inf)]
}