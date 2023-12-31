# library(checkmate, quietly = TRUE)
# library(hms, quietly = TRUE)
# library(lubritime, quietly = TRUE)
# library(rutils, quietly = TRUE)

# TODO: Refactor and move to `lubritime`.
transform_time <- function(x, threshold = hms::parse_hms("12:00:00")) {
  checkmate::assert_atomic(x)
  rutils:::assert_hms(
    threshold,
    lower = hms::hms(0),
    upper = hms::parse_hms("23:59:59"),
    null.ok = TRUE
  )

  classes <- c("Duration", "difftime", "hms", "POSIXt", "Interval")

  if (hms::is_hms(x) && !is.null(threshold)) {
    x |>
      lubritime:::link_to_timeline(threshold = threshold) |>
      as.numeric()
  } else if (checkmate::test_multi_class(x, classes)) {
    x |> lubritime:::extract_seconds()
  } else {
    x
  }
}

# library(hms, quietly = TRUE)
# library(lubridate, quietly = TRUE)
# library(lubritime, quietly = TRUE)
# library(rutils, quietly = TRUE)

# TODO: Try to integrate this with `lubritime:::link_to_timeline()`.
midday_trigger <- function(x, trigger = hms::parse_hm("22:00")) {
  rutils:::assert_hms(trigger)

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

# library(checkmate, quietly = TRUE)
# library(lubridate, quietly = TRUE)
# library(rutils, quietly = TRUE)

# TODO: Move to `lubritime`.
test_timeline_link <- function(x, tz = "UTC") {
  checkmate::assert_multi_class(x, c("numeric", "POSIXt"))
  checkmate::assert_choice(tz, OlsonNames())

  x <- x |> rutils:::drop_na()

  if (is.numeric(x)) x <- x |> lubridate::as_datetime(tz = tz)

  dates <-
    x |>
    lubridate::date() |>
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

# library(checkmate, quietly = TRUE)

# TODO: Move to `rutils`.
count_not_na <- function(x) {
  checkmate::assert_atomic(x)

  length(which(!is.na(x)))
}

# library(checkmate, quietly = TRUE)

# TODO: Move to `rutils`.
drop_inf <- function(x) {
  checkmate::assert_atomic(x)

  x[!(x == -Inf | x == Inf)]
}

# library(checkmate, quietly = TRUE)
# library(dplyr, quietly = TRUE)
# library(tidyr, quietly = TRUE)

list_as_tibble <- function(list) {
  checkmate::assert_list(list)

  list |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::everything(),
        .fns = as.character
      )
    ) |>
    tidyr::pivot_longer(cols = dplyr::everything())
}
