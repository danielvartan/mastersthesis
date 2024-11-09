# library(hms)
# library(lubritime) # github.com/danielvartan/lubritime
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils

# TODO: Refactor and move to `lubritime`.
transform_time <- function(x, threshold = hms::parse_hms("12:00:00")) {
  prettycheck:::assert_atomic(x)
  prettycheck:::assert_hms(
    threshold,
    lower = hms::hms(0),
    upper = hms::parse_hms("23:59:59"),
    null_ok = TRUE
  )

  classes <- c("Duration", "difftime", "hms", "POSIXt", "Interval")

  if (hms::is_hms(x) && !is.null(threshold)) {
    x |>
      lubritime:::link_to_timeline(threshold = threshold) |>
      as.numeric()
  } else if (prettycheck:::test_multi_class(x, classes)) {
    x |> lubritime:::extract_seconds()
  } else {
    x
  }
}

# library(hms)
# library(lubridate)
# library(lubritime) # github.com/danielvartan/lubritime
# library(rutils) # github.com/danielvartan/rutils

# TODO: Try to integrate this with `lubritime:::link_to_timeline()`.
midday_trigger <- function(x, trigger = hms::parse_hm("22:00")) {
  prettycheck:::assert_hms(trigger)

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

# library(lubridate)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils

# TODO: Move to `lubritime`.
test_timeline_link <- function(x, tz = "UTC") {
  prettycheck:::assert_multi_class(x, c("numeric", "POSIXt"))
  prettycheck:::assert_choice(tz, OlsonNames())

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

# library(prettycheck) # github.com/danielvartan/prettycheck

# TODO: Move to `rutils`.
count_not_na <- function(x) {
  prettycheck:::assert_atomic(x)

  length(which(!is.na(x)))
}

# library(prettycheck) # github.com/danielvartan/prettycheck

# TODO: Move to `rutils`.
drop_inf <- function(x) {
  prettycheck:::assert_atomic(x)

  x[!(x == -Inf | x == Inf)]
}

# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(tidyr)

list_as_tibble <- function(list) {
  prettycheck:::assert_list(list)

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

# library(prettycheck) # github.com/danielvartan/prettycheck

to_ascii <- function(x, from = "UTF-8") {
  prettycheck:::assert_character(x)
  prettycheck:::assert_string(from)

  x |> iconv(to = "ASCII//TRANSLIT")
}

# library(prettycheck) # github.com/danielvartan/prettycheck

to_ascii_and_lower <- function(x, from = "UTF-8") {
  prettycheck:::assert_character(x)
  prettycheck:::assert_string(from)

  x |> to_ascii(from) |> tolower()
}

# library(prettycheck) # github.com/danielvartan/prettycheck

vector_to_c <- function(x, quote = TRUE, clipboard = TRUE) {
  prettycheck:::assert_atomic(x)

  if (isTRUE(quote)) x <- paste0('"', x, '"')

  out <- paste0("c(", paste(x, collapse = ", "), ")")

  if (isTRUE(clipboard)) {
    cli::cli_alert_info("Copied to clipboard.")

    utils::writeClipboard(out)
  }

  cat(out)

  invisible(out)
}
