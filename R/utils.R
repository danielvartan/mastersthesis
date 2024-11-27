change_sign <- function(x) x * (-1)

# library(here)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(stringr)

to_relative_path <- function(path, root = ".") {
  prettycheck:::assert_string(path)
  prettycheck:::assert_string(root)

  path <- stringr::str_remove(path, here::here())

  paste0(root, path)
}

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

library(magrittr)
# library(prettycheck) # github.com/danielvartan/prettycheck

format_to_md_latex <- function(x, after = NULL, round = 3, key = "$") {
  prettycheck:::assert_numeric(x)
  prettycheck:::assert_string(after, null.ok = TRUE)
  prettycheck:::assert_number(round, lower = 0)
  prettycheck:::assert_string(key)

  if (is.null(after)) after <- ""

  x <-
    x |>
    round(round) |>
    format(big.mark = ",", scientific = FALSE) %>% # Don't change the pipe!
    paste0(key, ., after, key)
}

library(magrittr)
# library(prettycheck) # github.com/danielvartan/prettycheck

inverse_log_max <- function(x, base = exp(1)) {
  prettycheck:::assert_numeric(x)
  prettycheck:::assert_number(base)

  x |>
    log(base) |>
    max(na.rm = TRUE) |>
    ceiling() %>% # Don't change the pipe!
    `^`(base, .)
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

# library(prettycheck) # github.com/danielvartan/prettycheck

cli_test_fun <- function(test) {
  prettycheck:::assert_flag(test)

  if (isTRUE(test)) {
    cli::col_blue
  } else {
    cli::col_red
  }
}

# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(stringr)

# x <- c("BRT", "EST", "BRT")
# paired_vector <- c("EST" = "EST", "BRT" = "America/Sao_Paulo")
# look_and_replace_chr(x, paired_vector)
look_and_replace_chr <- function(x, paired_vector) {
  prettycheck:::assert_atomic(x)
  prettycheck:::assert_atomic(paired_vector)
  prettycheck:::assert_character(names(paired_vector), null.ok = FALSE)

  x <- stringr::str_squish(x)

  dplyr::case_when(
    x %in% names(paired_vector) ~
      paired_vector[match(x, names(paired_vector))],
    TRUE ~ NA
  ) |>
    unname()
}
