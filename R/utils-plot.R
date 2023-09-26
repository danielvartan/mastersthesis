# library(checkmate, quietly = TRUE)
# library(hms, quietly = TRUE)
# library(lubritime, quietly = TRUE)
# library(rutils, quietly = TRUE)

labels_hms <- function(x, type = "even") {
  classes <- c("numeric", "Duration", "difftime", "hms", "POSIXct",
               "POSIXlt", "Interval")

  checkmate::assert_multi_class(x, classes)

  if (hms::is_hms(x)) out <- lubritime:::fix_hms(x)

  out <-
    out |>
    hms::as_hms() |>
    substr(1, 5)

  if (!is.null(type)) out <- out |> rutils:::label_jump(type = type)

  out
}

# library(checkmate, quietly = TRUE)
# library(hms, quietly = TRUE)
# library(rutils, quietly = TRUE)

labels_char_hms <- function(x) {
  checkmate::assert_character(x, pattern = "^\\d{2}:\\d{2}:\\d{2}$")

  x |>
    hms::as_hms() |>
    rutils:::label_jump(type = "even")
}

# library(checkmate, quietly = TRUE)

label_decimal_fix <- function(x){
  checkmate::assert_character(x)

  out <- x[which(grepl("0$", x))]
  min_end_digit <- as.numeric(stringr::str_extract(x[1], ".$"))
  max_end_digit <- as.numeric(stringr::str_extract(x[length(x)], ".$"))

  if (min_end_digit <= 5) {
    out <- c(x[1], out)
  }

  if (max_end_digit <= 5) {
    out <- c(out, x[length(x)])
  }

  out
}

# library(checkmate, quietly = TRUE)
# library(here, quietly = TRUE)
# library(hms, quietly = TRUE)
# library(lubritime, quietly = TRUE)
library(magrittr, quietly = TRUE)
# library(mctq, quietly = TRUE)
# library(rutils, quietly = TRUE)
# library(stats, quietly = TRUE)

source(here::here("R/test_normality.R"))

summarise_inline <- function(data,
                             col,
                             x_label = col,
                             test_norm = FALSE) {
  classes <- c("numeric", "Duration", "difftime", "hms", "POSIXct",
               "POSIXlt", "Interval")

  checkmate::assert_tibble(data)
  checkmate::assert_choice(col, names(data))
  checkmate::assert_string(x_label)
  checkmate::assert_flag(test_norm)
  checkmate::assert_multi_class(data[[col]], classes)

  # R CMD Check variable bindings fix (see: https://bit.ly/3z24hbU) -----
  . <- NULL

  n <- paste0(
    "n = ",
    prettyNum(length(which(!(is.na(data[[col]])))), big.mark = ",")
  )

  if (rutils:::test_temporal(data[[col]], rm = "Period")) {
    data[[col]] <- mctq:::extract_seconds(data[[col]])
  }

  if (isTRUE(test_norm)) {
    test <- data |> test_normality(col)

    if ("test_shapiro" %in% names(test)) {
      value <-
        test$test_shapiro$p.value |>
        round(digits = 3) |>
        format(nsmall = 3, scientific = FALSE)

      if (test$test_shapiro$p.value > 0.05) {
        test <- paste0(
          "SW p-value \u2248 ",
          value,

          " (Can assume normality)"
          )
      } else {
        test <- paste0(
          "SW p-value \u2248 ",
          value,
          " (Cannot assume normality)"
          )
      }
    } else if ("test_lcks" %in% names(test)) {
      value <-
        test$test_lcks$p.value |>
        round(digits = 3) |>
        format(nsmall = 3, scientific = FALSE)

      if (test$test_lcks$p.value > 0.05) {
        test <- paste0(
          "LcKS p-value \u2248 ",
          value,

          " (Can assume normality)"
          )
      } else {
        test <- paste0(
          "LcKS p-value \u2248 ",
          value,

          " (Cannot assume normality)"
          )
      }
    }
  }

  mean <-
    data[[col]] |>
    mean(na.rm = TRUE) |>
    lubritime::cycle_time(lubridate::dhours(24)) |>
    round() |>
    hms::hms() %>% # Don't change the pipe.
    paste0("Mean = ", .)

  sd <-
    data[[col]] |>
    stats::sd(na.rm = TRUE) |>
    lubritime::cycle_time(lubridate::dhours(24)) |>
    round() |>
    hms::hms() %>% # Don't change the pipe.
    paste0("SD = ", .)

  if (isFALSE(test_norm) && !(is.na(mean)) && !(is.na(sd))) {
    paste0(x_label, ": ", n, " | ",  mean, " | ",  sd)
  } else if (!(is.na(mean)) && !(is.na(sd)) && !(is.na(test))) {
    paste0(x_label, ": ", n, " | ",  mean, " | ",  sd, " | ", test)
  } else {
    paste0(x_label, ": ", n)
  }
}

# library(checkmate, quietly = TRUE)
# library(cli, quietly = TRUE)
# library(lubridate, quietly = TRUE)
# library(stringr, quietly = TRUE)

transform_cut_levels <- function(x, tz = "UTC") {
  pattern <- "^[\\(\\[][\\d.e+]+,[\\d.e+]+[\\)\\]]$"

  checkmate::assert_character(x, min.len = 1)
  checkmate::assert_choice(tz, OlsonNames())

  if (!all(stringr::str_detect(x, pattern))) {
    cli::cli_abort(paste0(
      "{.strong {cli::col_red('x')}} must follow the pattern ",
      "{.strong {cli::col_blue(pattern)}}."
    ))
  }

  int_start <-
    stringr::str_extract(x, "(?<=[\\(\\[])[\\d.e+]+") |>
    as.numeric() |>
    lubridate::as_datetime()

  int_end <-
    stringr::str_extract(x, "[\\d.e+]+(?=[\\)\\]])") |>
    as.numeric() |>
    lubridate::as_datetime()

  lubridate::interval(
    int_start + lubridate::dmilliseconds(1),
    int_end,
    tz
  )
}

# library(checkmate, quietly = TRUE)
# library(stringr, quietly = TRUE)

cut_mean <- function(x, round = TRUE) {
  checkmate::assert_multi_class(x, c("character", "factor"))

  if (is.factor(x)) x <- as.character(x)

  left <-
    x |>
    stringr::str_extract("\\d+?\\.?\\d+(?=,)") |>
    as.numeric()

  right <-
    x |>
    stringr::str_extract("\\d+\\.?\\d*(?=\\D*\\]$)") |>
    as.numeric()

  out <- mapply(function(x, y) mean(c(x, y), na.rm = TRUE), left, right)

  if (isTRUE(round)) {
    round(out)
  } else {
    out
  }
}
