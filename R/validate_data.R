# library(checkmate)
# library(cli)
# library(dplyr)
# library(here)
# library(hms)
# library(lubridate)
# library(lubritime) # github.com/danielvartan/lubritime
# library(mctq)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(purrr)
# library(rlang)
# library(rutils) # github.com/danielvartan/rutils
# library(scaler) # github.com/danielvartan/scaler
# library(stringr)
# library(tidyr)

source(here::here("R", "get_lookup_data.R"))

# library(checkmate)
# library(cli)

#' Validate `tidy_data()` output
#'
#' @description
#'
#' `validate_data()` validates the output of `tidy_data()`.
#'
#' @details
#'
#' Here, the process of _validating_ a dataset is understood as detecting
#' invalid data, by checking whether data satisfies certain assumptions from
#' domain knowledge, to then,  removing or, if possible, fixing them. You can
#' find more about data validation and error location in Loo & Jonge (2018).
#'
#' This process can be considered as part of the process of transforming data,
#' described in the workflow proposed by Wickham & Grolemund (n.d.).
#'
#' @param data A [`tibble`][dplyr::tibble()] with the `tidy_data()`
#'   output.
#'
#' @return An invisible [`tibble`][dplyr::tibble()] with a validated dataset.
#'
#' @family data munging functions
#' @importFrom rlang := !!
#' @noRd
#'
#' @references
#'
#' Loo, M. van der, & Jonge, E de. (2018). _Statistical data cleaning with
#' applications in R_. John Wiley & Sons. \doi{10.1002/9781118897126}
#'
#' Wickham, H., & Grolemund, G. (n.d.). _R for data science_. (n.p.).
#' \url{https://r4ds.had.co.nz}
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("utils")) {
#'   validated_data <- tidy_data |> validate_data()
#'   utils::View(validated_data)
#' }
#' }
validate_data <- function(data) {
  checkmate::assert_tibble(data)

  cli::cli_progress_step("Validating data")

  data |>
    rm_test_cases() |>
    rm_invalid_cases_detected_manually() |>
    na_mctq_blank_cases() |>
    validate_emails() |>
    validate_ranges() |>
    validate_work_study() |>
    fix_bt_sprep_inversion() |>
    validate_sdu() |>
    validate_so() |>
    remove_duplicates_and_blanks()
}

# library(checkmate)
# library(dplyr)
# library(stringr)

rm_test_cases <- function(data) {
  checkmate::assert_tibble(data)

  pattern <- "^teste$|^teste | teste$"

  # `dplyr::filter(is.na(track))` preserve cases with `NA` values in `track`.
  data |>
    dplyr::filter(!track == "teste" | is.na(track)) |>
    dplyr::filter(
      !stringr::str_detect(
        string = name |> groomr::to_ascii() |> tolower(),
        pattern = pattern
      )
    )
}

# library(checkmate)
# library(dplyr)
# library(stringr)

source(here::here("R", "get_lookup_data.R"))

# These cases were detected via manual inspection of the data.
# Example: Cases when a respondent indicated that they were doing a
# test by including the word "test" in the "name" field.

rm_invalid_cases_detected_manually <- function(data) { #nolint
  checkmate::assert_tibble(data)

  cols <-
    get_lookup_data() |>
    names() |>
    stringr::str_subset(
      pattern = "^geocodes$|^special_cases$",
      negate = TRUE
    )

  pattern <- "(?i)^R E M O V E$"

  data |>
    dplyr::filter(
      !dplyr::if_any(
        .cols = dplyr::all_of(cols),
        .fns = ~ ifelse(is.na(.x), FALSE, stringr::str_detect(.x, pattern))
      )
    )
}

# library(checkmate)
# library(dplyr)
# library(hms)
# library(lubridate)
# library(rutils) # github.com/danielvartan/rutils

na_mctq_blank_cases <- function(data) {
  checkmate::assert_tibble(data)

  data |>
    dplyr::mutate(
      dummy = dplyr::case_when(
        wd == 0 &
          bt_w == hms::hms(0) & sprep_w == hms::hms(0) &
          slat_w == lubridate::dminutes(0) & se_w == hms::hms(0) &
          si_w == lubridate::dminutes(0) &
          bt_f == hms::hms(0) & sprep_f == hms::hms(0) &
          slat_f == lubridate::dminutes(0) & se_f == hms::hms(0) &
          si_f == lubridate::dminutes(0)
        ~ TRUE,
        TRUE ~ FALSE
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::matches("^wd$|_w$|_f$"),
        .fns = ~ dplyr::if_else(dummy, rutils::na_as(.x), .x)
      )
    ) |>
    dplyr::select(-dummy)
}

# library(checkmate)
# library(dplyr)
# library(stringr)

validate_emails <- function(data) {
  checkmate::assert_tibble(data)

  data |>
    dplyr::mutate(
      email = dplyr::case_when(
        stringr::str_detect(
          email,
          "^[[:alnum:]._-]+@[[:alnum:].-]+$"
        ) ~ email
      )
    )
}

# library(checkmate)
# library(dplyr)
# library(hms)
# library(scaler) # github.com/danielvartan/scaler

validate_ranges <- function(data) {
  checkmate::assert_tibble(data)

  data |>
    dplyr::mutate(
      birth_date = dplyr::case_when(
        dplyr::between(
          scaler::age(birth_date, timestamp), 12, 80
        ) ~ birth_date
      ),
      height = dplyr::case_when(
        dplyr::between(height, 1, 3) ~ height
      ),
      weight = dplyr::case_when(
        dplyr::between(weight, 30, 300) ~ weight
      ),
      wd = dplyr::case_when(
        dplyr::between(wd, 0, 7) ~ wd
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = c(
          "bt_w", "sprep_w", "se_w", "bt_f", "sprep_f", "se_f"
        ),
        .fns = ~ dplyr::case_when(
          .x == hms::parse_hm("24:00") ~ hms::parse_hm("00:00"),
          .x >= hms::parse_hm("00:00") &
            .x < hms::parse_hm("24:00") ~ .x
        )
      ),
      dplyr::across(
        .cols = c("slat_w", "si_w", "slat_f", "si_f"),
        .fns = ~ dplyr::case_when(
          dplyr::between(as.numeric(.x), 0,
                         as.numeric(lubridate::dhours(6))) ~ .x
        )
      ),
      dplyr::across(
        .cols = c("le_w", "le_f"),
        .fns = ~ dplyr::case_when(
          dplyr::between(as.numeric(.x), 0,
                         as.numeric(lubridate::dhours(12))) ~ .x
        )
      )
    )
}

# library(checkmate)
# library(dplyr)
# library(purrr)
library(rlang)
# library(stringr)
# library(tidyr)

validate_work_study <- function(data){
  checkmate::assert_tibble(data)

  out <-
    data |>
    dplyr::mutate(
      work = dplyr::if_else(no_work_no_study, FALSE, work),
      study = dplyr::if_else(no_work_no_study, FALSE, study)
    ) |>
    dplyr::mutate(
      wd = dplyr::case_when(
        (work == FALSE & study == FALSE) & is.na(wd) ~ as.integer(0),
        no_work_no_study == TRUE ~ as.integer(0),
        TRUE ~ wd
      ),
      dummy = dplyr::case_when(
        (work == FALSE | study == FALSE) & wd > 0 ~ TRUE,
        TRUE ~ FALSE
      ),
      work = dplyr::if_else(dummy, as.logical(NA), work),
      study = dplyr::if_else(dummy, as.logical(NA), study)
    ) |>
    dplyr::select(-dummy, -no_work_no_study)

  out <-
    out |>
    dplyr::mutate(
      dummy = dplyr::if_else(work == FALSE & study == FALSE, TRUE, FALSE)
    ) |>
    tidyr::unnest(cols = c(work_periods, study_periods))

  for (i in stringr::str_subset(names(out), "^work_|^study_")) {
    out <-
      out |>
      dplyr::mutate(
        !!as.symbol(i) := dplyr::if_else(dummy, FALSE, !!as.symbol(i))
      )
  }

  for (i in stringr::str_subset(names(out), "^work_")) {
    out <-
      out |>
      dplyr::mutate(
        !!as.symbol(i) := dplyr::if_else(work, !!as.symbol(i), FALSE)
      )
  }

  for (i in stringr::str_subset(names(out), "^study_")) {
    out <-
      out |>
      dplyr::mutate(
        !!as.symbol(i) := dplyr::if_else(study, !!as.symbol(i), FALSE)
      )
  }

  out |>
    tidyr::nest(
      work_periods = c(
        work_morning, work_afternoon, work_evening, work_wee_hours
      ),
      study_periods = c(
        study_morning, study_afternoon, study_evening, study_wee_hours
      )
    ) |>
    dplyr::relocate(work_periods, .after = study) |>
    dplyr::relocate(study_periods, .after = work_periods)|>
    dplyr::select(-dummy) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(c("work_periods", "study_periods")),
        .fns = ~ .x |>
          purrr::map(
            .f = ~ .x |>
              dplyr::rename_with(
                .fn = ~ c("morning", "afternoon", "evening", "wee_hours")
              )
          )
      )
    )
}

# library(checkmate)
# library(dplyr)
# library(lubridate)
# library(lubritime) # github.com/danielvartan/lubritime
library(rlang)

fix_bt_sprep_inversion <- function(data) {
  checkmate::assert_tibble(data)

  for (i in c("_w", "_f")) {
    bt_i <- paste0("bt", i)
    sprep_i <- paste0("sprep", i)

    out <-
      data |>
      dplyr::mutate(
        dummy = dplyr::case_when(
          lubritime::assign_date(!!as.symbol(bt_i), !!as.symbol(sprep_i)) >
            lubridate::dhours(12) ~ TRUE,
          TRUE ~ FALSE
          ),
        bkp = !!as.symbol(bt_i),
        !!as.symbol(bt_i) := dplyr::if_else(
          dummy,
          !!as.symbol(sprep_i),
          !!as.symbol(bt_i)
          ),
        !!as.symbol(sprep_i) := dplyr::if_else(
          dummy,
          bkp,
          !!as.symbol(sprep_i)
          )
        ) |>
      dplyr::select(-dummy, -bkp)
  }

  out
}

# library(checkmate)
# library(dplyr)
# library(lubridate)
# library(mctq)
library(rlang)
# library(rutils) # github.com/danielvartan/rutils

validate_sdu <- function(data) {
  checkmate::assert_tibble(data)

  for (i in c("_w", "_f")) {
    sprep_i <- paste0("sprep", i)
    slat_i <- paste0("slat", i)
    se_i <- paste0("se", i)

    test <-
      data |>
      dplyr::mutate(
        so_i = mctq::so(!!as.symbol(sprep_i), !!as.symbol(slat_i)),
        sd_i = mctq::sdu(so_i, !!as.symbol(se_i)),
        dummy = dplyr::case_when(
          sd_i < lubridate::dhours(4) |
            sd_i > lubridate::dhours(14) ~ TRUE,
          TRUE ~ FALSE)
      ) |>
      dplyr::select(dummy)

    out <-
      data |>
      dplyr::bind_cols(test) |>
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::matches(paste0("^wd$|", i, "$")),
          .fns = ~ dplyr::if_else(dummy, rutils::na_as(.x), .x)
        )
      ) |>
      dplyr::select(-dummy)
  }

  out
}

# library(checkmate)
# library(dplyr)
# library(hms)
# library(mctq)
# library(rutils) # github.com/danielvartan/rutils

validate_so <- function(data) {
  checkmate::assert_tibble(data)

  data |>
    dplyr::mutate(
      so_w = mctq::so(sprep_w, slat_w),
      so_f = mctq::so(sprep_f, slat_f),
      dummy = dplyr::case_when(
        (so_w >= hms::parse_hms("06:00:00") &
           so_w <= hms::parse_hms("18:00:00")) |
          (so_f >= hms::parse_hms("06:00:00") &
             so_f <= hms::parse_hms("18:00:00")) ~ TRUE,
        TRUE ~ FALSE)
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::matches("^wd$|_w$|_f$"),
        .fns = ~ dplyr::if_else(dummy, rutils::na_as(.x), .x)
      )
    ) |>
    dplyr::select(-so_w, -so_f, -dummy)
}

# library(checkmate)
# library(dplyr)

remove_duplicates_and_blanks <- function(data) {
  checkmate::assert_tibble(data)

  count <-
    data |>
    dplyr::mutate(
      dplyr::across(
        .col = dplyr::everything(),
        .fns = as.character
      )
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      length = rutils:::count_not_na(
        dplyr::c_across(cols = -dplyr::all_of(c("id", "timestamp", "track")))
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(length)

  data |>
    dplyr::bind_cols(count) |>
    dplyr::arrange(dplyr::desc(length)) |>
    dplyr::group_by(email, birth_date) |>
    dplyr::mutate(dup_rank = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      email_birth_dup = dup_rank > 1,
      email_birth_dup = dplyr::case_when(
        is.na(email) | is.na(birth_date) ~ FALSE,
        TRUE ~ email_birth_dup
      )
    ) |>
    dplyr::group_by(name, birth_date) |>
    dplyr::mutate(dup_rank = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      name_birth_dup = dup_rank > 1,
      name_birth_dup = dplyr::case_when(
        is.na(name) | is.na(birth_date) ~ FALSE,
        TRUE ~ name_birth_dup
      ),
      dup = email_birth_dup | name_birth_dup
    ) |>
    dplyr::select(-dup_rank, -email_birth_dup, -name_birth_dup) |>
    dplyr::filter(!dup == TRUE, length > 8) |>
    dplyr::select(-dup, -length) |>
    dplyr::arrange(id)
}
