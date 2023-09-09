#' Validate `tidy_field_form()` output
#'
#' @description
#'
#' `validate_micro_mctq()` validates the output of `tidy_field_form()`.
#'
#' @details
#'
#' Here, the process of _validating_ a dataset is understood as detecting
#' invalid data, by checking whether data satisfies certain assumptions from
#' domain knowledge, to then,  removing or, if possible, fixing them. You can
#' find more about data validation and error location in Loo and Jonge (2018).
#'
#' This process can be considered as part of the process of transforming data,
#' described in the workflow proposed by Wickham and Grolemund (n.d.).
#'
#' @param data a [`tibble`][tibble::tibble()] with the `tidy_field_form()`
#'   output.
#'
#' @return An invisible [`tibble`][dplyr::tibble()] with a validated
#'   `field_form` dataset.
#'
#' @inheritParams read_field_form
#' @template references_b
#' @importFrom dplyr %>%
#' @family data functions
#' @export
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("utils", quietly = TRUE)) {
#'     utils::View(validate_field_form())
#' }
#' }
validate_field_form <- function(data, write = FALSE) {
  checkmate::assert_tibble(data)
  checkmate::assert_flag(write)

  cli::cli_progress_step("Validating data")

  field_form <- data %>%
    na_mctq_blank_cases() %>%
    validate_ranges() %>%
    validate_work_study() %>%
    fix_bt_sprep_inversion() %>%
    validate_sdu() %>%
    validate_so() %>%
    remove_duplicates_and_blanks()

  if (isTRUE(write)) usethis::use_data(field_form, overwrite = TRUE)

  invisible(field_form)
}

validate_ranges <- function(field_form) {
  checkmate::assert_tibble(field_form)

  field_form <- field_form %>%
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
    ) %>%
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

  invisible(field_form)
}

validate_work_study <- function(field_form){
  checkmate::assert_tibble(field_form)

  field_form <- field_form %>%
    dplyr::mutate(
      work = dplyr::if_else(no_work_no_study, FALSE, work),
      study = dplyr::if_else(no_work_no_study, FALSE, study)
    ) %>%
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
    ) %>%
    dplyr::select(-dummy, -no_work_no_study)

  field_form <- field_form %>%
    dplyr::mutate(
      dummy = dplyr::if_else(
        work == FALSE & study == FALSE, TRUE, FALSE
      )
    ) %>%
    tidyr::unnest(cols = c(work_periods, study_periods))

  for (i in stringr::str_subset(names(field_form), "^work_|^study_")) {
    field_form <- field_form %>%
      dplyr::mutate(
        !!as.symbol(i) :=
          dplyr::if_else(dummy, FALSE, !!as.symbol(i))
      )
  }

  for (i in stringr::str_subset(names(field_form), "^work_")) {
    field_form <- field_form %>%
      dplyr::mutate(
        !!as.symbol(i) :=
          dplyr::if_else(work, !!as.symbol(i), FALSE)
      )
  }

  for (i in stringr::str_subset(names(field_form), "^study_")) {
    field_form <- field_form %>%
      dplyr::mutate(
        !!as.symbol(i) :=
          dplyr::if_else(study, !!as.symbol(i), FALSE)
      )
  }

  field_form <- field_form %>%
    tidyr::nest(
      work_periods = c(
        work_morning, work_afternoon, work_evening, work_wee_hours
      ),
      study_periods = c(
        study_morning, study_afternoon, study_evening, study_wee_hours
      )
    ) %>%
    dplyr::relocate(work_periods, .after = study) %>%
    dplyr::relocate(study_periods, .after = work_periods)%>%
    dplyr::select(-dummy)

  invisible(field_form)
}

fix_bt_sprep_inversion <- function(field_form) {
  checkmate::assert_tibble(field_form)

  for (i in c("_w", "_f")) {
    bt_i <- paste0("bt", i)
    sprep_i <- paste0("sprep", i)

    field_form <- field_form %>%
      dplyr::mutate(
        dummy = dplyr::case_when(
          lubritime::assign_date(!!as.symbol(bt_i),
                                 !!as.symbol(sprep_i)) >
            lubridate::dhours(12) ~ TRUE,
          TRUE ~ FALSE),
        bkp = !!as.symbol(bt_i),
        !!as.symbol(bt_i) :=
          dplyr::if_else(dummy, !!as.symbol(sprep_i),
                         !!as.symbol(bt_i)),
        !!as.symbol(sprep_i) :=
          dplyr::if_else(dummy, bkp, !!as.symbol(sprep_i))) %>%
      dplyr::select(-dummy, -bkp)
  }

  invisible(field_form)
}

validate_sdu <- function(field_form) {
  checkmate::assert_tibble(field_form)

  for (i in c("_w", "_f")) {
    sprep_i <- paste0("sprep", i)
    slat_i <- paste0("slat", i)
    se_i <- paste0("se", i)

    test <- field_form %>%
      dplyr::mutate(
        so_i = mctq::so(!!as.symbol(sprep_i), !!as.symbol(slat_i)),
        sd_i = mctq::sdu(so_i, !!as.symbol(se_i)),
        dummy = dplyr::case_when(
          sd_i < lubridate::dhours(4) |
            sd_i > lubridate::dhours(14) ~ TRUE,
          TRUE ~ FALSE)
      ) %>%
      dplyr::select(dummy)

    field_form <- field_form %>%
      dplyr::bind_cols(test) %>%
      dplyr::mutate(dplyr::across(
        .cols = dplyr::matches(paste0("^wd$|", i, "$")),
        .fns = ~ dplyr::if_else(dummy, gutils::na_as(.x), .x))
      ) %>%
      dplyr::select(-dummy)
  }

  invisible(field_form)
}

validate_so <- function(field_form) {
  checkmate::assert_tibble(field_form)

  field_form <- field_form %>%
    dplyr::mutate(
      so_w = mctq::so(sprep_w, slat_w),
      so_f = mctq::so(sprep_f, slat_f),
      dummy = dplyr::case_when(
        (so_w >= hms::parse_hms("06:00:00") &
           so_w <= hms::parse_hms("18:00:00")) |
          (so_f >= hms::parse_hms("06:00:00") &
             so_f <= hms::parse_hms("18:00:00")) ~ TRUE,
        TRUE ~ FALSE)
    ) %>%
    dplyr::mutate(dplyr::across(
      .cols = dplyr::matches("^wd$|_w$|_f$"),
      .fns = ~ dplyr::if_else(dummy, gutils::na_as(.x), .x))
    ) %>%
    dplyr::select(-so_w, -so_f, -dummy)

  invisible(field_form)
}

na_mctq_blank_cases <- function(field_form) {
  checkmate::assert_tibble(field_form)

  field_form <- field_form %>%
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
    ) %>%
    dplyr::mutate(dplyr::across(
      .cols = dplyr::matches("^wd$|_w$|_f$"),
      .fns = ~ dplyr::if_else(dummy, gutils::na_as(.x), .x))
    ) %>%
    dplyr::select(-dummy)

  invisible(field_form)
}

remove_duplicates_and_blanks <- function(field_form) {
  checkmate::assert_tibble(field_form)

  count <- field_form %>%
    dplyr::mutate(dplyr::across(
      .col = dplyr::everything(), .fns = as.character
    )) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      length = count_not_na(dplyr::c_across(
        cols = -dplyr::all_of(c(
          "field_form_id", "timestamp", "track"
        ))
      ))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(length)

  field_form <- field_form %>%
    dplyr::bind_cols(count) %>%
    dplyr::arrange(dplyr::desc(length)) %>%
    dplyr::group_by(email, birth_date) %>%
    dplyr::mutate(dup_rank = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      email_birth_dup = dup_rank > 1,
      email_birth_dup = dplyr::case_when(
        is.na(email) | is.na(birth_date) ~ FALSE,
        TRUE ~ email_birth_dup
      )
    ) %>%
    dplyr::group_by(name, birth_date) %>%
    dplyr::mutate(dup_rank = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      name_birth_dup = dup_rank > 1,
      name_birth_dup = dplyr::case_when(
        is.na(name) | is.na(birth_date) ~ FALSE,
        TRUE ~ name_birth_dup
      ),
      dup = email_birth_dup | name_birth_dup
    ) %>%
    dplyr::select(-dup_rank, -email_birth_dup, -name_birth_dup) %>%
    dplyr::filter(!dup == TRUE, length > 8) %>%
    dplyr::select(-dup, -length) %>%
    dplyr::arrange(field_form_id)

  invisible(field_form)
}
