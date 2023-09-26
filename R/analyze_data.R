# library(checkmate, quietly = TRUE)
# library(cli, quietly = TRUE)
# library(dplyr, quietly = TRUE)
# library(hms, quietly = TRUE)
# library(lubridate, quietly = TRUE)
# library(mctq, quietly = TRUE)
# library(rutils, quietly = TRUE)
# library(scaler, quietly = TRUE)

#' Analyze `validate_data()` output
#'
#' @description
#'
#' `analyze_data()` computes and creates the non-measured MCTQ variables
#' based on the output of `validate_data()`.
#'
#' @details
#'
#' Computing and creating new variables is part of the process of producing
#' statistics, like described in Loo & Jonge (2018). It's also a part of the
#' process of transforming data, described in the workflow proposed by Wickham
#' & Grolemund (n.d.).
#'
#' @param data A [`tibble`][tibble::tibble()] with the `validate_data()`
#'   output.
#' @param round (optional) a [`logical`][base::as.logical()] value indicating if
#'   [`Duration`][lubridate::duration()] and [`hms`][hms::hms()] objects must be
#'   rounded at the level of seconds (default: `TRUE`).
#' @param hms (optional) a `logical` value indicating if
#'   [`Duration`][lubridate::duration()] and [`difftime`][base::difftime()]
#'   objects must be converted to [`hms`][hms::hms() (default: `TRUE`).
#'
#' @return An invisible [`tibble`][dplyr::tibble()] with a validated
#'   dataset with all the variables proposed for a standard MCTQ dataset.
#'
#' @family data wrangling functions
#'
#' @noRd
#'
#' @references
#'
#' Loo, M. van der, & Jonge, E de. (2018). _Statistical data cleaning with
#' applications in R_. John Wiley & Sons. \doi{10.1002/9781118897126}
#'
#' Wickham, H. (2014). Tidy data. _Journal of Statistical Software_, _59_(10),
#' 1-23. \doi{10.18637/jss.v059.i10}
#'
#' Wickham, H., & Grolemund, G. (n.d.). _R for data science_. (n.p.).
#' \url{https://r4ds.had.co.nz}
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("utils", quietly = TRUE)) {
#'   analysis <- analyze_data()
#'   utils::View(analyze_data())
#' }
#' }
analyze_data <- function(data, round = FALSE, hms = FALSE) {
  checkmate::assert_tibble(data)
  checkmate::assert_flag(round)
  checkmate::assert_flag(hms)

  cli::cli_progress_step("Analyzing data")

  out <- data |>
    dplyr::mutate(
      age = scaler::age(birth_date, timestamp)
    ) |>
    dplyr::mutate(
      fd = mctq::fd(wd),
      so_w = mctq::so(sprep_w, slat_w),
      gu_w = mctq::gu(se_w, si_w),
      sd_w = mctq::sdu(so_w, se_w),
      tbt_w = mctq::tbt(bt_w, gu_w),
      msw = mctq::msl(so_w, sd_w),

      so_f = mctq::so(sprep_f, slat_f),
      gu_f = mctq::gu(se_f, si_f),
      sd_f = mctq::sdu(so_f, se_f),
      tbt_f = mctq::tbt(bt_f, gu_f),
      msf = mctq::msl(so_f, sd_f),

      sd_week = mctq::sd_week(sd_w, sd_f, wd),
      msf_sc = mctq::msf_sc(msf, sd_w, sd_f, sd_week, alarm_f),
      sloss_week = mctq::sloss_week(sd_w, sd_f, wd),
      le_week = mctq::le_week(le_w, le_f, wd),
      sjl_rel = mctq::sjl_rel(msw, msf),
      sjl = abs(sjl_rel),
      sjl_sc_rel = mctq::sjl_sc_rel(so_w, se_w, so_f, se_f),
      sjl_sc = abs(sjl_sc_rel),
    ) |>
    dplyr::relocate(
      id, timestamp, track,

      name, email, birth_date, age, sex, gender_identity,
      sexual_orientation, country, state, city, postal_code,

      height, weight, sleep_drugs, sleep_drugs_which, sleep_disorder,
      sleep_disorder_which, medication, medication_which,
      snore,

      work, study, work_periods, study_periods,

      wd, fd,

      bt_w, sprep_w, slat_w, so_w, se_w, si_w, gu_w, alarm_w,
      sd_w, tbt_w, le_w, msw,

      bt_f, sprep_f, slat_f, so_f, se_f, si_f, gu_f, alarm_f,
      sd_f, tbt_f, le_f, msf,

      sd_week, sloss_week, le_week, msf_sc, sjl_rel, sjl, sjl_sc_rel,
      sjl_sc
    )

  count_w <- length(names(out)[grepl("_w$", names(out))])
  count_f <- length(names(out)[grepl("_f$", names(out))])
  count_w <- count_w * 2/3
  count_f <- count_f * 2/3

  test <- out |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::everything(),
      .fns = as.character
    )) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      dummy_0_a = as.integer(wd) == 0,
      dummy_0_b = rutils:::count_na(
        dplyr::c_across(cols = dplyr::ends_with("_w"))) >= count_w,
      dummy_0_c = alarm_f == FALSE,
      dummy_7_a = as.integer(wd) == 7,
      dummy_7_b = rutils:::count_na(
        dplyr::c_across(cols = dplyr::ends_with("_f"))) >= count_f,
      dummy_0 = dummy_0_a & dummy_0_b & dummy_0_c & dummy_7_b == FALSE,
      dummy_7 = dummy_7_a & dummy_7_b & dummy_0_b == FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(dummy_0, dummy_7)

  out <- out |>
    dplyr::bind_cols(test) |>
    dplyr::mutate(
      sd_week = dplyr::case_when(
        dummy_0 == TRUE ~ sd_f,
        dummy_7 == TRUE ~ sd_w,
        TRUE ~ sd_week),
      msf_sc = dplyr::if_else(dummy_0, msf, msf_sc),
      sloss_week = dplyr::if_else(dummy_0, lubridate::dhours(0),
                                  sloss_week),
      le_week = dplyr::case_when(
        dummy_0 == TRUE ~ le_f,
        dummy_7 == TRUE ~ le_w,
        TRUE ~ le_week),
      sjl_rel = dplyr::if_else(dummy_0, lubridate::dhours(0), sjl_rel),
      sjl = dplyr::if_else(dummy_0, lubridate::dhours(0), sjl),
      sjl_sc_rel = dplyr::if_else(dummy_0, lubridate::dhours(0),
                                  sjl_sc_rel),
      sjl_sc = dplyr::if_else(dummy_0, lubridate::dhours(0), sjl_sc)
    ) |>
    dplyr::select(-dummy_0, -dummy_7)

  invisible(out)
}
