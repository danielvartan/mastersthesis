# library(checkmate, quietly = TRUE)
# library(cli, quietly = TRUE)
# library(curl, quietly = TRUE)
# library(dplyr, quietly = TRUE)
# library(here, quietly = TRUE)
# library(hms, quietly = TRUE)
# library(lockr, quietly = TRUE)
# library(lubridate, quietly = TRUE)
# library(rutils, quietly = TRUE)
# library(stringr, quietly = TRUE)
# library(tidyr, quietly = TRUE)

source(here::here("R/look_and_replace.R"))

#' Tidy `get_raw_data()` output
#'
#' @description
#'
#' `tidy_data_` tidy the output of `get_raw_data()`.
#'
#' The function has a `_` at the end to differentiated itself with the
#' `tidy_data` dataset produce by the `targets` R package.
#'
#' @details
#'
#' Here the process of _tiding_ a dataset is understood as transforming it in
#' input data, like described in Loo & Jonge (2018). It's a very similar
#' process of tiding data described in the workflow proposed by Wickham &
#' Grolemund (n.d.).
#'
#' Please note that input data is not the same as valid data. To get a valid
#' data, run `validate_data()`.
#'
#' To learn more about the concept of tidy data, see Wickham (2014) and
#' Wickham & Grolemund (n.d.).
#'
#' @param data A [`tibble`][dplyr::tibble()] with the `get_raw_data()`
#'   output.
#' @param public_key (optional) an [`openssl`][openssl::rsa_keygen()] RSA
#'   public key or a string specifying the public key path. See
#'   [`rsa_keygen()`][lockr::rsa_keygen] to learn how to create an RSA key
#'   pair (default: `here::here(".ssh/id_rsa.pub")`).
#' @param private_key (optional) an [`openssl`][openssl::rsa_keygen()] RSA
#'   private key or a string specifying the private key path. See
#'   [`rsa_keygen()`][lockr::rsa_keygen] to learn how to create an RSA key
#'   pair (default: `"here::here(.ssh/id_rsa")`).
#'
#' @return An invisible [`tibble`][dplyr::tibble()] with a tidied, but not
#'   validated, dataset.
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
#'   tidy <- tidy_data_()
#'   utils::View(tidy_data_())
#' }
#' }
tidy_data_ <- function(data,
                       public_key = here::here(".ssh/id_rsa.pub"),
                       private_key = here::here(".ssh/id_rsa")) {
  checkmate::assert_tibble(data)
  lockr:::assert_public_key(public_key)
  lockr:::assert_private_key(private_key)
  rutils:::assert_internet()

  ## TODO:
  ##
  ## * Remove blank cases

  cli::cli_progress_step("Tidying data")

  out <-
    data |>
    dplyr::rename(
      id = ID, track = track,

      name = pdNAME, email = pdEMAIL, birth_date = pdBIRTH,
      country = pdCOUNTRY, state = pdSTATE, city = pdCITY,
      postal_code = pdPOSTAL,

      height = pdHEIGHT, weight = pdWEIGHT, sex = pdGENDER,
      gender_identity = pdGENDERidentity,
      sexual_orientation = pdSEXUALORIENTATION, sleep_drugs = hhDRUGS,
      sleep_drugs_which = hhDRUGSwhich, sleep_disorder = hhSLEEPDISORDER,
      sleep_disorder_which = hhSLEEPDISORDERwhich,
      medication = hhMEDICATION, medication_which = hhMEDICATIONwhich,
      snore = hhSNORE,

      work = hhWORK, study = hhSTUDY, no_work_no_study = hhNOWORKORSTUDY,
      work_morning = hhWORKmorning, work_afternoon = hhWORKafternoon,
      work_evening = hhWORKevening, work_wee_hours = hhWORKweehours,
      study_morning = hhSTUDYmorning, study_afternoon = hhSTUDYafternoon,
      study_evening = hhSTUDYevening, study_wee_hours = hhSTUDYweehours,

      wd = mctqWD,

      slat_w = mctqSLatwMM, alarm_w = mctqAlarmw, si_w = mctqSIwMM,
      slat_f = mctqSLatfMM, alarm_f = mctqAlarmf, si_f = mctqSIfMM
    ) |>
    dplyr::mutate(
      id = as.integer(id),
      timestamp = lubridate::ymd_hms(paste(date, time)),
      name = dplyr::case_when(
        stringr::str_detect(name, "^[[:upper:] .-]+$") |
          stringr::str_detect(name, "^[[:lower:] .-]+$") ~
          stringr::str_to_title(name),
        TRUE ~ name
      ),
      name = stringr::str_squish(name),
      name = dplyr::case_when(
        stringr::str_detect(name, "^[A-Za-zÀ-ÖØ-öø-ÿ .-]+$") ~ name
      ),
      email = stringr::str_to_lower(email),
      email = stringr::str_squish(email),
      email = dplyr::case_when(
        stringr::str_detect(
          email,"^[[:alnum:]._-]+@[[:alnum:].-]+$") ~ email
      ),
      birth_date = lubridate::ymd(birth_date, quiet = TRUE),
      sex = factor(
        as.numeric(sex), levels = seq(2),
        labels = c("Female", "Male"), ordered = FALSE
      ),
      gender_identity = factor(
        as.numeric(gender_identity), levels = seq(3),
        labels = c("Woman", "Man", "Non-binary"),
        ordered = FALSE
      ),
      sexual_orientation = factor(
        as.numeric(sexual_orientation), levels = seq(4),
        labels = c("Heterosexual", "Homosexual", "Bisexual",
                   "Asexual"), ordered = FALSE
      ),
      country = look_and_replace(
        country, "country", public_key = public_key,
        private_key = private_key, na_unmatched = FALSE
      ),
      state = look_and_replace(
        state, "state", public_key = public_key,
        private_key = private_key, na_unmatched = FALSE
      ),
      height = as.numeric(height) / 100,
      wd = as.integer(wd),
      bt_w = hms::parse_hm(paste(mctqBTwHH, mctqBTwMM, sep = ":")),
      sprep_w = hms::parse_hm(paste(mctqSPrepwHH, mctqSPrepwMM,
                                    sep = ":")),
      se_w = hms::parse_hm(paste(mctqSEwHH, mctqSEwMM, sep = ":")),
      le_w = lubridate::dhours(as.numeric(mctqLEwHH)) +
        lubridate::dminutes(as.numeric(mctqLEwMM)),
      bt_f = hms::parse_hm(paste(mctqBTfHH, mctqBTfMM, sep = ":")),
      sprep_f = hms::parse_hm(paste(mctqSPrepfHH, mctqSPrepfMM,
                                    sep = ":")),
      se_f = hms::parse_hm(paste(mctqSEfHH, mctqSEfMM, sep = ":")),
      le_f = lubridate::dhours(as.numeric(mctqLEfHH)) +
        lubridate::dminutes(as.numeric(mctqLEfMM)),
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = c(
          "height", "weight", "sleep_drugs", "sleep_disorder",
          "medication", "snore", "work", "study", "no_work_no_study",
          "work_morning", "work_afternoon", "work_evening",
          "work_wee_hours", "study_morning", "study_afternoon",
          "study_evening", "study_wee_hours", "slat_w", "alarm_w",
          "si_w", "slat_f", "alarm_f", "si_f"
        ),
        .fns = ~ as.numeric(.x)
      ),
      dplyr::across(
        .cols = c(
          "sleep_drugs", "sleep_disorder", "medication", "snore",
          "work", "study", "no_work_no_study", "work_morning",
          "work_afternoon", "work_evening", "work_wee_hours",
          "study_morning", "study_afternoon", "study_evening",
          "study_wee_hours", "alarm_w", "alarm_f"
        ),
        .fns = ~ dplyr::case_when(
          .x == 0 ~ FALSE,
          .x == 1 ~ TRUE
        )
      ),
      dplyr::across(
        .cols = c("slat_w", "si_w", "slat_f", "si_f"),
        .fns = ~ lubridate::dminutes(.x)
      )
    ) |>
    tidyr::nest(
      work_periods = c(
        work_morning, work_afternoon, work_evening, work_wee_hours
      ),
      study_periods = c(
        study_morning, study_afternoon, study_evening, study_wee_hours
      )
    ) |>
    dplyr::select(
      id, timestamp, track,

      name, email, birth_date, sex, gender_identity, sexual_orientation,
      country, state, city, postal_code,

      height, weight, sleep_drugs, sleep_drugs_which, sleep_disorder,
      sleep_disorder_which, medication, medication_which,
      snore,

      work, study, no_work_no_study, work_periods, study_periods,

      wd, bt_w, sprep_w, slat_w, se_w, si_w, alarm_w, le_w, bt_f,
      sprep_f, slat_f, se_f, si_f, alarm_f, le_f)

  invisible(out)
}
