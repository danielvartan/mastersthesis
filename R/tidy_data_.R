# library(checkmate, quietly = TRUE)
# library(cli, quietly = TRUE)
# library(curl, quietly = TRUE)
# library(dplyr, quietly = TRUE)
# library(here, quietly = TRUE)
# library(hms, quietly = TRUE)
# library(lockr, quietly = TRUE)
# library(lubridate, quietly = TRUE)
# library(methods, quietly = TRUE)
# library(rutils, quietly = TRUE)
# library(stringr, quietly = TRUE)
# library(tidyr, quietly = TRUE)

source(here::here("R/look_and_replace.R"))

# library(checkmate, quietly = TRUE)
# library(cli, quietly = TRUE)
# library(here, quietly = TRUE)
# library(lockr, quietly = TRUE)
# library(rutils, quietly = TRUE)

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
#' @param osf_pat (optional) a string with the OSF personal access token
#'   (PAT) (default: `Sys.getenv("OSF_PAT")`).
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
tidy_data_ <- function(
    data,
    osf_pat = Sys.getenv("OSF_PAT"),
    public_key = here::here(".ssh/id_rsa.pub"),
    private_key = here::here(".ssh/id_rsa")
) {
  checkmate::assert_tibble(data)
  checkmate::assert_string(osf_pat, n.chars = 70, null.ok = TRUE)
  lockr:::assert_public_key(public_key)
  lockr:::assert_private_key(private_key)
  rutils:::assert_internet()

  ## TO DO:
  ##
  ## * Remove blank cases.

  cli::cli_progress_step("Tidying data")

  out <-
    data |>
    fix_var_names() |>
    fix_var_classes() |>
    look_and_replace_values(
      osf_pat = osf_pat,
      public_key = public_key,
      private_key = private_key
    ) |>
    fix_character_vars() |>
    nest_vars() |>
    select_vars()

  invisible(out)
}

# library(checkmate, quietly = TRUE)
# library(dplyr, quietly = TRUE)

fix_var_names <- function(data) {
  checkmate::assert_tibble(data)

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
    )
}

# library(checkmate, quietly = TRUE)
# library(dplyr, quietly = TRUE)
# library(lubridate, quietly = TRUE)

fix_var_classes <- function(data) {
  checkmate::assert_tibble(data)

  data |>
    dplyr::mutate(
      id = as.integer(id),
      timestamp = lubridate::ymd_hms(paste(date, time)),
      birth_date = lubridate::ymd(birth_date, quiet = TRUE),
      sex = factor(
        as.numeric(sex),
        levels = seq(2),
        labels = c("Female", "Male"),
        ordered = FALSE
      ),
      gender_identity = factor(
        as.numeric(gender_identity),
        levels = seq(3),
        labels = c("Woman", "Man", "Non-binary"),
        ordered = FALSE
      ),
      sexual_orientation = factor(
        as.numeric(sexual_orientation),
        levels = seq(4),
        labels = c(
          "Heterosexual", "Homosexual", "Bisexual", "Asexual"
          ),
        ordered = FALSE
      ),
      height = as.numeric(height) / 100,
      wd = as.integer(wd),
      bt_w = paste(mctqBTwHH, mctqBTwMM, sep = ":") |> hms::parse_hm(),
      sprep_w =
        paste(mctqSPrepwHH, mctqSPrepwMM, sep = ":") |>
        hms::parse_hm(),
      se_w = paste(mctqSEwHH, mctqSEwMM, sep = ":") |> hms::parse_hm(),
      le_w = lubridate::dhours(as.numeric(mctqLEwHH)) +
        lubridate::dminutes(as.numeric(mctqLEwMM)),
      bt_f = paste(mctqBTfHH, mctqBTfMM, sep = ":") |> hms::parse_hm(),
      sprep_f =
        paste(mctqSPrepfHH, mctqSPrepfMM, sep = ":") |>
        hms::parse_hm(),
      se_f = paste(mctqSEfHH, mctqSEfMM, sep = ":") |> hms::parse_hm(),
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
    )
}

# library(checkmate, quietly = TRUE)
# library(dplyr, quietly = TRUE)
# library(lockr, quietly = TRUE)
# library(methods, quietly = TRUE)
library(rlang, quietly = TRUE)
# library(rutils, quietly = TRUE)

look_and_replace_values <- function(
    data,
    osf_pat = Sys.getenv("OSF_PAT"),
    public_key = here::here(".ssh/id_rsa.pub"),
    private_key = here::here(".ssh/id_rsa")
) {
  checkmate::assert_tibble(data)
  checkmate::assert_string(osf_pat, n.chars = 70, null.ok = TRUE)
  lockr:::assert_public_key(public_key)
  lockr:::assert_private_key(private_key)
  rutils:::assert_internet()

  # char_vars <- c(
  #   "track", "name", "email", "country", "state", "city", "postal_code",
  #   "sleep_drugs_which", "sleep_disorder_which", "medication_which"
  # )

  char_vars <- c("track", "name", "email", "country", "state")
  out <- data

  lookup_data <- get_lookup_data(
    osf_pat = osf_pat,
    public_key = public_key,
    private_key = private_key
  )

  for (i in char_vars) {
    out <-
      out |>
      dplyr::mutate(
        !!as.symbol(i) := look_and_replace(
          x = !!as.symbol(i),
          table = i,
          public_key = public_key,
          private_key = private_key,
          na_unmatched = FALSE,
          lookup_data = lookup_data
        )
      )
  }

  special_cases <-
    lookup_data$special_cases |>
    dplyr::mutate(id = as.integer(id))

  for (i in seq_len(nrow(special_cases))) {
    i_id <- special_cases$id[i]
    i_var <- special_cases$var[i]
    i_value <-
      special_cases$value[i] |>
      methods::as(class(out[[special_cases$var]])[1])

    out <-
      out |>
      dplyr::mutate(
        !!as.symbol(i_var) := dplyr::if_else(
          id == i_id,
          i_value,
          !!as.symbol(i_var)
        )
      )
  }

  invisible(out)
}

# library(checkmate, quietly = TRUE)
# library(dplyr, quietly = TRUE)
# library(lockr, quietly = TRUE)
# library(rutils, quietly = TRUE)
# library(stringr, quietly = TRUE)

fix_character_vars <- function(data) {
  checkmate::assert_tibble(data)

  data |>
    dplyr::mutate(
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
        stringr::str_detect(email,"^[[:alnum:]._-]+@[[:alnum:].-]+$") ~ email
      )
    )
}

# library(checkmate, quietly = TRUE)
# library(dplyr, quietly = TRUE)
# library(tidyr, quietly = TRUE)

nest_vars <- function(data) {
  checkmate::assert_tibble(data)

  data |>
    tidyr::nest(
      work_periods = c(
        work_morning, work_afternoon, work_evening, work_wee_hours
      ),
      study_periods = c(
        study_morning, study_afternoon, study_evening, study_wee_hours
      )
    )
}

select_vars <- function(data) {
  checkmate::assert_tibble(data)

  data |>
    dplyr::select(
      id, timestamp, track,

      name, email, birth_date, sex, gender_identity, sexual_orientation,
      country, state, city, postal_code,

      height, weight, sleep_drugs, sleep_drugs_which, sleep_disorder,
      sleep_disorder_which, medication, medication_which,
      snore,

      work, study, no_work_no_study, work_periods, study_periods,

      wd, bt_w, sprep_w, slat_w, se_w, si_w, alarm_w, le_w, bt_f,
      sprep_f, slat_f, se_f, si_f, alarm_f, le_f
    )
}
