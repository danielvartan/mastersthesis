# library(cli)
# library(dplyr)
# library(hardhat)
# library(here)
# library(lockr) # github.com/danielvartan/lockr
# library(lubridate)
# library(lubritime)
# library(osfr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils
# library(tidyr)

source(here::here("R", "save_and_lock.R"))

lock_and_store_data <- function(
    data, #Nolint
    osf_pat = Sys.getenv("OSF_PAT"),
    public_key = here::here("_ssh", "id_rsa.pub"),
    upload_to_osf = FALSE,
    name_pattern = deparse(substitute(data))
  ) {
  checkmate::assert_tibble(data)
  checkmate::assert_string(osf_pat, n.chars = 70)
  lockr:::assert_public_key(public_key)
  checkmate::assert_flag(upload_to_osf)
  checkmate::assert_string(name_pattern)

  data |>
    lock_data(
      public_key = public_key,
      name_pattern = name_pattern
    ) |>
    store_data(
      osf_pat = osf_pat,
      upload_to_osf = upload_to_osf
    )
}

# library(cli)
# library(dplyr)
# library(hardhat)
# library(here)
# library(lockr) # github.com/danielvartan/lockr
# library(lubridate)
# library(lubritime)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(tidyr)

source(here::here("R", "save_and_lock.R"))

lock_data <- function(
    data, #nolint
    public_key = here::here("_ssh", "id_rsa.pub"),
    name_pattern = deparse(substitute(data))
  ) {
  checkmate::assert_tibble(data)
  lockr:::assert_public_key(public_key)
  checkmate::assert_string(name_pattern)

  cli::cli_progress_step("Locking and saving data.")

  file_name_pattern <- name_pattern

  csv_file <-
    data |>
    tidyr::unnest(
      cols = dplyr::everything(),
      names_sep = "_",
      names_repair = "universal"
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::where(hardhat::is_importance_weights),
        .fns = ~ .x |> unclass()
      ),
      dplyr::across(
        .cols = dplyr::where(lubridate::is.duration),
        .fns = ~ .x |> as.numeric() |> round() |> hms::hms()
      ),
      dplyr::across(
        .cols = dplyr::where(~ prettycheck::test_temporal(.x, rm = "Date")),
        .fns = ~ .x |> lubritime::round_time()
      ),
      dplyr::across(
        .cols = dplyr::where(is.numeric),
        .fns = ~ .x |> round(5)
      )
    ) |>
    save_and_lock(
      file = file.path(tempdir(), paste0(file_name_pattern, ".csv")),
      type = "csv",
      public_key = public_key
    )

  if (any(c("ghi_month", march_equinox) %in% names(data))) {
    data <- nest_solar_vars(data)
  }

  rds_file <-
    data |>
    save_and_lock(
      file = file.path(tempdir(), paste0(file_name_pattern, ".rds")),
      type = "rds",
      public_key = public_key,
      compress = "bz2"
    )

  # file.size(rds_file) / 1e+6
  # file.size(csv_file) / 1e+6

  c(rds_file, csv_file)
}

# library(cli)
# library(osfr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils

store_data <- function(
    file, #nolint
    osf_pat = Sys.getenv("OSF_PAT"),
    upload_to_osf = TRUE
  ) {
  checkmate::assert_flag(upload_to_osf)

  if (isTRUE(upload_to_osf)) {
    prettycheck::assert_internet()
    checkmate::assert_character(file)
    for (i in file) checkmate::assert_file_exists(i)
    checkmate::assert_string(osf_pat, n.chars = 70)

    osfr::osf_auth(osf_pat) |> rutils::shush()
    osf_id <- "https://osf.io/cbqsa"
    test <- try(osfr::osf_retrieve_node(osf_id), silent = TRUE)

    if (inherits(test, "try-error")) {
      cli::cli_abort(paste0(
        "The {.strong {cli::col_red('OSF PAT')}} provided is invalid. ",
        "Please, check the access token and try again."
      ))
    }

    cli::cli_progress_step("Uploading data to OSF.")

    osfr::osf_upload(
      x = osfr::osf_retrieve_node(osf_id),
      path = file,
      conflicts = "overwrite"
    )
  }

  invisible()
}

# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(purrr)
# library(tidyr)

nest_solar_vars <- function(data) {
  checkmate::assert_tibble(data)

  cli::cli_progress_step("Nesting solar variables.")

  data |>
    tidyr::nest(
      ghi = c(ghi_month, ghi_annual),
      march_equinox = c(
        march_equinox, march_equinox_sunrise, march_equinox_sunset,
        march_equinox_daylight
      ),
      june_solstice = c(
        june_solstice, june_solstice_sunrise, june_solstice_sunset,
        june_solstice_daylight
      ),
      september_equinox = c(
        september_equinox, september_equinox_sunrise,
        september_equinox_sunset, september_equinox_daylight
      ),
      december_solstice = c(
        december_solstice, december_solstice_sunrise,
        december_solstice_sunset, december_solstice_daylight
      )
    ) |>
    dplyr::mutate(
      ghi = ghi |>
        purrr::map(
          .f = ~ .x |>
            dplyr::rename_with(.fn = ~ c("month", "annual"))
        )
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(
          c(
            "march_equinox", "june_solstice", "september_equinox",
            "december_solstice"
          )
        ),
        .fns = ~ .x |>
          purrr::map(
            .f = ~ .x |>
              dplyr::rename_with(
                .fn = ~ c("moment", "sunrise", "sunset", "daylight")
              )
          )
      )
    ) |>
    dplyr::relocate(
      ghi, march_equinox, june_solstice, september_equinox,
      december_solstice,
      .after = longitude
    )
}
