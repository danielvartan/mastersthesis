# library(cli)
# library(dplyr)
# library(here)
# library(lockr) # github.com/danielvartan/lockr
# library(osfr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# libary(purrr)
# library(readr)
# library(rutils) # github.com/danielvartan/rutils
# library(stringr)

source(here::here("R", "save_and_lock.R"))

# Data from National Aeronautics and Space Administration (NASA) ModelE AR5
# Simulations. See
# <https://data.giss.nasa.gov/modelE/ar5plots/srvernal.html> and
# <https://data.giss.nasa.gov/cgi-bin/ar5/srevents.cgi>
# to learn more.

write_nasa_data_to_osf <- function(
    osf_pat = Sys.getenv("OSF_PAT"),
    public_key = here::here("_ssh", "id_rsa.pub")
  ) {
  prettycheck::assert_internet()
  checkmate::assert_string(osf_pat, n.chars = 70)
  lockr:::assert_public_key(public_key)

  osfr::osf_auth(osf_pat) |> rutils::shush()
  osf_id <- "https://osf.io/cbqsa"
  test <- try(osfr::osf_retrieve_node(osf_id), silent = TRUE)

  if (inherits(test, "try-error")) {
    cli::cli_abort(paste0(
      "The {.strong {cli::col_red('OSF PAT')}} provided is invalid. ",
      "Please, check the access token and try again."
    ))
  }

  cli::cli_progress_step("Downloading, importing and tidying the data.")

  names <- c(
    "year", "vernal_equinox", "summer_solstice", "autumnal_equinox",
    "winter_solstice", "perihelion", "aphelion"
  )

  data <-
    "https://data.giss.nasa.gov/cgi-bin/ar5/srevents.cgi" |>
    readr::read_lines(skip = 6) |>
    stringr::str_trim() |>
    stringr::str_split("  (?=\\d{1,2}/)") |>
    purrr::map(~ stringr::str_squish(.x)) |>
    purrr::map_df(
      function(x) {
        x |>
          as.list() |>
          magrittr::set_names(names) |>
          dplyr::as_tibble()
      }
    ) |>
    dplyr::mutate(
      dplyr::across(
        .col = dplyr::where(is.character),
        .fns = ~ paste0(year, "/", .x)
      )
    ) |>
    dplyr::mutate(
      year =
        year |>
        stringr::str_extract("^\\d{4}") |>
        as.integer()
    ) |>
    dplyr::mutate(
      dplyr::across(
        .col = dplyr::where(is.character),
        .fns = ~ lubridate::ymd_hm(.x, tz = "UTC")
      )
    ) |>
    dplyr::rename(
      march_equinox = vernal_equinox,
      june_solstice = summer_solstice,
      september_equinox = autumnal_equinox,
      december_solstice = winter_solstice
    )

  cli::cli_progress_step("Locking and saving data.")

  file_name_pattern <- "nasa-2020"

  rds_file <-
    data |>
    save_and_lock(
      file = file.path(tempdir(), paste0(file_name_pattern, ".rds")),
      type = "rds",
      public_key = public_key,
      compress = "bz2"
    )

  csv_file <-
    data |>
    save_and_lock(
      file = file.path(tempdir(), paste0(file_name_pattern, ".csv")),
      type = "csv",
      public_key = public_key
    )

  # file.size(rds_file) / 1e+6
  # file.size(csv_file) / 1e+6

  cli::cli_progress_step("Uploading data to OSF.")

  osfr::osf_upload(
    x = osfr::osf_retrieve_node(osf_id),
    path = c(rds_file, csv_file),
    conflicts = "overwrite"
  )

  invisible()
}
