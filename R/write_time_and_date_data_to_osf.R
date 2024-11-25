# library(cli)
# library(dplyr)
# library(here)
# library(janitor)
# library(lockr) # github.com/danielvartan/lockr
# library(osfr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils
# library(rvest)

source(here::here("R", "save_and_lock.R"))

# The Time and Date AS service offers a table with time and dates of the
# equinoxes and solstices in their website accordingly with Meeus (1991)
# algorithms. It also offer this data via its API, but, for the purpose of this
# analysis, the API will not be necessary. This function scrapes the table and
# tidy it in a tibble.
#
# Meeus, J. (1991). Astronomical algorithms. Willmann-Bell.

# The Time and Date website will change the way data is displayed accordingly
# with the user's location. Because of this, you may need to change the
# `orders` parameter to match the date and time format of the website.
# See `?lubridate::parse_date_time` to learn more.
#
# Source: <https://www.timeanddate.com/calendar/seasons.html?year=2000&n=1440>.

write_time_and_date_lookup_data <- function(
    orders = "db H:M",
    osf_pat = Sys.getenv("OSF_PAT"),
    public_key = here::here("_ssh", "id_rsa.pub")
  ) {
  prettycheck:::assert_internet()
  prettycheck:::assert_string(orders)
  prettycheck:::assert_string(osf_pat, n.chars = 70)
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

  data <-
    "https://www.timeanddate.com/calendar/seasons.html?year=2000&n=1440" |>
    rvest::read_html() |>
    rvest::html_node("table") |>
    rvest::html_table() |>
    janitor::clean_names() |>
    dplyr::slice(-dplyr::n()) |>
    dplyr::mutate(
      year = as.integer(year),
      march_equinox = paste(year, march_equinox, march_equinox_2),
      june_solstice = paste(year, june_solstice, june_solstice_2),
      september_equinox = paste(year, september_equinox, september_equinox_2),
      december_solstice = paste(year, december_solstice, december_solstice_2)
    ) |>
    dplyr::select(
      year, march_equinox, june_solstice, september_equinox, december_solstice
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = -dplyr::all_of("year"),
        .fns = ~ lubridate::parse_date_time(
          x = .x,
          orders = paste0("Y", orders),
          tz = "UTC"
        )
      )
    )

  cli::cli_progress_step("Locking and saving data.")

  file_name_pattern <- "time-and-date-2024"

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
