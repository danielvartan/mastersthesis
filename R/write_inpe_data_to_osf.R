# library(cli)
# library(curl)
# library(here)
# library(janitor)
# library(lockr) # github.com/danielvartan/lockr
# library(osfr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils
# library(utils)

source(here::here("R", "save_and_lock.R"))

# # Helpers
#
# file <- rstudioapi::selectFile()

# # Note
#
# The OSF API often presents issues when uploading large files.
# If you encounter any problems, try to upload the files manually.

write_inpe_data_to_osf <- function(
    osf_pat = Sys.getenv("OSF_PAT"),
    public_key = here::here("_ssh", "id_rsa.pub")
  ) {
  prettycheck:::assert_internet()
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

  cli::cli_progress_step("Downloading data.")

  raw_file <- file.path(tempdir(), "GLOBAL_HORIZONTAL_(csv).zip")

  paste0(
    "https://labren.ccst.inpe.br/projetos/atlas_2017/",
    "GLOBAL_HORIZONTAL_(csv).zip"
  ) |>
    curl::curl_download(raw_file)

  files <- raw_file |> utils::unzip(exdir = tempdir())

  cli::cli_progress_step("Importing data.")

  data <-
    files |>
    stringr::str_subset("global_horizontal_means.csv$") |>
    readr::read_delim(
      delim = ";",
      na = c("", " ", "NA"),
      col_names = TRUE,
      col_types = readr::cols(.default = "c")
    )

  cli::cli_progress_step("Tidying data.")

  data <-
    data |>
    janitor::clean_names() |>
    dplyr::rename(
      longitude = lon,
      latitude = lat,
      january = jan,
      february = feb,
      march = mar,
      april = apr,
      may = may,
      june = jun,
      july = jul,
      august = aug,
      september = sep,
      october = oct,
      november = nov,
      december = dec
    ) |>
    dplyr::mutate(
      country = dplyr::case_match(
        country,
        "Brasil" ~ "Brazil"
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(c("latitude", "longitude")),
        .fns = as.numeric
      ),
      dplyr::across(
        .cols = -dplyr::all_of(c("country", "latitude", "longitude")),
        .fns = as.integer
      )
    ) |>
    dplyr::relocate(latitude, .before = longitude)

  cli::cli_progress_step("Locking and saving data.")

  file_name_pattern <- "inpe-2017"
  raw_data_file_extension <- stringr::str_extract(raw_file, "\\.\\w+$")

  temp_file <- file.path(
    tempdir(),
    paste0(file_name_pattern, "-raw-data", raw_data_file_extension)
  )

  file.copy(from = raw_file, to = temp_file, overwrite = TRUE)

  if (prettycheck:::test_file_exists(paste0(temp_file, ".lockr"))) {
    file.remove(paste0(temp_file, ".lockr"))
  }

  lockr::lock_file(temp_file, public_key, remove_file = TRUE)

  raw_file <- paste0(temp_file, ".lockr")

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

  # file.size(raw_file) / 1e+6
  # file.size(rds_file) / 1e+6
  # file.size(csv_file) / 1e+6

  cli::cli_progress_step("Uploading data to OSF.")

  osfr::osf_upload(
    x = osfr::osf_retrieve_node(osf_id),
    path = c(raw_file, rds_file, csv_file),
    conflicts = "overwrite"
  )

  invisible()
}
