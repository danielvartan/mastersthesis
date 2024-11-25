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
    data,
    osf_pat = Sys.getenv("OSF_PAT"),
    public_key = here::here("_ssh", "id_rsa.pub"),
    upload_to_osf = FALSE,
    name_pattern = deparse(substitute(data))
  ) {
  prettycheck:::assert_internet()
  prettycheck:::assert_string(osf_pat, n.chars = 70)
  lockr:::assert_public_key(public_key)
  prettycheck:::assert_flag(upload_to_osf)
  prettycheck:::assert_string(name_pattern)

  osfr::osf_auth(osf_pat) |> rutils::shush()
  osf_id <- "https://osf.io/cbqsa"
  test <- try(osfr::osf_retrieve_node(osf_id), silent = TRUE)

  if (inherits(test, "try-error")) {
    cli::cli_abort(paste0(
      "The {.strong {cli::col_red('OSF PAT')}} provided is invalid. ",
      "Please, check the access token and try again."
    ))
  }

  cli::cli_progress_step("Locking and saving data.")

  file_name_pattern <- name_pattern

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
        .cols = dplyr::where(~ prettycheck:::test_temporal(.x, rm = "Date")),
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

  # file.size(rds_file) / 1e+6
  # file.size(csv_file) / 1e+6

  if (isTRUE(upload_to_osf)) {
    cli::cli_progress_step("Uploading data to OSF.")

    osfr::osf_upload(
      x = osfr::osf_retrieve_node(osf_id),
      path = c(rds_file, csv_file),
      conflicts = "overwrite"
    )
  }

  invisible(data)
}
