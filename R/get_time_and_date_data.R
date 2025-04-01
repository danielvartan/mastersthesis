# library(cli)
# library(here)
# library(lockr) # github.com/danielvartan/lockr
# library(osfr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(readr)
# library(rutils) # github.com/danielvartan/rutils
# library(stringr)

get_time_and_date_data <- function(
    file = NULL, #nolint
    pattern = "time-and-date-2024.rds",
    osf_pat = Sys.getenv("OSF_PAT"),
    public_key = here::here("_ssh", "id_rsa.pub"),
    private_key = here::here("_ssh", "id_rsa"),
    password = Sys.getenv("MASTERSTHESIS_PASSWORD"),
    save_to_globalenv = TRUE,
    force = FALSE
  ) {
  checkmate::assert_string(file, null.ok = TRUE)
  checkmate::assert_string(pattern)
  checkmate::assert_string(osf_pat, n.chars = 70)
  lockr:::assert_public_key(public_key)
  lockr:::assert_private_key(private_key, password = password)
  checkmate::assert_string(password, n.chars = 32)
  checkmate::assert_flag(save_to_globalenv)
  checkmate::assert_flag(force)

  time_and_date_temp_file <- file.path(tempdir(), pattern)

  if (!is.null(file)) {
    checkmate::assert_file_exists(file, extension = c("rds", "lockr"))

    if (stringr::str_detect(file, "\\.lockr$")) {
      lockr::unlock_file(file, private_key = private_key, password = password)

      file <- stringr::str_remove(file, "\\.lockr$")
    }

    out <- readr::read_rds(file)
  } else if ("time_and_date_data" %in% ls(envir = globalenv()) &&
             isFALSE(force)) {
    cli::cli_alert_info(
      paste0(
        "Using Time and Date AS data from the Global Enviroment ",
        "(`time_and_date_data`)."
      ),
      wrap = TRUE
    )

    return(get("time_and_date_data", envir = globalenv()))
  } else if (checkmate::test_file_exists(time_and_date_temp_file) &&
             isFALSE(force)) {
    cli::cli_alert_info(
      "Using Time and Date AS data file from the temporary directory.",
      wrap = TRUE
    )

    out <- readr::read_rds(time_and_date_temp_file)
  } else {
    prettycheck::assert_internet()

    cli::cli_progress_step("Downloading Time and Date AS data from OSF.")

    osfr::osf_auth(osf_pat) |> rutils::shush()
    osf_id <- "https://osf.io/cbqsa"
    test <- try(osfr::osf_retrieve_node(osf_id), silent = TRUE)

    if (inherits(test, "try-error")) {
      cli::cli_abort(paste0(
        "The {.strong {cli::col_red('OSF PAT')}} provided is invalid. ",
        "Please, check the access token and try again."
      ))
    }

    file <-
      osfr::osf_ls_files(
        osfr::osf_retrieve_node(osf_id),
        pattern = pattern
      ) |>
      osfr::osf_download(path = tempdir(), conflicts = "overwrite") |>
      magrittr::extract2("local_path")

    old_file_pattern <- stringr::str_remove(file, "\\.lockr$")

    if (checkmate::test_file_exists(old_file_pattern)) {
      file.remove(old_file_pattern)
    }

    lockr::unlock_file(file, private_key = private_key, password = password)

    file <- stringr::str_remove(file, "\\.lockr$")

    out <- readr::read_rds(file)
  }

  if (isTRUE(save_to_globalenv)) {
    cli::cli_progress_step(paste0(
      "Saving Time and Date AS data to the Global Enviroment ",
      "(`time_and_date_data`)."
    ))

    assign("time_and_date_data", out, envir = globalenv())

    out
  } else {
    out
  }
}
