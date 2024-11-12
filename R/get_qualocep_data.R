# library(cli)
# library(here)
# library(lockr) # github.com/danielvartan/lockr
# library(osfr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(readr)
# library(rutils) # github.com/danielvartan/rutils
# library(stringr)

get_qualocep_data <- function(
    file = NULL,
    pattern = "qualocep-2020-01-14.rds",
    osf_pat = Sys.getenv("OSF_PAT"),
    public_key = here::here("_ssh", "id_rsa.pub"),
    private_key = here::here("_ssh", "id_rsa"),
    password = Sys.getenv("MASTERSTHESIS_PASSWORD")
  ) {
  prettycheck:::assert_string(file, null.ok = TRUE)
  prettycheck:::assert_string(pattern)
  prettycheck:::assert_string(osf_pat, n.chars = 70)
  lockr:::assert_public_key(public_key)
  lockr:::assert_private_key(private_key, password = password)
  prettycheck:::assert_string(password, n.chars = 32)

  osfr::osf_auth(osf_pat) |> rutils::shush()
  osf_id <- "https://osf.io/cbqsa"
  test <- try(osfr::osf_retrieve_node(osf_id), silent = TRUE)

  qualocep_temp_file <- file.path(tempdir(), pattern)

  if (!is.null(file)) {
    prettycheck:::assert_file_exists(file, extension = c("rds", "lockr"))
  } else if (!prettycheck:::test_internet()) {
    prettycheck:::assert_internet()
  } else if (inherits(test, "try-error")) {
    cli::cli_abort(paste0(
      "The {.strong {cli::col_red('OSF PAT')}} provided is invalid. ",
      "Please, check the access token and try again."
    ))
  } else if (prettycheck:::test_file_exists(qualocep_temp_file)) {
    readr::read_rds(qualocep_temp_file)
  } else {
    file <-
      osfr::osf_ls_files(
        osfr::osf_retrieve_node(osf_id),
        pattern = pattern
      ) |>
      osfr::osf_download(path = tempdir(), conflicts = "overwrite") |>
      magrittr::extract2("local_path")

    lockr::unlock_file(file, private_key = private_key, password = password)

    file <- stringr::str_remove(file, "\\.lockr$")

    readr::read_rds(file)
  }
}
