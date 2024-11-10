# library(cli)
# library(dplyr)
# library(here)
# library(lockr) # github.com/danielvartan/lockr
# library(osfr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(readr)
# library(rutils) # github.com/danielvartan/rutils
# library(stringr)
# library(utils)

#' Download, unlock, and read the project's raw data
#'
#' @description
#'
#' `get_raw_data()` download, unlock, and read the project's raw data.
#'
#' `osfr` must be pre-configured to get the raw data from the cloud. You can
#' also use `get_raw_data()` using a local raw data file.
#'
#' As second protection layer, you will also need a pair of RSA keys to
#' lock/unlock the data.
#'
#' @param file (optional) a string pointing to the raw data path
#'   (default: `NULL`).
#' @param col_names (optional) a [`logical`][base::as.logical()] value
#'   indicating if the function must preserve the column names. See
#'   [`?readr::read_csv`][readr::read_csv()] to learn more (default: `TRUE`).
#' @param osf_pat (optional) a string with the OSF Personal Access Token (PAT).
#'   If you already configure it on `.Renviron`, use the default value. If not,
#'   enter the key using the [`askpass()`][askpass::askpass()] function
#'   (default: `Sys.getenv("OSF_PAT")`).
#' @param public_key (optional) an [`openssl`][openssl::rsa_keygen()] RSA public
#'   key or a string specifying the public key path. This key must be the same
#'   one provided by the thesis author (default:
#'   `here::here("_ssh/id_rsa.pub")`).
#' @param private_key (optional) an [`openssl`][openssl::rsa_keygen()] RSA
#'   private key or a string specifying the private key path. This key must be
#'   the same one provided by the thesis author (default:
#'   `"here::here(_ssh/id_rsa")`).
#' @param password (optional) a string with the password to unlock the data.
#'  If you already configure it on `.Renviron`, use the default value. If not,
#'  enter the key using the [`askpass()`][askpass::askpass()]
#'  function (default: `Sys.getenv("MASTERSTHESIS_PASSWORD")`).
#' @param iconv (optional) a [`logical`][base::as.logical()] flag indicating if
#'   the function must convert character vector between encodings
#'   (default: `TRUE`).
#'
#' @return An invisible [`tibble`][dplyr::tibble()] with a raw the dataset.
#'
#' @family data munging functions
#' @noRd
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("utils")) {
#'   raw_data <- get_raw_data()
#'   utils::View(raw_data)
#' }
#' }
get_raw_data <- function(
    file = NULL,
    col_names = TRUE,
    osf_pat = Sys.getenv("OSF_PAT"),
    public_key = here::here("_ssh/id_rsa.pub"),
    private_key = here::here("_ssh/id_rsa"),
    password = Sys.getenv("MASTERSTHESIS_PASSWORD"),
    iconv = TRUE
  ) {
  prettycheck:::assert_string(file, null.ok = TRUE)
  prettycheck:::assert_flag(col_names)
  prettycheck:::assert_string(osf_pat, n.chars = 70, null.ok = TRUE)
  lockr:::assert_public_key(public_key)
  prettycheck:::assert_string(password, n.chars = 32, null.ok = TRUE)
  lockr:::assert_private_key(private_key, password = password)
  prettycheck:::assert_flag(iconv)

  osfr::osf_auth(osf_pat) |> rutils::shush()

  osf_id <- "https://osf.io/huy4r"
  test <- try(osfr::osf_retrieve_node(osf_id), silent = TRUE)

  if (!is.null(file)) {
    prettycheck:::assert_file_exists(file, extension = c("csv", "zip", "lockr"))
  } else if (!prettycheck:::test_internet()) {
    prettycheck:::assert_internet()
  } else if (inherits(test, "try-error")) {
    cli::cli_abort(paste0(
      "The {.strong OSF PAT} provided is invalid. ",
      "Please, check the access token and try again."
    ))
  } else {
    cli::cli_progress_step("Downloading raw data")

    file <-
      osfr::osf_ls_files(
        osfr::osf_retrieve_node(osf_id),
        pattern = "raw-data.zip"
      ) |>
      osfr::osf_download(path = tempdir(), conflicts = "overwrite") |>
      magrittr::extract2("local_path")
  }

  if (any(grepl("\\.lockr$", file), na.rm = TRUE)) {
    cli::cli_progress_step("Decrypting data")

    for (i in file) {
      lockr::unlock_file(
        file = i,
        private_key = private_key,
        remove_file = TRUE,
        password = password
      )

      file[file == i] <- stringr::str_remove(file, "\\.lockr$")
    }

    cli::cli_progress_done()
  }

  if (any(stringr::str_detect(file, "(?i).zip$"), na.rm = TRUE)) {
    cli::cli_progress_step("Unziping data")

    temp_dir <- tempfile("dir")
    dir.create(temp_dir)

    zip_files <- file[stringr::str_detect(file, "(?i).zip$")]

    for (i in zip_files) {
      j <- utils::unzip(i, exdir = temp_dir)

      lockr::lock_file(i, public_key, remove_file = TRUE)

      file <- file[!file == i] |> append(j)
    }

    cli::cli_progress_done()
  }

  if (length(file) > 1) {
    out <- dplyr::tibble()

    for (i in file) {
      data <- get_raw_data(
        file = i,
        col_names = col_names,
        public_key = public_key,
        private_key = private_key,
        password = password,
        iconv = iconv
      )

      out <- dplyr::bind_rows(out, data)
    }
  } else {
    cli::cli_progress_step("Reading data")

    out <-
      file |>
      readr::read_csv(
        na = c("", "NA"),
        col_names = col_names,
        col_types = readr::cols(.default = "c"
        )
      )

    if (isTRUE(iconv)) {
      out <-
        out |>
        dplyr::mutate(
          dplyr::across(
            .cols = dplyr::everything(),
            .fns = ~ iconv(.x, from = "UTF-8", to = "iso-8859-1")
          ),
          dplyr::across(
            .cols = dplyr::everything(), .fns = ~ iconv(enc2utf8(.x))
          )
        )
    }

    if (isFALSE(col_names)) out <- out |> dplyr::slice(-1)

    lockr::lock_file(file, public_key, remove_file = TRUE)

    cli::cli_progress_done()
  }

  invisible(out)
}
