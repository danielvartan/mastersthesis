require(checkmate, quietly = TRUE)
require(cli, quietly = TRUE)
require(curl, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(gutils, quietly = TRUE)
require(googleCloudStorageR, quietly = TRUE)
require(here, quietly = TRUE)
require(lockr, quietly = TRUE)
require(readr, quietly = TRUE)
require(stringr, quietly = TRUE)
require(utils, quietly = TRUE)

#' Download, unlock, and read the project's raw data
#'
#' @description
#'
#' `get_raw_data()` download, unlock, and read the project's raw data.
#'
#' `googleCloudStorageR` must be pre-configured to get the raw data from the
#' cloud. You can also use `get_raw_data()` using a local raw data file.
#'
#' As second protection layer, you will also need a pair of RSA keys to
#' lock/unlock sensitive data.
#'
#' @param file (optional) a string pointing to the raw data path
#'   (default: `NULL`).
#' @param col_names (optional) a [`logical`][base::as.logical()] value
#'   indicating if the function must preserve the column names. See
#'   [`?readr::read_csv`][readr::read_csv()] to learn more.
#' @param iconv (optional) a [`logical`][base::as.logical()] flag indicating if
#'   the function must convert character vector between encodings
#'   (default: `TRUE`).
#'
#' @return An invisible [`tibble`][dplyr::tibble()] with a raw the dataset.
#'
#' @template param_public_key
#' @template param_private_key
#' @family data wrangling functions
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("utils", quietly = TRUE)) {
#'   raw <- get_raw_data()
#'   utils::View(raw)
#' }
#' }
get_raw_data <- function(
    file = NULL,
    col_names = TRUE,
    public_key = here::here(".ssh/id_rsa.pub"),
    private_key = here::here(".ssh/id_rsa"),
    iconv = TRUE
    ) {
  checkmate::assert_string(file, null.ok = TRUE)
  checkmate::assert_flag(col_names)
  lockr:::assert_public_key(public_key)
  lockr:::assert_private_key(private_key)
  checkmate::assert_flag(iconv)

  test <- try(googleCloudStorageR::gcs_list_objects(), silent = TRUE)

  if (!is.null(file)) {
    checkmate::assert_file_exists(file, extension = c("csv", "zip", "lockr"))
  } else if (!curl::has_internet()) {
    gutils:::assert_internet()
  } else if (inherits(test, "try-error")) {
    cli::cli_abort(paste0(
      "{.strong {cli::col_red('googleCloudStorageR')}} needs to be", " ",
      "configured to get the {.strong {cli::col_blue('raw data')}} from", " ",
      "the cloud. Contact the main author for more information."
    ))
  } else {
    cli::cli_progress_step("Downloading raw data")

    file <- tempfile(fileext = ".zip.lockr")
    googleCloudStorageR::gcs_get_object("raw-data.zip.lockr", saveToDisk = file)
  }

  if (any(grepl("\\.lockr$", file), na.rm = TRUE)) {
    cli::cli_progress_step("Decrypting data")

    for (i in file) {
      lockr::unlock_file(i, private_key, remove_file = TRUE)

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
        file = i, col_names = col_names, public_key = public_key,
        private_key = private_key, iconv = iconv
      )
      out <- dplyr::bind_rows(out, data)
    }
  } else {
    cli::cli_progress_step("Reading data")

    out <-
      file |>
      readr::read_csv(na = c("", "NA"),
                      col_names = col_names,
                      col_types = readr::cols(.default = "c")
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
