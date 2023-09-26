# library(checkmate, quietly = TRUE)
# library(cli, quietly = TRUE)
# library(curl, quietly = TRUE)
# library(dplyr, quietly = TRUE)
# library(googleCloudStorageR, quietly = TRUE)
# library(googlesheets4, quietly = TRUE)
# library(here, quietly = TRUE)
# library(lockr, quietly = TRUE)
# library(rutils, quietly = TRUE)
# library(stringr, quietly = TRUE)

look_and_replace <- function(
    x,
    table,
    public_key = here::here(".ssh/id_rsa.pub"),
    private_key = here::here(".ssh/id_rsa"),
    na_unmatched = FALSE
    ) {
  checkmate::assert_character(x)
  checkmate::assert_string(table)
  lockr:::assert_public_key(public_key)
  lockr:::assert_private_key(private_key)
  checkmate::assert_flag(na_unmatched)
  rutils:::assert_internet()

  cli::cli_progress_step("Downloading lookup tables")

  lookup <- get_lookup_data(public_key = public_key, private_key = private_key)
  checkmate::assert_choice(table, names(lookup))

  lookup_data <-
    lookup[[table]] |>
    dplyr::rename_with(.fn = ~ table, .cols = "key") |>
    dplyr::rename(lookup_value = value) |>
    dplyr::mutate(
      lookup_value = dplyr::if_else(
        is.na(lookup_value), "NA_lookup_", lookup_value)
    )

  out <-
    dplyr::tibble(!!as.symbol(table) := x) |>
    dplyr::left_join(lookup_data, by = table, na_matches = "never")

  if (isTRUE(na_unmatched)) {
    out <-
      out |>
      dplyr::mutate(
        !!as.symbol(table) := dplyr::case_when(
          lookup_value == "NA_lookup_" ~ as.character(NA),
          TRUE ~ lookup_value
        )
      )
  } else {
    out <-
      out |>
      dplyr::mutate(
        !!as.symbol(table) := dplyr::case_when(
          lookup_value == "NA_lookup_" ~ as.character(NA),
          is.na(lookup_value) ~ !!as.symbol(table),
          TRUE ~ lookup_value
        )
      )
  }

  out[[table]]
}

get_lookup_data <- function(
    file = NULL,
    public_key = here::here(".ssh/id_rsa.pub"),
    private_key = here::here(".ssh/id_rsa")
    ) {
  checkmate::assert_string(file, null.ok = TRUE)
  lockr:::assert_public_key(public_key)
  lockr:::assert_private_key(private_key)

  test <- try(googleCloudStorageR::gcs_list_objects(), silent = TRUE)

  if (!is.null(file)) {
    checkmate::assert_file_exists(file, extension = c("rda", "lockr"))
  } else if (!curl::has_internet()) {
    rutils:::assert_internet()
  } else if (inherits(test, "try-error") && curl::has_internet()) {
    cli::cli_abort(paste0(
      "{.strong {cli::col_red('googleCloudStorageR')}} needs to be", " ",
      "configured to get the {.strong {cli::col_blue('lookup data')}}", " ",
      "from the cloud. Contact the main author for more information."
    ))
  }  else {
    file <- tempfile(fileext = ".rda.lockr")
    googleCloudStorageR::gcs_get_object("lookup.rda.lockr", saveToDisk = file)
  }

  lockr::unlock_file(file, private_key  = private_key)
  file <- stringr::str_remove(file, "\\.lockr$")
  load(file)
  lockr::lock_file(file, public_key = public_key, remove_file = TRUE)

  invisible(lookup)
}

update_lookup <- function(
    ss = "1GJg7qVSb5srRe4wsFBBH5jIAMF7ZvCMyaB3hbFuhCDA",
    sheet_ignore = c("Documentation", "Codebook", "Validation", "Template"),
    public_key = here::here(".ssh/id_rsa.pub")
    ) {
  checkmate::assert_string(ss)
  checkmate::assert_character(sheet_ignore)
  lockr:::assert_public_key(public_key)
  rutils:::assert_interactive()
  rutils:::assert_internet()

  googlesheets4::gs4_auth()
  ss <- googlesheets4::gs4_get(ss)
  sheets <- ss$sheets$name[!ss$sheets$name %in% sheet_ignore]

  cli::cli_progress_step("Reading data from Google Sheets")

  lookup <- list()

  for (i in sheets) {
    lookup[[i]] <- googlesheets4::read_sheet(
      ss = ss, sheet = i, col_names = TRUE, col_types = "c",
      na = c("", "NA"), trim_ws = TRUE, skip = 0
    )
  }

  cli::cli_progress_step("Saving data in temp directory")

  file <- file.path(tempdir(), "lookup.rda")
  save(
    list = "lookup", file = file, compress = "bzip2", version = 2,
    ascii = FALSE
    )

  if (checkmate::test_file_exists(paste0(file, ".lockr"))) {
    file.remove(paste0(file, ".lockr"))
  }

  cli::cli_progress_step("Locking data")

  lockr::lock_file(file, public_key, remove_file = TRUE)

  cli::cli_progress_step("Uploading data to Google Cloud")

  file <- paste0(file, ".lockr")
  googleCloudStorageR::gcs_upload(
    file, name = basename(file), predefinedAcl = "default"
    )

  invisible(NULL)
}

filter_state <- function(data, state) {
    checkmate::assert_tibble(data)
    checkmate::assert_string(state)

    i <- state

    data |>
        dplyr::filter(state == i) |>
        dplyr::select(id, country, state, city)
}

write_unique_values_to_lookup_sheet <- function(
    data, col, sheet = col,
    ss = "1GJg7qVSb5srRe4wsFBBH5jIAMF7ZvCMyaB3hbFuhCDA"
    ) {
  checkmate::assert_tibble(data)
  checkmate::assert_string(col)
  checkmate::assert_choice(col, names(data))
  checkmate::assert_string(sheet)
  checkmate::assert_string(ss)
  rutils:::assert_interactive()
  rutils:::assert_internet()

  googlesheets4::gs4_auth()
  ss <- googlesheets4::gs4_get(ss)
  checkmate::assert_subset(sheet, ss$sheets$name)

  out <-
    dplyr::tibble(key = rutils:::drop_na(unique(data[[col]])), value = NA) |>
    dplyr::arrange(key)

  ss |>
    googlesheets4::sheet_resize(
      sheet = sheet, nrow = 2, ncol = NULL, exact = TRUE
      )

  ss |>
    googlesheets4::range_write(
      data = out, sheet = sheet, range = "A1", col_names = TRUE,
      reformat = FALSE
      )

  invisible(NULL)
}
