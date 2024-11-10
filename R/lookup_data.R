# library(cli)
# library(dplyr)
# library(here)
# library(lockr) # github.com/danielvartan/lockr
# library(prettycheck) # github.com/danielvartan/prettycheck
library(rlang)
# library(rutils) # github.com/danielvartan/rutils

look_and_replace <- function(
    x,
    table,
    osf_pat = Sys.getenv("OSF_PAT"),
    public_key = here::here("_ssh/id_rsa.pub"),
    private_key = here::here("_ssh/id_rsa"),
    password = Sys.getenv("MASTERSTHESIS_PASSWORD"),
    na_unmatched = FALSE,
    lookup_data = NULL
  ) {
  prettycheck:::assert_character(x)
  prettycheck:::assert_string(table)
  lockr:::assert_public_key(public_key)
  prettycheck:::assert_string(password, n.chars = 32)
  lockr:::assert_private_key(private_key, password = password)
  prettycheck:::assert_flag(na_unmatched)
  prettycheck:::assert_list(lookup_data, min.len = 1, null.ok = TRUE)
  prettycheck:::assert_internet()

  if (is.null(lookup_data)) {
    cli::cli_progress_step("Downloading lookup tables")

    lookup_data <- get_lookup_data(
      osf_pat = osf_pat,
      public_key = public_key,
      private_key = private_key
    )
  }

  prettycheck:::assert_choice(table, names(lookup_data))

  lookup_table <-
    lookup_data[[table]] |>
    dplyr::select(key, value) |>
    dplyr::rename_with(.fn = ~ table, .cols = "key") |>
    dplyr::rename(lookup_value = value) |>
    dplyr::mutate(
      lookup_value = dplyr::if_else(
        is.na(lookup_value), "NA_lookup_", lookup_value)
    )

  out <-
    dplyr::tibble(!!as.symbol(table) := x) |>
    dplyr::left_join(lookup_table, by = table, na_matches = "never")

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

# library(cli)
# library(here)
# library(lockr) # github.com/danielvartan/lockr
# library(osfr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(readr)
# library(rutils) # github.com/danielvartan/rutils
# library(stringr)

get_lookup_data <- function(
    file = NULL,
    pattern = "lookup-data.rds",
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

  if (!is.null(file)) {
    prettycheck:::assert_file_exists(file, extension = c("rds", "lockr"))
  } else if (!prettycheck:::test_internet()) {
    prettycheck:::assert_internet()
  } else if (inherits(test, "try-error")) {
    cli::cli_abort(paste0(
      "The {.strong OSF PAT} provided is invalid. ",
      "Please, check the access token and try again."
    ))
  }  else {
    file <-
      osfr::osf_ls_files(
        osfr::osf_retrieve_node(osf_id),
        pattern = pattern
      ) |>
      osfr::osf_download(path = tempdir(), conflicts = "overwrite") |>
      magrittr::extract2("local_path")
  }

  lockr::unlock_file(file, private_key = private_key, password = password)
  file <- stringr::str_remove(file, "\\.lockr$")
  lookup_data <- readr::read_rds(file)
  lockr::lock_file(file, public_key = public_key, remove_file = TRUE)

  invisible(lookup_data)
}

# library(cli)
# library(googlesheets4)
# library(here)
# library(lockr) # github.com/danielvartan/lockr
# library(osfr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils

source(here::here("R", "save_and_lock.R"))

update_lookup_data <- function(
    ss = "1GJg7qVSb5srRe4wsFBBH5jIAMF7ZvCMyaB3hbFuhCDA",
    sheet_ignore = c("Documentation", "Codebook", "Validation", "Template"),
    osf_pat = Sys.getenv("OSF_PAT"),
    public_key = here::here("_ssh", "id_rsa.pub")
  ) {
  prettycheck:::assert_string(ss)
  prettycheck:::assert_character(sheet_ignore)
  lockr:::assert_public_key(public_key)
  prettycheck:::assert_interactive()
  prettycheck:::assert_internet()

  osfr::osf_auth(osf_pat) |> rutils::shush()
  osf_id <- "https://osf.io/cbqsa"
  test <- try(osfr::osf_retrieve_node(osf_id), silent = TRUE)

  if (inherits(test, "try-error")) {
    cli::cli_abort(paste0(
      "The {.strong OSF PAT} provided is invalid. ",
      "Please, check the access token and try again."
    ))
  }

  googlesheets4::gs4_auth()
  ss <- googlesheets4::gs4_get(ss)
  sheets <- ss$sheets$name[!ss$sheets$name %in% sheet_ignore]

  cli::cli_progress_step("Reading data from Google Sheets")

  lookup_data <- list()

  for (i in sheets) {
    data_i <- googlesheets4::read_sheet(
      ss = ss, sheet = i,
      col_names = TRUE,
      col_types = "c",
      na = c("", "NA"),
      trim_ws = TRUE,
      skip = 0
    )

    lookup_data[[i]] <- data_i
  }

  cli::cli_progress_step("Saving data")

  rds_files <- character()
  csv_files <- character()

  for (i in names(lookup_data)) {
    fixed_i <- i |> stringr::str_replace_all("_| ", "-")

    rds_file_i <-
      lookup_data[[i]] |>
      save_and_lock(
        file = file.path(tempdir(), paste0(fixed_i, ".rds")),
        type = "rds",
        public_key = public_key,
        compress = "bz2"
      )

    csv_file_i <-
      lookup_data[[i]] |>
      save_and_lock(
        file = file.path(tempdir(), paste0(fixed_i, ".csv")),
        type = "csv",
        public_key = public_key
      )

    rds_files <- c(rds_files, rds_file_i)
    csv_files <- c(csv_files, csv_file_i)
  }

  rds_list_file <-
    lookup_data |>
    save_and_lock(
      file = file.path(tempdir(), "lookup-data.rds"),
      type = "rds",
      public_key = public_key,
      compress = "bz2"
    )

  cli::cli_progress_step("Uploading data to OSF")

  osfr::osf_upload(
    x = osfr::osf_retrieve_node(osf_id),
    path = c(rds_list_file, rds_files, csv_files),
    conflicts = "overwrite"
  )

  invisible()
}

# get_viacep_postalcode_table

# get_geocoding_table

# library(dplyr)
# library(googlesheets4)
# library(prettycheck) # github.com/danielvartan/prettycheck

write_values_to_lookup_sheet <- function(
    data,
    sheet,
    ss = "1GJg7qVSb5srRe4wsFBBH5jIAMF7ZvCMyaB3hbFuhCDA"
    ) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_string(sheet)
  prettycheck:::assert_string(ss)
  prettycheck:::assert_interactive()
  prettycheck:::assert_internet()

  googlesheets4::gs4_auth()
  ss <- googlesheets4::gs4_get(ss)
  prettycheck:::assert_subset(sheet, ss$sheets$name)

  ss |>
    googlesheets4::sheet_resize(
      sheet = sheet, nrow = 2, ncol = NULL, exact = TRUE
    )

  ss |>
    googlesheets4::range_write(
      data = data,
      sheet = sheet,
      range = "A1",
      col_names = TRUE,
      reformat = FALSE
    )

  invisible()
}

# library(dplyr)
# library(googlesheets4)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils

# data |> write_unique_values_to_lookup_sheet(col = "state")
# data |> fix_var_names() |> write_unique_values_to_lookup_sheet(col = "name")

write_unique_values_to_lookup_sheet <- function(
    data,
    col,
    sheet = col,
    ss = "1GJg7qVSb5srRe4wsFBBH5jIAMF7ZvCMyaB3hbFuhCDA"
    ) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_string(col)
  prettycheck:::assert_choice(col, names(data))
  prettycheck:::assert_string(sheet)
  prettycheck:::assert_string(ss)
  prettycheck:::assert_interactive()
  prettycheck:::assert_internet()

  googlesheets4::gs4_auth()
  ss <- googlesheets4::gs4_get(ss)
  prettycheck:::assert_subset(sheet, ss$sheets$name)

  out <-
    dplyr::tibble(
      key = rutils:::drop_na(unique(data[[col]])),
      value = NA
    ) |>
    dplyr::arrange(key)

  ss |>
    googlesheets4::sheet_resize(
      sheet = sheet, nrow = 2, ncol = NULL, exact = TRUE
    )

  ss |>
    googlesheets4::range_write(
      data = out,
      sheet = sheet,
      range = "A1",
      col_names = TRUE,
      reformat = FALSE
    )

  invisible()
}

# library(googlesheets4)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(targets)

source(here::here("R", "utils.R"))

test_unique_values_on_lookup_table <- function(
    sheet,
    ss = "1GJg7qVSb5srRe4wsFBBH5jIAMF7ZvCMyaB3hbFuhCDA",
    setdiff = FALSE
  ) {
  prettycheck:::assert_string(sheet)
  prettycheck:::assert_string(ss)
  prettycheck:::assert_flag(setdiff)
  prettycheck:::assert_interactive()
  prettycheck:::assert_internet()

  googlesheets4::gs4_auth()

  ss <- googlesheets4::gs4_get(ss)
  prettycheck:::assert_subset(sheet, ss$sheets$name)
  lookup_data <- googlesheets4::read_sheet(ss = ss, sheet = sheet)

  raw_data <- targets::tar_read("raw_data")

  data <-
    raw_data |>
    dplyr::rename(
      track = track, name = pdNAME, email = pdEMAIL, country = pdCOUNTRY,
      state = pdSTATE, municipality = pdCITY, postal_code = pdPOSTAL,
      sleep_drugs_which = hhDRUGSwhich,
      sleep_disorder_which = hhSLEEPDISORDERwhich,
      medication_which = hhMEDICATIONwhich,
    )

  prettycheck:::assert_subset(sheet, names(data))

  unique_lookup <- lookup_data$key |> unique() |> sort()
  unique_data <- data[[sheet]] |> unique() |> sort()

  # unique_lookup |> length()
  # unique_data |> length()

  test_length <- length(unique_data) == length(unique_lookup)
  test_fun <- cli_test_fun(test_length)

  cli::cli_alert_info(paste0(
    "Is the length of the lookup data the same as the unique values in ",
    "the raw data? {.strong {test_fun(test_length)}}."
  ))

  # `identical()` is too strict for this kind of use case.
  # test_identical <- identical(unique_data, unique_lookup)

  test_identical <- length(setdiff(unique_data, unique_lookup)) == 0
  test_fun <- cli_test_fun(test_identical)

  cli::cli_alert_info(paste0(
    "Are the lookup data and the unique values in the raw data identical? ",
    "{.strong {test_fun(test_identical)}}."
  ))

  if (isTRUE(setdiff)) {
    setdiff(unique_data, unique_lookup)
  } else {
    invisible()
  }
}
