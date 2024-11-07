# library(cli)
# library(curl)
# library(dplyr)
# library(googlesheets4)
# library(here)
# library(lockr) # github.com/danielvartan/lockr
# library(osfr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils
# library(stringr)

# library(cli)
# library(dplyr)
# library(here)
# library(lockr) # github.com/danielvartan/lockr
# library(prettycheck) # github.com/danielvartan/prettycheck
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
# library(curl)
# library(here)
# library(lockr) # github.com/danielvartan/lockr
# library(osfr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils
# library(stringr)

get_lookup_data <- function(
    file = NULL,
    pattern = "lookup.rda",
    osf_pat = Sys.getenv("OSF_PAT"),
    public_key = here::here("_ssh/id_rsa.pub"),
    private_key = here::here("_ssh/id_rsa"),
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
    prettycheck:::assert_file_exists(file, extension = c("rda", "lockr"))
  } else if (!curl::has_internet()) {
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
  load(file)
  lockr::lock_file(file, public_key = public_key, remove_file = TRUE)

  invisible(lookup)
}

# library(cli)
# library(googlesheets4)
# library(here)
# library(lockr) # github.com/danielvartan/lockr
# library(osfr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils

update_lookup_data <- function(
    ss = "1GJg7qVSb5srRe4wsFBBH5jIAMF7ZvCMyaB3hbFuhCDA",
    sheet_ignore = c("Documentation", "Codebook", "Validation", "Template"),
    osf_pat = Sys.getenv("OSF_PAT"),
    public_key = here::here("_ssh/id_rsa.pub")
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

  lookup <- list()

  for (i in sheets) {
    lookup[[i]] <- googlesheets4::read_sheet(
      ss = ss, sheet = i, col_names = TRUE, col_types = "c",
      na = c("", "NA"), trim_ws = TRUE, skip = 0
    )
  }

  cli::cli_progress_step("Saving data")

  file <- file.path(tempdir(), "lookup.rda")

  save(
    list = "lookup", file = file, compress = "bzip2", version = 2,
    ascii = FALSE
    )

  if (prettycheck:::test_file_exists(paste0(file, ".lockr"))) {
    file.remove(paste0(file, ".lockr"))
  }

  lockr::lock_file(file, public_key, remove_file = TRUE)

  cli::cli_progress_step("Uploading data to OSF")

  file <- paste0(file, ".lockr")

  osfr::osf_upload(
    x = osfr::osf_retrieve_node(osf_id),
    path = file,
    conflicts = "overwrite"
  )

  invisible(NULL)
}

# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck
library(rlang)

# data |> fix_var_names() |> filter_geo("id", 92238)

filter_geo <- function(data, var, value) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_choice(var, names(data))

  data |>
    dplyr::filter(!!as.symbol(var) == value) |>
    dplyr::select(id, country, state, city, postal_code)
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
    dplyr::tibble(key = rutils:::drop_na(unique(data[[col]])), value = NA) |>
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

  invisible(NULL)
}
