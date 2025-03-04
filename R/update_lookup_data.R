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
  prettycheck::assert_internet()
  prettycheck::assert_interactive()
  checkmate::assert_string(ss)
  checkmate::assert_character(sheet_ignore)
  checkmate::assert_string(osf_pat, n.chars = 70)
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

  ss <- googlesheets4::gs4_get(ss)
  sheets <- ss$sheets$name[!ss$sheets$name %in% sheet_ignore]

  cli::cli_progress_step("Reading data from Google Sheets")

  lookup_data <- list()

  for (i in sheets) {
    data_i <- googlesheets4::read_sheet(
      ss = ss,
      sheet = i,
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

  lookup_data$special_cases <-
    lookup_data$special_cases |>
    dplyr::mutate(id = as.integer(id))

  lookup_data$geocodes <-
    lookup_data$geocodes |>
    dplyr::mutate(
      municipality_code = as.integer(municipality_code),
      state_code = as.integer(state_code),
      latitude = as.numeric(latitude),
      longitude = as.numeric(longitude)
    )

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
