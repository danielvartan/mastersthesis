# library(cli)
# library(here)
# library(janitor)
# library(lockr) # github.com/danielvartan/lockr
# library(osfr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils
# library(utils)

source(here::here("R", "fix_qualocep_data_values.R"))
source(here::here("R", "save_and_lock.R"))

# # Helpers
#
# file <- rstudioapi::selectFile()

# # Notes
#
# The OSF API often presents issues when uploading large files.
# If you encounter any problems, try to upload the files manually.

write_qualocep_data_to_osf <- function(
    file, # 2024 version
    purchase_date = lubridate::as_date("2024-11-12"),
    osf_pat = Sys.getenv("OSF_PAT"),
    public_key = here::here("_ssh", "id_rsa.pub")
  ) {
  prettycheck:::assert_file_exists(file, access = "r")
  prettycheck:::assert_date(purchase_date)
  prettycheck:::assert_string(osf_pat, n.chars = 70)
  lockr:::assert_public_key(public_key)
  prettycheck:::assert_internet()

  osfr::osf_auth(osf_pat) |> rutils::shush()
  osf_id <- "https://osf.io/cbqsa"
  test <- try(osfr::osf_retrieve_node(osf_id), silent = TRUE)

  if (inherits(test, "try-error")) {
    cli::cli_abort(paste0(
      "The {.strong {cli::col_red('OSF PAT')}} provided is invalid. ",
      "Please, check the access token and try again."
    ))
  }

  cli::cli_progress_step("Importing data")

  raw_file <- file

  if (file |> stringr::str_detect("\\.zip$")) {
    files <-
      file |>
      utils::unzip(exdir = tempdir())

    file <-
      files |>
      stringr::str_subset("tabela_integrada\\.csv$")

    file_geo <-
      files |>
      stringr::str_subset("qualocep_geo\\.csv$")
  }

  if (!stringr::str_detect(file, "tabela_integrada\\.csv$")) {
    cli::cli_abort(paste0(
      "The file {.strong {cli::col_red(basename(raw_file))}} is not a valid ",
      "QualoCep table. It must be a {.strong .csv} file with the name ",
      "{.strong {cli::col_blue('tabela_integrada.csv')}}, or a ",
      "{.strong .zip} file containing the {.strong tabela_integrada.csv} file."
    ))
  }

  data <-
    file |>
    readr::read_delim(
      delim = "|",
      na = c("", " ", "NA"),
      col_names = TRUE,
      col_types = readr::cols(.default = "c")
    )

  cli::cli_progress_step("Tidying data")

  data <-
    data |>
    janitor::clean_names() |>
    dplyr::rename(
      postal_code = cep,
      street_type = tipo_logradouro,
      street_name = logradouro,
      complement = complemento,
      place = local,
      neighborhood = bairro,
      municipality_code = cod_cidade,
      municipality = cidade,
      federal_unit_code = cod_estado,
      federal_unit = uf,
      state = estado
    ) |>
    dplyr::mutate(
      street = paste0(street_type, " ", street_name),
      municipality_code = as.integer(municipality_code),
      federal_unit_code = as.integer(federal_unit_code)
    ) |>
    dplyr::relocate(
      postal_code, street_type, street_name, street, complement, place,
      neighborhood, municipality_code, municipality, federal_unit_code,
      federal_unit, state
    )

  if ("file_geo" %in% ls()) {
    if (!length(file_geo) == 0) {
      data_geo <-
        file_geo |>
        readr::read_delim(
          delim = "|",
          na = c("", " ", "NA"),
          col_names = TRUE,
          col_types = readr::cols(.default = "c")
        ) |>
        dplyr::rename(postal_code = cep) |>
        dplyr::mutate(
          latitude = as.numeric(latitude),
          longitude = as.numeric(longitude)
        ) |>
        rutils::shush()

      data <-
        data |>
        dplyr::left_join(data_geo, by = "postal_code")
    }
  }

  data <-
    data |>
    fix_qualocep_data_values()

  cli::cli_progress_step("Saving data")

  file_name_pattern <- paste0("qualocep-", purchase_date)
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

  cli::cli_progress_step("Uploading data to OSF")

  osfr::osf_upload(
    x = osfr::osf_retrieve_node(osf_id),
    path = c(raw_file, rds_file, csv_file),
    conflicts = "overwrite"
  )

  invisible()
}
