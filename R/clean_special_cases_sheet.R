# library(dplyr)
# library(googlesheets4)
# library(here)
# library(prettycheck) # github.com/danielvartan/prettycheck

source(here::here("R", "write_values_to_lookup_sheet.R"))

clean_special_cases_sheet <- function(
    ss = "1GJg7qVSb5srRe4wsFBBH5jIAMF7ZvCMyaB3hbFuhCDA"
  ) {
  prettycheck:::assert_internet()
  prettycheck:::assert_interactive()
  prettycheck:::assert_string(ss)

  ss <- googlesheets4::gs4_get(ss)

  cli::cli_progress_step("Reading data from Google Sheets")

  data <- googlesheets4::read_sheet(
    ss = ss, sheet = "special_cases",
    col_names = TRUE,
    col_types = "c",
    na = c("", "NA"),
    trim_ws = TRUE,
    skip = 0
  )

  data <-
    data |>
    dplyr::distinct() |>
    dplyr::mutate(
      id = as.integer(id),
      value = dplyr::if_else(is.na(value), "NA", value)
    ) |>
    dplyr::arrange(id)

  data |>
    write_values_to_lookup_sheet(
    ss = ss$spreadsheet_id,
    sheet = "special_cases"
  )
}
