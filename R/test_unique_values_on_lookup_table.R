# library(googlesheets4)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(targets)

source(here::here("R", "utils.R"))

test_unique_values_on_lookup_table <- function(
    sheet,
    ss = "1GJg7qVSb5srRe4wsFBBH5jIAMF7ZvCMyaB3hbFuhCDA"
  ) {
  prettycheck:::assert_string(sheet)
  prettycheck:::assert_string(ss)
  prettycheck:::assert_interactive()
  prettycheck:::assert_internet()

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

  c(test_length, test_fun)
}
