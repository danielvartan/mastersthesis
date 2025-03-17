# library(cli)
# library(dplyr)
# library(here)
# library(magrittr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils

source(here::here("R", "get_brazil_municipality.R"))
source(here::here("R", "get_country_names.R"))
source(here::here("R", "get_lookup_data.R"))
source(here::here("R", "test_unique_values_on_lookup_table.R"))
source(here::here("R", "utils.R"))

# # Helpers
#
# check_issues_with_the_lookup_tables()
#
# raw_data |>
#   cross_geographic_data_by_postal_code(
#     col = "municipality",
#     value = "Itaoca",
#     limit = Inf,
#     col_merge = c("municipality", "state"),
#     col_match = "state",
#     maxDist = 1
#   ) |>
#   dplyr::glimpse()
#
# get_brazil_municipality(force = TRUE) |>
#   dplyr::filter(stringr::str_detect(municipality, "Desterro")) |>
#   dplyr::select(state, municipality) |>
#   print(n = Inf)

check_issues_with_the_lookup_tables <- function() { #nolint
  data <-
    targets::tar_read("tidy_data") |>
    dplyr::filter(country == "Brazil")

  lookup_data <- get_lookup_data()

  ## `track`

  cli::cli_h1("track")

  test_unique_values_on_lookup_table("track")

  data_test <-
    lookup_data$track |>
    dplyr::pull("value") |>
    unique() |>
    rutils::drop_na()

  data_test |>
    stringr::str_detect("^[a-z0-9 ]*$", negate = TRUE) |>
    lookup_table_issue_warning("track", type = "logical")

  ## `name`

  cli::cli_h1("name")

  test_unique_values_on_lookup_table("name")

  data_test <-
    lookup_data$name |>
    dplyr::pull("value") |>
    unique() |>
    rutils::drop_na()

  data_test |>
    groomr::to_ascii() |>
    stringr::str_to_lower() |>
    stringr::str_detect("^[a-z'.\\- ]*$|^R E M O V E$", negate = TRUE) |>
    magrittr::extract(TRUE) |>
    lookup_table_issue_warning("name", type = "name")

  ## `email`

  cli::cli_h1("email")

  test_unique_values_on_lookup_table("email")

  data_test <-
    lookup_data$email |>
    dplyr::pull("value") |>
    unique() |>
    rutils::drop_na()

  data_test |>
    stringr::str_detect(
      "^[[:alnum:]._-]+@[[:alnum:].-]+$|^R E M O V E$",
      negate = TRUE
    ) |>
    lookup_table_issue_warning("email", type = "logical")

  ## `country`

  cli::cli_h1("country")

  test_unique_values_on_lookup_table("country")

  data_test <-
    lookup_data$country |>
    dplyr::pull("value") |>
    unique() |>
    rutils::drop_na()

  data_test |>
    setdiff(get_country_names()) |>
    lookup_table_issue_warning("country", type = "length")

  ## `state`

  cli::cli_h1("state")

  test_unique_values_on_lookup_table("state")

  data_test <-
    data |>
    dplyr::pull("state") |>
    unique() |>
    rutils::drop_na()

  data_test |>
    setdiff(get_brazil_state()) |>
    lookup_table_issue_warning("state", type = "length")

  ## `municipality`

  cli::cli_h1("municipality")

  test_unique_values_on_lookup_table("municipality")

  data_test <-
    data |>
    dplyr::pull("municipality") |>
    unique() |>
    rutils::drop_na()

  data_test |>
    setdiff(get_brazil_municipality(force = TRUE)$municipality) |>
    lookup_table_issue_warning("municipality", type = "length")

  ## `postal_code`

  cli::cli_h1("postal_code")

  test_unique_values_on_lookup_table("postal_code")

  data_test <-
    lookup_data$postal_code |>
    dplyr::pull("value") |>
    unique() |>
    rutils::drop_na()

  data_test |>
    stringr::str_detect("^\\d{8}$", negate = TRUE) |>
    lookup_table_issue_warning("postal_code", type = "postal_code")

  ## `sleep_drugs_which`

  cli::cli_h1("sleep_drugs_which")

  test_unique_values_on_lookup_table("sleep_drugs_which")

  ## `sleep_disorder_which`

  cli::cli_h1("sleep_disorder_which")

  test_unique_values_on_lookup_table("sleep_disorder_which")

  ## `medication_which`

  cli::cli_h1("medication_which")

  test_unique_values_on_lookup_table("medication_which")

  ## `geocodes`

  cli::cli_h1("geocodes")

  ## `special_cases`

  cli::cli_h1("special_cases")

  data_test <-
    lookup_data$special_cases|>
    dplyr::count(id) |>
    dplyr::filter(n > 4)

  data_test |>
    dplyr::pull(id) |>
    lookup_table_issue_warning("special_cases", type = "special_cases_1")

  data |>
    janitor::get_dupes() |>
    rutils::shush() |>
    dplyr::pull(id) |>
    lookup_table_issue_warning("special_cases", type = "special_cases_2")
}

# library(cli)
# library(prettycheck) # github.com/danielvartan/prettycheck

lookup_table_issue_warning <- function(test, col_name, type = "length") {
  checkmate::assert_atomic(test)
  checkmate::assert_string(col_name)
  checkmate::assert_string(type)

  if (type == "length" && !length(test) == 0) {
    cli::cli_alert_warning(
      paste0(
        "There {cli::qty(length(test))}{?is/are} {.strong {length(test)}} ",
        "value{?s} in the {.strong {cli::col_red(col_name)}} column that ",
        "{cli::qty(length(test))}{?is/are} incorrect."
      ),
      wrap = TRUE
    )

    cli::cat_line()
    print(test)
    cli::cat_line()
  } else if (type == "logical" && any(test)) {
    cli::cli_alert_warning(
      paste0(
        "There {cli::qty(sum(test))}{?is/are} {.strong {sum(test)}} ",
        "value{?s} in the {.strong {cli::col_red(col_name)}} column that ",
        "{cli::qty(sum(test))}{?is/are} incorrect."
      ),
      wrap = TRUE
    )

    cli::cat_line()
  } else if (type == "name" && any(test)) {
    test_length <- test[test == TRUE] |> length()

    cli::cli_alert_warning(
      paste0(
        "There {cli::qty(length(test))}{?is/are} {.strong {length(test)}} ",
        "value{?s} in the {.strong {cli::col_red(col_name)}} column that ",
        "{cli::qty(length(test))}{?is/are} incorrect."
      ),
      wrap = TRUE
    )

    cli::cat_line()
  } else if (type == "postal_code" && any(test)) {
    cli::cli_alert_warning(
      paste0(
        "There {cli::qty(test)}{?is/are} {.strong {length(test)}} value{?s} ",
        "in the {.strong {cli::col_red(col_name)}} column that don't have ",
        "8 digits."
      ),
      wrap = TRUE
    )

    cli::cat_line()
    print(test)
    cli::cat_line()
  }  else if (type == "special_cases_1" && !length(test) == 0) {
    cli::cli_alert_warning(
      paste0(
        "{.strong {length(test)}} {.strong {cli::col_blue('id')}} value{?s} ",
        "in the {.strong {cli::col_red(col_name)}} table have more than ",
        "4 entries."
      ),
      wrap = TRUE
    )

    cli::cat_line()
    print(test)
    cli::cat_line()
  } else if (type == "special_cases_2" && !length(test) == 0) {
    cli::cli_alert_warning(
      paste0(
        "There {cli::qty(length(test))}{?is/are} {.strong {length(test)}} ",
        "duplicated value{?s} in the {.strong {cli::col_red(col_name)}} ",
        "table."
      ),
      wrap = TRUE
    )

    cli::cat_line()
    print(test)
    cli::cat_line()
  }
}
