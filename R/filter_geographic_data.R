# library(cli)
# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck
library(rlang)
# library(stringr)

source(here::here("R", "tidy_data_.R"))
source(here::here("R", "get_brazil_address_by_postal_code.R"))

# # Helpers
#
# raw_data <- targets::tar_read("raw_data")
#
# source(here::here("R", "filter_geographic_data.R"))
# raw_data |> filter_geographic_data("municipality", "Tab")
#
# source(here::here("R", "get_brazil_address_by_postal_code.R"))
# get_brazil_address_by_postal_code("01223000") |> dplyr::glimpse()
#
# raw_data |> filter_geographic_data_and_get_address("municipality", "sp")

filter_geographic_data <- function(raw_data, col, value) {
  prettycheck:::assert_tibble(raw_data)
  prettycheck:::assert_string(col)
  prettycheck:::assert_string(value)

  data <- raw_data |> fix_col_names()
  col <- stringr::str_squish(col)

  prettycheck:::assert_choice(stringr::str_squish(col), names(data))

  data |>
    dplyr::filter(!!as.symbol(col) == value) |>
    dplyr::select(id, country, state, municipality, postal_code)
}

filter_geographic_data_and_get_address <- function(
    raw_data,
    col,
    value,
    method = "qualocep"
  ) {
  prettycheck:::assert_tibble(raw_data)
  prettycheck:::assert_string(col)
  prettycheck:::assert_string(value)
  prettycheck:::assert_choice(method, c("osm", "qualocep", "viacep"))

  filtered_data <-
    raw_data |>
    filter_geographic_data(col, value)

  filtered_data |> print()

  cli::cat_line()

  filtered_data$postal_code[1] |>
    stringr::str_squish() |>
    stringr::str_remove_all("\\D+") |>
    stringr::str_trunc(8, "right", ellipsis = "") |>
    stringr::str_pad(8, "right", pad = "0") |>
    get_brazil_address_by_postal_code(method = method) |>
    dplyr::glimpse()

  invisible()
}
