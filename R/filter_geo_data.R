# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck
library(rlang)
# library(stringr)

# # Helpers
#
# source(here::here("R", "tidy_data_.R"))
# raw_data <- targets::tar_read("raw_data")
# data <- raw_data |> fix_var_names()
#
# source(here::here("R", "filter_geo_data.R"))
# data |> filter_geo_data("municipality", "Tab")
#
# source(here::here("R", "get_brazil_address_by_postalcode.R"))
# get_brazil_address_by_postalcode("01223000") |> dplyr::glimpse()

filter_geo_data <- function(data, var, value) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_choice(stringr::str_squish(var), names(data))

  var <- stringr::str_squish(var)

  data |>
    dplyr::filter(!!as.symbol(var) == value) |>
    dplyr::select(id, country, state, municipality, postal_code)
}
