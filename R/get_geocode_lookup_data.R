# library(checkmate)
# library(dplyr)
# library(orbis) # github.com/danielvartan/orbis

source(here::here("R", "get_lookup_data.R"))
source(here::here("R", "get_qualocep_data.R"))

get_geocode_lookup_data <- function() {
  lookup_data <- get_lookup_data()
  qualocep_data <- get_qualocep_data()

  geocode_data <-
    lookup_data$geocodes |>
    dplyr::select(-dplyr::all_of(c("timestamp", "region", "source"))) |>
    dplyr::mutate(
      street_type = NA,
      street_name = NA,
      place = NA,
      federal_unit = orbis::get_brazil_fu(state),
      municipality_code = as.integer(municipality_code),
      state_code = as.integer(state_code),
      latitude = as.numeric(latitude),
      longitude = as.numeric(longitude)
    ) |>
    dplyr::select(
      postal_code, street_type, street_name, street, complement, place,
      neighborhood, municipality_code, municipality, state_code, state,
      federal_unit, latitude, longitude
    ) |>
    dplyr::bind_rows(qualocep_data) |>
    dplyr::filter(!is.na(latitude) | !is.na(longitude)) |>
    dplyr::distinct(postal_code, .keep_all = TRUE)
}
