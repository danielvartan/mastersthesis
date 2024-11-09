# get_geocode_data



# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(tidygeocoder)

update_geocode_data <- function(
    data,
    method = "google" # See `?tidygeocoder::geo`
  ) {
  cols <- c("country", "state", "municipality", "postal_code")

  prettycheck:::assert_tibble(data)
  prettycheck:::assert_subset(cols, names(data))
  prettycheck:::assert_string(method)
  prettycheck:::assert_internet()

  data <-
    data |>
    dplyr::select(dplyr::all_of(cols))

  dplyr::tibble(municipality = municipality, country = "Brasil") |>
    tidygeocoder::geocode(city = municipality, country = country)
}
