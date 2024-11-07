
# get_geocode_data

update_geocode_data <- function(
    data,
    method = "google" # See `?tidygeocoder::geo`
  ) {
  cols <- c(
    "country", "state", "county", "municipality", "postal_code"
  )

  prettycheck:::assert_tibble(data)
  prettycheck:::assert_subset(cols, names(data))
  prettycheck:::assert_string(method)
  prettycheck:::assert_internet()

  data |>
    dplyr::select(id, country, state, municipality, postal_code)
}
