# library(dplyr)
# library(here)
# library(lubridate)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils
# library(stringr)
# library(tidygeocoder)

source(here::here("R", "get_brazil_municipality.R"))
source(here::here("R", "to_title_case_pt.R"))

# The geocoding is made using the `tidygeocoder` R package.
# Lear more about package at <https://jessecambon.github.io/tidygeocoder/>.

# # Helpers
#
# source(here::here("R", "get_brazil_state_capitals.R"))
#
# geocodes <-
#   get_brazil_state_capitals() |>
#   get_brazil_municipality_geocode()
#
# options(digits = 10)
#
# geocodes |> print(n = Inf)
# geocodes$lat
# cat(geocodes$lat |> paste0(collapse = "\n"))
# geocodes$long
# cat(geocodes$long |> paste0(collapse = "\n"))

get_brazil_municipality_geocode <- function(
    municipality,
    year = 2017,
    method = "google" # See `?tidygeocoder::geo`
  ) {
  prettycheck:::assert_internet()
  prettycheck:::assert_character(municipality)

  prettycheck:::assert_number(
    year,
    lower = 1900,
    upper = Sys.Date() |> lubridate::year()
  )

  prettycheck:::assert_string(method)

  data <- get_brazil_municipality(year)

  prettycheck:::assert_subset(
    municipality |>
      rutils:::drop_na() |>
      to_title_case_pt(),
    data |>
      dplyr::pull(municipality)
  )

  dplyr::tibble(municipality = municipality, country = "Brasil") |>
    tidygeocoder::geocode(city = municipality, country = country)
}
