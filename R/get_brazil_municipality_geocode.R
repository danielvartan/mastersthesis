# library(dplyr)
# library(here)
# library(lubridate)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils
# library(stringr)
# library(tidygeocoder)

# The geocoding is made using the `tidygeocoder` R package.
# Lear more about package at <https://jessecambon.github.io/tidygeocoder/>.

source(here::here("R", "get_brazil_municipality.R"))

get_brazil_municipality_geocode <- function(
    municipality,
    year = 2017,
    method = "google" # See `?tidygeocoder::geo`
) {
  prettycheck:::assert_character(municipality)

  prettycheck:::assert_number(
    year,
    lower = 1900,
    upper = Sys.Date() |> lubridate::year()
  )

  prettycheck:::assert_string(method)
  prettycheck:::assert_internet()

  data <- get_brazil_municipality(year)

  prettycheck:::assert_subset(
    municipality |>
      rutils:::drop_na() |>
      stringr::str_to_title(),
    data |>
      dplyr::pull(municipality)
  )

  dplyr::tibble(municipality = municipality, country = "Brasil") |>
    tidygeocoder::geocode(city = municipality, country = country)
}

# geocodes <-
#   brazil_capitals |>
#   get_brazil_municipality_geocode()

# options(digits = 10)
# geocodes$lat
# cat(geocodes$lat |> paste0(collapse = "\n"))
# geocodes$long
# cat(geocodes$long |> paste0(collapse = "\n"))

brazil_capitals <- c(
  "Acre" = "Rio Branco",
  "Alagoas" = "Maceió",
  "Amapá" = "Macapá",
  "Amazonas" = "Manaus",
  "Bahia" = "Salvador",
  "Ceará" = "Fortaleza",
  "Distrito Federal" = "Brasília",
  "Espírito Santo" = "Vitória",
  "Goiás" = "Goiânia",
  "Maranhão" = "São Luís",
  "Mato Grosso" = "Cuiabá",
  "Mato Grosso do Sul" = "Campo Grande",
  "Minas Gerais" = "Belo Horizonte",
  "Pará" = "Belém",
  "Paraíba" = "João Pessoa",
  "Paraná" = "Curitiba",
  "Pernambuco" = "Recife",
  "Piauí" = "Teresina",
  "Rio de Janeiro" = "Rio de Janeiro",
  "Rio Grande do Norte" = "Natal",
  "Rio Grande do Sul" = "Porto Alegre",
  "Rondônia" = "Porto Velho",
  "Roraima" = "Boa Vista",
  "Santa Catarina" = "Florianópolis",
  "São Paulo" = "São Paulo",
  "Sergipe" = "Aracaju",
  "Tocantins" = "Palmas"
)
