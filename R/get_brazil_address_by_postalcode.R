# library(cli)
# library(dplyr)
# library(geobr)
# library(glue)
# library(here)
# library(jsonlite)
# library(lubridate)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(stringdist)
# library(stringr)
# library(tidygeocoder)

source(here::here("R", "get_brazil_fu.R"))
source(here::here("R", "get_brazil_municipality.R"))
source(here::here("R", "get_brazil_region.R"))
source(here::here("R", "utils.R"))

#' Get a Brazilian address by its postal code via reverse geocoding
#'
#' @description
#'
#' `get_brazil_address_by_postalcode()` is a function that retrieves a Brazilian
#' address based on a postal code. It uses reverse geocoding to approximate the
#' address based on the postal code.
#'
#' Please note that the accuracy of the results may vary depending on the method
#' used.
#'
#' @details
#'
#' The source of the data will depend on the method used. Run
#' [`?tidygeocoder::geo`][tidygeocoder::geo()] to learn more. The only exception
#' is the `"viacep"` method. This method will return the address data from the
#' [ViaCEP API](https://viacep.com.br/).
#'
#' @param postalcode A [`character`][base::character()] vector with the postal
#'   code(s) to be used to. The postal code must be in the format `XXXXX-XXX` or
#'   `XXXXXXXX`, where `X` is a digit.
#' @param method A [`character`][base::character()] value indicating the method
#'   to be used to retrieve the address data. The available options are `"osm"`
#'   and `"viacep"` (default: `viacep`).
#'
#' @return A [`tibble`][dplyr::tibble()] with the following columns:
#'  - `postalcode`: The postal code.
#'  - `street`: The street name.
#'  - `complement`: The address complement.
#'  - `neighborhood`: The neighborhood.
#'  - `municipality_code`: The IBGE code for the address municipality.
#'  - `municipality`: The municipality.
#'  - `state`: The state.
#'  - `region`: The region.
#'  - `address`: The full address.
#'  - `latitude`: The latitude.
#'  - `longitude`: The longitude.
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' "01223000" |>
#'   get_brazil_address_by_postalcode() |>
#'   dplyr::glimpse()
#' }
get_brazil_address_by_postalcode <- function(
    postalcode,
    method = "viacep"
  ) {
  pattern <- "^\\d{8}$|^\\d{5}-\\d{3}$"
  method_options <- c("osm", "viacep")

  prettycheck:::assert_character(postalcode, pattern = pattern)
  prettycheck:::assert_choice(method, method_options)
  prettycheck:::assert_internet()

  postalcode <- stringr::str_replace_all(postalcode, "-", "")

  if (method == "viacep") {
    get_brazil_address_by_postalcode_viacep(postalcode)
  } else {
    get_brazil_address_by_postalcode_osm(postalcode)
  }
}

get_brazil_address_by_postalcode_viacep <- function(postalcode) {
  pattern <- "^\\d{8}$|^\\d{5}-\\d{3}$"

  prettycheck:::assert_character(postalcode, pattern = pattern)
  prettycheck:::assert_internet()

  postalcode <- stringr::str_replace_all(postalcode, "-", "")

  blank_viacep_data <- dplyr::tibble(
    cep = NA_character_,
    logradouro = NA_character_,
    complemento = NA_character_,
    unidade = NA_character_,
    bairro = NA_character_,
    localidad = NA_character_,
    uf = NA_character_,
    estado = NA_character_,
    regiao = NA_character_,
    ibge = NA_character_,
    gia = NA_character_,
    ddd = NA_character_,
    siafi = NA_character_
  )

  out <- dplyr::tibble()

  for (i in postalcode) {
    query_data <-
      jsonlite::fromJSON(
        paste0("https://viacep.com.br/ws/", i, "/json/")
      ) |>
      dplyr::as_tibble()

    if (!is.null(query_data[["erro"]])) {
      cli::cli_alert_warning(
        paste0(
          "Postal code {.strong {cli::col_red(i)}} not found in the ",
          "ViaCEP API."
        )
      )

      out <- dplyr::bind_rows(
        out,
        blank_viacep_data |> dplyr::mutate(cep = i)
      )
    } else {
      out <- dplyr::bind_rows(
        out,
        jsonlite::fromJSON(
          paste0("https://viacep.com.br/ws/", i, "/json/")
        ) |>
          dplyr::as_tibble()
      )
    }
  }

  out |>
    dplyr::transmute(
      postalcode = cep,
      street = logradouro,
      complement = complemento,
      neighborhood = bairro,
      municipality_code = ibge,
      municipality = localidade,
      state = estado,
      region = regiao
    ) |>
    dplyr::mutate(
      municipality_code = as.integer(municipality_code),
      address = glue::glue(
        "{street}, {complement}, {neighborhood}, ",
        "{municipality}-{get_brazil_fu(state)}, {postalcode}, Brasil"
      ),
      address = as.character(address),
      latitude = NA_real_,
      longitude = NA_real_
    )
}

get_brazil_address_by_postalcode_osm <- function(postalcode) {
  pattern <- "^\\d{8}$|^\\d{5}-\\d{3}$"

  prettycheck:::assert_character(postalcode, pattern = pattern)
  prettycheck:::assert_internet()

  postalcode <- stringr::str_replace_all(postalcode, "-", "")

  brazil_municipalities <- get_brazil_municipality(year = 2022)

  out <-
    dplyr::tibble(postalcode = postalcode) |>
    tidygeocoder::geocode(
      postalcode = postalcode,
      method = method
    ) |>
    tidygeocoder::reverse_geocode(
      lat = lat,
      long = long,
      method = method,
      full_results = TRUE
    ) |>
    dplyr:: select(
      postalcode,
      osm_lat,
      osm_lon,
      house_number,
      road,
      suburb,
      city,
      state,
      region
    ) |>
    dplyr::rename(
      street = road,
      complement = house_number,
      neighborhood = suburb,
      municipality = city,
      latitude = osm_lat,
      longitude = osm_lon
    ) |>
    dplyr::mutate(
      municipality_code = NA_integer_,
      region = get_brazil_region(state, "state"),
      address = glue::glue(
        "{street}, {complement}, {neighborhood}, ",
        "{municipality}-{get_brazil_fu(state)}, {postalcode}, Brasil"
      ),
      address = as.character(address),
      latitude = as.numeric(latitude),
      longitude = as.numeric(longitude)
    ) |>
    dplyr::relocate(
      postalcode, street, complement, neighborhood, municipality_code,
      municipality, state, region, address, latitude, longitude
    )

  match <- stringdist::amatch(
    out$municipality |> to_ascii_and_lower(),
    brazil_municipalities$municipality |> to_ascii_and_lower(),
    maxDist = 1
  )

  out |>
    dplyr::mutate(
      municipality_code = brazil_municipalities$municipality_code[match]
    )
}
