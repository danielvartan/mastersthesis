# library(cli)
# library(dplyr)
# library(glue)
# library(here)
# library(jsonlite)
# library(lubridate)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(stringdist)
# library(stringr)
# library(tidygeocoder)

source(here::here("R", "fix_brazil_postal_code.R"))
source(here::here("R", "get_brazil_fu.R"))
source(here::here("R", "get_brazil_municipality.R"))
source(here::here("R", "get_brazil_region.R"))
source(here::here("R", "get_qualocep_data.R"))
source(here::here("R", "utils.R"))

#' Get a Brazilian address by its postal code via reverse geocoding
#'
#' @description
#'
#' `get_brazil_address_by_postal_code()` is a function that retrieves a Brazilian
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
#' @param postal_code A [`character`][base::character()] vector with the postal
#'   code(s) to be used to. The postal code must be in the format `XXXXX-XXX` or
#'   `XXXXXXXX`, where `X` is a digit.
#' @param method A [`character`][base::character()] value indicating the method
#'   to be used to retrieve the address data. The available options are `"osm"`
#'   and `"viacep"` (default: `viacep`).
#'
#' @return A [`tibble`][dplyr::tibble()] with the following columns:
#'  - `postal_code`: The postal code.
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
#'   get_brazil_address_by_postal_code() |>
#'   dplyr::glimpse()
#'
#' c("01223000", NA, "05411002") |> get_brazil_address_by_postal_code()
#' }
get_brazil_address_by_postal_code <- function(
    postal_code,
    method = "qualocep",
    fix_code = TRUE,
    limit = Inf
  ) {
  prettycheck:::assert_internet()
  prettycheck:::assert_atomic(postal_code)
  prettycheck:::assert_choice(method, c("osm", "qualocep", "viacep"))
  prettycheck:::assert_flag(fix_code)
  prettycheck:::assert_number(limit)

  if (!is.infinite(limit)) {
    prettycheck:::assert_integer_number(limit, lower = 1)

    postal_code <- postal_code[seq_len(min(length(postal_code), limit))]
  }

  if (isTRUE(fix_code)) postal_code <- postal_code |> fix_brazil_postal_code()

  prettycheck:::assert_character(postal_code, pattern = "^\\d{8}$")


  if (method == "osm") {
    get_brazil_address_by_postal_code_osm(postal_code, limit)
  } else if (method == "qualocep") {
    get_brazil_address_by_postal_code_qualocep(postal_code, limit)
  } else if (method == "viacep") {
    get_brazil_address_by_postal_code_viacep(postal_code, limit)
  }
}

get_brazil_address_by_postal_code_osm <- function(
    postal_code,
    limit = 100
  ) {
  postal_code <- fix_brazil_postal_code(postal_code, zero_na = FALSE)

  prettycheck:::assert_internet()
  prettycheck:::assert_character(postal_code, pattern = "^\\d{8}$")
  prettycheck:::assert_number(limit)

  if (!is.infinite(limit)) {
    prettycheck:::assert_integer_number(limit, lower = 1)

    postal_code <- postal_code[seq_len(min(length(postal_code), limit))]
  }

  brazil_municipalities <- get_brazil_municipality(year = 2022)

  out <-
    dplyr::tibble(postal_code = postal_code) |>
    tidygeocoder::geocode(
      postalcode = postal_code,
      method = "osm"
    ) |>
    tidygeocoder::reverse_geocode(
      lat = lat,
      long = long,
      method = "osm",
      full_results = TRUE
    )

  if (!"house_number" %in% names(out)) {
    out <- out |> dplyr::mutate(house_number = NA_character_)
  }

  if (!"city" %in% names(out)) {
    out <- out |> dplyr::mutate(city = NA_character_)
  }

  if ("town" %in% names(out)) {
    out <- out |> dplyr::mutate(
      city = dplyr::if_else(is.na(city), town, city)
    )
  }

  if (!"osm_lat" %in% names(out)) {
    dplyr::tibble(
      postal_code = fix_brazil_postal_code(postal_code),
      street = NA_character_,
      complement = NA_character_,
      neighborhood = NA_character_,
      municipality_code = NA_integer_,
      municipality = NA_character_,
      state = NA_character_,
      region = NA_character_,
      address = NA_character_,
      latitude = NA_real_,
      longitude = NA_real_
    )
  } else {
    out <-
      out |>
      dplyr:: select(
        postal_code,
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
        postal_code = fix_brazil_postal_code(postal_code),
        municipality_code = NA_integer_,
        region = get_brazil_region(state, "state"),
        address = render_brazil_address(
          street, complement, neighborhood, municipality, state, postal_code
        ),
        latitude = as.numeric(latitude),
        longitude = as.numeric(longitude)
      ) |>
      dplyr::mutate(
        dplyr::across(
          dplyr::where(is.character),
          .fns = ~ dplyr::if_else(.x == "", NA_character_, .x)
        )
      ) |>
      dplyr::relocate(
        postal_code, street, complement, neighborhood, municipality_code,
        municipality, state, region, address, latitude, longitude
      )

    match <- stringdist::amatch(
      out$municipality |> to_ascii_and_lower(),
      brazil_municipalities$municipality |> to_ascii_and_lower(),
      maxDist = 1
    )

    out |>
      dplyr::mutate(
        municipality_code = brazil_municipalities$municipality_code[match],
        municipality_code = as.integer(municipality_code)
      )
  }
}

get_brazil_address_by_postal_code_qualocep <- function(
    postal_code,
    limit = 1000
  ) {
  postal_code <- fix_brazil_postal_code(postal_code, zero_na = FALSE)

  prettycheck:::assert_internet()
  prettycheck:::assert_character(postal_code, pattern = "^\\d{8}$")
  prettycheck:::assert_number(limit)

  if (!is.infinite(limit)) {
    prettycheck:::assert_integer_number(limit, lower = 1)

    postal_code <- postal_code[seq_len(min(length(postal_code), limit))]
  }

  if ("qualocep_data" %in% ls(envir = globalenv())) {
    cli::cli_progress_step(paste0(
      "Using the QualOCep table from the Global Enviroment (`qualocep`)."
    ))

    qualocep_data <- get("qualocep_data", envir = globalenv())
  } else {
    cli::cli_progress_step(paste0(
      "Getting and saving the QualOCep table to the Global Enviroment ",
      " (`qualocep`)."
    ))

    qualocep_data <- get_qualocep_data()

    assign("qualocep_data", qualocep_data, envir = globalenv())
  }

  out <-
    dplyr::tibble(postal_code = postal_code) |>
    dplyr::left_join(
      qualocep_data,
      by = "postal_code"
    )

  if (any(c("latitude", "longitude") %in% names(qualocep_data))) {
    out |>
      dplyr::select(
        postal_code, street, complement, neighborhood, municipality_code,
        municipality, state, latitude, longitude
      ) |>
      dplyr::mutate(
        region = get_brazil_region(state, "state"),
        address = render_brazil_address(
          street, complement, neighborhood, municipality, state, postal_code
        )
      ) |>
      dplyr::relocate(latitude, longitude, .after = address)
  } else {
    out |>
      dplyr::select(
        postal_code, street, complement, neighborhood, municipality_code,
        municipality, state
      ) |>
      dplyr::mutate(
        region = get_brazil_region(state, "state"),
        address = render_brazil_address(
          street, complement, neighborhood, municipality, state, postal_code
        ),
        latitude = NA_real_,
        longitude = NA_real_
      )
  }
}

get_brazil_address_by_postal_code_viacep <- function(
    postal_code,
    limit = 100
  ) {
  postal_code <- fix_brazil_postal_code(postal_code, zero_na = FALSE)

  prettycheck:::assert_internet()
  prettycheck:::assert_character(postal_code, pattern = "^\\d{8}$")
  prettycheck:::assert_number(limit)

  if (!is.infinite(limit)) {
    prettycheck:::assert_integer_number(limit, lower = 1)

    postal_code <- postal_code[seq_len(min(length(postal_code), limit))]
  }

  blank_viacep_data <- dplyr::tibble(
    cep = NA_character_,
    logradouro = NA_character_,
    complemento = NA_character_,
    unidade = NA_character_,
    bairro = NA_character_,
    localidade = NA_character_,
    uf = NA_character_,
    estado = NA_character_,
    regiao = NA_character_,
    ibge = NA_character_,
    gia = NA_character_,
    ddd = NA_character_,
    siafi = NA_character_
  )

  cli::cli_progress_bar(
    name = "Getting address data from the ViaCEP API.",
    type = "tasks",
    total = length(postal_code),
    clear = FALSE
  )

  out <- dplyr::tibble()

  for (i in postal_code) {
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

    cli::cli_progress_update()
  }

  out |>
    dplyr::transmute(
      postal_code = fix_brazil_postal_code(cep),
      street = logradouro,
      complement = complemento,
      neighborhood = bairro,
      municipality_code = ibge,
      municipality = localidade,
      state = estado,
      region = get_brazil_region(estado, "state")
    ) |>
    dplyr::mutate(
      municipality_code = as.integer(municipality_code),
      address = render_brazil_address(
        street, complement, neighborhood, municipality, state, postal_code
      ),
      address = as.character(address),
      address = dplyr::if_else(is.na(street), NA, address),
      latitude = NA_real_,
      longitude = NA_real_
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.character),
        .fns = ~ dplyr::if_else(.x == "", NA_character_, .x)
      )
    )
}

render_brazil_address <- function(
    street = NA_character_,
    complement = NA_character_,
    neighborhood = NA_character_,
    municipality = NA_character_,
    state = NA_character_,
    postal_code = NA_character_
) {
  prettycheck:::assert_character(street)
  prettycheck:::assert_character(complement)
  prettycheck:::assert_character(neighborhood)
  prettycheck:::assert_character(municipality)
  prettycheck:::assert_character(state)
  prettycheck:::assert_character(postal_code, pattern = "^\\d{8}$")

  prettycheck::assert_identical(
    street, complement, neighborhood, municipality, state, postal_code,
    type = "length"
  )

  `%>%` <- dplyr::`%>%`
  out <- character()

  for (i in seq_along(postal_code)) {
    out <-
      glue::glue(
        ifelse(is.na(street[i]) | street[i] == "", "", "{street[i]}, "),
        ifelse(
          is.na(complement[i]) | complement[i] == "",
          "",
          "{complement[i]}, "
        ),
        ifelse(
          is.na(neighborhood)[i] | neighborhood[i] == "",
          "",
          "{neighborhood[i]}, "
        ),
        dplyr::case_when(
          !(is.na(municipality[i]) | municipality[i] == "") &
            !(is.na(state[i]) | state[i] == "") ~
            "{municipality[i]}-{get_brazil_fu(state[i])}, ",
          (is.na(municipality[i]) | municipality[i] == "") &
            !(is.na(state[i]) | state[i] == "") ~
            "{get_brazil_fu(state[i])}, ",
          !(is.na(municipality[i]) | municipality[i] == "") ~ "{municipality[i]}, ",
          TRUE ~ ""
        ),
        ifelse(
          is.na(postal_code[i]) | postal_code[i] == "",
          "",
          paste0(
            "{stringr::str_sub(postal_code[i], 1, 5)}",
            "-",
            "{stringr::str_sub(postal_code[i], 6, 8)}",
            ", "
          )
        ),
        "Brasil",
        .na = "",
        .null = ""
      ) |>
      as.character() %>%
      append(out, .)
  }

  out
}
