# library(dplyr)
# library(geobr)
# library(here)
# library(lubridate)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(readr)
library(rlang)
# library(rutils) # github.com/danielvartan/rutils
# library(stringr)

source(here::here("R", "get_brazil_state.R"))
source(here::here("R", "to_title_case_pt.R"))
source(here::here("R", "utils.R"))

# Based on data from the Brazilian Institute of Geography and Statistics (IBGE)
# via the `geobr` R package.

# The `geobr` R package is produced by Brazil's Institute for Applied
# Economic Research (IPEA) and access the Brazilian Institute of Geography and
# Statistics (IBGE) data. You can see a list of all `geobr` datasets by
# running `geobr::list_geobr()`.
# See <https://ipeagit.github.io/geobr/index.html> to learn more.

# # Helpers
#
# source(here::here("R", "get_brazil_municipality.R"))
#
# get_brazil_municipality(force = TRUE) |>
#   dplyr::pull(municipality) |>
#   sort() |>
#   utils::writeClipboard()
#
# get_brazil_municipality(c("são paulo","dsdasddsa", NA, "rio de janeiro"))

get_brazil_municipality <- function(
    municipality = NULL,
    state = NULL,
    year = 2017,
    force = FALSE
  ) {
  prettycheck:::assert_internet()
  prettycheck:::assert_character(municipality, null.ok = TRUE)
  prettycheck:::assert_character(state, null.ok = TRUE)
  prettycheck:::assert_flag(force)

  prettycheck:::assert_number(
    year,
    lower = 1900,
    upper = Sys.Date() |> lubridate::year()
  )

  brazil_municipalities_file <- file.path(
    tempdir(), paste0("brazil-municipalities-", year, ".rds")
  )

  if (prettycheck:::test_file_exists(brazil_municipalities_file) &&
      isFALSE(force)) {
    brazil_municipalities_data <- readr::read_rds(brazil_municipalities_file)
  } else {
    brazil_municipalities_data <-
      geobr::read_municipality(
        year = year,
        showProgress = FALSE
      ) |>
      dplyr::as_tibble() |>
      dplyr::select(-geom) |>
      dplyr::rename(
        state_code = code_state,
        federal_unit = abbrev_state,
        municipality_code = code_muni,
        municipality = name_muni
      ) |>
      dplyr::mutate(
        country = "Brazil",
        state_code = as.integer(state_code),
        state = get_brazil_state(federal_unit),
        municipality_code = as.integer(municipality_code),
        municipality = to_title_case_pt(
          municipality,
          articles = TRUE,
          conjuctions = FALSE,
          oblique_pronouns = FALSE,
          prepositions = FALSE,
          custom = c(
            # E
            "(.)\\bE( )\\b" = "\\1e\\2",
            # Às
            "(.)\\bÀ(s)?\\b" = "\\1à\\2",
            # Da | Das | Do | Dos | De
            "(.)\\bD(((a|o)(s)?)|(e))\\b" = "\\1d\\2",
            # Em
            "(.)\\bE(m)\\b" =  "\\1e\\2",
            # Na | Nas | No | Nos
            "(.)\\bN((a|o)(s)?)\\b" = "\\1n\\2",
            # Del
            "(.)\\bD(el)\\b" = "\\1d\\2"
          )
        )
      ) |>
      dplyr::relocate(
        municipality,
        municipality_code,
        state_code,
        state,
        federal_unit,
        country
      ) |>
      rutils::shush()

    readr::write_rds(brazil_municipalities_data, brazil_municipalities_file)
  }

  if (is.null(municipality)) {
    brazil_municipalities_data
  } else {
    municipality <-
      municipality |>
      to_ascii_and_lower() |>
      stringr::str_remove_all("[^a-z'\\- ]") |>
      stringr::str_squish()

    if (!is.null(state)) {
      prettycheck::assert_identical(
        municipality,
        state,
        type = "length"
      )

      state <-
        state |>
        to_ascii_and_lower() |>
        stringr::str_remove_all("[^a-z'\\- ]") |>
        stringr::str_squish()
    }

    out <- dplyr::tibble()

    for (i in seq_along(municipality)) {
      if (is.null(state)) {
        data <-
          brazil_municipalities_data |>
          dplyr::filter(
            to_ascii_and_lower(municipality) %in% .env$municipality[i]
          )
      } else {
        data <-
          brazil_municipalities_data |>
          dplyr::filter(
            to_ascii_and_lower(municipality) %in% .env$municipality[i] &
            to_ascii_and_lower(state) %in% .env$state[i]
          )
      }

      if (nrow(data) == 0) {
        out <-
          out |>
          dplyr::bind_rows(
            dplyr::tibble(
              municipality = .env$municipality[i],
              municipality_code = NA_integer_,
              state_code = NA_integer_,
              state = NA_character_,
              federal_unit = NA_character_,
              country = "Brazil"
            )
          )
      } else {
        out <- out |> dplyr::bind_rows(data)
      }
    }

    out
  }
}

# library(dplyr)
# library(here)
# library(lubridate)
# library(prettycheck) # github.com/danielvartan/prettycheck

# Based on data from the Brazilian Institute of Geography and Statistics (IBGE)
# via the `geobr` R package. See `./R/get_brazil_municipality.R` to learn more.

# # Helpers
#
# source(here::here("R", "get_brazil_municipality.R"))
#
# get_brazil_municipality_code(c("são paulo","dsdasddsa", NA, "rio de janeiro"))

get_brazil_municipality_code <- function(
    municipality,
    state = NULL,
    year = 2017
  ) {
  prettycheck:::assert_internet()
  prettycheck:::assert_character(municipality)
  prettycheck:::assert_character(state, null.ok = TRUE)

  prettycheck:::assert_number(
    year,
    lower = 1900,
    upper = Sys.Date() |> lubridate::year()
  )

  get_brazil_municipality(municipality, state, year = year) |>
    dplyr::pull(municipality_code)
}
