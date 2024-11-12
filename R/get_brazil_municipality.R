# library(dplyr)
# library(geobr)
# library(here)
# library(lubridate)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(readr)
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
# get_brazil_municipality(c("são paulo","dsdasddsa", NA, "rio de janeiro"))

get_brazil_municipality <- function(x = NULL, year = 2017) {
  prettycheck:::assert_internet()
  prettycheck:::assert_character(x, null.ok = TRUE)

  prettycheck:::assert_number(
    year,
    lower = 1900,
    upper = Sys.Date() |> lubridate::year()
  )

  brazil_municipalities_file <- file.path(
    tempdir(), paste0("brazil-municipalities-", year, ".rds")
  )

  if (prettycheck:::test_file_exists(brazil_municipalities_file)) {
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
        federal_unit_code = code_state,
        federal_unit = abbrev_state,
        municipality_code = code_muni,
        municipality = name_muni
      ) |>
      dplyr::mutate(
        country = "Brazil",
        federal_unit_code = as.integer(federal_unit_code),
        state = get_brazil_state(federal_unit),
        municipality_code = as.integer(municipality_code),
        municipality = to_title_case_pt(municipality)
      ) |>
      dplyr::relocate(
        country,
        federal_unit_code,
        federal_unit,
        state,
        municipality_code,
        municipality
      ) |>
      rutils::shush()

    readr::write_rds(brazil_municipalities_data, brazil_municipalities_file)
  }

  if (is.null(x)) {
    brazil_municipalities_data
  } else {
    x <-
      x |>
      to_ascii_and_lower() |>
      stringr::str_remove_all("[^a-z'\\- ]") |>
      stringr::str_squish()

    out <- dplyr::tibble()

    for (i in x) {
      data <-
        brazil_municipalities_data |>
        dplyr::filter(to_ascii_and_lower(municipality) %in% i)

      if (nrow(data) == 0) {
        out <-
          out |>
          dplyr::bind_rows(
            dplyr::tibble(
              country = "Brazil",
              federal_unit_code = NA_integer_,
              federal_unit = NA_character_,
              state = NA_character_,
              municipality_code = NA_integer_,
              municipality = i
            )
          )
      } else {
        out <- out |> dplyr::bind_rows(data)
      }
    }

    out
  }
}
