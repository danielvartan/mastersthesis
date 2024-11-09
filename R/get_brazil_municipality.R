# library(dplyr)
# library(geobr)
# library(here)
# library(lubridate)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils

source(here::here("R", "get_brazil_state.R"))

# Based on data from the Brazilian Institute of Geography and Statistics (IBGE)
# via the `geobr` R package.

# The `geobr` R package is produced by Brazil's Institute for Applied
# Economic Research (IPEA) and access the Brazilian Institute of Geography and
# Statistics (IBGE) data. You can see a list of all `geobr` datasets by
# running `geobr::list_geobr()`.
# See <https://ipeagit.github.io/geobr/index.html> to learn more.

# source(here::here("R", "to_title_case_pt.R"))
#
# get_brazil_municipality () |>
#   dplyr::pull(municipality) |>
#   to_title_case_pt() |>
#   sort() |>
#   utils::writeClipboard()

get_brazil_municipality <- function(year = 2017) {
  prettycheck:::assert_number(
    year,
    lower = 1900,
    upper = Sys.Date() |> lubridate::year()
  )

  prettycheck:::assert_internet()

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
      state = get_brazil_state(federal_unit)
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
}
