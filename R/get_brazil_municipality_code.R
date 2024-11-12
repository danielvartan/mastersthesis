# library(dplyr)
# library(here)
# library(lubridate)
# library(prettycheck) # github.com/danielvartan/prettycheck

source(here::here("R", "get_brazil_municipality.R"))

# Based on data from the Brazilian Institute of Geography and Statistics (IBGE)
# via the `geobr` R package. See `./R/get_brazil_municipality.R` to learn more.

# # Helpers
#
# source(here::here("R", "get_brazil_municipality_code.R"))
#
# get_brazil_municipality_code(c("são paulo","dsdasddsa", NA, "rio de janeiro"))

get_brazil_municipality_code <- function(x, year = 2017) {
  prettycheck:::assert_internet()
  prettycheck:::assert_character(x)

  prettycheck:::assert_number(
    year,
    lower = 1900,
    upper = Sys.Date() |> lubridate::year()
  )

  get_brazil_municipality(x, year) |> dplyr::pull(municipality_code)
}
