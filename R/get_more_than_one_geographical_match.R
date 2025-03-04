# library(beeper)
# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(stringr)

source(here::here("R", "get_brazil_municipality.R"))
source(here::here("R", "utils.R"))

# # Helpers
#
# source(here::here("R", "get_lookup_data.R"))
# lookup_data <- get_lookup_data()
#
# lookup_data$municipality$key |> length()
#
# data <- get_more_than_one_geographical_match(
#   x = lookup_data$municipality$key[seq(6001, 7000)],
#   col = "municipality",
#   maxDist = 1
# )
#
# data$key |> utils:: writeClipboard()
#
# raw_data <- targets::tar_read("raw_data")
#
# Check the municipality names here: <https://sidra.ibge.gov.br/territorio#/N6>.
#
# source(here::here("R", "cross_geographic_data_by_postal_code.R"))
#
# for (i in data$key) {
#   raw_data |>
#     cross_geographic_data_by_postal_code(
#        col = "municipality",
#        value = i,
#        limit = Inf,
#        col_merge = c("municipality", "state"),
#        col_match = "municipality",
#        maxDist = 1
#      ) |>
#      print(n = 100)
#
#   readline("Press enter to continue: ")
# }
#
# source(here::here("R", "get_brazil_municipality.R"))
# get_brazil_municipality("Bauru")

get_more_than_one_geographical_match <- function(
    x,
    col,
    beep = TRUE,
    ...
  ) {
  checkmate::assert_character(x)
  checkmate::assert_string(col)
  checkmate::assert_flag(beep)

  brazil_municipalities <- get_brazil_municipality()
  checkmate::assert_choice(col, names(brazil_municipalities))
  brazil_municipalities <- brazil_municipalities |> dplyr::pull(col)

  out <- dplyr::tibble()

  for (i in x) {
    match_dist_i <-
      rep(i, length(brazil_municipalities)) |>
      match_strings(brazil_municipalities, one_by_one = TRUE, ...) |>
      dplyr::filter(!is.na(value))

    match_pattern_i <- stringr::str_subset(
      brazil_municipalities |> to_ascii_and_lower(),
      i |>
        to_ascii_and_lower() |>
        stringr::str_squish() |>
        stringr::str_escape()
    )

    if (nrow(match_dist_i) > 1 || length(match_pattern_i) > 1) {
      out <-
        out |>
        dplyr::bind_rows(
          dplyr::tibble(
            key = i,
            match_dist = list(match_dist_i$value),
            match_pattern = list(match_pattern_i)
          )
        )
    }
  }

  if (isTRUE(beep)) {
    beepr::beep()
    Sys.sleep(3)
  }

  out
}
