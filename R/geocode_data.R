# library(cli)
# library(dplyr)
# library(geobr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils
# library(tidyr)

source(here::here("R", "get_brazil_municipality.R"))
source(here::here("R", "get_brazil_region.R"))
source(here::here("R", "get_geocode_lookup_data.R"))

# # Helpers
#
# data <- targets::tar_read("analyzed_data")
#
# source(here::here("R", "filter_geographic_data.R"))
# data |> filter_geographic_data("postal_code", "41610010", fix_col = FALSE)
#
# source(here::here("R", "get_brazil_address_by_postal_code.R"))
# get_brazil_address_by_postal_code("41610010", method = "google") |>
#   dplyr::glimpse()
#
# postal_code <-
#   data |>
#   dplyr::filter(is_postal_code_valid == FALSE) |>
#   dplyr::pull(postal_code)

geocode_data <- function(
    data,
    year = 2017,
    geocode_lookup_data = get_geocode_lookup_data()
  ) {
  geo_vars <- c("country", "state", "municipality", "postal_code")

  checkmate::assert_tibble(data)
  checkmate::assert_subset(geo_vars, names(data))
  checkmate::assert_tibble(geocode_lookup_data)

  checkmate::assert_number(
    year,
    lower = 1900,
    upper = Sys.Date() |> lubridate::year()
  )

  cli::cli_progress_step("Geocoding the data")

  data |>
    add_ibge_codes() |>
    validate_postal_codes(geocode_lookup_data) |>
    adjust_state_and_municipality_by_postal_code() |>
    add_region() |>
    add_geocode_data_by_postal_code(geocode_lookup_data) |>
    add_geocode_data_by_municipality(geocode_lookup_data) |>
    remove_invalid_postal_codes() |>
    validate_brazil_geocodes(year = year)
}

# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck

source(here::here("R", "get_brazil_municipality.R"))

add_ibge_codes <- function(data) {
  checkmate::assert_tibble(data)

  data |>
    dplyr::left_join(
      get_brazil_municipality(year = 2017) |>
        dplyr::select(state_code, state, municipality_code, municipality),
      by = c("state", "municipality")
    ) |>
    dplyr::mutate(
      state_code = dplyr::if_else(
        country == "Brazil",
        state_code,
        NA_integer_
      ),
      municipality_code = dplyr::if_else(
        country == "Brazil",
        municipality_code,
        NA_integer_
      )
    ) |>
    dplyr::relocate(state_code, .before = state) |>
    dplyr::relocate(municipality_code, .before = municipality)
}

# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils

source(here::here("R", "get_geocode_lookup_data.R"))

validate_postal_codes <- function(
    data,
    geocode_lookup_data = get_geocode_lookup_data()
  ) {
  checkmate::assert_tibble(data)
  checkmate::assert_tibble(geocode_lookup_data)

  data |>
    dplyr::left_join(
      geocode_lookup_data |>
        rutils::shush() |>
        dplyr::select(state_code, municipality_code, postal_code),
      by = "postal_code",
      suffix = c("", "_lookup_data")
    ) |>
    dplyr::mutate(
      was_postal_code_changed = !postal_code == postal_code_raw,
      validity_condition_1 = dplyr::if_else(
        !was_postal_code_changed &
          (state_code == state_code_lookup_data |
             municipality_code == municipality_code_lookup_data),
        TRUE,
        FALSE
      ),
      validity_condition_2 = dplyr::if_else(
        was_postal_code_changed &
          state_code == state_code_lookup_data &
          municipality_code == municipality_code_lookup_data,
        TRUE,
        FALSE
      ),
      is_postal_code_valid = validity_condition_1 | validity_condition_2
    ) |>
    dplyr::select(
      -dplyr::all_of(
        c(
          "postal_code_raw", "was_postal_code_changed",
          "validity_condition_1", "validity_condition_2"
        )
      )
    )
}

# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck

source(here::here("R", "get_brazil_municipality.R"))

adjust_state_and_municipality_by_postal_code <- function(data) {
  checkmate::assert_tibble(data)

  data |>
    dplyr::mutate(
      state_code = dplyr::if_else(
        is_postal_code_valid & country == "Brazil",
        state_code_lookup_data,
        state_code
      ),
      municipality_code = dplyr::if_else(
        is_postal_code_valid & country == "Brazil",
        municipality_code_lookup_data,
        municipality_code
      )
    ) |>
    dplyr::left_join(
      get_brazil_municipality(year = 2017) |>
        dplyr::select(state_code, state, municipality_code, municipality),
      by = c("state_code", "municipality_code"),
      suffix = c("", "_geobr")
    ) |>
    dplyr::mutate(
      state = dplyr::if_else(country == "Brazil", state_geobr, state),
      municipality = dplyr::if_else(
        country == "Brazil",
        municipality_geobr,
        municipality
      )
    )|>
    dplyr::select(
      -dplyr::all_of(
        c(
          "state_code_lookup_data",
          "municipality_code_lookup_data",
          "state_geobr", "municipality_geobr"
        )
      )
    )
}

# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck

source(here::here("R", "get_brazil_region.R"))

add_region <- function(data) {
  checkmate::assert_tibble(data)

  data |>
    dplyr::mutate(region = get_brazil_region(state, "state")) |>
    dplyr::relocate(region, .after = country)
}

# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils

source(here::here("R", "get_geocode_lookup_data.R"))

add_geocode_data_by_postal_code <- function(
    data,
    geocode_lookup_data = get_geocode_lookup_data()
  ) {
  checkmate::assert_tibble(data)
  checkmate::assert_tibble(geocode_lookup_data)

  data |>
    dplyr::left_join(
      geocode_lookup_data |>
        rutils::shush() |>
        dplyr::select(postal_code, latitude, longitude),
      by = "postal_code"
    ) |>
    dplyr::mutate(
      latitude = dplyr::if_else(
        is_postal_code_valid & country == "Brazil",
        latitude,
        NA_real_
      ),
      longitude = dplyr::if_else(
        is_postal_code_valid & country == "Brazil",
        longitude,
        NA_real_
      )
    ) |>
    dplyr::relocate(latitude, longitude, .after = postal_code)
}

# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils
# library(tidyr)

source(here::here("R", "get_geocode_lookup_data.R"))

add_geocode_data_by_municipality <- function(
    data,
    geocode_lookup_data = get_geocode_lookup_data()
  ) {
  checkmate::assert_tibble(data)
  checkmate::assert_tibble(geocode_lookup_data)

  data |>
    dplyr::left_join(
      geocode_lookup_data |>
        rutils::shush() |>
        dplyr::summarise(
          latitude = mean(latitude, na.rm = TRUE),
          longitude = mean(longitude, na.rm = TRUE),
          .by = c("state_code", "municipality_code")
        ) |>
        tidyr::drop_na(),
      by = c("state_code", "municipality_code"),
      suffix = c("", "_lookup_data")
    ) |>
    dplyr::mutate(
      latitude = dplyr::if_else(
        is.na(latitude),
        latitude_lookup_data,
        latitude
      ),
      longitude = dplyr::if_else(
        is.na(longitude),
        longitude_lookup_data,
        longitude
      ),
      latitude = dplyr::if_else(
        country == "Brazil",
        latitude,
        NA_real_
      ),
      longitude = dplyr::if_else(
        country == "Brazil",
        longitude,
        NA_real_
      )
    ) |>
    dplyr::select(-dplyr::ends_with("_lookup_data"))
}

# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck

remove_invalid_postal_codes <- function(data) {
  checkmate::assert_tibble(data)

  data |>
    dplyr::mutate(
      postal_code = dplyr::if_else(
        !is_postal_code_valid & country == "Brazil",
        NA_character_,
        postal_code
      )
    )
    # dplyr::select(-is_postal_code_valid)
}

# library(dplyr)
# library(geobr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils

validate_brazil_geocodes <- function(data, year = 2017) {
  checkmate::assert_tibble(data)

  data |>
    plotr:::filter_points_on_land(
      geobr::read_country(year = year, showProgress = FALSE) |>
        dplyr::pull(geom) |>
        rutils::shush()
    )
}
