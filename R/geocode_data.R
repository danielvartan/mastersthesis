# library(cli)
# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils

source(here::here("R", "get_brazil_municipality.R"))
source(here::here("R", "get_brazil_fu.R"))
source(here::here("R", "get_brazil_region.R"))
source(here::here("R", "get_lookup_data.R"))
source(here::here("R", "get_qualocep_data.R"))

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
    lookup_data = get_lookup_data(),
    qualocep_data = get_qualocep_data()
  ) {
  geo_vars <- c("country", "state", "municipality", "postal_code")

  prettycheck:::assert_tibble(data)
  prettycheck:::assert_subset(geo_vars, names(data))
  prettycheck:::assert_list(lookup_data)
  prettycheck:::assert_tibble(qualocep_data)

  cli::cli_progress_step("Geocoding the data")

  data |>
    add_ibge_codes() |>
    validate_postal_codes() |>
    adjust_state_and_municipality_by_postal_code() |>
    add_region() |>
    add_qualocep_geocode_data_by_postal_code() |>
    add_qualocep_geocode_data_by_municipality()
}

add_ibge_codes <- function(data) {
  prettycheck:::assert_tibble(data)

  brazil_municipalties_data <- get_brazil_municipality(year = 2017)

  data |>
    dplyr::left_join(
      brazil_municipalties_data |>
        dplyr::select(
          federal_unit_code, state, municipality_code, municipality
        ) |>
        dplyr::rename(state_code = federal_unit_code),
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

validate_postal_codes <- function(data) {
  prettycheck:::assert_tibble(data)

  lookup_data <- get_lookup_data() |> rutils::shush()
  qualocep_data <- get_qualocep_data() |> rutils::shush()

  geocode_data <-
    lookup_data$geocodes |>
    dplyr::select(-dplyr::all_of(c("timestamp", "region", "source"))) |>
    dplyr::mutate(
      street_type = NA,
      street_name = NA,
      place = NA,
      federal_unit = get_brazil_fu(state),
      municipality_code = as.integer(municipality_code),
      state_code = as.integer(state_code),
      latitude = as.numeric(latitude),
      longitude = as.numeric(longitude)
    ) |>
    dplyr::rename(federal_unit_code = state_code) |>
    dplyr::select(
      postal_code, street_type, street_name, street, complement, place,
      neighborhood, municipality_code, municipality, federal_unit_code,
      federal_unit, state, latitude, longitude
    ) |>
    dplyr::bind_rows(qualocep_data) |>
    dplyr::distinct(postal_code, .keep_all = TRUE)

  data |>
    dplyr::left_join(
      geocode_data |>
        dplyr::select(federal_unit_code, municipality_code, postal_code) |>
        dplyr::rename(state_code = federal_unit_code),
      by = "postal_code",
      suffix = c("", "_qualocep")
    ) |>
    dplyr::mutate(
      was_postal_code_changed = !postal_code == postal_code_raw,
      validity_condition_1 = dplyr::if_else(
        !was_postal_code_changed &
          (state_code == state_code_qualocep |
             municipality_code == municipality_code_qualocep),
        TRUE,
        FALSE
      ),
      validity_condition_2 = dplyr::if_else(
        was_postal_code_changed &
          state_code == state_code_qualocep &
          municipality_code == municipality_code_qualocep,
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

adjust_state_and_municipality_by_postal_code <- function(data) {
  qualocep_data <- get_qualocep_data() |> rutils::shush()

  brazil_municipalties_data <- get_brazil_municipality(year = 2017)

  data |>
    dplyr::mutate(
      state_code = dplyr::if_else(
        is_postal_code_valid,
        state_code_qualocep,
        state_code
      ),
      municipality_code = dplyr::if_else(
        is_postal_code_valid,
        municipality_code_qualocep,
        municipality_code
      )
    ) |>
    dplyr::left_join(
      brazil_municipalties_data |>
        dplyr::select(
          federal_unit_code, state, municipality_code, municipality
        ) |>
        dplyr::rename(state_code = federal_unit_code),
      by = c("state_code", "municipality_code"),
      suffix = c("", "_geobr")
    ) |>
    dplyr::mutate(
      state = state_geobr,
      municipality = municipality_geobr
    )|>
    dplyr::select(
      -dplyr::all_of(
        c(
          "state_code_qualocep", "municipality_code_qualocep",
          "state_geobr", "municipality_geobr"
        )
      )
    )
}

add_region <- function(data) {
  prettycheck:::assert_tibble(data)

  data |>
    dplyr::mutate(region = get_brazil_region(state, "state")) |>
    dplyr::relocate(region, .after = country)
}

add_qualocep_geocode_data_by_postal_code <- function(data) {
  prettycheck:::assert_tibble(data)

  qualocep_data <- get_qualocep_data()

  data |>
    dplyr::left_join(
      qualocep_data |>
        dplyr::select(postal_code, latitude, longitude),
      by = "postal_code"
    ) |>
    dplyr::mutate(
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
    dplyr::relocate(latitude, longitude, .after = postal_code)
}

add_qualocep_geocode_data_by_municipality <- function(data) {
  prettycheck:::assert_tibble(data)

  qualocep_data <- get_qualocep_data() |> rutils::shush()

  qualocep_data_summary <-
    qualocep_data |>
    dplyr::summarise(
      latitude = mean(latitude, na.rm = TRUE),
      longitude = mean(longitude, na.rm = TRUE),
      .by = c("federal_unit_code", "municipality_code")
    ) |>
    dplyr::rename(state_code = federal_unit_code)

  data |>
    dplyr::left_join(
      qualocep_data_summary,
      by = c("state_code", "municipality_code"),
      suffix = c("", "_qualocep")
    ) |>
    dplyr::mutate(
      latitude = dplyr::if_else(
        is.na(latitude),
        latitude_qualocep,
        latitude
      ),
      longitude = dplyr::if_else(
        is.na(longitude),
        longitude_qualocep,
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
    dplyr::select(-dplyr::ends_with("_qualocep"))
}
