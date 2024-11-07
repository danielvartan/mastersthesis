# library(cli)
# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck

source(here::here("R", "get_brazil_state_latitude.R"))
source(here::here("R", "get_brazil_state_longitude.R"))
source(here::here("R", "get_brazil_region.R"))

# geocoded_data <- filtered_data |> add_geocode_data()

add_geocode_data <- function(data) {
  prettycheck:::assert_tibble(data)

  cli::cli_progress_step("Adding geocode data")

  out <- data |>
    dplyr::mutate(
      latitude = get_brazil_state_latitude(state, "state"),
      longitude = get_brazil_state_longitude(state, "state"),
      region = get_brazil_region(state, "state")
    ) |>
    dplyr::relocate(region, .after = country) |>
    dplyr::relocate(latitude, longitude, .after = postal_code)

  invisible(out)
}
