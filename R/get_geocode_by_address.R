# library(cli)
# library(dplyr)
# library(here)
# library(jsonlite)
# library(lubridate)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(stringdist)
# library(stringr)
# library(tidygeocoder)

source(here::here("R", "get_qualocep_data.R"))
source(here::here("R", "utils.R"))

# # Helpers
#
# get_geocode_by_address(
#   state = "Rio Grande do Norte",
#   municipality = "Bom Jesus",
#   method = "qualocep",
#   mean_values = TRUE,
#   limit = Inf
# )
#
# get_geocode_by_address(
#   address = "Bom Jesus, Rio Grande do Norte, Brazil",
#   method = "google"
# )

get_geocode_by_address <- function(
    address = NULL, # Use `render_brazil_address()` to format the address.
    method = "qualocep",
    limit = 10, # Inf for all.
    ...
  ) {
  prettycheck:::assert_internet()
  prettycheck:::assert_character(address, null.ok = TRUE)
  prettycheck:::assert_choice(method, c("osm", "google", "qualocep"))
  prettycheck:::assert_number(limit)

  if (!is.infinite(limit)) {
    limit <- as.integer(ceiling(limit))
    address <- address[seq_len(min(length(address), limit))]
  }

  if (method %in% c("osm", "google")) {
    get_geocode_by_address_tidygeocoder(address, method, limit)
  } else if (method == "qualocep") {
    get_geocode_by_address_qualocep(limit, ...)
  }
}

get_geocode_by_address_tidygeocoder <- function(
    address,
    method = "osm",
    limit = 10
  ) {
  prettycheck:::assert_internet()
  prettycheck:::assert_character(address)
  prettycheck:::assert_number(limit)

  if (!is.infinite(limit)) {
    limit <- as.integer(ceiling(limit))
    address <- address[seq_len(min(length(address), limit))]
  }

  out <-
    dplyr::tibble(address = address) |>
    tidygeocoder::geocode(
      address = address,
      method = method,
      lat = "latitude",
      long = "longitude"
    )

  if (method == "google") {
    out |>
      dplyr::mutate(
        latitude = dplyr::if_else(
          latitude == -14.235004,
          NA_real_,
          latitude),
        longitude = dplyr::if_else(
          longitude == -51.92528,
          NA_real_,
          longitude)
      )
  } else {
    out
  }
}

get_geocode_by_address_qualocep <- function(
    limit = 10,
    mean_values = TRUE,
    ...
  ) {
  prettycheck:::assert_internet()
  prettycheck:::assert_number(limit)

  if (length(list(...)) == 0) {
    cli::cli_abort(paste0(
      "You must provide at least one argument to ",
      "{.strong {cli::col_red('...')}} " ,
      "when using the {.strong qualocep} method."
    ))
  }

  out <- get_qualocep_data()
  args <- list(...)

  prettycheck:::assert_subset(names(args), names(out))

  for (i in seq_along(args)) {
    out <- out |> dplyr::filter(.data[[names(args)[i]]] %in% args[[i]])
  }

  address <- args |> unlist() |> list()

  if (length(out) == 0) {
    out |>
      dplyr::transmute(
        address = address,
        latitude = NA_real_,
        longitude = NA_real_
      )
  } else {
    out <-
      out |>
      dplyr::transmute(
        address = address,
        latitude = latitude,
        longitude = longitude
      )

    if (!is.infinite(limit)) {
      limit <- as.integer(ceiling(limit))

      out <- out |> dplyr::slice(seq_len(limit))
    }

    if (isTRUE(mean_values)) {
      out |>
        dplyr::summarise(
          latitude = mean(latitude, na.rm = TRUE),
          longitude = mean(longitude, na.rm = TRUE)
        ) |>
        dplyr::mutate(address = address, .before = latitude) |>
        dplyr::mutate(
          dplyr::across(
            .cols = dplyr::where(is.numeric),
            .fns = ~ dplyr::if_else(is.nan(.x), NA_real_, .x)
          )
        )
    } else {
      out
    }
  }
}
