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

# library(cli)
# library(dplyr)
# library(here)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(tidygeocoder)

source(here::here("R", "fix_postal_code.R"))
source(here::here("R", "get_qualocep_data.R"))

# # Helpers
#
# get_geocode_by_postal_code("01223000")
# get_geocode_by_postal_code("01223000", method = "osm")

get_geocode_by_postal_code <- function(
    postal_code,
    method = "qualocep",
    fix_code = TRUE, # Just for Brazilian postal codes.
    limit = 10, # Inf for all.
    suffix = ", Brazil"
  ) {
  prettycheck:::assert_internet()
  prettycheck:::assert_atomic(postal_code)
  prettycheck:::assert_choice(method, c("osm", "google", "qualocep"))
  prettycheck:::assert_flag(fix_code)
  prettycheck:::assert_number(limit)
  prettycheck:::assert_string(suffix)

  if (!is.infinite(limit)) {
    limit <- as.integer(ceiling(limit))
    postal_code <- postal_code[seq_len(min(length(postal_code), limit))]
  }

  if (isTRUE(fix_code)) postal_code <- postal_code |> fix_postal_code()

  prettycheck:::assert_character(postal_code, pattern = "^\\d{8}$")

  if (method %in% c("osm", "google")) {
    get_geocode_by_postal_code_tidygeocoder(
      postal_code, method, limit, suffix
    )
  } else if (method == "qualocep") {
    get_geocode_by_postal_code_qualocep(postal_code, limit)
  }
}

get_geocode_by_postal_code_tidygeocoder <- function(
    postal_code,
    method = "osm",
    limit = 10,
    suffix = ", Brazil"
) {
  postal_code <- fix_postal_code(postal_code, zero_na = FALSE)

  prettycheck:::assert_internet()
  prettycheck:::assert_character(postal_code, pattern = "^\\d{8}$")
  prettycheck:::assert_number(limit)

  if (!is.infinite(limit)) {
    limit <- as.integer(ceiling(limit))
    postal_code <- postal_code[seq_len(min(length(postal_code), limit))]
  }

  if (method == "osm") {
    dplyr::tibble(postal_code = postal_code) |>
      tidygeocoder::geocode(
        postalcode = postal_code,
        method = method,
        lat = "latitude",
        long = "longitude"
      )
  } else {
    dplyr::tibble(address = paste0(postal_code, suffix)) |>
      tidygeocoder::geocode(
        address = address,
        method = method,
        lat = "latitude",
        long = "longitude"
      ) |>
      dplyr::transmute(
        postal_code = postal_code,
        latitude = dplyr::if_else(
          latitude == -14.235004,
          NA_real_,
          latitude),
        longitude = dplyr::if_else(
          longitude == -51.92528,
          NA_real_,
          longitude)
      )
  }
}

get_geocode_by_postal_code_qualocep <- function(postal_code, limit = 10) {
  postal_code <- fix_postal_code(postal_code)

  prettycheck:::assert_internet()
  prettycheck:::assert_character(postal_code, pattern = "^\\d{8}$")
  prettycheck:::assert_number(limit)

  if (!is.infinite(limit)) {
    limit <- as.integer(ceiling(limit))
    postal_code <- postal_code[seq_len(min(length(postal_code), limit))]
  }

  qualocep_data <- get_qualocep_data()

  out <-
    dplyr::tibble(postal_code = postal_code) |>
    dplyr::left_join(
      qualocep_data,
      by = "postal_code"
    )

  if (any(c("latitude", "longitude") %in% names(qualocep_data))) {
    out |> dplyr::select(postal_code, latitude, longitude)
  } else {
    out |>
      dplyr::transmute(
        postal_code = postal_code,
        latitude = NA_real_,
        longitude = NA_real_
      )
  }
}
