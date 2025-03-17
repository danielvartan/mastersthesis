# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(stringdist)

source(here::here("R", "utils.R"))

# # Helpers
#
# source(here::here("R", "tidy_data_.R"))
# raw_data <- targets::tar_read("raw_data")
# data <- raw_data |> fix_var_names()
# raw <- data |> dplyr::pull(municipality) |> unique()
#
# source(here::here("R", "get_brazil_municipality.R"))
#
# reference <-
#   get_brazil_municipality() |>
#   dplyr::pull(municipality)
#
# data <-
#   match_strings(raw, reference, maxDist = 1) |>
#   dplyr::arrange(key) |>
#   dplyr::arrange(value)
#
# source(here::here("R", "write_values_to_lookup_sheet.R"))
# # (!CAREFUL!) data |> write_values_to_lookup_sheet("municipality")

#' Approximate string matching
#'
#' @description
#'
#' `match_strings()` performs approximate string matching between two
#' [`character`][base::character()] vectors using the
#' [`amatch()`][stringdist::amatch()] function from the `stringdist` package.
#'
#' The difference between this function and [`amatch()`][stringdist::amatch()]
#' is that `match_strings()` returns a [`tibble`][tibble::tibble()] with the
#' original strings and their matched counterparts, while
#' [`amatch()`][stringdist::amatch()] returns only the indices of the matched
#' strings.
#'
#' @param raw A [`character`][base::character()] vector with the data to be
#'   matched.
#' @param reference A [`character`][base::character()] vector with the reference
#'  to match the raw data.
#' @param ... Additional arguments to be passed to
#'   [`amatch()`][stringdist::amatch()].
#'
#' @return A [`tibble`][tibble::tibble()] with the original strings and their
#'   matched counterparts.
#'
#' @noRd
#'
#' @examples
#'
#' raw <- c("sao paulo", "rio de janeiro", "SAO PULO", "RiO de Janiro")
#' reference <- c("São Paulo", "Rio de Janeiro")
#' match_strings(raw, reference, maxDist = 1)
match_strings <- function(raw, reference, one_by_one = FALSE, ...) {
  checkmate::assert_character(raw)
  checkmate::assert_character(reference)
  checkmate::assert_flag(one_by_one)

  if (isTRUE(one_by_one)) {
    prettycheck::assert_identical(raw, reference, type = "length")

    match <-
      purrr::map2_chr(
        .x = raw,
        .y = reference,
        .f = function(x, y) {
          match_test <-
            stringdist::amatch(
              x = x |> groomr::to_ascii() |> tolower(),
              table = y |> groomr::to_ascii() |> tolower(),
              ...
            )

          if (is.na(match_test)) NA_character_ else y
        }
      )

    dplyr::tibble(
      key = raw,
      value = match
    )
  } else {
    match <- stringdist::amatch(
      x = raw |> groomr::to_ascii() |> tolower(),
      table = reference |> groomr::to_ascii() |> tolower(),
      ...
    )

    dplyr::tibble(
      key = raw,
      value = reference[match]
    )
  }
}
