# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck

source(here::here("R", "get_brazil_address_by_postal_code.R"))
source(here::here("R", "filter_geographic_data.R"))
source(here::here("R", "match_strings.R"))
source(here::here("R", "tidy_data_.R"))

# To learn more about the source of the crossing data, see
# "./R/get_brazil_address_by_postal_code.R".

# # Helpers
#
# raw_data <- targets::tar_read("raw_data")
#
# source(here::here("R", "cross_geographic_data_by_postal_code.R"))
#
# data <-
#   raw_data |>
#   cross_geographic_data_by_postal_code(
#     col = "municipality",
#     value = "Goiás",
#     limit = Inf,
#     col_merge = c("municipality", "state"),
#     col_match = "state",
#     maxDist = 1
#   )
#
# data |> nrow()
# data |> dplyr::pull("id") |> utils::writeClipboard()
# data |> dplyr::pull("municipality_qualocep") |> utils::writeClipboard()
#
# data_na <- data|> dplyr::filter(is.na(.data$match))
#
# source(here::here("R", "get_brazil_municipality.R"))
# get_brazil_municipality("PORTO FERREIRA")

cross_geographic_data_by_postal_code <- function(
  raw_data,
  col,
  value,
  method = "qualocep",
  limit = 10,
  col_merge = c("municipality", "state"),
  col_match = col_merge[1],
  ...
  ) {
  prettycheck:::assert_tibble(raw_data)
  prettycheck:::assert_string(col)
  prettycheck:::assert_string(value)
  prettycheck:::assert_string(method)
  prettycheck:::assert_number(limit)
  prettycheck:::assert_character(col_merge)
  prettycheck:::assert_string(col_match)

  geo_data <- raw_data |> filter_geographic_data(col, value)

  if (!is.infinite(limit)) {
    prettycheck:::assert_integer_number(limit, lower = 1)

    geo_data <- geo_data |> dplyr::slice(seq_len(min(nrow(geo_data), limit)))
  }

  prettycheck:::assert_subset(col_match, names(geo_data))

  postal_code_data <-
    geo_data |>
    dplyr::pull("postal_code") |>
    get_brazil_address_by_postal_code(method = method, limit = limit)

  prettycheck:::assert_subset(col_merge, names(postal_code_data))
  prettycheck:::assert_subset(col_match, names(postal_code_data))

  geo_data |>
    # dplyr::select(postal_code, id, municipality) |>
    dplyr::bind_cols(
      postal_code_data |>
        dplyr::select(dplyr::all_of(col_merge)) |>
        dplyr::rename_with(~ paste0(.x, "_", method))
    ) |>
    dplyr::mutate(
      match = match_strings(
        raw = .data[[col_match]],
        reference = .data[[paste0(col_match, "_", method)]],
        one_by_one = TRUE,
        ...
      ),
      match = match$value
    )
}
