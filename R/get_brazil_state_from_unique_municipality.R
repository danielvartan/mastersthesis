# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(vctrs)

# # Helper
#
# data <- targets::tar_read("tidy_data")

get_brazil_state_from_unique_municipality <- function(data) {
  var_set <- c("id", "country", "state", "municipality", "postal_code")

  prettycheck:::assert_tibble(data)
  prettycheck:::assert_subset(var_set, names(data))

  data |>
    dplyr::select(dplyr::all_of(var_set)) |>
    dplyr::filter(is.na(state), !is.na(municipality)) |>
    dplyr::left_join(
      get_brazil_municipality() |>
        dplyr::select(municipality, state) |>
        dplyr::filter(!vctrs::vec_duplicate_detect(municipality)),
      by = "municipality"
    )
}
