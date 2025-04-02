# library(checkmate)
# library(dplyr)

anonymize_data <- function(data) {
  checkmate::assert_tibble(data)

  data |>
    remove_data_identification() |>
    remove_sensitive_data()
}

# library(checkmate)
# library(dplyr)

remove_data_identification <- function(data) {
  id_vars <- c("track", "name", "email")

  checkmate::assert_tibble(data)
  checkmate::assert_subset(id_vars, names(data))

  data |>
    dplyr::select(-dplyr::all_of(id_vars))
}

# library(checkmate)
# library(dplyr)

remove_sensitive_data <- function(data) {
  sensible_vars <- c("gender_identity", "sexual_orientation")

  checkmate::assert_tibble(data)
  checkmate::assert_subset(sensible_vars, names(data))

  data |>
    dplyr::select(-dplyr::all_of(sensible_vars))
}
