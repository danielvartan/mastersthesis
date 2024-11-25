# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck

anonymize_data <- function(data) {
  prettycheck:::assert_tibble(data)

  data |>
    remove_data_identification() |>
    remove_sensitive_data()
}

# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck

remove_data_identification <- function(data) {
  id_vars <- c("track", "name", "email")

  prettycheck:::assert_tibble(data)
  prettycheck:::assert_subset(id_vars, names(data))

  data |>
    dplyr::select(-dplyr::all_of(id_vars))
}

# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck

remove_sensitive_data <- function(data) {
  sensible_vars <- c("gender_identity", "sexual_orientation")

  prettycheck:::assert_tibble(data)
  prettycheck:::assert_subset(sensible_vars, names(data))

  data |>
    dplyr::select(-dplyr::all_of(sensible_vars))
}
