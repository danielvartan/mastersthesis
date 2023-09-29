# library(checkmate, quietly = TRUE)
# library(cli, quietly = TRUE)
# library(dplyr, quietly = TRUE)
# library(here)
# library(lubridate, quietly = TRUE)

source(here::here("R/utils-stats.R"))

filter_data <- function(data) {
  checkmate::assert_tibble(data)

  cli::cli_progress_step("Filtering data")

  data |>
    dplyr::filter(
      lubridate::date(timestamp) >= lubridate::ymd("2017-10-15"),
      lubridate::date(timestamp) <= lubridate::ymd("2017-10-21"),
      country == "Brazil",
      age >= 18
    ) |>
    dplyr::filter(
      !is_outlier(age),
      !is_outlier(transform_time(msf_sc))
    )
}
