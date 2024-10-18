# library(cli)
# library(dplyr)
# library(here)
# library(lubridate)
# library(prettycheck) # https://github.com/danielvartan/prettycheck

source(here::here("R/utils.R"))
source(here::here("R/utils-stats.R"))

# filtered_data <- analyzed_data |> filter_data()

filter_data <- function(data) {
  prettycheck:::assert_tibble(data)

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
