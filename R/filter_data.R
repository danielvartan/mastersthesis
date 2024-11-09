# library(cli)
# library(dplyr)
# library(here)
# library(lubridate)
# library(prettycheck) # github.com/danielvartan/prettycheck

source(here::here("R/utils.R"))
source(here::here("R/utils-stats.R"))

# filtered_data <- analyzed_data |> filter_data()

# # Outlier removal notice
#
# The data were collected via a form without field validation, resulting in
# considerable noise. Despite subsequent validation steps, some noise remained.
# To mitigate this, data points exceeding 1.5 times the interquartile range
# (IQR) above the third quartile (Q3) or below 1.5 times the IQR from the first
# quartile (Q1) were filtered out. This outlier removal was applied solely to
# the age and msf_sc variables.

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
