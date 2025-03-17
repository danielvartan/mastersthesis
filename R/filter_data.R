# library(cli)
# library(dplyr)
# library(here)
# library(lubridate)
# library(prettycheck) # github.com/danielvartan/prettycheck

source(here::here("R", "utils.R"))
source(here::here("R", "utils-stats.R"))

# # Outlier removal notice
#
# The data were collected via a form without field validation, resulting in
# considerable noise. Despite subsequent validation steps, some noise remained.
# To mitigate this, data points exceeding 1.5 times the interquartile range
# (IQR) above the third quartile (Q3) or below 1.5 times the IQR from the first
# quartile (Q1) were filtered out. This outlier removal was applied solely to
# the `age` and `msf_sc` variables.

# # Helpers
#
# geocoded_data <- targets::tar_read("geocoded_data")
# filtered_data <- geocoded_data |> filter_data()

filter_data <- function(data) {
  checkmate::assert_tibble(data)

  cli::cli_progress_step("Filtering data")

  data |>
    dplyr::filter(
      lubridate::date(timestamp) >= lubridate::ymd("2017-10-15"),
      lubridate::date(timestamp) <= lubridate::ymd("2017-10-21"),
      country == "Brazil",
      state %in% get_brazil_state_by_utc(-3, "state"),
      age >= 18
    ) |>
    dplyr::filter(
      !rutils::test_outlier(age, method = "iqr", iqr_mult = 1.5),
      !rutils::test_outlier(
        msf_sc |>
          lubritime::link_to_timeline() |>
          as.numeric(),
        method = "iqr",
        iqr_mult = 1.5
      )
    ) |>
    tidyr::drop_na(msf_sc, sex, age, state, latitude, longitude)
}
