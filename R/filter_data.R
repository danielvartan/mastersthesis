# # TODO:
#
# * Document functions.

require(checkmate, quietly = TRUE)
require(cli, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(lubridate, quietly = TRUE)

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
