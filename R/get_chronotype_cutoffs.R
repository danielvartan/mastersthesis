# library(dplyr)
# library(here)
# library(hms)
# library(prettycheck) # github.com/danielvartan/prettycheck
library(rlang)
# library(rutils) # github.com/danielvartan/rutils
# library(stats)
# library(tidyr)

source(here::here("R/utils.R"))
source(here::here("R/utils-plots.R"))

# # Helpers
#
# out |>
#   dplyr::group_by(
#     interval = cut( # Bins by quantiles.
#       !!as.symbol(col_msf_sc),
#       breaks = quantile(
#         transform_time(rutils:::drop_na(!!as.symbol(col_msf_sc))),
#         probs
#       ),
#       dig.lab = 10,
#       include.lowest = TRUE
#     )
#   ) |>
#   dplyr::pull(interval) |>
#   unique() |>
#   cut_mean() |>
#   as.POSIXct(tz = "UTC") |>
#   sort()

get_chronotype_cutoffs <- function(
    data,
    col_msf_sc = "msf_sc",
    pretty = TRUE
  ) {
  checkmate::assert_tibble(data)
  checkmate::assert_string(col_msf_sc)
  checkmate::assert_choice(col_msf_sc, names(data))
  checkmate::assert_flag(pretty)

  out <-
    data |>
    dplyr::select(!!as.symbol(col_msf_sc)) |>
    tidyr::drop_na() |>
    dplyr::mutate(
      !!as.symbol(col_msf_sc) := transform_time(
        !!as.symbol(col_msf_sc),
        threshold = hms::parse_hms("12:00:00")
      )
    )

  ## See `ggplot2::cut_interval()`.
  # Quantiles (e.g., "Extremely early" is 0 to `1 / 3 / 3``) (7 slots).
  probs <- c(
    0 / 3 / 3,
    1 / 3 / 3,
    2 / 3 / 3,
    3 / 3 / 3,
    6 / 3 / 3,
    7 / 3 / 3,
    8 / 3 / 3,
    9 / 3 / 3
  )

  out <-
    out |>
    dplyr::group_by(
      interval = cut(
        !!as.symbol(col_msf_sc),
        breaks = stats::quantile(
          transform_time(rutils:::drop_na(!!as.symbol(col_msf_sc))),
          probs
        ),
        dig.lab = 10,
        include.lowest = TRUE
      )
    ) |>
    dplyr::summarise(freq = dplyr::n()) |>
    dplyr::mutate(
      order = seq_along(interval),
      interval = transform_cut_levels(as.character(interval)),
      freq = (freq / sum(freq)) * 100,
      label = c(
        "Extremely early", "Moderately early", "Slightly early",
        "Intermediate", "Slightly late", "Moderately late", "Extremely late"
      )
    )

  if (isTRUE(pretty)) {
    out |>
      dplyr::mutate(
        interval = paste0(
          interval |>
            lubridate::int_start() |>
            hms::as_hms() |>
            lubritime::round_time(),
          "-",
          interval |>
            lubridate::int_end() |>
            hms::as_hms() |>
            lubritime::round_time()
        )
      )
  } else {
    out
  }
}
