# library(dplyr)
# library(here)
library(lubridate)
# library(prettycheck) # github.com/danielvartan/prettycheck

source(here::here("R", "get_chronotype_cutoffs.R"))
source(here::here("R", "utils.R"))

categorize_msf_sc <- function(x) {
  prettycheck:::assert_hms(x)

  fill <-
    dplyr::tibble(msf_sc = x) |>
    get_chronotype_cutoffs("msf_sc", pretty = FALSE)

  x <- x |> lubritime:::link_to_timeline()

  dplyr::case_when(
    x < lubridate::int_start(fill$interval[1]) ~ fill$label[1],
    x %within% fill$interval[1] ~ fill$label[1],
    x %within% fill$interval[2] ~ fill$label[2],
    x %within% fill$interval[3] ~ fill$label[3],
    x %within% fill$interval[4] ~ fill$label[4],
    x %within% fill$interval[5] ~ fill$label[5],
    x %within% fill$interval[6] ~ fill$label[6],
    x %within% fill$interval[7] ~ fill$label[7],
    x > lubridate::int_end(fill$interval[7]) ~ fill$label[7]
  )
}
