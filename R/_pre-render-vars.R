# library(dplyr)
# library(lubritime)
library(magrittr)
library(targets)

source(here::here("R", "utils.R"))

# targets::tar_make(script = here::here("_targets.R"))

raw_data <- targets::tar_read(
  "raw_data",
  store = here::here("_targets")
)

weighted_data <- targets::tar_read(
  "weighted_data",
  store = here::here("_targets")
)

# Chapter 6 -----

analysis_sample_per_nrow_2017_10_15 <-
  weighted_data |>
  dplyr::filter(lubridate::date(timestamp) == as.Date("2017-10-15")) |>
  nrow() |>
  magrittr::divide_by(weighted_data |> nrow()) |>
  magrittr::multiply_by(100)

# Supplemental Material 1 -----

# Supplemental Material 2 -----

# Supplemental Material 3 -----

# Supplemental Material 4 -----

# Supplemental Material 5 -----

# Supplemental Material 6 -----

# Supplemental Material 7 -----

# Supplemental Material 8 -----

# Supplemental Material 9 -----

# Supplemental Material 10 -----

# Write in `results.yml` -----

write_in_results_yml(
  list(
    pr_raw_data_nrow = raw_data |> nrow(),
    pr_analysis_sample_per_nrow_2017_10_15 = analysis_sample_per_nrow_2017_10_15
  )
)

rm(
  raw_data,
  weighted_data,
  analysis_data_per_nrow_2017_10_15
)

results_vars <- yaml::read_yaml(here::here("_results.yml"))
