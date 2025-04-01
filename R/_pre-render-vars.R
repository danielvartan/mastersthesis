# Load packages -----

# library(dplyr)
# library(lubritime)
library(magrittr)
library(targets)

# Load variables -----

env_vars <- yaml::read_yaml(here::here("_variables.yml"))
res_vars <- yaml::read_yaml(here::here("_results.yml"))

# Load data -----

# targets::tar_make(script = here::here("_targets.R"))

raw_data <- targets::tar_read(
  "raw_data",
  store = here::here("_targets")
)

tidy_data <- targets::tar_read(
  "tidy_data",
  store = here::here("_targets")
)

weighted_data <- targets::tar_read(
  "weighted_data",
  store = here::here("_targets")
)

# Chapter 5 -----

pr_analysis_sample_msf_sc_mean <-
  weighted_data |>
  dplyr::pull(msf_sc) |>
  lubritime:::link_to_timeline(threshold = hms::parse_hms("12:00:00")) |>
  mean(na.rm = TRUE) |>
  hms::as_hms() |>
  lubritime::round_time() |>
  as.character()

pr_analysis_sample_msf_sc_sd <-
  weighted_data |>
  dplyr::pull(msf_sc) |>
  lubritime:::link_to_timeline(threshold = hms::parse_hms("12:00:00")) |>
  stats::sd(na.rm = TRUE) |>
  hms::as_hms() |>
  lubritime::round_time() |>
  as.character()

pr_tidy_data_per_nrow_2017_10_15_21 <-
  tidy_data |>
  dplyr::filter(
    lubridate::date(timestamp) >= as.Date("2017-10-15"),
    lubridate::date(timestamp) <= as.Date("2017-10-21")
  ) |>
  nrow() |>
  magrittr::divide_by(tidy_data |> nrow()) |>
  magrittr::multiply_by(100)

data_sex_per <-
  weighted_data |>
  dplyr::summarise(
    n = dplyr::n(),
    .by = sex
  ) |>
  dplyr::mutate(n_per = (n / sum(n)) * 100)

pr_weighted_data_male_per <-
  data_sex_per |>
  dplyr::filter(sex == "Male") |>
  dplyr::pull(n_per)

pr_weighted_data_female_per <-
  data_sex_per |>
  dplyr::filter(sex == "Female") |>
  dplyr::pull(n_per)

# Chapter 6 -----

pr_analysis_sample_per_nrow_2017_10_15 <-
  weighted_data |>
  dplyr::filter(lubridate::date(timestamp) == as.Date("2017-10-15")) |>
  nrow() |>
  magrittr::divide_by(weighted_data |> nrow()) |>
  magrittr::multiply_by(100)

# Others -----

if (res_vars$hta_effect_size$f_squared >
    res_vars$htb_effect_size$f_squared) {
  final_effect_size <- res_vars$hta_effect_size
} else {
  final_effect_size <- res_vars$htb_effect_size
}

# Write in `results.yml` -----

quartor:::write_in_results_yml(
  list(
    pr_raw_data_nrow = raw_data |> nrow(),
    pr_analysis_sample_msf_sc_mean = pr_analysis_sample_msf_sc_mean,
    pr_analysis_sample_msf_sc_sd = pr_analysis_sample_msf_sc_sd,
    pr_tidy_data_per_nrow_2017_10_15_21 = pr_tidy_data_per_nrow_2017_10_15_21,
    pr_weighted_data_male_per = pr_weighted_data_male_per,
    pr_weighted_data_female_per = pr_weighted_data_female_per,
    pr_analysis_sample_per_nrow_2017_10_15 = pr_analysis_sample_per_nrow_2017_10_15,
    final_effect_size = final_effect_size
  )
)

# Clean environment -----

rm(
  raw_data,
  weighted_data,
  pr_analysis_sample_msf_sc_mean,
  pr_analysis_sample_msf_sc_sd,
  pr_tidy_data_per_nrow_2017_10_15_21,
  pr_weighted_data_male_per,
  pr_weighted_data_female_per,
  pr_analysis_sample_per_nrow_2017_10_15,
  final_effect_size
)

# Reload `result_vars` -----

res_vars <- yaml::read_yaml(here::here("_results.yml"))
