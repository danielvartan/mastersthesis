# See <https://books.ropensci.org/targets/> to learn more.

# library(here)
library(tarchetypes)
library(targets)

source(here::here("R", "get_raw_data.R"))
source(here::here("R", "tidy_data_.R"))
source(here::here("R", "validate_data.R"))
source(here::here("R", "analyze_data.R"))
source(here::here("R", "geocode_data.R"))
source(here::here("R", "add_solar_data.R"))
source(here::here("R", "anonymize_data.R"))
source(here::here("R", "filter_data.R"))
source(here::here("R", "weigh_data.R"))
source(here::here("R", "lock_and_store_data.R"))

targets::tar_option_set(
  packages = c(
    "cli",
    "curl",
    "dplyr",
    "here",
    "hms",
    "lockr", # github.com/danielvartan/lockr
    "lubridate", # For masking reasons.
    "lubritime", # github.com/danielvartan/lubritime
    "methods",
    "mctq",
    "prettycheck", # github.com/danielvartan/prettycheck
    "osfr",
    "readr",
    "rlang",
    "rutils", # github.com/danielvartan/rutils
    "scaler", # github.com/danielvartan/scaler
    "stringr",
    "tidyr",
    "utils"
  )
)

# tar_make_clustermq() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
options(clustermq.scheduler = "multiprocess")

# tar_make_future() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
future::plan(future.callr::callr)

# Run the R scripts in the R/ folder with your custom functions:
# targets::tar_source(files = here::here("R"))
# source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  targets::tar_target(
    name = raw_data,
    command = get_raw_data()
  ),
  targets::tar_target(
    name = tidy_data,
    command = tidy_data_(raw_data)
  ),
  targets::tar_target(
    name = validated_data,
    command = validate_data(tidy_data)
  ),
  targets::tar_target(
    name = analyzed_data,
    command = analyze_data(validated_data)
  ),
  targets::tar_target(
    name = geocoded_data,
    command = geocode_data(analyzed_data)
  ),
  targets::tar_target(
    name = added_data,
    command = add_solar_data(geocoded_data)
  ),
  targets::tar_target(
    name = anonymized_data,
    command = anonymize_data(added_data)
  ),
  targets::tar_target(
    name = filtered_data,
    command = filter_data(anonymized_data)
  ),
  targets::tar_target(
    name = weighted_data,
    command = weigh_data(filtered_data)
  )
  # targets::tar_target(
  #   name = locked_data,
  #   command = lock_and_store_data(weighted_data)
  # )
)
