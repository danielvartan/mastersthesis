# See <https://books.ropensci.org/targets/> to learn more.

library(here)
library(tarchetypes)
library(targets)

source("R/get_raw_data.R")
source("R/tidy_data.R")
source("R/validate_data.R")
source("R/analyze_data.R")
source("R/filter_data.R")
source("R/add_geocode_data.R")
source("R/lock_data.R")
source("R/look_and_replace.R")

targets::tar_option_set(
  packages = c(
    "lubridate", # For masking reasons.
    "checkmate", "cli", "curl", "dplyr", "gutils", "googleCloudStorageR",
    "here", "hms", "lockr", "lubritime", "mctq", "readr", "rlang",
    "scaler", "stringr", "tidyr", "utils"
    )
)

# tar_make_clustermq() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
options(clustermq.scheduler = "multiprocess")

# tar_make_future() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
future::plan(future.callr::callr)

# Run the R scripts in the R/ folder with your custom functions:
targets::tar_source()
# source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  targets::tar_target(name = data, command = get_raw_data()),
  targets::tar_target(name = tidy, command = tidy_data(data)),
  targets::tar_target(name = validated, command = validate_data(tidy)),
  targets::tar_target(name = analyzed, command = analyze_data(validated)),
  targets::tar_target(name = filtered, command = filter_data(analyzed)),
  targets::tar_target(name = geocoded, command = add_geocode_data(filtered)),
  tarchetypes::tar_quarto(name = book, path = here::here())
)
