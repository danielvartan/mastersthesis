# library(beepr)
# library(groomr) # github.com/danielvartan/groomr
# library(here)
# library(lubridate)
# library(readr)
# library(stringr)

# Remove empty lines from `README.md` -----

here::here("README.md") |> groomr::remove_blank_line_dups()

# Update project year -----

file <- c(here::here("LICENSE.md"))

data <-
  file |>
  readr::read_lines() |>
  stringr::str_replace_all(
    pattern = "20\\d{2}",
    replacement = as.character(Sys.Date() |> lubridate::year())
  )

data |> readr::write_lines(file)

# Check if the script ran successfully -----

beepr::beep(1)

Sys.sleep(3)
