# library(beepr)
# library(groomr) # https://github.com/danielvartan/groomr
# library(here)
# library(readr)
# library(stringr)

# Remove empty lines from `README.md` -----

groomr::remove_blank_line_dups(here::here("README.md"))

# Update project year -----

files <- here::here("LICENSE.md")

for (i in files) {
  data <-
    i |>
    readr::read_lines() |>
    stringr::str_replace_all(
      pattern = "20\\d{2}",
      replacement = as.character(Sys.Date() |> lubridate::year())
    )

  data |> readr::write_lines(i)
}

# Check if the script ran successfully -----

beepr::beep(1)

Sys.sleep(3)
