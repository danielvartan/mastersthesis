# library(beepr)
# library(groomr) # github.com/danielvartan/groomr
# library(here)
# library(rutils) # https://github.com/danielvartan/rutils

# Remove empty lines from `README.md` -----

groomr::remove_blank_line_dups(here::here("README.md"))

# Update project year -----

c(
  here::here("CITATION.cff"),
  here::here("LICENSE.md")
) |>
rutils::update_pkg_year()

# Check if the script ran successfully -----

beepr::beep(1)

Sys.sleep(3)
