# library(beepr)
# library(groomr) # github.com/danielvartan/groomr
# library(here)

# Remove empty lines from `README.md` -----

groomr::remove_blank_line_dups(here::here("README.md"))

# Check if the script ran successfully -----

beepr::beep(1)

Sys.sleep(3)
