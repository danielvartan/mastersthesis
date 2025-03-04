# library(dplyr)
# library(ISOcodes)
# library(magrittr)
# library(prettycheck) # github.com/danielvartan/prettycheck
library(rlang)

# Based on data from the International Organization for Standardization (ISO)
# (ISO 3166-1) via the `ISOcodes` R package.
# See <https://www.iso.org/iso-3166-country-codes.html> to learn more.

# The `ISOcodes` R package uses XML files provided by Debian’s `iso-codes`
# package for the data. More about the `iso-codes` package can be found at
# <https://salsa.debian.org/iso-codes-team/iso-codes>.

# # Helpers
#
# source(here::here("R", "get_country_names.R"))
#
# get_country_names() |>
#   sort() |>
#   utils::writeClipboard()

get_country_names <- function(format = "common name") {
  format_options <- c(
    "alpha 2", "alpha 3", "numeric", "name", "oficcial name", "common name"
  )

  checkmate::assert_choice(format, format_options)

  out <-
    ISOcodes::ISO_3166_1 |>
    magrittr::set_names(format_options) |>
    dplyr::as_tibble()

  if (format %in% c("oficcial name", "common name")) {
    out <-
      out |>
      dplyr::mutate(
        !!as.symbol(format) := dplyr::if_else(
          is.na(!!as.symbol(format)),
          `name`,
          !!as.symbol(format)
        )
      )
  }

  out |> dplyr::pull(format)
}
