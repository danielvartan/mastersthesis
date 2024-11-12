# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(stringr)

fix_brazil_postal_code <- function(
    postal_code,
    squish = TRUE,
    remove_non_numeric = TRUE,
    trunc = TRUE,
    pad = TRUE,
    zero_na = TRUE
  ) {
  prettycheck:::assert_atomic(postal_code)
  prettycheck:::assert_flag(squish)
  prettycheck:::assert_flag(remove_non_numeric)
  prettycheck:::assert_flag(trunc)
  prettycheck:::assert_flag(pad)

  postal_code <- as.character(postal_code)

  if (isTRUE(squish)) postal_code <- stringr::str_squish(postal_code)

  if (isTRUE(remove_non_numeric)) {
    postal_code <- stringr::str_remove_all(postal_code, "\\D+")
  }

  if (isTRUE(trunc)) {
    postal_code <- stringr::str_trunc(postal_code, 8, "right", ellipsis = "")
  }

  if (isTRUE(pad)) {
    postal_code <- stringr::str_pad(postal_code, 8, "right", "0")
  }

  postal_code <- dplyr::if_else(postal_code == "", NA_character_, postal_code)

  if (isTRUE(zero_na)) {
    postal_code <- dplyr::if_else(is.na(postal_code), "00000000", postal_code)
  }

  postal_code
}
