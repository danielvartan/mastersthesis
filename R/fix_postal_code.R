# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(stringr)

fix_postal_code <- function(
    postal_code,
    min_char = 3,
    max_char = 8,
    squish = TRUE,
    remove_non_numeric = TRUE,
    remove_number_sequences = TRUE,
    trunc = TRUE, # To `max_char` width.
    pad = TRUE, # To `max_char` width.
    zero_na = TRUE # To `max_char` width.
  ) {
  prettycheck:::assert_atomic(postal_code)
  prettycheck:::assert_number(min_char, lower = 1)
  prettycheck:::assert_number(max_char, lower = min_char)
  prettycheck:::assert_flag(squish)
  prettycheck:::assert_flag(remove_non_numeric)
  prettycheck:::assert_flag(remove_number_sequences)
  prettycheck:::assert_flag(trunc)
  prettycheck:::assert_flag(pad)

  postal_code <- postal_code |> as.character()

  posta_code <- dplyr::case_when(
    nchar(postal_code) < min_char ~ NA_character_,
    nchar(postal_code) > max_char ~ NA_character_,
    TRUE ~ postal_code
  )

  if (isTRUE(squish)) postal_code <- postal_code |> stringr::str_squish()

  if (isTRUE(remove_non_numeric)) {
    postal_code <- postal_code |> stringr::str_remove_all("\\D+")
  }

  if (isTRUE(remove_number_sequences)) {
    for (i in seq(0, 9)) {
      postal_code <-
        dplyr::if_else(
          postal_code |>
            stringr::str_detect(paste0("^", i, "{1,}$")),
          NA_character_,
          postal_code
        )
    }

    sequence_string <- character()

    for (i in seq(1, max_char)) {
      sequence_string <- paste0(sequence_string, i)

      postal_code <-
        dplyr::if_else(
          postal_code |>
            stringr::str_detect(paste0("^", sequence_string, "{1,}$")),
          NA_character_,
          postal_code
        )
    }
  }

  if (isTRUE(trunc)) {
    postal_code <-
      postal_code |>
      stringr::str_trunc(
        width = max_char,
        side = "right",
        ellipsis = ""
      )
  }

  if (isTRUE(pad)) {
    postal_code <-
      postal_code |>
      stringr::str_pad(
        width = max_char,
        side = "right",
        pad = "0"
      )
  }

  postal_code <- dplyr::if_else(postal_code == "", NA_character_, postal_code)

  if (isTRUE(zero_na)) {
    postal_code <- dplyr::if_else(is.na(postal_code), "00000000", postal_code)
  }

  postal_code
}
