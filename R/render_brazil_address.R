# library(dplyr)
# library(glue)
# library(here)

source(here::here("R", "utils.R"))

render_brazil_address <- function(
    street = NA_character_,
    complement = NA_character_,
    neighborhood = NA_character_,
    municipality = NA_character_,
    state = NA_character_,
    postal_code = NA_character_
  ) {
  checkmate::assert_character(street)
  checkmate::assert_character(complement)
  checkmate::assert_character(neighborhood)
  checkmate::assert_character(municipality)
  checkmate::assert_character(state)
  checkmate::assert_character(postal_code, pattern = "^\\d{8}$")

  prettycheck::assert_identical(
    street, complement, neighborhood, municipality, state, postal_code,
    type = "length"
  )

  `%>%` <- dplyr::`%>%`
  out <- character()

  for (i in seq_along(postal_code)) {
    out <-
      glue::glue(
        ifelse(street[i] %in% c(NA, "", "NA"), "", "{street[i]}, "),
        ifelse(
          complement[i] %in% c(NA, "", "NA"),
          "",
          "{complement[i]}, "
        ),
        ifelse(
          neighborhood[i] %in% c(NA, "", "NA"),
          "",
          "{neighborhood[i]}, "
        ),
        dplyr::case_when(
          !(municipality[i] %in% c(NA, "", "NA")) &
            !(state[i] %in% c(NA, "", "NA")) ~
            "{municipality[i]}-{get_brazil_fu(state[i])}, ",
          (municipality[i] %in% c(NA, "", "NA")) &
            !(state[i] %in% c(NA, "", "NA")) ~
            "{get_brazil_fu(state[i])}, ",
          !(municipality[i] %in% c(NA, "", "NA")) ~ "{municipality[i]}, ",
          TRUE ~ ""
        ),
        ifelse(
          postal_code[i] %in% c(NA, "", "NA"),
          "",
          paste0(
            "{stringr::str_sub(postal_code[i], 1, 5)}",
            "-",
            "{stringr::str_sub(postal_code[i], 6, 8)}",
            ", "
          )
        ),
        "Brasil",
        .na = "",
        .null = ""
      ) |>
      as.character() %>%
      append(out, .)
  }

  out
}
