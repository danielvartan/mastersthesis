# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck

fix_qualocep_data_values <- function(data) {
  checkmate::assert_tibble(data)

  data |>
    dplyr::mutate(
      municipality = dplyr::case_when(
        state == "Espírito Santo" & municipality == "Itaipava" ~ "Itapemirim",
        state == "Minas Gerais" & municipality == "Monte Verde" ~ "Camanducaia",
        TRUE ~ municipality
      )
    )
}
