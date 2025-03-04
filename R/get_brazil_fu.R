# library(dplyr)
# library(here)
# library(prettycheck) # github.com/danielvartan/prettycheck

# Based on data from the Brazilian Institute of Geography and Statistics (IBGE).
# See <https://sidra.ibge.gov.br/territorio> to learn more.
# Last update: 2024-11-07.

source(here::here("R", "utils.R"))
source(here::here("R", "utils-checks.R"))

get_brazil_fu <- function(x = NULL, by = "state") {
  checkmate::assert_character(x, null.ok = TRUE)
  checkmate::assert_choice(by, c("state", "region"))

  if (!is.null(x)) x <- x |> to_ascii_and_lower()

  if (is.null(x)) {
    c(
      "AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT",
      "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO",
      "RR", "SC", "SE", "SP", "TO"
    )
  } else if (by == "region") {
    # assert_brazil_region(x)

    out <- character()

    for (i in x) {
      out <- c(
        out,
        switch(
          i,
          "central-west" = c("DF", "GO", "MT", "MS"),
          "north" = c("AC", "AP", "AM", "PA", "RO", "RR", "TO"),
          "northeast" = c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE"),
          "south" = c("PR", "RS", "SC"),
          "southeast" = c("ES", "MG", "RJ", "SP")
        )
      )
    }

    if (length(out) == 0) as.character(NA) else out
  } else if (by == "state") {
    # assert_brazil_state(x)

    dplyr::case_when(
      x == "acre" ~ "AC",
      x == "alagoas" ~ "AL",
      x == "amapa" ~ "AP",
      x == "amazonas" ~ "AM",
      x == "bahia" ~ "BA",
      x == "ceara" ~ "CE",
      x == "distrito federal" ~ "DF",
      x == "espirito santo" ~ "ES",
      x == "goias" ~ "GO",
      x == "maranhao" ~ "MA",
      x == "mato grosso" ~ "MT",
      x == "mato grosso do sul" ~ "MS",
      x == "minas gerais" ~ "MG",
      x == "para" ~ "PA",
      x == "paraiba" ~ "PB",
      x == "parana" ~ "PR",
      x == "pernambuco" ~ "PE",
      x == "piaui" ~ "PI",
      x == "rio de janeiro" ~ "RJ",
      x == "rio grande do norte" ~ "RN",
      x == "rio grande do sul" ~ "RS",
      x == "rondonia" ~ "RO",
      x == "roraima" ~ "RR",
      x == "santa catarina" ~ "SC",
      x == "sao paulo" ~ "SP",
      x == "sergipe" ~ "SE",
      x == "tocantins" ~ "TO"
    )
  }
}
