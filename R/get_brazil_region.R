# library(dplyr)
# library(here)
# library(prettycheck) # github.com/danielvartan/prettycheck

source(here::here("R", "utils.R"))

# Based on data from the Brazilian Institute of Geography and Statistics (IBGE).
# See <https://sidra.ibge.gov.br/territorio> to learn more.
# Last update: 2024-11-07.

get_brazil_region <- function(x = NULL, type = "fu") {
  checkmate::assert_character(x, null.ok = TRUE)
  checkmate::assert_choice(type, c("fu", "state"))

  if (!is.null(x)) x <- x |> groomr::to_ascii() |> tolower()

  if (is.null(x)) {
    c("Central-West", "North", "Northeast", "South", "Southeast")
  } else if (type == "fu") {

    dplyr::case_when(
      x == "ac" ~ "North",
      x == "al" ~ "Northeast",
      x == "ap" ~ "North",
      x == "am" ~ "North",
      x == "ba" ~ "Northeast",
      x == "ce" ~ "Northeast",
      x == "df" ~ "Central-West",
      x == "es" ~ "Southeast",
      x == "go" ~ "Central-West",
      x == "ma" ~ "Northeast",
      x == "mt" ~ "Central-West",
      x == "ms" ~ "Central-West",
      x == "mg" ~ "Southeast",
      x == "pa" ~ "North",
      x == "pb" ~ "Northeast",
      x == "pr" ~ "South",
      x == "pe" ~ "Northeast",
      x == "pi" ~ "Northeast",
      x == "rj" ~ "Southeast",
      x == "rn" ~ "Northeast",
      x == "rs" ~ "South",
      x == "ro" ~ "North",
      x == "rr" ~ "North",
      x == "sc" ~ "South",
      x == "sp" ~ "Southeast",
      x == "se" ~ "Northeast",
      x == "to" ~ "North"
    )
  } else if (type == "state") {

    dplyr::case_when(
      x == "acre" ~ "North",
      x == "alagoas" ~ "Northeast",
      x == "amapa" ~ "North",
      x == "amazonas" ~ "North",
      x == "bahia" ~ "Northeast",
      x == "ceara" ~ "Northeast",
      x == "distrito federal" ~ "Central-West",
      x == "espirito santo" ~ "Southeast",
      x == "goias" ~ "Central-West",
      x == "maranhao" ~ "Northeast",
      x == "mato grosso" ~ "Central-West",
      x == "mato grosso do sul" ~ "Central-West",
      x == "minas gerais" ~ "Southeast",
      x == "para" ~ "North",
      x == "paraiba" ~ "Northeast",
      x == "parana" ~ "South",
      x == "pernambuco" ~ "Northeast",
      x == "piaui" ~ "Northeast",
      x == "rio de janeiro" ~ "Southeast",
      x == "rio grande do norte" ~ "Northeast",
      x == "rio grande do sul" ~ "South",
      x == "rondonia" ~ "North",
      x == "roraima" ~ "North",
      x == "santa catarina" ~ "South",
      x == "sao paulo" ~ "Southeast",
      x == "sergipe" ~ "Northeast",
      x == "tocantins" ~ "North"
    )
  }
}
