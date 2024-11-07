# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck

source(here::here("R", "utils.R"))
source(here::here("R", "utils-checks.R"))

# Based on data from the Brazilian Institute of Geography and Statistics (IBGE).
# See <https://sidra.ibge.gov.br/territorio> to learn more.
# Last update: 2024-11-07.

get_brazil_region <- function(x = NULL, type = "fu") {
  prettycheck:::assert_character(x, null.ok = TRUE)
  prettycheck:::assert_choice(type, c("fu", "state"))

  if (!is.null(x)) x <- x |> to_ascii() |> tolower()

  if (is.null(x)) {
    c("Central-West", "North", "Northeast", "South", "Southeast")
  } else if (type == "fu") {
    # assert_brazil_fu(x)

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
      x == "pr" ~ "South",
      x == "pb" ~ "Northeast",
      x == "pa" ~ "North",
      x == "pe" ~ "Northeast",
      x == "pi" ~ "Northeast",
      x == "rj" ~ "Southeast",
      x == "rn" ~ "Northeast",
      x == "rs" ~ "South",
      x == "ro" ~ "North",
      x == "rr" ~ "North",
      x == "sc" ~ "South",
      x == "se" ~ "Northeast",
      x == "sp" ~ "Southeast",
      x == "to" ~ "North"
    )
  } else if (type == "state") {
    # assert_brazil_region(x)

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
      x == "parana" ~ "South",
      x == "paraiba" ~ "Northeast",
      x == "para" ~ "North",
      x == "pernambuco" ~ "Northeast",
      x == "piaui" ~ "Northeast",
      x == "rio de janeiro" ~ "Southeast",
      x == "rio grande do norte" ~ "Northeast",
      x == "rio grande do sul" ~ "South",
      x == "rondonia" ~ "North",
      x == "roraima" ~ "North",
      x == "santa catarina" ~ "South",
      x == "sergipe" ~ "Northeast",
      x == "sao paulo" ~ "Southeast",
      x == "tocantins" ~ "North"
    )
  }
}
