# library(dplyr)
# library(here)
# library(prettycheck) # github.com/danielvartan/prettycheck

# Based on data from the Brazilian Institute of Geography and Statistics (IBGE).
# See <https://sidra.ibge.gov.br/territorio> to learn more.
# Last update: 2024-11-07.

source(here::here("R", "utils.R"))
source(here::here("R", "utils-checks.R"))

get_brazil_state_capitals <- function(x = NULL, by = "fu") {
  prettycheck:::assert_character(x, null.ok = TRUE)
  prettycheck:::assert_choice(by, c("fu", "state"))

  if (!is.null(x)) x <- x |> to_ascii_and_lower()

  if (is.null(x)) {
    c(
      "Acre" = "Rio Branco",
      "Alagoas" = "Maceió",
      "Amapá" = "Macapá",
      "Amazonas" = "Manaus",
      "Bahia" = "Salvador",
      "Ceará" = "Fortaleza",
      "Distrito Federal" = "Brasília",
      "Espírito Santo" = "Vitória",
      "Goiás" = "Goiânia",
      "Maranhão" = "São Luís",
      "Mato Grosso" = "Cuiabá",
      "Mato Grosso do Sul" = "Campo Grande",
      "Minas Gerais" = "Belo Horizonte",
      "Pará" = "Belém",
      "Paraíba" = "João Pessoa",
      "Paraná" = "Curitiba",
      "Pernambuco" = "Recife",
      "Piauí" = "Teresina",
      "Rio de Janeiro" = "Rio de Janeiro",
      "Rio Grande do Norte" = "Natal",
      "Rio Grande do Sul" = "Porto Alegre",
      "Rondônia" = "Porto Velho",
      "Roraima" = "Boa Vista",
      "Santa Catarina" = "Florianópolis",
      "São Paulo" = "São Paulo",
      "Sergipe" = "Aracaju",
      "Tocantins" = "Palmas"
    )
  } else if (by == "fu") {
    # assert_brazil_uf(x)

    dplyr::case_when(
      x == "ac" ~ "Rio Branco",
      x == "al" ~ "Maceió",
      x == "ap" ~ "Macapá",
      x == "am" ~ "Manaus",
      x == "ba" ~ "Salvador",
      x == "ce" ~ "Fortaleza",
      x == "df" ~ "Brasília",
      x == "es" ~ "Vitória",
      x == "go" ~ "Goiânia",
      x == "ma" ~ "São Luís",
      x == "mt" ~ "Cuiabá",
      x == "ms" ~ "Campo Grande",
      x == "mg" ~ "Belo Horizonte",
      x == "pa" ~ "Belém",
      x == "pb" ~ "João Pessoa",
      x == "pr" ~ "Curitiba",
      x == "pe" ~ "Recife",
      x == "pi" ~ "Teresina",
      x == "rj" ~ "Rio de Janeiro",
      x == "rn" ~ "Natal",
      x == "rs" ~ "Porto Alegre",
      x == "ro" ~ "Porto Velho",
      x == "rr" ~ "Boa Vista",
      x == "sc" ~ "Florianópolis",
      x == "sp" ~ "São Paulo",
      x == "se" ~ "Aracaju",
      x == "to" ~ "Palmas"
    )
  } else if (by == "state") {
    # assert_brazil_state(x)

    dplyr::case_when(
      x == "acre" ~ "Rio Branco",
      x == "alagoas" ~ "Maceió",
      x == "amapá" ~ "Macapá",
      x == "amazonas" ~ "Manaus",
      x == "bahia" ~ "Salvador",
      x == "ceará" ~ "Fortaleza",
      x == "distrito federal" ~ "Brasília",
      x == "espírito santo" ~ "Vitória",
      x == "goiás" ~ "Goiânia",
      x == "maranhão" ~ "São Luís",
      x == "mato grosso" ~ "Cuiabá",
      x == "mato grosso do sul" ~ "Campo Grande",
      x == "minas gerais" ~ "Belo Horizonte",
      x == "pará" ~ "Belém",
      x == "paraíba" ~ "João Pessoa",
      x == "paraná" ~ "Curitiba",
      x == "pernambuco" ~ "Recife",
      x == "piauí" ~ "Teresina",
      x == "rio de janeiro" ~ "Rio de Janeiro",
      x == "rio grande do norte" ~ "Natal",
      x == "rio grande do sul" ~ "Porto Alegre",
      x == "rondônia" ~ "Porto Velho",
      x == "roraima" ~ "Boa Vista",
      x == "santa catarina" ~ "Florianópolis",
      x == "são paulo" ~ "São Paulo",
      x == "sergipe" ~ "Aracaju",
      x == "tocantins" ~ "Palmas"
    )
  }
}
