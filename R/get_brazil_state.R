# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck

# Based on data from the Brazilian Institute of Geography and Statistics (IBGE).
# See <https://sidra.ibge.gov.br/territorio> to learn more.
# Last update: 2024-11-07.

source(here::here("R", "utils.R"))
source(here::here("R", "utils-checks.R"))

get_brazil_state <- function(x = NULL, by = "fu") {
  prettycheck:::assert_character(x, null.ok = TRUE)
  prettycheck:::assert_choice(by, c("fu", "region"))

  if (!is.null(x)) x <- x |> to_ascii() |> tolower()

  if (is.null(x)) {
    c(
      "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia", "Ceará",
      "Distrito Federal", "Espírito Santo", "Goiás", "Maranhão",
      "Mato Grosso", "Mato Grosso do Sul", "Minas Gerais", "Paraná",
      "Paraíba", "Pará", "Pernambuco", "Piauí", "Rio de Janeiro",
      "Rio Grande do Norte", "Rio Grande do Sul", "Rondônia", "Roraima",
      "Santa Catarina", "Sergipe", "São Paulo", "Tocantins"
    )
  } else if (by == "region") {
    # assert_brazil_region(x)

    out <- character()

    for (i in x) {
      out <- c(
        out,
        switch(
          i,
          "central-eest" = c(
            "Distrito Federal", "Goiás", "Mato Grosso", "Mato Grosso do Sul"
          ),
          "north" = c(
            "Acre", "Amapá", "Amazonas", "Pará", "Rondônia", "Roraima",
            "Tocantins"
          ),
          "northeast" = c(
            "Alagoas", "Bahia", "Ceará", "Maranhão", "Paraíba", "Pernambuco",
            "Piauí", "Rio Grande do Norte", "Sergipe"
          ),
          "south" = c("Paraná", "Rio Grande do Sul", "Santa Catarina"),
          "Southeast" = c(
            "Espírito Santo", "Minas Gerais", "Rio de Janeiro", "São Paulo"
          )
        )
      )
    }

    if (length(out) == 0) as.character(NA) else out
  } else if (by == "fu") {
    # assert_brazil_fu(x)

    dplyr::case_when(
      x == "ac" ~ "Acre",
      x == "al" ~ "Alagoas",
      x == "ap" ~ "Amapá",
      x == "am" ~ "Amazonas",
      x == "ba" ~ "Bahia",
      x == "ce" ~ "Ceará",
      x == "df" ~ "Distrito Federal",
      x == "es" ~ "Espírito Santo",
      x == "go" ~ "Goiás",
      x == "ma" ~ "Maranhão",
      x == "mt" ~ "Mato Grosso",
      x == "ms" ~ "Mato Grosso do Sul",
      x == "mg" ~ "Minas Gerais",
      x == "pr" ~ "Paraná",
      x == "pb" ~ "Paraíba",
      x == "pa" ~ "Pará",
      x == "pe" ~ "Pernambuco",
      x == "pi" ~ "Piauí",
      x == "rj" ~ "Rio de Janeiro",
      x == "rn" ~ "Rio Grande do Norte",
      x == "rs" ~ "Rio Grande do Sul",
      x == "ro" ~ "Rondônia",
      x == "rr" ~ "Roraima",
      x == "sc" ~ "Santa Catarina",
      x == "se" ~ "Sergipe",
      x == "sp" ~ "São Paulo",
      x == "to" ~ "Tocantins"
    )
  }
}
