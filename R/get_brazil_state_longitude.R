# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck

source(here::here("R", "get_brazil_fu.R"))
source(here::here("R", "utils.R"))
source(here::here("R", "utils-checks.R"))

get_brazil_state_longitude <- function(x, type = "fu") {
  prettycheck:::assert_character(x)
  prettycheck:::assert_choice(type, c("fu", "state"))

  if (type == "fu") assert_brazil_fu(x)
  if (type == "state") x <- get_brazil_fu(x)

  x <- x |> to_ascii() |> tolower()

  dplyr::case_when(
    x == "ac" ~ -67.9869962, # Rio Branco
    x == "al" ~ -35.8514919, # Maceió
    x == "ap" ~ -51.2616744, # Macapá
    x == "am" ~ -60.049506, # Manaus
    x == "ba" ~ -38.6666115, # Salvador
    x == "ce" ~ -38.6020174, # Fortaleza
    x == "df" ~ -48.1021708, # Brasília
    x == "es" ~ -40.3269035, # Vitória
    x == "go" ~ -49.4690802, # Goiânia
    x == "ma" ~ -44.3405241, # São Luís
    x == "mt" ~ -56.2240693, # Cuiabá
    x == "ms" ~ -54.7179359, # Campo Grande
    x == "mg" ~ -44.1288626, # Belo Horizonte
    x == "pr" ~ -49.4546099, # Curitiba
    x == "pb" ~ -34.9639996, # João Pessoa
    x == "pa" ~ -48.6116775, # Belém
    x == "pe" ~ -35.0166191, # Recife
    x == "pi" ~ -42.8234802, # Teresina
    x == "rj" ~ -43.7756411, # Rio de Janeiro
    x == "rn" ~ -35.3046462, # Natal
    x == "rs" ~ -51.3419529, # Porto Alegre
    x == "ro" ~ -63.9373089, # Porto Velho
    x == "rr" ~ -60.7789636, # Boa Vista
    x == "sc" ~ -48.7999353, # Florianópolis
    x == "se" ~ -37.2679695, # Aracaju
    x == "sp" ~ -46.9249578, # São Paulo
    x == "to" ~ -48.4296364 # Palmas
  )
}
