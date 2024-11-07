# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck

source(here::here("R", "get_brazil_fu.R"))
source(here::here("R", "utils.R"))
source(here::here("R", "utils-checks.R"))

get_brazil_state_latitude <- function(x, type = "fu") {
  prettycheck:::assert_character(x)
  prettycheck:::assert_choice(type, c("fu", "state"))

  if (type == "fu") assert_brazil_fu(x)
  if (type == "state") x <- get_brazil_fu(x)

  x <- x |> to_ascii() |> tolower()

  dplyr::case_when(
    x == "ac" ~ -9.9862943, # Rio Branco
    x == "al" ~ -9.5945358, # Maceió
    x == "ap" ~ 0.1017719, # Macapá
    x == "am" ~ -3.0446589, # Manaus
    x == "ba" ~ -12.8753927, # Salvador
    x == "ce" ~ -3.7933105, # Fortaleza
    x == "df" ~ -15.7217003, # Brasília
    x == "es" ~ -20.2821867, # Vitória
    x == "go" ~ -16.6958107, # Goiânia
    x == "ma" ~ -2.5606296, # São Luís
    x == "mt" ~ -15.4196336, # Cuiabá
    x == "ms" ~ -20.4810804, # Campo Grande
    x == "mg" ~ -19.9026404, # Belo Horizonte
    x == "pr" ~ -25.4950245, # Curitiba
    x == "pb" ~ -7.1466015, # João Pessoa
    x == "pa" ~ -1.3413464, # Belém
    x == "pe" ~ -8.043303, # Recife
    x == "pi" ~ -5.0937344, # Teresina
    x == "rj" ~ -22.9137906, # Rio de Janeiro
    x == "rn" ~ -5.799913, # Natal
    x == "rs" ~ -30.1087672, # Porto Alegre
    x == "ro" ~ -8.7565367, # Porto Velho
    x == "rr" ~ 2.8071961, # Boa Vista
    x == "sc" ~ -27.5712063, # Florianópolis
    x == "se" ~ -11.0059634, # Aracaju
    x == "sp" ~ -23.6820636, # São Paulo
    x == "to" ~ -10.2600493 # Palmas
  )
}
