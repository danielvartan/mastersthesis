# library(dplyr)
# library(here)
# library(prettycheck) # github.com/danielvartan/prettycheck

source(here::here("R", "get_brazil_fu.R"))
source(here::here("R", "utils.R"))
source(here::here("R", "utils-checks.R"))

# Data from the Google Geocoding API gathered via the `tidygeocoder` R package.
# See `./R/get_brazil_municipality_geocode.R` to learn more.

get_brazil_state_latitude <- function(x, type = "fu") {
  prettycheck:::assert_character(x)
  prettycheck:::assert_choice(type, c("fu", "state"))

  # if (type == "fu") assert_brazil_fu(x)
  if (type == "state") x <- get_brazil_fu(x)

  x <- x |> to_ascii_and_lower()

  dplyr::case_when(
    x == "ac" ~ -9.9765362, # Rio Branco
    x == "al" ~ -9.6476843, # Maceió
    x == "ap" ~ 0.0401529, # Macapá
    x == "am" ~ -3.1316333, # Manaus
    x == "ba" ~ -12.9822499, # Salvador
    x == "ce" ~ -3.7304512, # Fortaleza
    x == "df" ~ -15.7934036, # Brasília
    x == "es" ~ -20.3200917, # Vitória
    x == "go" ~ -16.6808820, # Goiânia
    x == "ma" ~ -2.5295265, # São Luís
    x == "mt" ~ -15.5986686, # Cuiabá
    x == "ms" ~ -20.4640173, # Campo Grande
    x == "mg" ~ -19.9227318, # Belo Horizonte
    x == "pa" ~ -1.4505600, # Belém
    x == "pb" ~ -7.1215981, # João Pessoa
    x == "pr" ~ -25.4295963, # Curitiba
    x == "pe" ~ -8.0584933, # Recife
    x == "pi" ~ -5.0874608, # Teresina
    x == "rj" ~ -22.9110137, # Rio de Janeiro
    x == "rn" ~ -5.8053980, # Natal
    x == "rs" ~ -30.0324999, # Porto Alegre
    x == "ro" ~ -8.7494525, # Porto Velho
    x == "rr" ~ 2.8208478, # Boa Vista
    x == "sc" ~ -27.5973002, # Florianópolis
    x == "sp" ~ -23.5506507, # São Paulo
    x == "se" ~ -10.9162061, # Aracaju
    x == "to" ~ -10.1837852 # Palmas
  )
}
