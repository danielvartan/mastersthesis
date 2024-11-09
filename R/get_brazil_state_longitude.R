# library(dplyr)
# library(here)
# library(prettycheck) # github.com/danielvartan/prettycheck

source(here::here("R", "get_brazil_fu.R"))
source(here::here("R", "utils.R"))
source(here::here("R", "utils-checks.R"))

# Data from the Google Geocoding API gathered via the `tidygeocoder` R package.
# See `./R/get_brazil_municipality_geocode.R` to learn more.

get_brazil_state_longitude <- function(x, type = "fu") {
  prettycheck:::assert_character(x)
  prettycheck:::assert_choice(type, c("fu", "state"))

  # if (type == "fu") assert_brazil_fu(x)
  if (type == "state") x <- get_brazil_fu(x)

  x <- x |> to_ascii_and_lower()

  dplyr::case_when(
    x == "ac" ~ -67.8220778, # Rio Branco
    x == "al" ~ -35.7339264, # Maceió
    x == "ap" ~ -51.0569588, # Macapá
    x == "am" ~ -59.9825041, # Manaus
    x == "ba" ~ -38.4812772, # Salvador
    x == "ce" ~ -38.5217989, # Fortaleza
    x == "df" ~ -47.8823172, # Brasília
    x == "es" ~ -40.3376682, # Vitória
    x == "go" ~ -49.2532691, # Goiânia
    x == "ma" ~ -44.2963942, # São Luís
    x == "mt" ~ -56.0991301, # Cuiabá
    x == "ms" ~ -54.6162947, # Campo Grande
    x == "mg" ~ -43.9450948, # Belo Horizonte
    x == "pa" ~ -48.4682453, # Belém
    x == "pb" ~ -34.8820280, # João Pessoa
    x == "pr" ~ -49.2712724, # Curitiba
    x == "pe" ~ -34.8848193, # Recife
    x == "pi" ~ -42.8049571, # Teresina
    x == "rj" ~ -43.2093727, # Rio de Janeiro
    x == "rn" ~ -35.2080905, # Natal
    x == "rs" ~ -51.2303767, # Porto Alegre
    x == "ro" ~ -63.8735438, # Porto Velho
    x == "rr" ~ -60.6719582, # Boa Vista
    x == "sc" ~ -48.5496098, # Florianópolis
    x == "sp" ~ -46.6333824, # São Paulo
    x == "se" ~ -37.0774655, # Aracaju
    x == "to" ~ -48.3336423  # Palmas
  )}
