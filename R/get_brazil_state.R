# library(dplyr)
# library(here)
# library(prettycheck) # github.com/danielvartan/prettycheck

# Based on data from the Brazilian Institute of Geography and Statistics (IBGE).
# See <https://sidra.ibge.gov.br/territorio> to learn more.
# Last update: 2024-11-07.

source(here::here("R", "utils.R"))
source(here::here("R", "utils-checks.R"))

get_brazil_state <- function(x = NULL, by = "fu") {
  checkmate::assert_character(x, null.ok = TRUE)
  checkmate::assert_choice(by, c("fu", "region"))

  if (!is.null(x)) x <- x |> to_ascii_and_lower()

  if (is.null(x)) {
    c(
      "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia", "Ceará",
      "Distrito Federal", "Espírito Santo", "Goiás", "Maranhão",
      "Mato Grosso", "Mato Grosso do Sul", "Minas Gerais", "Pará",
      "Paraíba", "Paraná", "Pernambuco", "Piauí", "Rio de Janeiro",
      "Rio Grande do Norte", "Rio Grande do Sul", "Rondônia", "Roraima",
      "Santa Catarina", "São Paulo", "Sergipe", "Tocantins"
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
      x == "pa" ~ "Pará",
      x == "pb" ~ "Paraíba",
      x == "pr" ~ "Paraná",
      x == "pe" ~ "Pernambuco",
      x == "pi" ~ "Piauí",
      x == "rj" ~ "Rio de Janeiro",
      x == "rn" ~ "Rio Grande do Norte",
      x == "rs" ~ "Rio Grande do Sul",
      x == "ro" ~ "Rondônia",
      x == "rr" ~ "Roraima",
      x == "sc" ~ "Santa Catarina",
      x == "sp" ~ "São Paulo",
      x == "se" ~ "Sergipe",
      x == "to" ~ "Tocantins"
    )
  }
}

# library(dplyr)
# library(here)
# library(prettycheck) # github.com/danielvartan/prettycheck

# Based on data from the Brazilian Institute of Geography and Statistics (IBGE).
# See <https://sidra.ibge.gov.br/territorio> to learn more.
# Last update: 2024-11-07.

source(here::here("R", "utils.R"))
source(here::here("R", "utils-checks.R"))

get_brazil_state_capitals <- function(x = NULL, by = "fu") {
  checkmate::assert_character(x, null.ok = TRUE)
  checkmate::assert_choice(by, c("fu", "state"))

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

# library(dplyr)
# library(here)
# library(prettycheck) # github.com/danielvartan/prettycheck

source(here::here("R", "get_brazil_fu.R"))
source(here::here("R", "utils.R"))
source(here::here("R", "utils-checks.R"))

# Data from the Google Geocoding API gathered via the `tidygeocoder` R package.
# See `./R/get_brazil_municipality_geocode.R` to learn more.

get_brazil_state_latitude <- function(x, type = "fu") {
  checkmate::assert_character(x)
  checkmate::assert_choice(type, c("fu", "state"))

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

# library(dplyr)
# library(here)
# library(prettycheck) # github.com/danielvartan/prettycheck

source(here::here("R", "get_brazil_fu.R"))
source(here::here("R", "utils.R"))
source(here::here("R", "utils-checks.R"))

# Data from the Google Geocoding API gathered via the `tidygeocoder` R package.
# See `./R/get_brazil_municipality_geocode.R` to learn more.

get_brazil_state_longitude <- function(x, type = "fu") {
  checkmate::assert_character(x)
  checkmate::assert_choice(type, c("fu", "state"))

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
  )
}

# library(here)
# library(prettycheck) # github.com/danielvartan/prettycheck

# Based on the 2024b dataset (Released 2024-09-04) from the
# Internet Assigned Numbers Authority (IANA).
# See <https://www.iana.org/time-zones> to learn more.

get_brazil_state_by_utc <- function(utc = -3, type = "fu") {
  checkmate::assert_choice(utc, -5:-2)
  checkmate::assert_choice(type, c("fu", "state"))

  if (utc == -2) {
    # PE -> Except Atlantic islands -> Fernando de Noronha
    out <- "PE"

    if (type == "fu") out else get_brazil_state(out)
  } else if (utc == -3) {
    out <- c(
      "AL", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "PA", "PB", "PR",
      "PE", "PI", "RJ", "RN", "RS", "SC", "SP", "SE", "TO"
    )

    if (type == "fu") out else get_brazil_state(out)
  } else if (utc == -4) {
    # AM -> East (Except far west)
    out <- c("AM", "MT", "MS", "RO", "RR")

    if (type == "fu") out else get_brazil_state(out)
  } else if (utc == -5) {
    # AM -> West (Far west)
    out <- c("AC", "AM")

    if (type == "fu") out else get_brazil_state(out)
  }
}

# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(vctrs)

# # Helpers
#
# data <- targets::tar_read("tidy_data")

get_brazil_state_from_unique_municipality <- function(data) {
  var_set <- c("id", "country", "state", "municipality", "postal_code")

  checkmate::assert_tibble(data)
  checkmate::assert_subset(var_set, names(data))

  data |>
    dplyr::select(dplyr::all_of(var_set)) |>
    dplyr::filter(is.na(state), !is.na(municipality)) |>
    dplyr::left_join(
      get_brazil_municipality() |>
        dplyr::select(municipality, state) |>
        dplyr::filter(!vctrs::vec_duplicate_detect(municipality)),
      by = "municipality"
    )
}

# library(dplyr)
# library(here)
# library(methods)
# library(prettycheck) # github.com/danielvartan/prettycheck

source(here::here("R", "get_brazil_fu.R"))
source(here::here("R", "utils.R"))
source(here::here("R", "utils-checks.R"))

# Data from the Google Geocoding API gathered via the `tidygeocoder` R package.
# See `./R/get_brazil_municipality_geocode.R` to learn more.

get_brazil_state_code <- function(x = NULL, type = "fu") {
  checkmate::assert_character(x, null.ok = TRUE)
  checkmate::assert_choice(type, c("fu", "state"))

  if (is.null(x)) {
    c(
      "Acre" = 12,
      "Alagoas" = 27,
      "Amapá" = 16,
      "Amazonas" = 13,
      "Bahia" = 29,
      "Ceará" = 23,
      "Distrito Federal" = 53,
      "Espírito Santo" = 32,
      "Goiás" = 52,
      "Maranhão" = 21,
      "Mato Grosso" = 51,
      "Mato Grosso do Sul" = 50,
      "Minas Gerais" = 31,
      "Pará" = 15,
      "Paraíba" = 25,
      "Paraná" = 41,
      "Pernambuco" = 26,
      "Piauí" = 22,
      "Rio de Janeiro" = 33,
      "Rio Grande do Norte" = 24,
      "Rio Grande do Sul" = 43,
      "Rondônia" = 11,
      "Roraima" = 14,
      "Santa Catarina" = 42,
      "São Paulo" = 35,
      "Sergipe" = 28,
      "Tocantins" = 17
    ) |>
      methods::as("Integer")
  } else {
    if (type == "state") x <- get_brazil_fu(x)

    x <- x |> to_ascii_and_lower()

    dplyr::case_when(
      x == "ac" ~ 12,
      x == "al" ~ 27,
      x == "ap" ~ 16,
      x == "am" ~ 13,
      x == "ba" ~ 29,
      x == "ce" ~ 23,
      x == "df" ~ 53,
      x == "es" ~ 32,
      x == "go" ~ 52,
      x == "ma" ~ 21,
      x == "mt" ~ 51,
      x == "ms" ~ 50,
      x == "mg" ~ 31,
      x == "pa" ~ 15,
      x == "pb" ~ 25,
      x == "pr" ~ 41,
      x == "pe" ~ 26,
      x == "pi" ~ 22,
      x == "rj" ~ 33,
      x == "rn" ~ 24,
      x == "rs" ~ 43,
      x == "ro" ~ 11,
      x == "rr" ~ 14,
      x == "sc" ~ 42,
      x == "sp" ~ 35,
      x == "se" ~ 28,
      x == "to" ~ 17
    ) |>
      as.integer()
  }
}
