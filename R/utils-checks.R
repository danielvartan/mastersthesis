# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils

source(here::here("R", "utils.R"))

assert_brazil_region <- function(
    x,
    insensitive = TRUE,
    any_missing = TRUE
  ) {
  prettycheck:::assert_character(x)
  prettycheck:::assert_logical(insensitive)
  prettycheck:::assert_logical(any_missing)

  if (any(is.na(x)) && isFALSE(any_missing)) {
    name <- deparse(substitute(x))

    cli::cli_abort(
      "{.strong {cli::col_red(name)}} cannot have missing values."
    )
  }

  x <- x |> rutils:::drop_na()

  if (length(x) == 0) {
    as.character(NA)
  } else if (isTRUE(insensitive)) {
    region_options_insensitive <- c(
      "central-west", "north", "northeast", "south", "southeast"
    )

    x <-
      to_ascii(x) |>
      tolower() |>
      prettycheck:::assert_subset(region_options_insensitive)
  } else {
    region_options_sensitive <- c(
      "Central-West", "North", "Northeast", "South", "Southeast"
    )

    x |> prettycheck:::assert_subset(region_options_sensitive)
  }
}

# library(prettycheck) # github.com/danielvartan/prettycheck

source(here::here("R", "utils.R"))

assert_brazil_fu <- function(
    x,
    insensitive = TRUE,
    any_missing = TRUE
  ) {
  prettycheck:::assert_character(x)
  prettycheck:::assert_logical(insensitive)
  prettycheck:::assert_logical(any_missing)

  if (any(is.na(x)) && isFALSE(any_missing)) {
    name <- deparse(substitute(x))

    cli::cli_abort(
      "{.strong {cli::col_red(name)}} cannot have missing values."
    )
  }

  x <- x |> rutils:::drop_na()

  if (length(x) == 0) {
    as.character(NA)
  } else if (isTRUE(insensitive)) {
    fu_options_insensitive <- c(
      "ac", "al", "ap", "am", "ba", "ce", "df", "es", "go", "ma", "mt",
      "ms", "mg", "pr", "pb", "pa", "pe", "pi", "rj", "rn", "rs", "ro",
      "rr", "sc", "se", "sp", "to"
    )

    x <-
      to_ascii(x) |>
      tolower() |>
      prettycheck:::assert_subset(fu_options_insensitive)
  } else {
    fu_options_sensitive <- c(
      "AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT",
      "MS", "MG", "PR", "PB", "PA", "PE", "PI", "RJ", "RN", "RS", "RO",
      "RR", "SC", "SE", "SP", "TO"
    )

    x |> prettycheck:::assert_subset(fu_options_sensitive)
  }
}

# library(prettycheck) # github.com/danielvartan/prettycheck

source(here::here("R", "utils.R"))

assert_brazil_state <- function(
    x,
    insensitive = TRUE,
    any_missing = TRUE
  ) {
  prettycheck:::assert_character(x)
  prettycheck:::assert_logical(insensitive)
  prettycheck:::assert_logical(any_missing)

  if (any(is.na(x)) && isFALSE(any_missing)) {
    name <- deparse(substitute(x))

    cli::cli_abort(
      "{.strong {cli::col_red(name)}} cannot have missing values."
    )
  }

  x <- x |> rutils:::drop_na()

  if (length(x) == 0) {
    as.character(NA)
  } else if (isTRUE(insensitive)) {
    state_options_insensitive <- c(
      "acre", "alagoas", "amapa", "amazonas", "bahia", "ceara",
      "distrito federal", "espirito santo", "goias", "maranhao",
      "mato grosso", "mato grosso do sul", "minas gerais", "parana",
      "paraiba", "para", "pernambuco", "piaui", "rio de janeiro",
      "rio grande do norte", "rio grande do sul", "rondonia", "roraima",
      "santa catarina", "sergipe", "sao paulo", "tocantins"
    )

    x <-
      to_ascii(x) |>
      tolower() |>
      prettycheck:::assert_subset(state_options_insensitive)
  } else {
    state_options_sensitive <- c(
      "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia", "Ceará",
      "Distrito Federal", "Espírito Santo", "Goiás", "Maranhão",
      "Mato Grosso", "Mato Grosso do Sul", "Minas Gerais", "Paraná",
      "Paraíba", "Pará", "Pernambuco", "Piauí", "Rio de Janeiro",
      "Rio Grande do Norte", "Rio Grande do Sul", "Rondônia", "Roraima",
      "Santa Catarina", "Sergipe", "São Paulo", "Tocantins"
    )

    x |> prettycheck:::assert_subset(state_options_sensitive)
  }
}
