# library(lubridate)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils

# TODO: Move to `lubritime`.
test_timeline_link <- function(x, tz = "UTC") {
  prettycheck:::assert_multi_class(x, c("numeric", "POSIXt"))
  prettycheck:::assert_choice(tz, OlsonNames())

  x <- x |> rutils:::drop_na()

  if (is.numeric(x)) x <- x |> lubridate::as_datetime(tz = tz)

  dates <-
    x |>
    lubridate::date() |>
    unique()

  if (((lubridate::as_date("1970-01-01") %in% dates) &&
       length(dates) == 2) ||
      ((lubridate::as_date("1970-01-02") %in% dates) &&
       length(dates) == 1)) {
    TRUE
  } else {
    FALSE
  }
}

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
      x |>
      to_ascii_and_lower() |>
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
      x |>
      to_ascii_and_lower() |>
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
      x |>
      to_ascii_and_lower() |>
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

assert_gg_label <- function(x) {
  class_options <- c("character", "latexexpression")

  prettycheck:::assert_length(x, len = 1, null_ok = TRUE)
  prettycheck:::assert_multi_class(x, class_options, null.ok = TRUE)
}

# library(cli)
# library(grDevices)
# library(prettycheck) # github.com/danielvartan/prettycheck

assert_color_options <- function(
    color_low = NULL, color_high = NULL, viridis = NULL
) {
  viridis_choices <- c(
    "magma", "A", "inferno", "B", "plasma", "C", "viridis", "D",
    "cividis", "E", "rocket", "F", "mako", "G", "turbo", "H"
  )

  prettycheck:::assert_string(color_low, null.ok = TRUE)
  prettycheck:::assert_string(color_high, null.ok = TRUE)
  prettycheck:::assert_choice(viridis, viridis_choices, null.ok = TRUE)

  color_pattern <- "(?i)^#[a-f0-9]{3}$|^#[a-f0-9]{6}$|^transparent$"
  name_color_low <- deparse(substitute(color_low))
  name_color_high <- deparse(substitute(color_high))

  for (i in c(color_low, color_high)) {
    name <- ifelse(i == color_low, name_color_low, name_color_high)

    if (!is.null(i) &&
        !i %in% grDevices::colors() &&
        !prettycheck:::test_string(i, pattern = color_pattern)) {
      cli::cli_abort(
        paste0(
          "{.strong {cli::col_red(name)}} is not a valid color code. ",
          "It must contain a hexadecimal color code or one of the ",
          "values in {.strong {cli::col_blue('grDevices::color()')}}."
        )
      )
    }
  }

  if (is.null(color_low) && !is.null(color_high) ||
      !is.null(color_low) && is.null(color_high)) {
    cli::cli_abort(
      paste0(
        "You must provide both ",
        "{.strong {cli::col_blue('color_low')}} and ",
        "{.strong {cli::col_red('color_high')}} ",
        "arguments at the same time."
      )
    )
  } else if ((!is.null(color_low) | !is.null(color_high)) &&
             !is.null(viridis)) {
    cli::cli_abort(
      paste0(
        "You can't use both ",
        "{.strong {cli::col_blue('color_low/color_high')}} and ",
        "{.strong {cli::col_red('viridis')}} ",
        "arguments at the same time."
      )
    )
  } else {
    invisible(NULL)
  }
}
