# library(cli)
# library(dplyr)
# library(magrittr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils
# library(targets)

# # Helpers
#
# geocoded_data <- targets::tar_read("geocoded_data")
# geocoded_data |> check_valid_dataset()
#
# weighted_data <- targets::tar_read("weighted_data")
# weighted_data |> check_valid_dataset()

check_valid_dataset <- function(data) {
  prettycheck:::assert_tibble(data)

  # !

  cli::cli_h2("What is the total of the valid sample?")

  total_cases <- data |> nrow()

  total_cases |> format_number() |> cat()

  # !

  cli::cat_line(); cli::cat_line()
  cli::cli_h2("How many Brazilian respondents are in the dataset?")

  total_brazilians <-
    data |>
    dplyr::filter(country == "Brazil") |>
    nrow()

  glue::glue(
    "{format_number(total_brazilians)} ",
    "({format_number(total_brazilians / total_cases, per = TRUE)}%) ",
  ) |>
    cat()

  # !

  cli::cat_line(); cli::cat_line()
  cli::cli_h2("How many foreign respondents are in the dataset?")

  total_foreign <-
    data |>
    dplyr::filter(country != "Brazil") |>
    nrow()

  glue::glue(
    "{format_number(total_foreign)} ",
    "({format_number(total_foreign / total_cases, per = TRUE)}%) ",
  ) |>
    cat()

  # !

  cli::cat_line(); cli::cat_line()
  cli::cli_h2("How many respondents have postal code values?")

  total_have_postal_code <-
    data |>
    dplyr::filter(!is.na(postal_code)) |>
    nrow()

  glue::glue(
    "{format_number(total_have_postal_code)} ",
    "({format_number(total_have_postal_code / total_cases, per = TRUE)}%) ",
  ) |>
    cat()

  # !

  cli::cat_line(); cli::cat_line()
  cli::cli_h2("How many respondents do not have postal code values?")

  total_do_not_have_postal_code <-
    data |>
    dplyr::filter(is.na(postal_code)) |>
    nrow()

  glue::glue(
    "{format_number(total_do_not_have_postal_code)} ",
    "({format_number(total_do_not_have_postal_code / total_cases, ",
    "per = TRUE)}%) ",
  ) |>
    cat()

  # !

  if ("is_postal_code_valid" %in% names(data)) {
    cli::cat_line(); cli::cat_line()
    cli::cli_h2("How many postal codes were valid? (Only Brazilian PC)")

    total_valid_postal_codes <-
      data |>
      tidyr::drop_na(is_postal_code_valid) |>
      dplyr::pull(is_postal_code_valid) |>
      magrittr::equals(TRUE) |>
      sum()

    glue::glue(
      "{format_number(total_valid_postal_codes)} ",
      "({format_number(total_valid_postal_codes / total_have_postal_code, ",
      "per = TRUE)}%) ",
    ) |>
      cat()
  }

  # !

  if ("is_postal_code_valid" %in% names(data)) {
    cli::cat_line(); cli::cat_line()
    cli::cli_h2(
      paste0(
      "How many postal codes were invalid? (Only Brazilian PC) ",
      "(These PC were removed from the dataset)."
      )
    )

    total_invalid_postal_codes_1 <-
      data |>
      tidyr::drop_na(is_postal_code_valid) |>
      dplyr::pull(is_postal_code_valid) |>
      magrittr::equals(FALSE) |>
      sum()

    glue::glue(
      "{format_number(total_invalid_postal_codes_1)} ",
      "({format_number(total_invalid_postal_codes_1 / total_have_postal_code, ",
      "per = TRUE)}%)",
    ) |>
      cat()
  }

  # !

  cli::cat_line(); cli::cat_line()
  cli::cli_h2(
    paste0(
      "How many cases are geocoded? (only Brazilian cases were geocoded)"
    )
  )

  total_geocoded_cases <-
    data |>
    tidyr::drop_na(latitude) |>
    dplyr::pull(latitude) |>
    length()

  glue::glue(
    "{format_number(total_geocoded_cases)} ",
    "({format_number(total_geocoded_cases / total_brazilians, ",
    "per = TRUE)}%)",
  ) |>
    cat()

  # !

  cli::cat_line(); cli::cat_line()
  cli::cli_h2(
    paste0(
      "How many cases could not be coded because of missing data (state and ",
      "municipality) (only Brazilian cases were geocoded)?"
    )
  )

  total_missing_state_municipality_cases <-
    data |>
    dplyr::filter(
      country == "Brazil",
      is.na(state) | is.na(municipality)
    ) |>
    nrow()

  glue::glue(
    "{format_number(total_missing_state_municipality_cases)} ",
    "({format_number(total_missing_state_municipality_cases / ",
    "total_brazilians, ",
    "per = TRUE)}%)",
  ) |>
    cat()

  invisible()
}

format_number <- function(x, per = FALSE) {
  prettycheck:::assert_number(x)

  if (isTRUE(per)) {
    x <- x * 100
  }

  x |>
  round(digits = 3) |>
    format(
      big.mark = ",",
      decimal.mark = "."
    )
}
