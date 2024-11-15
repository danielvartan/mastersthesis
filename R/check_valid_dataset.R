# library(cli)
# library(dplyr)
# library(magrittr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils
# library(targets)

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

  cli::cat_line(); cli::cat_line()
  cli::cli_h2("How many postal codes were valid?")

  total_valid_postal_codes <-
    data |>
    tidyr::drop_na(is_postal_code_valid) |>
    dplyr::pull(is_postal_code_valid) |>
    rutils:::drop_na() |>
    magrittr::equals(TRUE) |>
    sum()

  glue::glue(
    "{format_number(total_valid_postal_codes)} ",
    "({format_number(total_valid_postal_codes / total_have_postal_code, ",
    "per = TRUE)}%) ",
  ) |>
    cat()

  # !

  cli::cat_line(); cli::cat_line()
  cli::cli_h2("How many postal codes were invalid?")

  cli::cli_h3(
    paste0(
      "Considering only cases containing values in the `state` and ",
      "`municipality` variables"
    )
  )
  cli::cat_line()

  total_invalid_postal_codes_1 <-
    data |>
    tidyr::drop_na(is_postal_code_valid) |>
    dplyr::pull(is_postal_code_valid) |>
    rutils:::drop_na() |>
    magrittr::equals(FALSE) |>
    sum()

  glue::glue(
    "{format_number(total_invalid_postal_codes_1)} ",
    "({format_number(total_invalid_postal_codes_1 / total_have_postal_code, ",
    "per = TRUE)}%)",
  ) |>
    cat()

  cli::cat_line()
  cli::cli_h3(
    "Regardless of values in the `state` and `municipality` variables."
  )
  cli::cat_line()

  total_invalid_postal_codes_2 <-
    data |>
    dplyr::mutate(
      is_postal_code_valid = dplyr::if_else(
        !is.na(postal_code) & is.na(is_postal_code_valid),
        FALSE,
        is_postal_code_valid
      )
    ) |>
    tidyr::drop_na(is_postal_code_valid) |>
    dplyr::pull(is_postal_code_valid) |>
    rutils:::drop_na() |>
    magrittr::equals(FALSE) |>
    sum()

  glue::glue(
    "{format_number(total_invalid_postal_codes_2)} ",
    "({format_number(total_invalid_postal_codes_2 / total_have_postal_code, ",
    "per = TRUE)}%) ",
  ) |>
    cat()

  # !

  cli::cli_h2(
    paste0(
      "How many cases could not be coded (by ZIP code or municipality) ",
      "before filtering for analysis?"
    )
  )
  cli::cat_line()

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
