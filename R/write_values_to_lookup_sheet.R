# library(dplyr)
# library(googlesheets4)
# library(prettycheck) # github.com/danielvartan/prettycheck

write_values_to_lookup_sheet <- function(
    data,
    sheet,
    ss = "1GJg7qVSb5srRe4wsFBBH5jIAMF7ZvCMyaB3hbFuhCDA"
  ) {
  prettycheck::assert_internet()
  prettycheck::assert_interactive()
  checkmate::assert_tibble(data)
  checkmate::assert_string(sheet)
  checkmate::assert_string(ss)

  ss <- googlesheets4::gs4_get(ss)
  checkmate::assert_subset(sheet, ss$sheets$name)

  ss |>
    googlesheets4::sheet_resize(
      sheet = sheet, nrow = 2, ncol = NULL, exact = TRUE
    )

  ss |>
    googlesheets4::range_write(
      data = data,
      sheet = sheet,
      range = "A1",
      col_names = TRUE,
      reformat = FALSE
    )

  invisible()
}

# library(dplyr)
# library(googlesheets4)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils

# # Helpers
#
# source(here::here("R", "tidy_data_.R"))
# raw_data <- targets::tar_read("raw_data")
# data <- raw_data |> fix_col_names()
# data |> write_unique_values_to_lookup_sheet(col = "name")

write_unique_values_to_lookup_sheet <- function(
    data,
    col,
    sheet = col,
    ss = "1GJg7qVSb5srRe4wsFBBH5jIAMF7ZvCMyaB3hbFuhCDA"
) {
  checkmate::assert_tibble(data)
  checkmate::assert_string(col)
  checkmate::assert_choice(col, names(data))
  checkmate::assert_string(sheet)
  checkmate::assert_string(ss)
  prettycheck::assert_interactive()
  prettycheck::assert_internet()

  ss <- googlesheets4::gs4_get(ss)
  checkmate::assert_subset(sheet, ss$sheets$name)

  out <-
    dplyr::tibble(
      key = rutils::drop_na(unique(data[[col]])),
      value = NA
    ) |>
    dplyr::arrange(key)

  ss |>
    googlesheets4::sheet_resize(
      sheet = sheet, nrow = 2, ncol = NULL, exact = TRUE
    )

  ss |>
    googlesheets4::range_write(
      data = out,
      sheet = sheet,
      range = "A1",
      col_names = TRUE,
      reformat = FALSE
    )

  invisible()
}
