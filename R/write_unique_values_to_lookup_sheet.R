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
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_string(col)
  prettycheck:::assert_choice(col, names(data))
  prettycheck:::assert_string(sheet)
  prettycheck:::assert_string(ss)
  prettycheck:::assert_interactive()
  prettycheck:::assert_internet()

  ss <- googlesheets4::gs4_get(ss)
  prettycheck:::assert_subset(sheet, ss$sheets$name)

  out <-
    dplyr::tibble(
      key = rutils:::drop_na(unique(data[[col]])),
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
