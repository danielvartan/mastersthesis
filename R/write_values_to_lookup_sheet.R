# library(dplyr)
# library(googlesheets4)
# library(prettycheck) # github.com/danielvartan/prettycheck

write_values_to_lookup_sheet <- function(
    data,
    sheet,
    ss = "1GJg7qVSb5srRe4wsFBBH5jIAMF7ZvCMyaB3hbFuhCDA"
) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_string(sheet)
  prettycheck:::assert_string(ss)
  prettycheck:::assert_interactive()
  prettycheck:::assert_internet()

  googlesheets4::gs4_auth()
  ss <- googlesheets4::gs4_get(ss)
  prettycheck:::assert_subset(sheet, ss$sheets$name)

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
