## TODO:
##
## * Document functions

look_and_replace <- function(x, table, na_unmatched = FALSE) {
  checkmate::assert_character(x)
  checkmate::assert_string(table)
  checkmate::assert_flag(na_unmatched)
  gutils:::assert_data("lookup", gutils:::get_package_name())
  checkmate::assert_choice(table, names(lookup))

  lookup_data <- lookup[[table]] %>%
    dplyr::rename_with(.fn = ~ table, .cols = "key") %>%
    dplyr::rename(lookup_value = value) %>%
    dplyr::mutate(
      lookup_value = dplyr::if_else(
        is.na(lookup_value), "NA_lookup_", lookup_value)
    )

  out <- dplyr::tibble(!!as.symbol(table) := x) %>%
    dplyr::left_join(
      lookup_data, by = table, na_matches = "never"
    )

  if (isTRUE(na_unmatched)) {
    out <- out %>%
      dplyr::mutate(
        !!as.symbol(table) := dplyr::case_when(
          lookup_value == "NA_lookup_" ~ as.character(NA),
          TRUE ~ lookup_value
        )
      )
  } else {
    out <- out %>%
      dplyr::mutate(
        !!as.symbol(table) := dplyr::case_when(
          lookup_value == "NA_lookup_" ~ as.character(NA),
          is.na(lookup_value) ~ !!as.symbol(table),
          TRUE ~ lookup_value
        )
      )
  }

  out[[table]]
}

update_lookup <- function(
    ss = "1GJg7qVSb5srRe4wsFBBH5jIAMF7ZvCMyaB3hbFuhCDA",
    sheet_ignore = c("Documentation", "Codebook", "Validation", "Template")
    ) {
  checkmate::assert_string(ss)
  checkmate::assert_character(sheet_ignore)
  gutils:::assert_interactive()
  gutils:::assert_internet()

  googlesheets4::gs4_auth()
  ss <- googlesheets4::gs4_get("1GJg7qVSb5srRe4wsFBBH5jIAMF7ZvCMyaB3hbFuhCDA")
  sheets <- ss$sheets$name[!ss$sheets$name %in% sheet_ignore]

  lookup <- list()

  for (i in sheets) {
    lookup[[i]] <- googlesheets4::read_sheet(
      ss = ss, sheet = i, col_names = TRUE, col_types = "c",
      na = c("", "NA"), trim_ws = TRUE, skip = 0
    )
  }

  usethis::use_data(lookup, overwrite = TRUE)

  invisible(NULL)
}

filter_state <- function(data, state) {
    checkmate::assert_tibble(data)
    checkmate::assert_string(state)

    i <- state

    data %>%
        dplyr::filter(state == i) %>%
        dplyr::select(field_form_id, country, state, city)
}

write_unique_values_to_lookup_sheet <- function(
    data, col, sheet = col,
    ss = "1GJg7qVSb5srRe4wsFBBH5jIAMF7ZvCMyaB3hbFuhCDA"
    ) {
  checkmate::assert_tibble(data)
  checkmate::assert_string(col)
  checkmate::assert_choice(col, names(data))
  checkmate::assert_string(sheet)
  checkmate::assert_string(ss)
  gutils:::assert_interactive()
  gutils:::assert_internet()

  googlesheets4::gs4_auth()
  ss <- googlesheets4::gs4_get(ss)
  checkmate::assert_subset(sheet, ss$sheets$name)

  out <- dplyr::tibble(
    key = gutils:::drop_na(unique(data[[col]])),
    value = NA
  ) %>%
    dplyr::arrange(key)

  ss %>% googlesheets4::sheet_resize(
    sheet = sheet, nrow = 2, ncol = NULL, exact = TRUE
  )

  ss %>% googlesheets4::range_write(
    data = out, sheet = sheet, range = "A1", col_names = TRUE,
    reformat = FALSE
  )

  invisible(NULL)
}
