# library(cli)
# library(dplyr)
# library(googlesheets4)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils

source(here::here("R", "get_brazil_address_by_postal_code.R"))

get_and_write_geocode_data <- function(
    data, #nolint
    ss = "1GJg7qVSb5srRe4wsFBBH5jIAMF7ZvCMyaB3hbFuhCDA",
    method = "google",
    limit = 100
  ) {
  prettycheck::assert_internet()
  prettycheck::assert_interactive()
  checkmate::assert_string(ss)
  checkmate::assert_string(method)
  prettycheck::assert_numeric(limit, lower = 1)

  ss <- googlesheets4::gs4_get(ss)
  sheet <- "geocodes"

  sheets_data <- googlesheets4::read_sheet(
    ss = ss,
    sheet = sheet,
    col_names = TRUE,
    col_types = "c",
    na = c("", "NA"),
    trim_ws = TRUE,
    skip = 0
  ) |>
    rutils::shush()

  postal_code <-
    data |>
    dplyr::filter(is_postal_code_valid == FALSE) |>
    dplyr::pull(postal_code) |>
    setdiff(sheets_data$postal_code)

  if (length(postal_code) == 0) {
    cli::cli_alert_info(
      paste0(
        "All postal codes are already geocoded. ",
        "No new data was written to the Google Sheet."
      )
    )
  } else {
    if (!is.infinite(limit)) {
      limit <- as.integer(ceiling(limit))
      postal_code <- postal_code[seq_len(min(length(postal_code), limit))]
    }

    n_batches <- (length(postal_code) / 100) |> ceiling()

    cli::cli_progress_bar(
      name = "Downloading and writing geocode data in batches of 100",
      type = "tasks",
      total = n_batches,
      clear = FALSE
    )

    for (i in seq_len(n_batches)) {
      start <- (i - 1) * 100 + 1
      end <- ifelse(i == n_batches, length(postal_code), i * 100)

      geocode_data <-
        postal_code[seq(start, end)] |>
        get_brazil_address_by_postal_code(
          method = method,
          limit = Inf
        ) |>
        dplyr::select(-address) |>
        dplyr::mutate(
          timestamp = paste(
            as.Date(Sys.Date()),
            hms::as_hms(Sys.time()) |> round() |> hms::as_hms()
          ),
          source = "Google Geocoding API"
        ) |>
        dplyr::relocate(timestamp) |>
        rutils::shush() |>
        dplyr::mutate(
          dplyr::across(
            .cols = everything(),
            .fns = as.character
          )
        )

      sheets_data <- googlesheets4::read_sheet(
        ss = ss,
        sheet = sheet,
        col_names = TRUE,
        col_types = "c",
        na = c("", "NA"),
        trim_ws = TRUE,
        skip = 0
      ) |>
        rutils::shush()

      sheets_data |>
        dplyr::bind_rows(geocode_data) |>
        dplyr::mutate(
          dplyr::across(
            .cols = everything(),
            .fns = ~ ifelse(is.na(.x), "NA", .x)
          )
        ) |>
        write_values_to_lookup_sheet(
          sheet = sheet,
          ss = ss$spreadsheet_id
        ) |>
        rutils::shush()

      cli::cli_progress_update()
    }
  }

  invisible()
}
