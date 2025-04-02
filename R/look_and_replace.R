# library(checkmate)
# library(cli)
# library(dplyr)
# library(here)
# library(lockr) # github.com/danielvartan/lockr
# library(prettycheck) # github.com/danielvartan/prettycheck
library(rlang)
# library(rutils) # github.com/danielvartan/rutils

look_and_replace <- function(
    x, #nolint
    table,
    osf_pat = Sys.getenv("OSF_PAT"),
    public_key = here::here("_ssh/id_rsa.pub"),
    private_key = here::here("_ssh/id_rsa"),
    password = Sys.getenv("MASTERSTHESIS_PASSWORD"),
    na_unmatched = FALSE,
    lookup_data = NULL
) {
  checkmate::assert_character(x)
  checkmate::assert_string(table)
  checkmate::assert_string(osf_pat, n.chars = 70)
  lockr:::assert_public_key(public_key)
  checkmate::assert_string(password, n.chars = 32)
  lockr:::assert_private_key(private_key, password = password)
  checkmate::assert_flag(na_unmatched)
  checkmate::assert_list(lookup_data, min.len = 1, null.ok = TRUE)
  prettycheck::assert_internet()

  if (is.null(lookup_data)) {
    cli::cli_progress_step("Downloading lookup tables")

    lookup_data <- get_lookup_data(
      osf_pat = osf_pat,
      public_key = public_key,
      private_key = private_key
    )
  }

  checkmate::assert_choice(table, names(lookup_data))

  lookup_table <-
    lookup_data[[table]] |>
    dplyr::select(key, value) |>
    dplyr::rename_with(.fn = ~ table, .cols = "key") |>
    dplyr::rename(lookup_value = value) |>
    dplyr::mutate(
      lookup_value = dplyr::if_else(
        is.na(lookup_value), "NA_lookup_", lookup_value)
    )

  out <-
    dplyr::tibble(!!as.symbol(table) := x) |>
    dplyr::left_join(lookup_table, by = table, na_matches = "never")

  if (isTRUE(na_unmatched)) {
    out <-
      out |>
      dplyr::mutate(
        !!as.symbol(table) := dplyr::case_when(
          lookup_value == "NA_lookup_" ~ as.character(NA),
          TRUE ~ lookup_value
        )
      )
  } else {
    out <-
      out |>
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
