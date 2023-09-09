filter_field_form <- function(data) {
  checkmate::assert_tibble(data)

  cli::cli_progress_step("Filtering data")

  utc_minus_3_states <- c(
    "Amapá", "Pará", "Maranhão", "Tocantins", "Piauí", "Ceará",
    "Rio Grande do Norte", "Paraíba", "Pernambuco", "Alagoas", "Sergipe",
    "Bahia", "Distrito Federal", "Goiás", "Minas Gerais", "Espírito Santo",
    "Rio de Janeiro", "São Paulo", "Paraná", "Santa Catarina",
    "Rio Grande do Sul"
  )

  data |>
    dplyr::filter(
      lubridate::date(timestamp) >= lubridate::ymd("2017-10-15"),
      lubridate::date(timestamp) <= lubridate::ymd("2017-10-21"),
      country == "Brazil",
      age >= 18
    ) |>
    dplyr::filter(
      !is_outlier(age),
      !is_outlier(transform_time(msf_sc)),
      state %in% utc_minus_3_states
    )
}

