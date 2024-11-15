# library(dplyr)
# library(here)
# library(parsnip)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils
# library(sidrar)

source(here::here("R", "get_brazil_region.R"))
source(here::here("R", "get_brazil_state.R"))

weigh_data <- function(data) {
  parameters <- c("country", "region", "state", "sex", "age")

  prettycheck:::assert_tibble(data)
  prettycheck:::assert_subset(parameters, names(data))

  pnad_data <-
    sidrar::get_sidra(
      api = paste0(
        "/t/6407/n1/all/n3/all/v/606/p/2017/c2/all/c58/1144,1145,",
        "1152,2793,3299,3300,3301,3302,6798"
      )
    ) |>
    rutils::shush() |>
    dplyr::as_tibble() |>
    dplyr::select(
      dplyr::all_of(
        c(
          "Valor", "Brasil e Unidade da Federação", "Ano", "Sexo",
          "Grupo de idade"
        )
      )
    ) |>
    dplyr::rename(
      n = Valor,
      state = `Brasil e Unidade da Federação`,
      year = Ano,
      sex = Sexo,
      age_group = `Grupo de idade`
    ) |>
    dplyr::filter(
      state != "Brasil",
      sex != "Total",
      age_group != "60 anos ou mais"
    ) |>
    dplyr::arrange(state, sex, age_group) |>
    dplyr::mutate(
      year = as.integer(year),
      country = "Brazil",
      region = get_brazil_region(state, "state"),
      sex = dplyr::case_when(
        sex == "Homens" ~ "Male",
        sex == "Mulheres" ~ "Female"
      ),
      sex = factor(sex, ordered = FALSE),
      age_group = dplyr::case_when(
        age_group == "18 a 19 anos" ~ "18-19",
        age_group == "20 a 24 anos" ~ "20-24",
        age_group == "25 a 29 anos" ~ "25-29",
        age_group == "30 a 39 anos" ~ "30-39",
        age_group == "40 a 49 anos" ~ "40-49",
        age_group == "50 a 59 anos" ~ "50-59",
        age_group == "60 a 64 anos" ~ "60-64",
        age_group == "65 anos ou mais" ~ "65+"
      ),
      age_group = factor(age_group, ordered = TRUE),
      n = as.integer(n * 1000)
    ) |>
    dplyr::relocate(year, country, region, state, sex, age_group, n) |>
    dplyr::mutate(
      n_rel = n / sum(n),
      n_per = (n / sum(n)) * 100
    ) |>
    dplyr::filter(state %in% get_brazil_state_by_utc(-3, "state"))

  out <-
    data |>
    dplyr::filter(state %in% get_brazil_state_by_utc(-3, "state")) |>
    tidyr::drop_na(state, sex, age) |>
    dplyr::mutate(
      age_group = dplyr::case_when(
        age >= 18 & age < 20 ~ "18-19",
        age >= 20 & age < 25 ~ "20-24",
        age >= 25 & age < 30 ~ "25-29",
        age >= 30 & age < 40 ~ "30-39",
        age >= 40 & age < 50 ~ "40-49",
        age >= 50 & age < 60 ~ "50-59",
        age >= 60 & age < 65 ~ "60-64",
        age >= 65 ~ "65+"
      ),
      age_group = factor(
        age_group,
        levels = c(
          "18-19", "20-24", "25-29", "30-39", "40-49", "50-59", "60-64",
          "65+"
        ),
        ordered = TRUE
      )
    ) |>
    dplyr::relocate(age_group, .after = age)

  weights_data <-
    pnad_data |>
    dplyr::mutate(
      n_rel = n / sum(n),
      n_per = (n / sum(n)) * 100
    )|>
    dplyr::left_join(
      out |>
        dplyr::summarise(
          n = dplyr::n(),
          .by = c("country", "region", "state", "sex", "age_group")
        ) |>
        dplyr::mutate(
          n_rel = n / sum(n),
          n_per = (n / sum(n)) * 100
        ),
      by = dplyr::all_of(c("country", "region", "state", "sex", "age_group")),
      suffix = c("_pnad", "_sample")
    ) |>
    dplyr::mutate(
      cell_weight = parsnip::importance_weights(n_per_pnad / n_per_sample)
    ) |>
    dplyr::select(
      country, region, state, sex, age_group, cell_weight
    ) |>
    dplyr::arrange(state, sex, age_group)

  out |>
    dplyr::left_join(
      weights_data,
      by = dplyr::all_of(c("country", "region", "state", "sex", "age_group"))
    )
}
