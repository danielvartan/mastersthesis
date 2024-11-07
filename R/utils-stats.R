# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck
library(rlang)

summarise_data <- function(data, by) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_string(by)
  prettycheck:::assert_choice(by, names(data))

  data |>
    dplyr::summarise(n = sum(n), .by = !!as.symbol(by)) |>
    dplyr::mutate(
      n_cum = cumsum(n),
      n_per = (n / sum(n)),
      n_per_cum = cumsum(n_per * 100),
      n_per_cum =
        n_per_cum |>
        round(3) |>
        format(nsmall = 3) |>
        paste0("%")
    ) |>
    janitor::adorn_totals(
      where ="row",
      fill = "-",
      na.rm = TRUE,
      name = "Total",
      -n_cum,
      -n_per_cum
    ) |>
    janitor::adorn_pct_formatting(
      digits = 3,
      rounding = "half to even",
      affix_sign = TRUE,
      n_per
    ) |>
    dplyr::as_tibble()
}

# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck
library(rlang)

compare_sample <- function(sample_data, pop_data, by) {
  prettycheck:::assert_tibble(sample_data)
  prettycheck:::assert_tibble(pop_data)
  prettycheck:::assert_string(by)
  prettycheck:::assert_choice(by, names(sample_data))
  prettycheck:::assert_choice(by, names(pop_data))

  sample_data |>
    dplyr::summarise(n = dplyr::n(), .by = !!as.symbol(by)) |>
    dplyr::mutate(
      n_rel = n / sum(n),
      n_per = (n / sum(n)) * 100
    ) |>
    dplyr::left_join(
      pop_data |>
        dplyr::summarise(n = sum(n), .by = !!as.symbol(by))  |>
        dplyr::mutate(
          n_rel = n / sum(n),
          n_per = (n / sum(n)) * 100
        ),
      by = dplyr::all_of(by),
      suffix = c("_sample", "_pop")
    ) |>
    dplyr::mutate(
      n_per_diff = n_per_sample - n_per_pop,
      diff_per = (n_per_diff / n_per_pop) * 100
    ) |>
    dplyr::select(
      !!as.symbol(by),
      n_per_sample, n_per_pop,
      n_per_diff, diff_per
    )
}

# library(cli)
# library(prettycheck) # github.com/danielvartan/prettycheck

extract_from_summary <- function(x, element = "r.squared") {
  prettycheck:::assert_list(x)
  prettycheck:::assert_string(element)

  summary <- summary(x)

  if (is.null(names(summary))) {
    cli::cli_abort(paste0(
      "{.strong {cli::col_red('summary(x)')}} has no names."
    ))
  }

  if (!element %in% names(summary)) {
    cli::cli_abort(paste0(
      "There is no element named {.strong {cli::col_red(element)}} in ",
      "{.strong summary(x)}."
    ))
  }

  summary(x)[[element]]
}

r_squared <- function(x) extract_from_summary(x, "r.squared")
adj_r_squared <- function(x) extract_from_summary(x, "adj.r.squared")
f_statistic <- function(x) extract_from_summary(x, "fstatistic")

# library(stats)
# library(prettycheck) # github.com/danielvartan/prettycheck

std_error <- function(x){
  prettycheck:::assert_numeric(x)

  stats::sd(x, na.rm = TRUE) / sqrt(length(x))
}

# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(stats)

# TODO: Move to `rutils`.
is_outlier <- function( # Change name to `test_outlier`
    x,
    method = "iqr",
    iqr_mult = 1.5,
    sd_mult = 3
) {
  prettycheck:::assert_numeric(x)
  prettycheck:::assert_choice(method, c("iqr", "sd"))
  prettycheck:::assert_number(iqr_mult)
  prettycheck:::assert_number(sd_mult)

  if (method == "iqr") {
    iqr <- stats::IQR(x, na.rm = TRUE)
    min <- stats::quantile(x, 0.25, na.rm = TRUE) - (iqr_mult * iqr)
    max <- stats::quantile(x, 0.75, na.rm = TRUE) + (iqr_mult * iqr)
  } else if (method == "sd") {
    min <- mean(x, na.rm = TRUE) - (sd_mult * stats::sd(x, na.rm = TRUE))
    max <- mean(x, na.rm = TRUE) + (sd_mult * stats::sd(x, na.rm = TRUE))
  }

  dplyr::if_else(x > min & x < max, FALSE, TRUE, missing = FALSE)
}

# library(prettycheck) # github.com/danielvartan/prettycheck

# TODO: Move to `rutils`.
remove_outliers <- function(
    x,
    method = "iqr",
    iqr_mult = 1.5,
    sd_mult = 3
) {
  prettycheck:::assert_numeric(x)
  prettycheck:::assert_choice(method, c("iqr", "sd"))
  prettycheck:::assert_number(iqr_mult, lower = 1)
  prettycheck:::assert_number(sd_mult, lower = 0)

  x |>
    test_outlier(
      method = method,
      iqr_mult = iqr_mult,
      sd_mult = sd_mult
    ) %>%
    `!`() %>%
    magrittr::extract(x, .)
}
