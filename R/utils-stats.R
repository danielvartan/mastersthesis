# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck
library(rlang)

summarise_data <- function(data, by) {
  checkmate::assert_tibble(data)
  checkmate::assert_string(by)
  checkmate::assert_choice(by, names(data))

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

summary_by <- function(data, col, col_group, col_n = NULL) {
  checkmate::assert_tibble(data)
  checkmate::assert_string(col)
  checkmate::assert_choice(col, names(data))
  prettycheck::assert_length(col_group, len = 1)
  checkmate::assert_multi_class(col_group, c("character", "factor"))
  checkmate::assert_choice(col_group, names(data))
  checkmate::assert_string(col_n, null.ok = TRUE)
  checkmate::assert_choice(col_n, names(data), null.ok = TRUE)

  col_group <- col_group |> as.character()

  out <-
    data |>
    dplyr::select(dplyr::all_of(c(col, col_group, col_n))) %>%
    {
      if (!is.null(col_n)) {
        tidyr::uncount(., !!as.symbol(col_n))
      } else {
        .
      }
    } |>
    tidyr::drop_na(!!as.symbol(col_group)) |>
    dplyr::arrange(!!as.symbol(col_group)) |>
    dplyr::group_by(!!as.symbol(col_group)) |>
    dplyr::group_split() |>
    purrr::map_dfr(
      .f = ~ .x |>
        stats_summary(
          col = col,
          name = unique(.x[[col_group]]) |> as.character(),
          as_list = TRUE
        ) |>
        dplyr::as_tibble()
    ) |>
    dplyr::rename(!!as.symbol(col_group) := name)

  if (data[[col]] |> hms::is_hms()) {
    out |>
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::where(hms::is_hms),
          .fns = lubritime::round_time
        )
      )
  } else {
    out
  }
}

# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck
library(rlang)

compare_sample <- function(sample_data, pop_data, by) {
  checkmate::assert_tibble(sample_data)
  checkmate::assert_tibble(pop_data)
  checkmate::assert_string(by)
  checkmate::assert_choice(by, names(sample_data))
  checkmate::assert_choice(by, names(pop_data))

  sample_data |>
    dplyr::summarise(n = dplyr::n(), .by = !!as.symbol(by)) |>
    dplyr::mutate(
      n_rel = n / sum(n)
    ) |>
    dplyr::full_join(
      pop_data |>
        dplyr::summarise(n = sum(n), .by = !!as.symbol(by))  |>
        dplyr::mutate(
          n_rel = n / sum(n)
        ),
      by = by,
      suffix = c("_sample", "_pop")
    ) |>
    dplyr::mutate(
      n_rel_sample = ifelse(is.na(n_rel_sample), 0, n_rel_sample),
      n_rel_diff = n_rel_sample - n_rel_pop,
      diff_rel = n_rel_diff / n_rel_pop
    ) |>
    dplyr::select(
      !!as.symbol(by),
      n_rel_sample, n_rel_pop,
      n_rel_diff, diff_rel
    ) |>
    janitor::adorn_totals(
      where ="row",
      fill = "-",
      na.rm = TRUE,
      name = "Total"
    ) |>
    janitor::adorn_pct_formatting(
      digits = 3,
      rounding = "half to even",
      affix_sign = TRUE,
      -dplyr::all_of(by)
    ) |>
    dplyr::as_tibble()
}

# library(cli)
# library(prettycheck) # github.com/danielvartan/prettycheck

extract_from_summary <- function(x, element = "r.squared") {
  checkmate::assert_list(x)
  checkmate::assert_string(element)

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

# This is based on: <https://stackoverflow.com/a/61354463/8258804>

# library(dplyr)
# library(methods)
# library(prettycheck) # github.com/danielvartan/prettycheck

mode <- function(x) {
  checkmate::assert_atomic(x)

  out <-
    dplyr::tibble(x = x) |>
    dplyr::count(x) |>
    dplyr::arrange(dplyr::desc(n))

  if (out$n[1] == 1 | is.na(out$n[1]) |
      out$n[1] == out$n[2]) {
    methods::as(NA, class(x)[1])
  } else {
    out$x[1]
  }
}

# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(stats)

test_outlier <- function(
    x,
    method = "iqr",
    iqr_mult = 1.5,
    sd_mult = 3
  ) {
  prettycheck::assert_numeric(x)
  checkmate::assert_choice(method, c("iqr", "sd"))
  checkmate::assert_number(iqr_mult)
  checkmate::assert_number(sd_mult)

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

library(magrittr)
# library(prettycheck) # github.com/danielvartan/prettycheck

remove_outliers <- function(
    x,
    method = "iqr",
    iqr_mult = 1.5,
    sd_mult = 3
  ) {
  prettycheck::assert_numeric(x)
  checkmate::assert_choice(method, c("iqr", "sd"))
  checkmate::assert_number(iqr_mult, lower = 1)
  checkmate::assert_number(sd_mult, lower = 0)

  x |>
    test_outlier(
      method = method,
      iqr_mult = iqr_mult,
      sd_mult = sd_mult
    ) %>% # Don't change the pipe.
    `!`() %>% # Don't change the pipe.
    magrittr::extract(x, .)
}
