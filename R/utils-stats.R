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

summary_by <- function(data, col, col_group, col_n = NULL) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_string(col)
  prettycheck:::assert_choice(col, names(data))
  prettycheck::assert_length(col_group, len = 1)
  prettycheck:::assert_multi_class(col_group, c("character", "factor"))
  prettycheck:::assert_choice(col_group, names(data))
  prettycheck:::assert_string(col_n, null.ok = TRUE)
  prettycheck:::assert_choice(col_n, names(data), null.ok = TRUE)

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
  prettycheck:::assert_tibble(sample_data)
  prettycheck:::assert_tibble(pop_data)
  prettycheck:::assert_string(by)
  prettycheck:::assert_choice(by, names(sample_data))
  prettycheck:::assert_choice(by, names(pop_data))

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

# This is based on: <https://stackoverflow.com/a/61354463/8258804>

# library(dplyr)
# library(methods)
# library(prettycheck) # github.com/danielvartan/prettycheck

mode <- function(x) {
  prettycheck:::assert_atomic(x)

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

get_midpoint <- function(cuts) {
  prettycheck:::assert_factor(cuts)

  out <- cuts |> levels() |> magrittr::extract(as.numeric(cuts))

  get_fun <- function(x) {
    x |>
      stringr::str_remove_all("\\(|\\[|\\)|\\]") |>
      stringr::str_split(",") |>
      unlist() |>
      as.numeric() |>
      mean()
  }

  out |> sapply(get_fun)
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

library(magrittr)
# library(prettycheck) # github.com/danielvartan/prettycheck

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
    ) %>% # Don't change the pipe.
    `!`() %>% # Don't change the pipe.
    magrittr::extract(x, .)
}

# library(broom)
# library(glue)
# library(prettycheck) # github.com/danielvartan/prettycheck

lm_fun <- function(model, fix_all_but = NULL, data = NULL) {
  prettycheck:::assert_class(model, "lm")

  prettycheck:::assert_number(
    fix_all_but,
    lower = 1,
    upper = length(stats::coef(model)) - 1,
    null.ok = TRUE
  )

  coef <- broom::tidy(fit)
  vars <- letters[seq_len((nrow(coef) - 1))]

  fixed_vars <- vars

  if (!is.null(fix_all_but)) {
    prettycheck:::assert_data_frame(data)
    # prettycheck:::assert_subset(coef$term[-1], names(data))

    for (i in seq_along(fixed_vars)[-fix_all_but]) {
      fixed_vars[i] <- mean(data[[coef$term[i + 1]]], na.rm = TRUE)
    }

    vars <- vars[fix_all_but]
  }

  fun_exp <- str2expression(
    glue::glue(
      "function({paste0(vars, collapse = ', ')}) {{", "\n",
      "  {paste0('prettycheck:::assert_numeric(', vars, ')', collapse = '\n')}",
      "\n\n",
      "  {coef$estimate[1]} +",
      "{paste0(coef$estimate[-1], ' * ', fixed_vars, collapse = ' + ')}",
      "\n",
      "}}"
    )
  )

  out <- eval(fun_exp)

  out
}

# library(latex2exp)
# library(prettycheck) # github.com/danielvartan/prettycheck

lm_str_fun <- function(
    model,
    digits = 3,
    latex2exp = TRUE,
    fix_all_but = NULL, # Ignore the intercept coefficient.
    fix_fun = "Mean",
    coef_names = NULL # Ignore the intercept coefficient.
  ) {
  prettycheck:::assert_class(model, "lm")
  prettycheck:::assert_number(digits)
  prettycheck:::assert_flag(latex2exp)

  prettycheck:::assert_number(
    fix_all_but,
    lower = 1,
    upper = length(stats::coef(model)) - 1,
    null.ok = TRUE
  )

  prettycheck:::assert_string(fix_fun)

  prettycheck:::assert_character(
    coef_names,
    any.missing = FALSE,
    len = length(names(stats::coef(model))) - 1,
    null.ok = TRUE
  )

  if (is.null(coef_names)) coef_names <- names(stats::coef(model))[-1]

  coef <- list()

  for (i in seq_along(coef_names)) {
    coef[[coef_names[i]]] <-
      stats::coef(model) |>
      magrittr::extract(i + 1) |>
      rutils:::clear_names() |>
      round(digits)
  }

  coef_names <-
    coef_names |>
    stringr::str_replace_all("\\_|\\.", " ") |>
    stringr::str_to_title() |>
    stringr::str_replace(" ", "")

  if (!is.null(fix_all_but)) {
    for (i in seq_along(coef_names)[-fix_all_but]) {
      coef_names[i] <- paste0(fix_fun, "(", coef_names[i], ")")
    }
  }

  out <- paste0(
    "$", "y =", " ",
    round(stats::coef(model)[1], digits), " + ",
    paste0(coef, " \\times ", coef_names, collapse = " + "),
    "$"
  )

  out <- out |> stringr::str_replace("\\+ \\-", "\\- ")

  if (isTRUE(latex2exp)) {
    out |> latex2exp::TeX()
  } else {
    out
  }
}
