# library(here)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(yaml)

md_effect_size <- function(
    test = "final", #nolint
    prefix = "Cohen's ",
    max_digits = 10,
    results_yml = here::here("_results.yml")
  ) {
  checkmate::assert_choice(test, c("hta", "htb", "final"))
  checkmate::assert_string(prefix)
  checkmate::assert_int(max_digits, lower = 1)
  checkmate::assert_file_exists(results_yml)

  data <-
    results_yml |> yaml::read_yaml() |>
    magrittr::extract2(paste(test, "effect_size", sep = "_"))

  paste0(
    prefix,
    "$",
    "f^2 = ",
    data$f_squared |> round(max_digits),
    ", ",
    "95\\% \\ \\text{CI}[",
    data$lower_ci_limit |> round(max_digits),
    ", ",
    data$upper_ci_limit |> round(max_digits),
    "]",
    "$"
  ) |>
    I()
}

# library(here)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(yaml)

md_adj_r_squared <- function(
    test = "hta", #nolint
    model = "restricted",
    max_digits = 10,
    results_yml = here::here("_results.yml")
) {
  checkmate::assert_choice(test, c("hta", "htb"))
  checkmate::assert_choice(model, c("restricted", "full"))
  checkmate::assert_int(max_digits, lower = 1)
  checkmate::assert_file_exists(results_yml)

  data <-
    results_yml |> yaml::read_yaml() |>
    magrittr::extract2(paste(test, model, "model_adj_r_squared", sep = "_"))

  # $\text{F}(4, 65814) = 51.71$

  paste0(
    "$",
    "\\text{R}^2_{\\text{adj}} = ",
    data$value[1],
    "$"
  ) |>
    I()
}

# library(here)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(yaml)

md_f_test <- function(
    test = "hta", #nolint
    model = NULL,
    max_digits = 3,
    results_yml = here::here("_results.yml")
  ) {
  checkmate::assert_choice(test, c("hta", "htb"))
  checkmate::assert_choice(model, c("restricted", "full"), null.ok = TRUE)
  checkmate::assert_int(max_digits, lower = 1)
  checkmate::assert_file_exists(results_yml)

  if (is.null(model)) {
    data <-
      results_yml |> yaml::read_yaml() |>
      magrittr::extract2(paste(test, "f_test", sep = "_"))

    paste0(
      "$",
      "\\text{F}(",
      data$Df[2],
      ", ",
      data$Res.Df[2],
      ") = ",
      data$F[2] |> round(max_digits),
      "$"
    ) |>
      I()
  } else {
    data <-
      results_yml |> yaml::read_yaml() |>
      magrittr::extract2(paste(test, model, "model_fit_stats", sep = "_"))

    paste0(
      "$",
      "\\text{F}(",
      data$df,
      ", ",
      data$df.residual,
      ") = ",
      data$statistic |> round(max_digits),
      "$"
    ) |>
      I()
  }
}

# library(here)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(yaml)

md_p_value <- function(
    test = "hta", #nolint
    model = NULL,
    max_digits = 5,
    results_yml = here::here("_results.yml")
  ) {
  checkmate::assert_choice(test, c("hta", "htb"))
  checkmate::assert_choice(model, c("restricted", "full"), null.ok = TRUE)
  checkmate::assert_int(max_digits, lower = 1)
  checkmate::assert_file_exists(results_yml)

  if (is.null(model)) {
    data <-
      results_yml |> yaml::read_yaml() |>
      magrittr::extract2(paste(test, "f_test", sep = "_"))

    p_value <- stats::pf(
      data$F[2], data$Df[2], data$Res.Df[2], lower.tail = FALSE
    )
  } else {
    data <-
      results_yml |> yaml::read_yaml() |>
      magrittr::extract2(paste(test, model, "model_fit_stats", sep = "_"))

    p_value <- stats::pf(
      data$statistic, data$df, data$df.residual, lower.tail = FALSE
    )
  }

  if (p_value  == 0 || nchar(p_value) > 10) {
    p_value <- "1e-05"
  } else if (prettycheck::test_scientific_notation(p_value)) {
    p_value
  } else {
    p_value <- p_value |> round(max_digits)
  }

  paste0(
    "$",
    "p\\text{-value} < ",
    p_value,
    "$"
  ) |>
    I()
}

md_text <- function(x) {
  checkmate::assert_atomic(x)

  x |>
    as.character() %>%
    paste0("$", "\\text{", ., "}", "$") |>
    I()
}
