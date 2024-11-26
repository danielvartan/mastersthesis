# library(dplyr)
# library(ggplot2)
# library(glue)
# library(here)
# library(latex2exp)
library(magrittr)
# library(parsnip)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(readr)
# library(rutils) # github.com/danielvartan/rutils
# library(stats)
# library(utils)

source(here::here("R", "utils.R"))

panel_tabset_var_homoscedasticity <- function(
    data,
    fit,
    cols,
    col_labels = cols,
    source = rep("Created by the author.", length(cols)),
    heading = "###",
    data_name = "data",
    fit_name = "fit",
    suffix = ""
  ) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_class(fit, "workflow")
  prettycheck:::assert_character(cols, min.len = 1, any.missing = FALSE)
  prettycheck:::assert_subset(cols, names(data))
  prettycheck:::assert_character(col_labels, len = length(cols))
  prettycheck:::assert_character(source, len = length(cols))
  prettycheck:::assert_string(heading, pattern = "^#*")
  prettycheck:::assert_string(data_name)
  prettycheck:::assert_string(fit_name)
  prettycheck:::assert_string(suffix)

  if (!file.exists(here::here("qmd"))) dir.create(here::here("qmd"))

  file <- here::here(
    "qmd",
    glue::glue("_panel-tabset-var-homoscedasticity-{suffix}.qmd")
  )

  libraries <-
    c("dplyr", "ggplot2", "latex2exp", "parsnip", "rutils", "stats") |>
    sort() %>%
    paste0("library(", ., ")", collapse = "\n")

  scripts <-
    c("utils.R") |>
    sort() %>%
    paste0("source(here::here('R', '", ., "'))", collapse = "\n")

  out <- ":::: {.panel-tabset}\n"

  # Source: {source[i]}

  for (i in seq_along(cols)) {
    if (i == length(cols)) {
      end <- ""
    } else {
      end <- "\n\n"
    }

    col_fix <-
      cols[i] |>
      stringr::str_to_lower() |>
      stringr::str_replace_all("_", "-")

    out <- c(
      out,
      glue::glue(
        "
      {heading} {col_labels[i]}

      ::: {{#tbl-{suffix}-diag-homoscedasticity-{col_fix}}}
      ```{{r}}
      #| code-fold: true

      plot <-
        {fit_name} |>
        stats::predict({data_name}) |>
        dplyr::mutate(
          .sd_resid =
            {fit_name} |>
            parsnip::extract_fit_engine() |>
            stats::rstandard() |>
            abs() |>
            sqrt()
        ) |>
        dplyr::bind_cols(data) |>
        ggplot2::ggplot(ggplot2::aes({cols[i]}, .sd_resid)) +
        ggplot2::geom_point() +
        ggplot2::geom_smooth(color = 'red') +
        ggplot2::labs(
          x = '{col_labels[i]}',
          y = latex2exp::TeX('$\\\\sqrt{{|Standardized \\\\ Residuals|}}$')
        )

      plot |> print() |> rutils::shush()
      ```

      Relation between `{cols[i]}` and the model standardized residuals.
      :::{end}
      "
      )
    )
  }

  out <-
    c(out, "\n::::") |>
    paste0(collapse = "") |>
    readr::write_lines(file)

  include_string <- glue::glue(
    "{{{{< include {to_relative_path(file)} >}}}}"
  )

  cli::cli_alert_info(glue:::glue(
    "Use `{{include_string}}` to include ",
    "the panel in the file (Copied to clipboard).",
    "\n\n",
    "Also, don't forget call the libraries and to source the scripts ",
    "below in the file.",
    "\n\n",
    libraries,
    "\n\n",
    scripts,
    wrap = TRUE
  ))

  utils::writeClipboard(include_string)

  invisible()
}
