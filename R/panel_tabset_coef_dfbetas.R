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

panel_tabset_coef_dfbetas <- function(
    fit_engine,
    coef = names(stats::coef(fit_engine)),
    coef_labels = coef,
    source = rep("Created by the author.", length(coef)),
    heading = "###",
    fit_engine_name = "fit_engine",
    suffix = ""
  ) {
  prettycheck:::assert_class(fit_engine, "lm")

  prettycheck:::assert_character(
    coef,
    len = length(stats::coef(fit_engine)),
    any.missing = FALSE
  )

  prettycheck:::assert_subset(coef, names(stats::coef(fit_engine)))
  prettycheck:::assert_character(coef_labels, len = length(coef))
  prettycheck:::assert_character(source, len = length(coef))
  prettycheck:::assert_string(heading, pattern = "^#*")
  prettycheck:::assert_string(fit_engine_name)
  prettycheck:::assert_string(suffix)

  if (!file.exists(here::here("qmd"))) dir.create(here::here("qmd"))

  file <- here::here(
    "qmd",
    glue::glue("_panel-tabset-coef-dfbetas-{suffix}.qmd")
  )

  libraries <-
    c("ggplot2", "olsrr", "stats") |>
    sort() %>%
    paste0("library(", ., ")", collapse = "\n")

  scripts <-
    c("utils.R") |>
    sort() %>%
    paste0("source(here::here('R', '", ., "'))", collapse = "\n")

  out <- glue::glue(
    "
    :::: {{.panel-tabset}}

    ```{{r}}
    #| code-fold: true

      plots <- {fit_engine_name} |> olsrr::ols_plot_dfbetas(print_plot = FALSE)
      coef_names <- stats::coef({fit_engine_name}) |> names()
    ```
    \n
    "
  )

  coef_labels <- coef_labels |> stringr::str_remove_all("\\(|\\)")

  # Source: {source[i]}

  for (i in seq_along(coef)) {
    if (i == length(coef)) {
      end <- ""
    } else {
      end <- "\n\n"
    }

    coef_fix_1 <-
      coef[i] |>
      stringr::str_to_lower() |>
      stringr::str_remove_all("\\(|\\)") |>
      stringr::str_replace_all("_", "-")

    coef_fix_2 <-
      coef[i] |>
      stringr::str_remove_all("\\(|\\)")

    out <- c(
      out,
      glue::glue(
      "
      {heading} {coef_labels[i]}

      ::: {{#tbl-{suffix}-diag-influence-{coef_fix_1}}}
      ```{{r}}
      #| code-fold: true

      plots$plots[[{i}]] +
       ggplot2::labs(title = '{coef_labels[i]} coefficient')
      ```

      Standardized DFBETAS values for each observation concerning the
      `{coef_fix_2}` coefficient.
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
