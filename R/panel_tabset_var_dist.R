# library(dplyr)
# library(glue)
# library(prettycheck) # github.com/danielvartan/prettycheck

source(here::here("R", "stats_sum.R"))
source(here::here("R", "test_normality.R"))
source(here::here("R", "utils.R"))

panel_tabset_var_dist <- function(
  data,
  cols,
  col_labels = NULL,
  source = NULL,
  heading = "###"
  ) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_character(cols, min.len = 1, any.missing = FALSE)
  prettycheck:::assert_subset(cols, names(data))
  prettycheck:::assert_character(col_labels, len = length(cols), null.ok = TRUE)
  prettycheck:::assert_character(source, len = length(cols), null.ok = TRUE)
  prettycheck:::assert_string(heading, pattern = "#*")

  if (is.null(col_labels)) col_labels <- cols

  sample <- sample(1000:9999, 1)

  if (!file.exists(here::here("qmd"))) dir.create(here::here("qmd"))

  file <- here::here(
    "qmd",
    glue::glue("panel-tabset-var-dist.qmd")
  )

  out <- ":::: {.panel-tabset}\n"

  for (i in seq_along(cols)) {
    if (i == length(cols)) {
      end <- ""
    } else {
      end <- "\n\n"
    }

    out <- c(
      out,
      glue::glue(
        "
      {heading} {col_labels[i]}

      ::: {{#tbl-var-dist-stats-{cols[i]}}}
      ```{{r}}
      #| code-fold: true

      data |>
        dplyr::pull({cols[i]}) |>
        stats_sum(name = '{col_labels[i]}')
      ```

      Source: {source[i]}

      Statistics for the `{cols[i]}` variable.
      :::

      ::: {{#fig-var-dist-hist-{cols[i]}}}
      ```{{r}}
      #| code-fold: true

      data |>
        dplyr::pull({cols[i]}) |>
        test_normality(name = '{col_labels[i]}')
      ```

      Source: {source[i]}

      Histogram of the `{cols[i]}` variable with a kernel density
      estimate, along with a quantile-quantile (Q-Q) plot between the
      variable and the theoretical quantiles of the normal distribution.
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
    "Also, don't forget to source the scripts below in the file.",
    "\n\n",
    "`source(here::here('R', 'stats_sum.R'))`", "\n",
    "`source(here::here('R', 'test_normality.R'))`", "\n",
    "`source(here::here('R'', 'utils.R')))`"
  ))

  utils::writeClipboard(include_string)

  invisible()
}
