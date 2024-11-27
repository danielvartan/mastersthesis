# library(dplyr)
# library(glue)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(readr)

source(here::here("R", "plot_box_plot.R"))
source(here::here("R", "stats_summary.R"))
source(here::here("R", "test_normality.R"))
source(here::here("R", "utils.R"))

panel_tabset_var_distribution <- function(
    data,
    cols,
    col_labels = cols,
    source = rep("Created by the author.", length(cols)),
    heading = "###",
    data_name = "data",
    suffix = "",
    root = ".."
  ) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_character(cols, min.len = 1, any.missing = FALSE)
  prettycheck:::assert_subset(cols, names(data))
  prettycheck:::assert_character(col_labels, len = length(cols))
  prettycheck:::assert_character(source, len = length(cols))
  prettycheck:::assert_string(heading, pattern = "^#*")
  prettycheck:::assert_string(data_name)
  prettycheck:::assert_string(suffix)
  prettycheck:::assert_string(root)

  if (!file.exists(here::here("qmd"))) dir.create(here::here("qmd"))

  file <- here::here(
    "qmd",
    glue::glue("_panel-tabset-var-distribution-{suffix}.qmd")
  )

  libraries <-
    c(
      "cli", "dplyr", "fBasics", "ggplot2", "here", "hms", "lubridate",
      "lubritime", "magrittr", "moments", "nortest", "patchwork",
      "prettycheck", "purrr", "rutils", "stats", "stringr", "tidyr", "tseries"
    ) |>
    sort() %>%
    paste0("library(", ., ")", collapse = "\n")

  scripts <-
    c(
      "plot_box_plot.R", "plot_hist.R", "plot_qq.R", "stats_summary.R",
      "test_normality.R", "utils.R", "utils-checks.R", "utils-plots.R",
      "utils-stats.R"
    ) |>
    sort() %>%
    paste0("source(here::here('R', '", ., "'))", collapse = "\n")

  out <- ":::: {.panel-tabset}\n"

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

    # Source: {source[i]}

    out <- c(
      out,
      glue::glue(
        "
      {heading} {col_labels[i]}

      ::: {{#tbl-{suffix}-var-distribution-stats-{col_fix}}}
      ```{{r}}
      #| code-fold: true

      {data_name} |>
        stats_summary(
          col = '{cols[i]}',
          name = '{col_labels[i]}',
          as_list = FALSE
        )
      ```

      Statistics for the `{cols[i]}` variable.
      :::

      ::: {{#fig-{suffix}-var-distribution-histogram-{col_fix}}}
      ```{{r}}
      #| code-fold: true

      {data_name} |>
        test_normality(
          col = '{cols[i]}',
          name = '{col_labels[i]}'
        )
      ```

      Histogram of the `{cols[i]}` variable with a kernel density
      estimate, along with a quantile-quantile (Q-Q) plot between the
      variable and the theoretical quantiles of the normal distribution.
      :::

      ::: {{#fig-{suffix}-var-distribution-box-plot-{col_fix}}}
      ```{{r}}
      #| code-fold: true

      {data_name} |>
        plot_box_plot(
          col = '{cols[i]}'
        )
      ```

      Boxplot of the `{cols[i]}` variable.
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
    "{{{{< include {to_relative_path(file, root)} >}}}}"
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
