# library(dplyr)
library(ggplot2)
# library(prettycheck) # github.com/danielvartan/prettycheck
library(rlang)
# library(tidyr)

source(here::here("R", "utils-plots.R"))

plot_qq <- function(
    data,
    col,
    line_color = get_brand_color("primary"), # "red"
    na_rm = TRUE,
    title = NULL,
    subtitle = NULL,
    x_label = "Theoretical quantiles (Std. normal)",
    y_label = "Sample quantiles",
    theme = "bw",
    text_size = NULL,
    print = TRUE,
    ...
  ) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_string(col)
  prettycheck:::assert_subset(col, names(data))
  prettycheck:::assert_numeric(data[[col]])
  prettycheck:::assert_color(line_color)
  prettycheck:::assert_flag(na_rm)
  prettycheck:::assert_flag(print)

  data <- data |> dplyr::select(dplyr::all_of(col))

  if (isTRUE(na_rm)) data <- data |> tidyr::drop_na()

  plot <-
    data |>
    ggplot2::ggplot(
      ggplot2::aes(sample = !!as.symbol(col))
    ) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line(color = line_color, linewidth = 1) +
    add_labels(
      x = x_label,
      y = y_label,
      title = title,
      subtitle = subtitle,
    ) +
    add_theme(
      theme = theme,
      legend = FALSE,
      text_size = text_size,
      ...
    )

  if (isTRUE(print)) print(plot)

  invisible(plot)
}
