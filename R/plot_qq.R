# library(dplyr)
library(ggplot2)
# library(prettycheck) # github.com/danielvartan/prettycheck
library(rlang)
# library(tidyr)

source(here::here("R", "utils-plots.R"))

plot_qq <- function(data, col, na_rm = TRUE, print = TRUE) {
  checkmate::assert_tibble(data)
  checkmate::assert_string(col)
  checkmate::assert_subset(col, names(data))
  prettycheck::assert_numeric(data[[col]])
  checkmate::assert_flag(na_rm)
  checkmate::assert_flag(print)

  data <- data |> dplyr::select(dplyr::all_of(col))
  if (isTRUE(na_rm)) data <- data |> tidyr::drop_na()

  plot <-
    data |>
    ggplot2::ggplot(ggplot2::aes(sample = !!as.symbol(col))) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line(
      color = get_brand_color("primary"),
      linewidth = 1
    ) +
    ggplot2::labs(
      x = "Theoretical quantiles (Std. normal)",
      y = "Sample quantiles"
    ) +
    ggplot2::theme(legend.position = "none")

  if (isTRUE(print)) print(plot)

  invisible(plot)
}
