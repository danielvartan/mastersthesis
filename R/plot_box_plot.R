# library(dplyr)
library(ggplot2)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(tidyr)

source(here::here("R", "utils.R"))
source(here::here("R", "utils-plots.R"))
source(here::here("R", "utils-stats.R"))

plot_box_plot <- function(
    data,
    col,
    direction = 1,
    label = col,
    jitter = FALSE,
    print = TRUE
  ) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_character(col)
  prettycheck:::assert_subset(col, names(data))
  prettycheck:::assert_choice(direction, c(-1, 1))
  prettycheck:::assert_flag(jitter)
  prettycheck:::assert_character(label)
  prettycheck:::assert_flag(print)

  for (i in col) {
    if (prettycheck:::test_temporal(data[[i]])) {
      data[[i]] <- data[[i]] |> transform_time()
    } else {
      prettycheck:::assert_numeric(data[[i]])
    }
  }

  names(col) <- label

  data <-
    data |>
    dplyr::select(dplyr::all_of(col)) |>
    tidyr::pivot_longer(dplyr::all_of(col |> unname())) |>
    dplyr::mutate(
      name = factor(
        name,
        levels = col |> rev() |> names(),
        labels = col |> rev() |> names()
      )
    ) |>
    tidyr::drop_na(value)

  plot <-
    {
      if (length(col) == 1) {
        ggplot2::ggplot(
          data = data,
          ggplot2::aes(
            x = name,
            y = value
          )
        )
      } else {
        ggplot2::ggplot(
          data = data,
          ggplot2::aes(
            x = name,
            y = value,
            fill = name
          )
        )
      }
    } +
    ggplot2::geom_boxplot(
      outlier.colour = get_brand_color("primary"),
      outlier.shape = 1,
      width = 0.75
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = "Variable",
      y = "Value",
      fill = NULL
    ) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )

  if (isTRUE(jitter)) {
    plot <-
      plot +
      ggplot2::geom_jitter(
        width = 0.3,
        alpha = 0.1,
        color = "black",
        size = 0.5
      )
  }

  if (!length(col) == 1) {
    plot <-
      plot +
      scale_fill_brand_d(
        direction = direction,
        breaks = names(col)
      )
  }

  if (isTRUE(print)) print(plot)

  invisible(plot)
}
