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
    viridis = NULL,
    # color_brewer = "Set1", # RColorBrewer::display.brewer.all(),
    jitter = FALSE,
    label = col,
    title = NULL,
    subtitle = NULL,
    x_label = "Variable",
    y_label = "Value",
    fill_label = NULL,
    theme = "bw",
    text_size = NULL,
    print = TRUE,
    ...
  ) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_character(col)
  prettycheck:::assert_subset(col, names(data))
  assert_color_options(viridis = viridis)
  prettycheck:::assert_flag(jitter)

  # prettycheck:::assert_choice(
  #   color_brewer,
  #   RColorBrewer::brewer.pal.info |> rownames()
  # )

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
      outlier.colour = "red",
      outlier.shape = 1,
      width = 0.75
    ) +
    ggplot2::coord_flip() +
    add_labels(
      title = title,
      subtitle = subtitle,
      x_label = x_label,
      y_label = y_label,
      fill_label = fill_label
    ) +
    add_theme(
      theme = theme,
      legend = TRUE,
      text_size = text_size,
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      ...
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

  if (!is.null(viridis)) {
    plot <-
      plot +
      ggplot2::scale_fill_viridis_d(
        breaks = names(col),
        direction = -1,
        option = viridis
      )
      # ggplot2::scale_fill_brewer(
      #   palette = color_brewer,
      #   breaks = names(col),
      #   direction = -1
      # ) +
  }

  if (isTRUE(print)) print(plot)

  invisible(plot)
}
