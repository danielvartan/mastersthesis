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
    thematic = TRUE,
    thematic_direction = 1,
    viridis = "viridis",
    viridis_direction = 1,
    color_brewer = "Set1", # RColorBrewer::display.brewer.all(),
    color_brewer_direction = 1,
    outline_color = get_brand_color("primary"), # "red"
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
  prettycheck:::assert_flag(thematic)
  prettycheck:::assert_choice(thematic_direction, c(-1, 1))
  assert_color_options(viridis = viridis)
  prettycheck:::assert_choice(viridis_direction, c(-1, 1))

  prettycheck:::assert_choice(
    color_brewer,
    RColorBrewer::brewer.pal.info |> rownames()
  )

  prettycheck:::assert_choice(color_brewer_direction, c(-1, 1))
  prettycheck:::assert_color(outline_color)
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
      outlier.colour = outline_color,
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

  if (!length(col) == 1) {
    if (isTRUE(thematic)) {
      scale_fill_brand_d(
        thematic_direction = thematic_direction,
        breaks = names(col)
      )
    } else if (!is.null(viridis)) {
      plot <-
        plot +
        ggplot2::scale_fill_viridis_d(
          breaks = names(col),
          direction = viridis_direction,
          option = viridis
        )

    } else if (!is.null(color_brewer)) {
      plot <-
        plot +
        ggplot2::scale_fill_brewer(
          palette = color_brewer,
          breaks = names(col),
          direction = color_brewer_direction
        )
    }
  }

  if (isTRUE(print)) print(plot)

  invisible(plot)
}
