# library(checkmate, quietly = TRUE)
# library(dplyr, quietly = TRUE)
library(ggplot2, quietly = TRUE)
# library(viridis, quietly = TRUE)

plot_world_map <- function(data, option = NULL, text_size = NULL) {
  option_choices <- c(
    "magma", "A", "inferno", "B", "plasma", "C", "viridis", "D",
    "cividis", "E", "rocket", "F", "mako", "G", "turbo", "H"
  )

  checkmate::assert_tibble(data)
  checkmate::assert_subset("country", names(data))
  checkmate::assert_choice(option, option_choices, null.ok = TRUE)
  checkmate::assert_number(text_size, null.ok = TRUE)

  world_map <- ggplot2::map_data("world")
  countries_list <- unique(world_map$region)

  plot <-
    data |>
    dplyr::count(country) |>
    dplyr::rename(region = country) |>
    dplyr::full_join(
      dplyr::tibble(region = countries_list),
      by = "region"
      ) |>
    dplyr::arrange(region) |>
    ggplot2::ggplot(ggplot2::aes(map_id = region)) +
    ggplot2::geom_map(ggplot2::aes(fill = n), map = world_map) +
    ggplot2::expand_limits(x = world_map$long, y = world_map$lat) +
    ggplot2::labs(x = "Longitude", y = "Latitude") +
    ggplot2::scale_fill_gradient(low = "#3A571B", high = "#90d743") +
    ggplot2::theme(
      text = ggplot2::element_text(size = text_size)
    )

  # https://waldyrious.net/viridis-palette-generator/
  # https://color.adobe.com/pt/create/color-wheel

  if (!is.null(option)) {
    plot <- plot + viridis::scale_fill_viridis(option = option)
  }

  print(plot)
  invisible(plot)
}
