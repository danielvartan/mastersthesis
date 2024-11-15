# library(dplyr)
# library(geobr)
library(ggplot2)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(tidyr)
# library(viridis)

plot_brazil_point <- function(data) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_subset(c("latitude", "longitude"), names(data))

  brazil_data <- geobr::read_country()

  # Add chronotype category as colors.

  data |>
    dplyr::select(latitude, longitude) |>
    dplyr::filter(
      latitude >= -33 & latitude <= 5.5,
    ) |>
    tidyr::drop_na() |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = longitude,
        y = latitude
      )
    ) +
    ggplot2::geom_sf(
      data = brazil_data,
      color = NA,
      fill = "gray",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_point(size = 0.5, alpha = 0.75, color = "blue") +
    ggplot2::labs(x = "Longitude", y = "Latitude") +
    ggplot2::guides(color = "none") +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank()
    )
}

plot_world_point <- function(data) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_subset(c("latitude", "longitude"), names(data))

  brazil_data <- ggplot2::map_data("world")plot_brazil_point <- function(data) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_subset(c("latitude", "longitude"), names(data))

  world_data <- ggplot2::map_data("world")

  # Add chronotype category as colors.

  data |>
    tidyr::drop_na() |>
    dplyr::count(country) |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = world_data,
      color = NA,
      fill = "gray",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_point(size = 0.5, alpha = 0.75, color = "blue") +
    ggplot2::labs(x = "Longitude", y = "Latitude") +
    ggplot2::guides(color = "none") +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank()
    )
}

  # Add chronotype category as colors.

  data |>
    dplyr::select(latitude, longitude) |>
    dplyr::filter(
      latitude >= -33 & latitude <= 5.5,
    ) |>
    tidyr::drop_na() |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = longitude,
        y = latitude
      )
    ) +
    ggplot2::geom_sf(
      data = brazil_data,
      color = NA,
      fill = "gray",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_point(size = 0.5, alpha = 0.75, color = "blue") +
    ggplot2::labs(x = "Longitude", y = "Latitude") +
    ggplot2::guides(color = "none") +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank()
    )
}
