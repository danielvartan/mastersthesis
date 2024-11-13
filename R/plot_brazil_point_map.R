# library(dplyr)
# library(geobr)
library(ggplot2)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(viridis)

plot_brazil_point_map <- function(data) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_subset(c("latitude", "longitude"), names(data))

  brazil_data <- geobr::read_country()

  # Add chronotype category as colors.

  data |>
    dplyr::select(latitude, longitude) |>
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
    ggplot2::geom_point(size = 0.75, alpha = 0.75) +
    viridis::scale_color_viridis(discrete = TRUE) +
    ggplot2::labs(x = "Longitude", y = "Latitude") +
    ggplot2::guides(color = "none") +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank()
    )
}
