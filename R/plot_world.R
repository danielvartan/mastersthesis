# library(dplyr)
library(ggplot2)
# library(maps)
# library(prettycheck) # github.com/danielvartan/prettycheck
library(rlang)

source(here::here("R", "utils-plots.R"))

# # Helpers
#
# geocoded_data <- targets::tar_read("geocoded_data")
# geocoded_data |> plot_world_countries()
#
# geocoded_data |>
#   dplyr::mutate(
#     country = dplyr::case_match(
#       country,
#       "Cabo Verde" ~ "Cape Verde",
#       "Czechia" ~ "Czech Republic",
#       "Russian Federation" ~ "Russia",
#       "United Kingdom" ~ "UK",
#       "United States" ~ "USA",
#       .default = country
#     )
#   ) |>
#   tidyr::drop_na() |>
#   dplyr::pull(country) |>
#   unique() |>
#   setdiff(world_data$region |> unique())

plot_world_countries <- function(
    data,
    col_fill = NULL,
    col_country = "country",
    transform = "log10", # See ?ggplot2::scale_fill_gradient
    viridis = "viridis",
    alpha = 1,
    direction = 1,
    color_low = NULL,
    color_high = NULL,
    color_brewer = "YlOrRd", # RColorBrewer::display.brewer.all()
    color_na = NA,
    color_border = "gray75",
    color_bg = "white",
    linewidth = 0.1,
    binned = TRUE,
    breaks = ggplot2::waiver(),
    title = NULL,
    subtitle = NULL,
    x_label = NULL, # "Longitude"
    y_label = NULL, # Latitude"
    fill_label = NULL,
    theme = "bw",
    legend = TRUE,
    text_size = NULL,
    print = TRUE
  ) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_subset("country", names(data))
  prettycheck:::assert_string(col_fill, null.ok = TRUE)
  prettycheck:::assert_choice(col_fill, names(data), null.ok = TRUE)
  prettycheck:::assert_string(col_country)
  prettycheck:::assert_choice(col_country, names(data))
  prettycheck:::assert_character(data[[col_country]])
  prettycheck:::assert_color(color_border, na_ok = TRUE)
  prettycheck:::assert_color(color_bg, na_ok = TRUE)
  prettycheck:::assert_number(linewidth, lower = 0, na.ok = TRUE)
  prettycheck:::assert_flag(print)

  world_data <-


  plot <-
    data |>
    dplyr::mutate(
      !!as.symbol(col_country) := dplyr::case_match(
        !!as.symbol(col_country),
        "Cabo Verde" ~ "Cape Verde",
        "Czechia" ~ "Czech Republic",
        "Russian Federation" ~ "Russia",
        "United Kingdom" ~ "UK",
        "United States" ~ "USA",
        .default = !!as.symbol(col_country)
      )
    ) |>
    get_map_fill_data(
      col_fill = col_fill,
      col_code = col_country,
      name_col_ref = "ID"
    ) |>
    dplyr::right_join(
      maps::map("world", plot = FALSE, fill = TRUE) |>
        sf::st_as_sf(),
      by = "ID"
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(
      ggplot2::aes(geometry = geom, fill = n),
      color = color_border,
      linewidth = linewidth,
      fill = color_bg
    ) +
    ggplot2::geom_sf(
      ggplot2::aes(geometry = geom,fill = n),
      color = NA
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(-150, 150, 50)
      # limits = c(-180, 180)
    ) +
    add_color_scale(
      viridis = viridis,
      alpha = alpha,
      direction = direction,
      color_low = color_low,
      color_high = color_high,
      color_brewer = color_brewer,
      color_na = color_na,
      binned = binned,
      breaks = breaks,
      limits = NULL,
      point = FALSE,
      transform = transform
    ) +
    add_labels(
      x = x_label,
      y = y_label,
      title = title,
      subtitle = subtitle,
      fill = fill_label
    ) +
    add_theme(
      theme = theme,
      legend = legend,
      text_size = text_size
    )

  if (isTRUE(print)) print(plot)

  invisible(plot)
}
