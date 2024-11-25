# library(dplyr)
# library(geobr)
library(ggplot2)
# library(lubridate)
library(magrittr)
# library(prettycheck) # github.com/danielvartan/prettycheck
library(rlang)
# library(rutils) # github.com/danielvartan/rutils
# library(stringr)
# library(tidyr)

source(here::here("R", "utils-plots.R"))

# # Helpers
#
# geocoded_data <- targets::tar_read("geocoded_data")
# geocoded_data |> plot_brazil_point()
#
# weighted_data <- targets::tar_read("weighted_data")
#
# # Note that this data went through a filtering process that relied not
# # only on latitude, but also in sex and age (`tidyr::drop_na()`).
# weighted_data |> plot_brazil_point()

plot_brazil_point <- function(
    data,
    col_latitude = "latitude",
    col_longitude = "longitude",
    col_group = NULL,
    year = 2017,
    transform = "identity",
    viridis = "viridis",
    alpha = 0.75,
    discrete = TRUE,
    direction = 1,
    size_point = 0.5,
    color_point =
      viridis::plasma(1, begin = 0.65, direction = 1) |>
      stringr::str_trunc(7, ellipsis = ""),
    color_border = "gray75",
    color_bg = "white",
    linewidth = 0.1,
    breaks = ggplot2::waiver(),
    labels = ggplot2::waiver(),
    limits = NULL,
    title = NULL,
    subtitle = NULL,
    x_label = NULL, # "Longitude"
    y_label = NULL, # Latitude"
    theme = "bw",
    legend = TRUE,
    legend_size = 5,
    legend_position = "right",
    text_size = NULL,
    print = TRUE,
    ...
  ) {
  prettycheck:::assert_internet()
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_string(col_latitude)
  prettycheck:::assert_string(col_longitude)
  prettycheck:::assert_subset(c(col_latitude, col_longitude), names(data))
  prettycheck:::assert_string(col_group, null.ok = TRUE)
  prettycheck:::assert_choice(col_group, names(data), null.ok = TRUE)
  prettycheck:::assert_multi_class(transform, c("character", "transform"))

  prettycheck:::assert_number(
    year,
    lower = 1900,
    upper = Sys.Date() |> lubridate::year()
  )

  assert_color_options(NULL, NULL, viridis)
  prettycheck:::assert_number(alpha, lower = 0, upper = 1)
  prettycheck:::assert_flag(discrete)
  prettycheck:::assert_choice(direction, c(-1, 1))
  prettycheck:::assert_number(size_point, lower = 0)
  prettycheck:::assert_color(color_point, null_ok = TRUE)
  prettycheck:::assert_color(color_border, na_ok = TRUE)
  prettycheck:::assert_color(color_bg, na_ok = TRUE)
  prettycheck:::assert_number(linewidth, lower = 0, na.ok = TRUE)
  prettycheck:::assert_multi_class(breaks, c("waiver", "numeric"))
  prettycheck:::assert_multi_class(labels, c("waiver", "numeric"))
  prettycheck:::assert_number(legend_size, lower = 0)
  prettycheck:::assert_flag(print)

  brazil_state_data <-
    geobr::read_state(
      year = year,
      showProgress = FALSE
    ) |>
    rutils::shush()

  plot <-
    data |>
    filter_points_on_land(brazil_state_data |> dplyr::pull(geom)) |>
    dplyr::select(
      dplyr::all_of(c(col_latitude, col_longitude, col_group))
    ) |>
    tidyr::drop_na() %>% # Don't change the pipe!
    {
      if (is.null(col_group)) {
        ggplot2::ggplot(
          ggplot2::aes(
            x = !!as.symbol(col_longitude),
            y = !!as.symbol(col_latitude)
          ),
          data = .
        )
      } else {
        ggplot2::ggplot(
          ggplot2::aes(
            x = !!as.symbol(col_longitude),
            y = !!as.symbol(col_latitude),
            color = !!as.symbol(col_group)
          ),
          data = .
        )
      }
    } +
    ggplot2::geom_sf(
      data = brazil_state_data,
      color = color_border,
      fill = color_bg,
      linewidth = linewidth,
      inherit.aes = FALSE
    ) +
    {
      if (is.null(col_group)) {
        ggplot2::geom_point(
          size = size_point,
          color = color_point,
          alpha = alpha
        )
      } else {
        ggplot2::geom_point(
          size = size_point,
          alpha = alpha
        )
      }
    } +
    ggspatial::annotation_scale(
      location = "br",
      style = "tick",
      height = unit(0.15, "cm")
    ) +
    ggspatial::annotation_north_arrow(
      location = "br",
      height = unit(1, "cm"),
      width = unit(1, "cm"),
      pad_x = ggplot2::unit(0.1, "cm"),
      pad_y = ggplot2::unit(0.55, "cm"),
      style = ggspatial::north_arrow_fancy_orienteering
    ) +
    ggspatial::coord_sf(crs = 4674) +
    viridis::scale_color_viridis(
      alpha = alpha,
      direction = direction,
      discrete = discrete,
      option = viridis,
      breaks = breaks,
      labels = labels,
      limits = limits
    ) +
    add_labels(
      x = x_label,
      y = y_label,
      title = title,
      subtitle = subtitle
    ) +
    add_theme(
      theme = theme,
      legend = legend,
      legend_position = legend_position,
      text_size = text_size,
      ...
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(override.aes = list(size = legend_size))
    )

  if (isTRUE(print)) print(plot) |> rutils::shush()

  invisible(plot)
}

# library(dplyr)
# library(geobr)
library(ggplot2)
# library(lubridate)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils

source(here::here("R", "utils-plots.R"))

# # Helpers
#
# geocoded_data <- targets::tar_read("geocoded_data")
# geocoded_data |> plot_brazil_state()
#
# weighted_data <- targets::tar_read("weighted_data")
#
# # Note that this data went through a filtering process that relied not
# # only on latitude, but also in sex and age (`tidyr::drop_na()`).
# weighted_data |> plot_brazil_state()

plot_brazil_state <- function(
    data,
    col_fill = NULL,
    col_code = "state_code",
    year = 2017,
    transform = "identity", # See ?ggplot2::scale_fill_gradient
    viridis = "viridis",
    alpha = 1,
    direction = 1,
    color_brewer = "YlOrRd", # RColorBrewer::display.brewer.all()
    color_low = NULL,
    color_high = NULL,
    color_na = NA,
    color_border = "gray75",
    color_bg = "white",
    linewidth = 0.1,
    binary = FALSE,
    binned = TRUE,
    breaks = ggplot2::waiver(),
    labels = ggplot2::waiver(),
    limits = NULL,
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
  prettycheck:::assert_internet()
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_string(col_fill, null.ok = TRUE)
  prettycheck:::assert_choice(col_fill, names(data), null.ok = TRUE)
  prettycheck:::assert_string(col_code)
  prettycheck:::assert_choice(col_code, names(data))
  prettycheck:::assert_integerish(data[[col_code]])

  prettycheck:::assert_number(
    year,
    lower = 1900,
    upper = Sys.Date() |> lubridate::year()
  )

  prettycheck:::assert_color(color_border, na_ok = TRUE)
  prettycheck:::assert_color(color_bg, na_ok = TRUE)
  prettycheck:::assert_number(linewidth, lower = 0, na.ok = TRUE)
  prettycheck:::assert_flag(print)

  plot <-
    data |>
    get_map_fill_data(
      col_fill = col_fill,
      col_code = col_code,
      name_col_ref = "code_state"
    ) |>
    dplyr::right_join(
      geobr::read_state(
        year = year,
        showProgress = FALSE
      ) |>
        rutils::shush(),
      by = "code_state"
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(
      ggplot2::aes(geometry = geom),
      color = color_border,
      linewidth = linewidth,
      fill = color_bg
    ) +
    ggplot2::geom_sf(
      ggplot2::aes(geometry = geom, fill = n),
      color = NA
    ) +
    ggspatial::annotation_scale(
      ggplot2::aes(),
      location = "br",
      style = "tick",
      height = unit(0.15, "cm")
    ) +
    ggspatial::annotation_north_arrow(
      location = "br",
      height = unit(1, "cm"),
      width = unit(1, "cm"),
      pad_x = ggplot2::unit(0.1, "cm"),
      pad_y = ggplot2::unit(0.55, "cm"),
      style = ggspatial::north_arrow_fancy_orienteering
    ) +
    ggspatial::coord_sf(crs = 4674) +
    add_labels(
      x = x_label,
      y = y_label,
      title = title,
      subtitle = subtitle,
      fill = fill_label
    ) +
    add_color_scale(
      viridis = viridis,
      alpha = alpha,
      direction = direction,
      color_low = color_low,
      color_high = color_high,
      color_brewer = color_brewer,
      color_na = color_na,
      binary = binary,
      binned = binned,
      breaks = breaks,
      labels= labels,
      limits = limits,
      point = FALSE,
      transform = transform
    ) +
    add_theme(
      theme = theme,
      legend = legend,
      text_size = text_size
    )

  if (isTRUE(print)) print(plot) |> rutils::shush()

  invisible(plot)
}

# library(dplyr)
# library(geobr)
# library(ggplot2)
# library(lubridate)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils

source(here::here("R", "utils-plots.R"))

plot_brazil_municipality <- function(
    data,
    col_fill = NULL,
    col_code = "municipality_code",
    year = 2017,
    transform = "identity", # See `?ggplot2::scale_fill_gradient`
    viridis ="viridis",
    alpha = 1,
    direction = 1,
    color_low = NULL,
    color_high = NULL,
    color_brewer = "YlOrRd", # See `RColorBrewer::display.brewer.all()`
    color_na = NA,
    color_border = "gray75",
    color_bg = "white",
    linewidth = 0.1,
    binary = FALSE,
    binned = TRUE,
    range = c(0, 10),
    breaks = ggplot2::waiver(),
    labels = ggplot2::waiver(),
    limits = NULL,
    zero_na = FALSE,
    point = FALSE,
    title = NULL,
    subtitle = NULL,
    x_label = NULL, # "Longitude"
    y_label = NULL, # Latitude"
    color_label = NULL,
    fill_label = NULL,
    size_label = NULL,
    theme = "bw",
    legend = TRUE,
    text_size = NULL,
    print = TRUE
  ) {
  prettycheck:::assert_internet()
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_string(col_fill, null.ok = TRUE)
  prettycheck:::assert_choice(col_fill, names(data), null.ok = TRUE)
  prettycheck:::assert_string(col_code)
  prettycheck:::assert_choice(col_code, names(data))
  prettycheck:::assert_integerish(data[[col_code]])

  prettycheck:::assert_number(
    year,
    lower = 1900,
    upper = Sys.Date() |> lubridate::year()
  )

  prettycheck:::assert_color(color_border, na_ok = TRUE)
  prettycheck:::assert_color(color_bg, na_ok = TRUE)
  prettycheck:::assert_integerish(range, len = 2)
  prettycheck:::assert_multi_class(breaks, c("waiver", "numeric"))
  prettycheck:::assert_number(linewidth, lower = 0, na.ok = TRUE)
  prettycheck:::assert_flag(zero_na)
  prettycheck:::assert_flag(point)
  prettycheck:::assert_flag(print)

  out <-
    data |>
    get_map_fill_data(
      col_fill = col_fill,
      col_code = col_code,
      name_col_ref = "code_muni"
    ) |>
    dplyr::right_join(
      geobr::read_municipality(
        year = year,
        showProgress = FALSE
      ) |>
        rutils::shush(),
      by = "code_muni"
    )

  if (isTRUE(zero_na)) {
    out <- out |> dplyr::mutate(n = ifelse(is.na(n), 0, n))
  }

  if (isTRUE(point)) {
    plot <-
      out |>
      plot_brazil_municipality_point(
        color_border = color_border,
        color_bg = color_bg,
        linewidth = linewidth,
        alpha = alpha,
        range = range,
        breaks = breaks
      )
  } else {
    plot <-
      out |>
      ggplot2::ggplot() +
      ggplot2::geom_sf(
        ggplot2::aes(geometry = geom),
        color = color_border,
        linewidth = linewidth,
        fill = color_bg
      ) +
      ggplot2::geom_sf(
        ggplot2::aes(geometry = geom, fill = n),
        color = NA
      )
  }

  plot <-
    plot +
    ggspatial::annotation_scale(
      location = "br",
      style = "tick",
      width_hint = 0.25,
      height = unit(0.15, "cm")
    ) +
    ggspatial::annotation_north_arrow(
      location = "br",
      height = unit(1, "cm"),
      width = unit(1, "cm"),
      pad_x = ggplot2::unit(0.1, "cm"),
      pad_y = ggplot2::unit(0.55, "cm"),
      style = ggspatial::north_arrow_fancy_orienteering
    ) +
    ggspatial::coord_sf(crs = 4674) +
    add_labels(
      x = x_label,
      y = y_label,
      title = title,
      subtitle = subtitle,
      color = color_label,
      fill = fill_label,
      size = size_label
    ) +
    add_color_scale(
      viridis = viridis,
      alpha = alpha,
      direction = direction,
      color_low = color_low,
      color_high = color_high,
      color_brewer = color_brewer,
      color_na = color_na,
      binary = binary,
      binned = binned,
      breaks = breaks,
      labels= labels,
      limits = limits,
      point = point,
      transform = transform
    ) +
    add_theme(
      theme = theme,
      legend = legend,
      text_size = text_size
    )

  if (isTRUE(print)) print(plot) |> rutils::shush()

  invisible(plot)
}

# library(dplyr)
# library(geobr)
# library(ggplot2)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils
# library(sf)
# library(tidyr)

plot_brazil_municipality_point <- function(
    data,
    year = 2017,
    color_border = "gray75",
    color_bg = "white",
    linewidth = 0.1,
    alpha = 0.7,
    range = c(0, 10),
    breaks = ggplot2::waiver()
  ) {
  prettycheck:::assert_internet()
  prettycheck:::assert_tibble(data)

  prettycheck:::assert_number(
    year,
    lower = 1900,
    upper = Sys.Date() |> lubridate::year()
  )

  prettycheck:::assert_color(color_border, na_ok = TRUE)
  prettycheck:::assert_color(color_bg, na_ok = TRUE)
  prettycheck:::assert_number(linewidth, lower = 0, na.ok = TRUE)
  prettycheck:::assert_number(alpha, lower = 0, upper = 1)
  prettycheck:::assert_multi_class(breaks, c("waiver", "numeric"))
  prettycheck:::assert_integerish(range, len = 2)

  data_points <-
    data |>
    sf::st_as_sf() |>
    sf::st_centroid() |>
    rutils:::shush()

  data_points <- dplyr::tibble(
    longitude = sf::st_coordinates(data_points)[, 1],
    latitude = sf::st_coordinates(data_points)[, 2],
    n = data$n,
    order = rank(n, ties.method = "first")
  ) |>
    tidyr::drop_na() |>
    dplyr::arrange(order)

  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = geobr::read_state(
        year = year,
        showProgress = FALSE
      ) |>
        rutils::shush(),
      ggplot2::aes(geometry = geom),
      color = color_border,
      linewidth = linewidth,
      fill = color_bg,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_point(
      data = data_points,
      mapping = ggplot2::aes(
        x = longitude,
        y = latitude,
        size = n,
        color = n
      ),
      alpha = alpha
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(reverse = TRUE),
      size = ggplot2::guide_legend(reverse = TRUE),
    ) +
    ggplot2::scale_size_continuous(
      range = range,
      breaks = breaks
    ) +
    ggplot2::theme(legend.key = ggplot2::element_blank())
}
