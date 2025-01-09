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

plot_brazil_point <- function(
    data,
    col_latitude = "latitude",
    col_longitude = "longitude",
    col_group = NULL,
    transform = "identity",
    direction = 1,
    alpha = 0.75,
    size_point = 0.5,
    breaks = ggplot2::waiver(),
    labels = ggplot2::waiver(),
    reverse = FALSE,
    limits = NULL,
    print = TRUE
  ) {
  prettycheck:::assert_internet()
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_string(col_latitude)
  prettycheck:::assert_subset(col_latitude, names(data))
  prettycheck:::assert_numeric(data[[col_latitude]])
  prettycheck:::assert_string(col_longitude)
  prettycheck:::assert_subset(col_longitude, names(data))
  prettycheck:::assert_numeric(data[[col_longitude]])
  prettycheck:::assert_string(col_group, null.ok = TRUE)
  prettycheck:::assert_choice(col_group, names(data), null.ok = TRUE)
  prettycheck:::assert_multi_class(transform, c("character", "transform"))
  prettycheck:::assert_choice(direction, c(-1, 1))
  prettycheck:::assert_number(alpha, lower = 0, upper = 1)
  prettycheck:::assert_number(size_point, lower = 0)
  prettycheck:::assert_multi_class(breaks, c("function", "numeric", "waiver"))
  prettycheck:::assert_multi_class(labels, c("function", "numeric", "waiver"))
  prettycheck:::assert_flag(reverse)
  prettycheck:::assert_flag(print)

  prettycheck:::assert_multi_class(
    limits, c("numeric", "function"), null.ok = TRUE
  )

  brazil_state_data <-
    geobr::read_state(
      year = 2017,
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
      color = "gray75",
      fill = "white",
      linewidth = 0.1,
      inherit.aes = FALSE
    ) +
    {
      if (is.null(col_group)) {
        ggplot2::geom_point(
          size = size_point,
          color = "#964D01",
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
      height = ggplot2::unit(0.15, "cm")
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
    scale_color_brand_d(
      direction = direction,
      breaks = breaks,
      labels = labels,
      reverse = reverse,
      limits = limits
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(override.aes = list(size = 5))
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
    transform = "identity", # See ?ggplot2::scale_fill_gradient
    direction = 1,
    binned = TRUE,
    breaks = ggplot2::waiver(),
    n_breaks = NULL,
    labels = ggplot2::waiver(),
    reverse = TRUE,
    limits = NULL,
    print = TRUE,
    quiet = FALSE
  ) {
  prettycheck:::assert_internet()
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_string(col_fill, null.ok = TRUE)
  prettycheck:::assert_choice(col_fill, names(data), null.ok = TRUE)
  if (!is.null(col_fill)) prettycheck:::assert_numeric(data[[col_fill]])
  prettycheck:::assert_string(col_code)
  prettycheck:::assert_choice(col_code, names(data))
  prettycheck:::assert_integerish(data[[col_code]])
  prettycheck:::assert_multi_class(transform, c("character", "transform"))
  prettycheck:::assert_choice(direction, c(-1, 1))
  prettycheck:::assert_flag(binned)
  prettycheck:::assert_multi_class(breaks, c("function", "numeric", "waiver"))
  prettycheck:::assert_integer_number(n_breaks, lower = 1, null.ok = TRUE)
  prettycheck:::assert_multi_class(labels, c("function", "numeric", "waiver"))
  prettycheck:::assert_flag(reverse)
  prettycheck:::assert_flag(print)
  prettycheck:::assert_flag(quiet)

  prettycheck:::assert_multi_class(
    limits, c("numeric", "function"), null.ok = TRUE
  )

  plot <-
    data |>
    get_map_fill_data(
      col_fill = col_fill,
      col_code = col_code,
      name_col_ref = "code_state",
      quiet = quiet
    ) |>
    dplyr::right_join(
      geobr::read_state(
        year = 2017,
        showProgress = FALSE
      ) |>
        rutils::shush(),
      by = "code_state"
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(
      ggplot2::aes(geometry = geom),
      color = "gray75",
      linewidth = 0.1,
      fill = "white"
    ) +
    ggplot2::geom_sf(
      ggplot2::aes(geometry = geom, fill = n),
      color = NA
    ) +
    ggspatial::annotation_scale(
      ggplot2::aes(),
      location = "br",
      style = "tick",
      height = ggplot2::unit(0.15, "cm")
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
    ggplot2::labs(
      x = NULL,
      y = NULL,
      fill = NULL
    ) +
    scale_brand(
      aesthetics = "fill",
      scale_type = ifelse(isTRUE(binned), "binned", "continuous"),
      direction = direction,
      breaks = breaks,
      n.breaks = n_breaks,
      labels = labels,
      reverse = reverse,
      limits = limits,
      transform = transform
    )

  # if (isFALSE(binned)) {
  #   plot <-
  #     plot + ggplot2::theme(
  #       legend.ticks = ggplot2::element_line(color = "white")
  #     )
  # }

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
    transform = "identity", # See `?ggplot2::scale_fill_gradient`
    direction = 1,
    alpha = 1,
    binned = TRUE,
    range = c(0, 10),
    breaks = ggplot2::waiver(),
    n_breaks = NULL,
    labels = ggplot2::waiver(),
    reverse = TRUE,
    limits = NULL,
    zero_na = FALSE,
    point = FALSE,
    print = TRUE,
    quiet = FALSE
  ) {
  prettycheck:::assert_internet()
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_string(col_fill, null.ok = TRUE)
  prettycheck:::assert_choice(col_fill, names(data), null.ok = TRUE)
  if (!is.null(col_fill)) prettycheck:::assert_numeric(data[[col_fill]])
  prettycheck:::assert_string(col_code)
  prettycheck:::assert_choice(col_code, names(data))
  prettycheck:::assert_integerish(data[[col_code]])
  prettycheck:::assert_multi_class(transform, c("character", "transform"))
  prettycheck:::assert_choice(direction, c(-1, 1))
  prettycheck:::assert_number(alpha, lower = 0, upper = 1)
  prettycheck:::assert_flag(binned)
  prettycheck:::assert_integerish(range, len = 2)
  prettycheck:::assert_multi_class(breaks, c("function", "numeric", "waiver"))
  prettycheck:::assert_integer_number(n_breaks, lower = 1, null.ok = TRUE)
  prettycheck:::assert_multi_class(labels, c("function", "numeric", "waiver"))
  prettycheck:::assert_flag(reverse)
  prettycheck:::assert_flag(zero_na)
  prettycheck:::assert_flag(point)
  prettycheck:::assert_flag(print)
  prettycheck:::assert_flag(quiet)

  prettycheck:::assert_multi_class(
    limits, c("numeric", "function"), null.ok = TRUE
  )

  out <-
    data |>
    get_map_fill_data(
      col_fill = col_fill,
      col_code = col_code,
      name_col_ref = "code_muni",
      quiet = quiet
    ) |>
    dplyr::right_join(
      geobr::read_municipality(
        year = 2017,
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
        color = "gray75",
        linewidth = 0.1,
        fill = "white"
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
    ggplot2::labs(
      x = NULL,
      y = NULL,
      fill = NULL,
      color = NULL,
      size = NULL
    ) +
    scale_brand(
      aesthetics = ifelse(isTRUE(point), "color", "fill"),
      scale_type = ifelse(isTRUE(binned), "binned", "continuous"),
      direction = direction,
      breaks = breaks,
      n.breaks = n_breaks,
      labels = labels,
      reverse = ifelse(isTRUE(point), FALSE, reverse),
      limits = limits,
      transform = transform
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
    alpha = 0.7,
    range = c(0, 10),
    breaks = ggplot2::waiver()
  ) {
  prettycheck:::assert_internet()
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_number(alpha, lower = 0, upper = 1)
  prettycheck:::assert_multi_class(breaks, c("function", "numeric", "waiver"))
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
        year = 2017,
        showProgress = FALSE
      ) |>
        rutils::shush(),
      ggplot2::aes(geometry = geom),
      color = "gray75",
      linewidth = 0.1,
      fill = "white",
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
