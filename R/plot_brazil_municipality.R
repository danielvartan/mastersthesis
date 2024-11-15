# library(dplyr)
# library(geobr)
# library(ggplot2)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(RColorBrewer)
library(rlang)
# library(rutils) # github.com/danielvartan/rutils
# library(viridis)

plot_brazil_city_map <- function(
    data,
    col_value = "value",
    col_muni_code = "municipality_code",
    col_year = "year",
    year = NULL,
    transform = "identity", # See ?ggplot2::scale_fill_gradient
    color_low = NULL,
    color_high = NULL,
    color_brewer = "YlOrRd", # RColorBrewer::display.brewer.all()
    color_na = "white",
    color_border = NA,
    viridis = NULL,
    binned = TRUE,
    breaks = ggplot2::waiver(),
    limits = NULL,
    zero_na = FALSE,
    point = FALSE,
    alpha = 0.7,
    title = NULL,
    subtitle = NULL,
    x_label = "Longitude",
    y_label = "Latitude",
    color_label = NULL,
    fill_label = NULL,
    size_label = NULL,
    legend = TRUE,
    text_size = NULL,
    theme = NULL,
    print = TRUE
  ) {
  viridis_choices <- c(
    "magma", "A", "inferno", "B", "plasma", "C", "viridis", "D",
    "cividis", "E", "rocket", "F", "mako", "G", "turbo", "H"
  )

  theme_choices <- c("minimal", "void")

  prettycheck:::assert_tibble(data)
  prettycheck:::assert_string(col_value)
  prettycheck:::assert_string(col_muni_code)
  prettycheck:::assert_string(col_year)
  prettycheck:::assert_subset(c(col_value, col_muni_code), names(data))
  prettycheck:::assert_integer_number(year, null.ok = TRUE)
  prettycheck:::assert_multi_class(transform, c("character", "transform"))
  prettycheck:::assert_color(color_low, null_ok = TRUE)
  prettycheck:::assert_color(color_high, null_ok = TRUE)

  prettycheck:::assert_choice(
    color_brewer, RColorBrewer::brewer.pal.info |> row.names()
  )

  prettycheck:::assert_color(color_na, null_ok = TRUE)
  prettycheck:::assert_color(color_border, na_ok = TRUE)
  prettycheck:::assert_choice(viridis, viridis_choices, null.ok = TRUE)
  prettycheck:::assert_flag(binned)
  prettycheck:::assert_multi_class(breaks, c("waiver", "numeric"))

  prettycheck:::assert_multi_class(
    limits, c("numeric", "function"), null.ok = TRUE
  )

  prettycheck:::assert_flag(zero_na)
  prettycheck:::assert_flag(point)
  prettycheck:::assert_number(alpha, lower = 0, upper = 1)
  prettycheck:::assert_string(title, null.ok = TRUE)
  prettycheck:::assert_string(subtitle, null.ok = TRUE)
  prettycheck:::assert_string(x_label, null.ok = TRUE)
  prettycheck:::assert_string(y_label, null.ok = TRUE)
  prettycheck:::assert_string(color_label, null.ok = TRUE)
  prettycheck:::assert_string(fill_label, null.ok = TRUE)
  prettycheck:::assert_string(size_label, null.ok = TRUE)
  prettycheck:::assert_flag(legend)
  prettycheck:::assert_number(text_size, null.ok = TRUE)
  prettycheck:::assert_choice(theme, theme_choices, null.ok = TRUE)
  assert_color_options(color_low, color_high, viridis)
  prettycheck:::assert_flag(print)
  prettycheck:::assert_internet()

  geobr_data <-
    geobr::read_municipality(year = 2019, showProgress = FALSE) |>
    rutils::shush()

  if (is.null(color_low) || is.null(color_high)) {
    colors <- RColorBrewer::brewer.pal(5, color_brewer)
    color_low <- dplyr::first(colors)
    color_high <- dplyr::last(colors)
  }

  if (!is.null(year)) {
    prettycheck:::assert_subset(col_year, names(data))

    year_value <- year

    data <- data |> dplyr::filter(!!as.symbol(col_year) == year_value)
  }

  if (prettycheck:::test_subset(col_year, names(data))) {
    cols <- c(col_value, col_muni_code, col_year)
  } else {
    cols <- c(col_value, col_muni_code)
  }

  data <-
    data |>
    dplyr::select(dplyr::all_of(cols)) |>
    dplyr::rename(
      code_muni = {{ col_muni_code }},
      value = {{ col_value }}
    )

  data <-
    geobr_data |>
    dplyr::left_join(data, by = "code_muni")

  if (isTRUE(zero_na)) {
    data <- data |> dplyr::mutate(value = ifelse(is.na(value), 0, value))
  }

  if (isTRUE(point)) {
    plot <- data |> plot_point(color_na, color_border, breaks, alpha)
  } else {
    plot <- data |> plot_sf(color_border)
  }

  plot <-
    plot |>
    add_labels(
      title, subtitle, x_label, y_label, color_label, fill_label, size_label
    ) |>
    add_theme(theme, legend, text_size)

  if (is.null(viridis)) {
    plot <-
      plot |>
      add_fill_scale(
        color_low = color_low,
        color_high = color_high,
        color_na = color_na,
        binned = binned,
        breaks = breaks,
        limits = limits,
        point = point,
        transform = transform
      )
  } else {
    plot <-
      plot |>
      add_viridis_scale(
        viridis = viridis,
        color_na = color_na,
        binned = binned,
        breaks = breaks,
        limits = limits,
        point = point,
        transform = transform
      )
  }

  if (isTRUE(print)) print(plot)

  invisible(plot)
}

# library(ggplot2)
# library(prettycheck) # github.com/danielvartan/prettycheck

plot_sf <- function(data, color_border = NA) {
  prettycheck:::assert_data_frame(data)
  prettycheck:::assert_subset(c("name_muni", "value"), names(data))
  prettycheck:::assert_color(color_border, na_ok = TRUE)

  geobr_country <-
    geobr::read_country(year = 2019, showProgress = FALSE) |>
    rutils::shush()

  data |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(
      ggplot2::aes(fill = value),
      color = color_border
    )
}

# library(dplyr)
# library(geobr)
# library(ggplot2)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils
# library(sf)

plot_point <- function(
    data,
    color_na = "white",
    color_border = "white",
    breaks = ggplot2::waiver(),
    alpha = 0.7
) {
  prettycheck:::assert_data_frame(data)
  prettycheck:::assert_subset(c("abbrev_state", "name_muni", "value"), names(data))
  prettycheck:::assert_color(color_na, null_ok = TRUE)
  prettycheck:::assert_color(color_border, na_ok = TRUE)
  prettycheck:::assert_multi_class(breaks, c("waiver", "numeric"))
  prettycheck:::assert_number(alpha, lower = 0, upper = 1)
  prettycheck:::assert_internet()

  data_points <- data |> sf::st_centroid() |> rutils:::shush()

  data_points <- dplyr::tibble(
    lon = sf::st_coordinates(data_points)[, 1],
    lat = sf::st_coordinates(data_points)[, 2],
    value = data$value,
    order = rank(value, ties.method = "first")
  ) |>
    tidyr::drop_na() |>
    dplyr::arrange(order)

  geobr_data_br <-
    geobr::read_country(year = 2019, showProgress = FALSE) |>
    rutils:::shush()

  data |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = geobr_data_br,
      fill = color_na, # "transparent"
      color = color_border
    ) +
    ggplot2::geom_point(
      data = data_points,
      mapping = ggplot2::aes(
        x = lon,
        y = lat,
        size = value,
        color = value
      ),
      alpha = alpha
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(),
      size = ggplot2::guide_legend(),
    ) +
    ggplot2::scale_size_continuous(
      range = c(0, 5),
      breaks = breaks
    ) +
    ggplot2::theme(legend.key = ggplot2::element_blank())
}

# library(ggplot2)
# library(prettycheck) # github.com/danielvartan/prettycheck

add_labels <- function(
    plot,
    title = NULL,
    subtitle = NULL,
    x_label = "Longitude",
    y_label = "Latitude",
    color_label = NULL,
    fill_label = NULL,
    size_label = NULL
  ) {
  prettycheck:::assert_class(plot, "gg")
  prettycheck:::assert_string(title, null.ok = TRUE)
  prettycheck:::assert_string(subtitle, null.ok = TRUE)
  prettycheck:::assert_string(x_label, null.ok = TRUE)
  prettycheck:::assert_string(y_label, null.ok = TRUE)
  prettycheck:::assert_string(color_label, null.ok = TRUE)
  prettycheck:::assert_string(fill_label, null.ok = TRUE)
  prettycheck:::assert_string(size_label, null.ok = TRUE)

  plot +
    ggplot2::labs(
      x = x_label,
      y = y_label,
      title = title,
      subtitle = subtitle,
      color = color_label,
      fill = fill_label,
      size = size_label
    )
}

# library(ggplot2)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(viridis)

add_viridis_scale <- function(
    plot,
    option,
    color_na = NULL,
    binned = FALSE,
    breaks = ggplot2::waiver(),
    limits = NULL,
    point = FALSE,
    transform = "identity"
) {
  option_choices <- c(
    "magma", "A", "inferno", "B", "plasma", "C", "viridis", "D",
    "cividis", "E", "rocket", "F", "mako", "G", "turbo", "H"
  )

  prettycheck:::assert_class(plot, "gg")
  prettycheck:::assert_choice(option, option_choices, null.ok = TRUE)
  prettycheck:::assert_color(color_na, null_ok = TRUE)
  prettycheck:::assert_flag(binned)
  prettycheck:::assert_multi_class(breaks, c("waiver", "numeric"))

  prettycheck:::assert_multi_class(
    limits, c("numeric", "function"), null.ok = TRUE
  )

  prettycheck:::assert_flag(point)
  prettycheck:::assert_multi_class(transform, c("character", "transform"))

  if (isTRUE(point)) {
    plot +
      viridis::scale_color_viridis(
        option = option,
        na.value = color_na,
        breaks = breaks,
        limits = limits,
        transform = transform
      )
  } else if (isTRUE(binned)) {
    plot +
      ggplot2::scale_fill_binned(
        type = "viridis",
        na.value = color_na,
        breaks = breaks,
        limits = limits,
        transform = transform,
      )
  } else {
    plot +
      viridis::scale_fill_viridis(
        option = option,
        na.value = color_na,
        breaks = breaks,
        limits = limits,
        transform = transform
      )
  }
}

# library(ggplot2)
# library(prettycheck) # github.com/danielvartan/prettycheck

add_fill_scale <- function(
    plot,
    color_low,
    color_high,
    color_na = NULL,
    binned = FALSE,
    breaks = ggplot2::waiver(),
    limits = NULL,
    point = FALSE,
    transform = "identity"
) {
  prettycheck:::assert_class(plot, "gg")
  prettycheck:::assert_color(color_low, null_ok = TRUE)
  prettycheck:::assert_color(color_high, null_ok = TRUE)
  prettycheck:::assert_color(color_na, null_ok = TRUE)
  prettycheck:::assert_flag(binned)
  prettycheck:::assert_multi_class(breaks, c("waiver", "numeric"))

  prettycheck:::assert_multi_class(
    limits, c("numeric", "function"), null.ok = TRUE
  )

  prettycheck:::assert_flag(point)
  prettycheck:::assert_multi_class(transform, c("character", "transform"))

  if (isTRUE(point)) {
    plot +
      ggplot2::scale_color_continuous(
        low = color_low,
        high = color_high,
        na.value = color_na,
        breaks = breaks,
        limits = limits,
        transform = transform
      )
  } else if (isTRUE(binned)) {
    plot +
      ggplot2::scale_fill_binned(
        type = "gradient",
        low = color_low,
        high = color_high,
        na.value = color_na,
        breaks = breaks,
        limits = limits,
        transform = transform
      )
  } else {
    plot +
      ggplot2::scale_fill_gradient(
        low = color_low,
        high = color_high,
        na.value = color_na,
        breaks = breaks,
        limits = limits,
        transform = transform
      )
  }
}

# library(ggplot2)
# library(prettycheck) # github.com/danielvartan/prettycheck

add_theme <- function(
    plot,
    theme = NULL,
    legend = TRUE,
    text_size = NULL
  ) {
  theme_choices <- c("minimal", "void")

  prettycheck:::assert_class(plot, "gg")
  prettycheck:::assert_choice(theme, theme_choices, null.ok = TRUE)
  prettycheck:::assert_flag(legend)
  prettycheck:::assert_number(text_size, null.ok = TRUE)

  if (isFALSE(legend)) {
    plot <- plot + ggplot2::guides(fill="none")
  }

  if (is.null(theme)) {
    plot + ggplot2::theme(text = ggplot2::element_text(size = text_size))
  } else if (theme == "minimal") {
    plot + ggplot2::theme_minimal()
  } else if (theme == "void") {
    plot + ggplot2::theme_void()
  } else {
    NULL
  }
}

# library(cli)
# library(grDevices)
# library(prettycheck) # github.com/danielvartan/prettycheck

assert_color_options <- function(
    color_low = NULL, color_high = NULL, viridis = NULL
) {
  viridis_choices <- c(
    "magma", "A", "inferno", "B", "plasma", "C", "viridis", "D",
    "cividis", "E", "rocket", "F", "mako", "G", "turbo", "H"
  )

  prettycheck:::assert_string(color_low, null.ok = TRUE)
  prettycheck:::assert_string(color_high, null.ok = TRUE)
  prettycheck:::assert_choice(viridis, viridis_choices, null.ok = TRUE)

  color_pattern <- "(?i)^#[a-f0-9]{3}$|^#[a-f0-9]{6}$|^transparent$"
  name_color_low <- deparse(substitute(color_low))
  name_color_high <- deparse(substitute(color_high))

  for (i in c(color_low, color_high)) {
    name <- ifelse(i == color_low, name_color_low, name_color_high)

    if (!is.null(i) &&
        !i %in% grDevices::colors() &&
        !prettycheck:::test_string(i, pattern = color_pattern)) {
      cli::cli_abort(
        paste0(
          "{.strong {cli::col_red(name)}} is not a valid color code. ",
          "It must contain a hexadecimal color code or one of the ",
          "values in {.strong {cli::col_blue('grDevices::color()')}}."
        )
      )
    }
  }

  if (is.null(color_low) && !is.null(color_high) ||
      !is.null(color_low) && is.null(color_high)) {
    cli::cli_abort(
      paste0(
        "You must provide both ",
        "{.strong {cli::col_blue('color_low')}} and ",
        "{.strong {cli::col_red('color_high')}} ",
        "arguments at the same time."
      )
    )
  } else if ((!is.null(color_low) | !is.null(color_high)) &&
             !is.null(viridis)) {
    cli::cli_abort(
      paste0(
        "You can't use both ",
        "{.strong {cli::col_blue('color_low/color_high')}} and ",
        "{.strong {cli::col_red('viridis')}} ",
        "arguments at the same time."
      )
    )
  } else {
    invisible(NULL)
  }
}
