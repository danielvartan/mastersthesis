# library(grDevices)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
}

# library(hms)
# library(lubritime) # github.com/danielvartan/lubritime
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils

labels_hms <- function(x, type = NULL) {
  classes <- c("numeric", "Duration", "difftime", "hms", "POSIXct",
               "POSIXlt", "Interval")

  prettycheck:::assert_multi_class(x, classes)

  if (hms::is_hms(x)) {
    out <- lubritime:::fix_hms(x)
  } else {
    out <-
      x |>
      lubritime::cycle_time(lubridate::ddays()) |>
      hms::as_hms() |>
      substr(1, 5)
  }

  if (!is.null(type)) out <- out |> rutils:::label_jump(type = type)

  out
}

# library(hms)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils

labels_char_hms <- function(x) {
  prettycheck:::assert_character(x, pattern = "^\\d{2}:\\d{2}:\\d{2}$")

  x |>
    hms::as_hms() |>
    rutils:::label_jump(type = "even")
}

# library(prettycheck) # github.com/danielvartan/prettycheck

label_decimal_fix <- function(x){
  prettycheck:::assert_character(x)

  out <- x[which(grepl("0$", x))]
  min_end_digit <- as.numeric(stringr::str_extract(x[1], ".$"))
  max_end_digit <- as.numeric(stringr::str_extract(x[length(x)], ".$"))

  if (min_end_digit <= 5) {
    out <- c(x[1], out)
  }

  if (max_end_digit <= 5) {
    out <- c(out, x[length(x)])
  }

  out
}

# library(cli)
# library(lubridate)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(stringr)

transform_cut_levels <- function(x, tz = "UTC") {
  pattern <- "^[\\(\\[][\\d.e+]+,[\\d.e+]+[\\)\\]]$"

  prettycheck:::assert_character(x, min.len = 1)
  prettycheck:::assert_choice(tz, OlsonNames())

  if (!all(stringr::str_detect(x, pattern))) {
    cli::cli_abort(paste0(
      "{.strong {cli::col_red('x')}} must follow the pattern ",
      "{.strong {cli::col_blue(pattern)}}."
    ))
  }

  int_start <-
    stringr::str_extract(x, "(?<=[\\(\\[])[\\d.e+]+") |>
    as.numeric() |>
    lubridate::as_datetime()

  int_end <-
    stringr::str_extract(x, "[\\d.e+]+(?=[\\)\\]])") |>
    as.numeric() |>
    lubridate::as_datetime()

  lubridate::interval(
    int_start + lubridate::dmilliseconds(1),
    int_end,
    tz
  )
}

# library(prettycheck) # github.com/danielvartan/prettycheck
# library(stringr)

cut_mean <- function(x, round = TRUE) {
  prettycheck:::assert_multi_class(x, c("character", "factor"))

  if (is.factor(x)) x <- as.character(x)

  left <-
    x |>
    stringr::str_extract("\\d+?\\.?\\d+(?=,)") |>
    as.numeric()

  right <-
    x |>
    stringr::str_extract("\\d+\\.?\\d*(?=\\D*\\]$)") |>
    as.numeric()

  out <- mapply(function(x, y) mean(c(x, y), na.rm = TRUE), left, right)

  if (isTRUE(round)) {
    round(out)
  } else {
    out
  }
}

# library(dplyr)
# library(magrittr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(sf)

filter_points_on_land <- function(data, geometry) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_subset(c("longitude", "latitude"), names(data))
  prettycheck:::assert_class(geometry, "sfc_MULTIPOLYGON")

  box <- geometry |> sf::st_bbox()

  data <-
    data |>
    dplyr::mutate(row_number = dplyr::row_number()) |>
    dplyr::relocate(row_number)

  na_cases <-
    data |>
    dplyr::select(row_number, latitude, longitude) |>
    dplyr::filter(is.na(latitude) | is.na(longitude))

  points <-
    data |>
    dplyr::select(row_number, latitude, longitude) |>
    tidyr::drop_na() |>
    sf::st_as_sf(
      coords = c("longitude", "latitude"),
      crs = sf::st_crs(geometry)
    ) |>
    sf::st_filter(geometry)

  valid_rows <- c(points$row_number, na_cases$row_number)

  data |>
    dplyr::filter(row_number %in% valid_rows) |>
    dplyr::select(-row_number)
}

# library(cli)
# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils
library(rlang)

get_map_fill_data <- function(
    data,
    col_fill = NULL,
    col_code,
    name_col_value = "n",
    name_col_ref = col_code
  ) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_string(col_fill, null.ok = TRUE)
  prettycheck:::assert_choice(col_fill, names(data), null.ok = TRUE)
  prettycheck:::assert_string(col_code)
  prettycheck:::assert_choice(col_code, names(data))
  prettycheck:::assert_string(name_col_value)
  prettycheck:::assert_string(name_col_ref)

  if (is.null(col_fill)) {
    data |>
      dplyr::rename(!!as.symbol(name_col_ref) := !!as.symbol(col_code)) |>
      dplyr::select(!!as.symbol(name_col_ref)) |>
      tidyr::drop_na() |>
      dplyr::count(!!as.symbol(name_col_ref))
  } else {
    prettycheck:::assert_numeric(data[[col_fill]], null_ok = TRUE)

    out <-
      data |>
      dplyr::rename(
        !!as.symbol(name_col_ref) := !!as.symbol(col_code),
        !!as.symbol(name_col_value) := !!as.symbol(col_fill)
      ) |>
      dplyr::select(!!as.symbol(name_col_ref), !!as.symbol(name_col_value)) |>
      tidyr::drop_na()

    if (any(duplicated(out[[name_col_ref]]))) {
      cli::cli_alert_warning(
        paste0(
          "There are duplicated values in ",
          "{.strong {cli::col_red(col_code)}}. ",
          "{.strong {cli::col_blue(col_fill)}} will be aggregated ",
          "using the mean."
        )
      )

      out |>
        dplyr::summarise(
          !!as.symbol(name_col_value) := mean(!!as.symbol(name_col_value)),
          .by = !!as.symbol(name_col_ref)
        )
    } else{
      out
    }
  }
}

# library(gginnards)
# library(prettycheck) # github.com/danielvartan/prettycheck

rm_scale <- function(plot) {
  prettycheck:::assert_class(plot, "gg")

  # plot$layers[[3]]$constructor[[1]][[3]]
  # plot$layers[[3]] <- NULL

  plot |>
    gginnards::delete_layers("GeomScaleBar") |>
    gginnards::delete_layers("GeomNorthArrow")
}

library(ggplot2)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(viridis)

add_color_scale <- function(
    viridis = "viridis",
    alpha = 1,
    direction = 1,
    color_brewer = "YlOrRd",
    color_low = NULL,
    color_high = NULL,
    color_na = NA,
    binary = FALSE,
    binned = FALSE,
    breaks = ggplot2::waiver(),
    labels = ggplot2::waiver(),
    limits = NULL,
    point = FALSE,
    transform = "identity",
    ...
  ) {
  assert_color_options(color_low, color_high, viridis)
  prettycheck:::assert_number(alpha, lower = 0, upper = 1)
  prettycheck:::assert_choice(direction, c(-1, 1))

  prettycheck:::assert_choice(
    color_brewer, RColorBrewer::brewer.pal.info |> row.names(), null.ok = TRUE
  )

  prettycheck:::assert_color(color_na, na_ok = TRUE)
  prettycheck:::assert_flag(binary)
  prettycheck:::assert_flag(binned)
  prettycheck:::assert_multi_class(breaks, c("function", "numeric", "waiver"))
  prettycheck:::assert_multi_class(labels, c("function", "numeric", "waiver"))

  prettycheck:::assert_multi_class(
    limits, c("numeric", "function"), null.ok = TRUE
  )

  prettycheck:::assert_flag(point)
  prettycheck:::assert_multi_class(transform, c("character", "transform"))


  if (isTRUE(binary)) binned <- FALSE

  if (is.null(viridis)) {
    if (is.null(color_low) || is.null(color_high)) {
      colors <- RColorBrewer::brewer.pal(5, color_brewer)
      color_low <- dplyr::first(colors)
      color_high <- dplyr::last(colors)
    }

    if (isTRUE(point)) {
      ggplot2::scale_color_continuous(
        low = color_low,
        high = color_high,
        na.value = color_na,
        breaks = breaks,
        labels = labels,
        limits = limits,
        transform = transform,
        ...
      )
    } else if (isTRUE(binned)) {
      ggplot2::scale_fill_binned(
        type = "gradient",
        low = color_low,
        high = color_high,
        na.value = color_na,
        breaks = breaks,
        labels = labels,
        limits = limits,
        transform = transform,
        ...
      )
    } else if (isTRUE(binary)) {
      ggplot2::scale_fill_continuous(
        low = color_high,
        high = color_high,
        na.value = color_na,
        breaks = breaks,
        labels = labels,
        limits = limits,
        transform = transform,
        ...
      )
    } else {
      ggplot2::scale_fill_gradient(
        low = color_low,
        high = color_high,
        na.value = color_na,
        breaks = breaks,
        labels = labels,
        limits = limits,
        transform = transform,
        ...
      )
    }
  } else {
    if (isTRUE(point)) {
      viridis::scale_color_viridis(
        alpha = alpha,
        option = viridis,
        direction = direction,
        na.value = color_na,
        breaks = breaks,
        labels = labels,
        limits = limits,
        transform = transform,
        ...
      )
    } else if (isTRUE(binned)) {
      ggplot2::scale_fill_stepsn(
        colors = viridis::viridis(
          30,
          alpha = alpha,
          direction = direction,
          option = viridis
        ),
        na.value = color_na,
        breaks = breaks,
        labels = labels,
        limits = limits,
        transform = transform,
        ...
      )
    } else {
      viridis::scale_fill_viridis(
        alpha = alpha,
        option = viridis,
        direction = direction,
        na.value = color_na,
        breaks = breaks,
        labels = labels,
        limits = limits,
        transform = transform,
        ...
      )
    }
  }
}

library(ggplot2)
# library(prettycheck) # github.com/danielvartan/prettycheck

add_labels <- function(
    title = NULL,
    subtitle = NULL,
    x_label = NULL,
    y_label = NULL,
    color_label = NULL,
    fill_label = NULL,
    size_label = NULL
  ) {
  class_options <- c("character", "latexexpression")

  prettycheck:::assert_length(title, len = 1, null_ok = TRUE)
  prettycheck:::assert_multi_class(title, class_options, null.ok = TRUE)
  prettycheck:::assert_length(subtitle, len = 1, null_ok = TRUE)
  prettycheck:::assert_multi_class(subtitle, class_options, null.ok = TRUE)
  prettycheck:::assert_length(x_label, len = 1, null_ok = TRUE)
  prettycheck:::assert_multi_class(x_label, class_options, null.ok = TRUE)
  prettycheck:::assert_length(y_label, len = 1, null_ok = TRUE)
  prettycheck:::assert_multi_class(y_label, class_options, null.ok = TRUE)
  prettycheck:::assert_length(color_label, len = 1, null_ok = TRUE)
  prettycheck:::assert_multi_class(color_label, class_options, null.ok = TRUE)
  prettycheck:::assert_length(fill_label, len = 1, null_ok = TRUE)
  prettycheck:::assert_multi_class(fill_label, class_options, null.ok = TRUE)
  prettycheck:::assert_length(size_label, len = 1, null_ok = TRUE)
  prettycheck:::assert_multi_class(size_label, class_options, null.ok = TRUE)

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

library(ggplot2)
# library(prettycheck) # github.com/danielvartan/prettycheck

add_theme <- function(
    theme = "gray",
    legend = TRUE,
    legend_position = "right",
    text_size = NULL,
    ...
  ) {
  theme_choices <- c(
    "default", "gray", "bw", "linedraw", "light", "dark", "minimal",
    "classic", "test", "void"
  )

  legend_position_choices <- c(
    "none", "left", "right", "bottom", "top", "inside"
  )

  prettycheck:::assert_choice(theme, theme_choices)
  prettycheck:::assert_flag(legend)
  prettycheck:::assert_choice(legend_position, legend_position_choices)
  prettycheck:::assert_number(text_size, null.ok = TRUE)

  theme_fun <- switch(
    theme,
    gray = ggplot2::theme_gray,
    bw = ggplot2::theme_bw,
    linedraw = ggplot2::theme_linedraw,
    light = ggplot2::theme_light,
    dark = ggplot2::theme_dark,
    minimal = ggplot2::theme_minimal,
    classic = ggplot2::theme_classic,
    void = ggplot2::theme_void,
    test = ggplot2::theme_test
  )

  theme_fun() +
    ggplot2::theme(
      legend.position = ifelse(isTRUE(legend), legend_position, "none"),
      text = ggplot2::element_text(size = text_size),
      ...
    )
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
