# library(ggplot2)
# library(prettycheck) # github.com/danielvartan/prettycheck

source(here::here("R", "utils.R"))

# See <https://ggplot2-book.org/extensions#sec-new-scales> to learn more.

scale_brand <- function(
    aesthetics = "color",
    scale_type = "c",
    color_type = "seq",
    direction = 1,
    na.value = "grey50", # Must follow ggplot2 arg names.
    reverse = FALSE,
    ...
  ) {
  # See <https://ggplot2.tidyverse.org/reference/scale_viridis.html>.
  scale_type_choices <- c(
    "d", "discrete",
    "c", "continuous",
    "b", "binned"
  )

  # See <https://ggplot2.tidyverse.org/reference/scale_brewer.html>.
  color_type_choices <- c(
    "seq", "sequential",
    "div", "diverging",
    "qual", "qualitative"
  )

  prettycheck:::assert_string(aesthetics)
  prettycheck:::assert_choice(scale_type, scale_type_choices)
  prettycheck:::assert_choice(color_type, color_type_choices)
  prettycheck:::assert_choice(direction, c(-1, 1))
  prettycheck:::assert_color(na.value, na_ok = TRUE)
  prettycheck:::assert_flag(reverse)

  if (scale_type %in% c("d", "discrete")) {
    scale_fun <- ggplot2::discrete_scale
  } else if (scale_type %in% c("c", "continuous")) {
    scale_fun <- ggplot2::continuous_scale
  } else if (scale_type %in% c("b", "binned")) {
    scale_fun <- ggplot2::binned_scale
  }

  if (color_type %in% c("seq", "sequential")) {
    palette <- \(x) color_brand_sequential(x, direction = direction)
  } else if (color_type %in% c("div", "diverging")) {
    palette <- \(x) color_brand_diverging(x, direction = direction)
  } else if (color_type %in% c("qual", "qualitative")) {
    palette <- \(x) color_brand_qualitative(x, direction = direction)
  }

  arg_list <- list(
    aesthetics = aesthetics,
    palette = palette,
    na.value = na.value
  )

  if (identical(scale_fun, ggplot2::continuous_scale)) {
    arg_list <- c(
      arg_list,
      list(guide =ggplot2::guide_colourbar(reverse = reverse))
    )
  } else if (identical(scale_fun, ggplot2::binned_scale)) {
    arg_list <- c(
      arg_list,
      list(guide = ggplot2::guide_colorsteps(reverse = reverse))
    )
  } else if (identical(scale_fun, ggplot2::discrete_scale)) {
    arg_list <- c(
      arg_list,
      list(guide = ggplot2::guide_legend(reverse = reverse))
    )
  }

  do.call(
    what = scale_fun,
    args = c(
      list(...)[names(list(...)) %in% names(formals(scale_fun))],
      arg_list
    ) |>
      clean_arg_list()
  )
}

source(here::here("R", "utils.R"))

scale_color_brand_d <- function(
    aesthetics = "color",
    scale_type = "d",
    color_type = "seq",
    direction = 1,
    ...
  ) {
  do.call("scale_brand", grab_fun_par())
}

scale_color_brand_c <- function(
    aesthetics = "color",
    scale_type = "c",
    color_type = "seq",
    direction = 1,
    ...
  ) {
  do.call("scale_brand", grab_fun_par())
}

scale_color_brand_b <- function(
    aesthetics = "color",
    scale_type = "b",
    color_type = "seq",
    direction = 1,
    ...
  ) {
  do.call("scale_brand", grab_fun_par())
}

scale_colour_brand_d <- scale_color_brand_d
scale_colour_brand_c <- scale_color_brand_c
scale_colour_brand_b <- scale_color_brand_b

scale_fill_brand_d <- function(
    aesthetics = "fill",
    scale_type = "d",
    color_type = "seq",
    direction = 1,
    ...
  ) {
  do.call("scale_brand", grab_fun_par())
}

scale_fill_brand_c <- function(
    aesthetics = "fill",
    scale_type = "c",
    color_type = "seq",
    direction = 1,
    ...
  ) {
  do.call("scale_brand", grab_fun_par())
}

scale_fill_brand_b <- function(
    aesthetics = "fill",
    scale_type = "b",
    color_type = "seq",
    direction = 1,
    ...
  ) {
  do.call("scale_brand", grab_fun_par())
}

# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck

color_brand_sequential <- function(n, direction = 1) {
  prettycheck:::assert_numeric(n, lower = 0, min_len = 1)
  prettycheck:::assert_choice(direction, c(-1, 1))

  colors <- c(
    get_brand_color("primary"),
    get_brand_color("secondary")
  )

  if (length(n > 1) && all(dplyr::between(n, 0, 1), na.rm = TRUE)) {
    make_color_vector(
      n_prop = n,
      direction = direction,
      colors = colors
    )
  } else {
    make_color_vector(
      n = n,
      direction = direction,
      colors = colors
    )
  }
}

# library(prettycheck) # github.com/danielvartan/prettycheck

color_brand_diverging <- function(n, direction = 1) {
  prettycheck:::assert_numeric(n, lower = 0, min_len = 1)
  prettycheck:::assert_choice(direction, c(-1, 1))

  colors <- c(
    get_brand_color("primary"),
    get_brand_color("white"),
    get_brand_color("secondary")
  )

  if (length(n > 1) && all(dplyr::between(n, 0, 1), na.rm = TRUE)) {
    make_color_vector(
      n_prop = n,
      direction = direction,
      colors = colors
    )
  } else {
    make_color_vector(
      n = n,
      direction = direction,
      colors = colors
    )
  }
}

# library(prettycheck) # github.com/danielvartan/prettycheck

color_brand_qualitative <- function(n, direction = 1) {
  prettycheck:::assert_integerish(n, lower = 1, min.len = 1)
  prettycheck:::assert_choice(direction, c(-1, 1))

  base <- c(
    "#EA7701", # orange (primary)
    "#142A32", # black (secondary)
    "#964D01", # brown
    "#D67C20",
    "#4F5556", # grey (tertiary)
    "#2B4A5E",
    "#F5BD83", # light-orange
    "#F08C3E",
    "#CC5A15",
    "#B23300"
  )

  if (direction == -1) base <- rev(base)

  rep(base, length.out = n)
}

# library(here)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(yaml)

get_brand_color <- function(color) {
  brands_list <- yaml::read_yaml(here::here("_brand.yml"))

  palette_names <- brands_list$color$palette |> names()
  theme_names <- brands_list$color |> names()

  choices <- c(palette_names, theme_names)

  prettycheck:::assert_choice(color, choices)

  if (color %in% theme_names) {
    for (i in theme_names) {
      if (color == i) {
        color <- brands_list$color[[i]]
      }
    }
  }

  brands_list$color$palette[[color]]
}

# library(here)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(yaml)

get_brand_font <- function(type) {
  brands_list <- yaml::read_yaml(here::here("_brand.yml"))

  choices <- c(
    "base", "headings", "monospace", "monospace-inline", "monospace-block"
  )

  choices <- choices[choices %in% names(brands_list$typography)]

  prettycheck:::assert_choice(type, choices)

  if (is.null(names(brands_list$typography[[type]]))) {
    brands_list$typography[[type]]
  } else {
    brands_list$typography[[type]]$family
  }
}

# library(grDevices)
# library(prettycheck) # github.com/danielvartan/prettycheck

# # Helper
#
# get_brand_color_tint(c(1, seq(100, 1000, 100))) |> vector_to_c()
#
# c(1, seq(100, 1000, 100))
# #> [1]    1  100  200  300  400  500  600  700  800  900 1000
#
# c("#000000", "#2E1700", "#5D2F00", "#8C4700", "#BA5F00", "#E97600",
#   "#EE9233", "#F2AD66", "#F6C899", "#FAE3CC", "#FFFFFF")

get_brand_color_tint <- function(
    position = 500,
    color = "primary",
    n = 1000
  ) {
  prettycheck:::assert_integerish(position, lower = 0, upper = 1000)
  prettycheck:::assert_integer_number(n, lower = 1)

  color <- get_brand_color(color)

  color_fun <- grDevices::colorRampPalette(c("black", color, "white"))
  color_values <- color_fun(n)

  color_values[position]
}

# library(grDevices)
# library(prettycheck) # github.com/danielvartan/prettycheck

# # Helpers
#
# make_color_vector(10) |> vector_to_c()
# make_color_vector(n_prop = c(0.1, 0.5, 0.99)) |> vector_to_c()

make_color_vector <- function(
    n = NULL,
    colors = c(get_brand_color("primary"), get_brand_color("secondary")),
    direction = 1,
    values = NULL,
    n_prop = NULL,
    n_prop_res = 10000,
    ...
  ) {
  prettycheck:::assert_integerish(n, lower = 1, null.ok = TRUE)
  for (i in colors) prettycheck:::assert_color(i)
  prettycheck:::assert_choice(direction, c(-1, 1))

  prettycheck:::assert_numeric(
    n_prop, lower = 0, upper = 1, null_ok = TRUE
  )

  prettycheck:::assert_integer_number(n_prop_res, lower = 1)

  if (direction == -1) colors <- rev(colors)

  color_fun <- function(n) {
    color_ramp_fun <- grDevices::colorRampPalette(colors, ...)

    dplyr::case_when(
      n == 0 ~ color_ramp_fun(1),
      is.na(n) ~ NA,
      TRUE ~ color_ramp_fun(n)
    )
  }

  if (!is.null(n_prop)) {
    n <- n_prop * n_prop_res
    n <- ifelse(n == 0, 1, n)

    color_values <- color_fun(n_prop_res)[n]
    n <- length(n)
  } else {
    color_values <- color_fun(n)
  }

  if (!is.null(values)){
    prettycheck:::assert_atomic(values, len = n)

    names(color_values) <- as.character(values)
  }

  color_values
}

# library(grDevices)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
}

# library(ggplot2)

# Colors based on a data visualization found in @roenneberg2019b.

scale_fill_rainbow <- function(direction = 1) {
  colors <- c(
    "#FF1E00", "#FF7C00", "#FDD400", "#00CB00", "#009DF5",
    "#0040F7", "#981EAF"
  )

  if (direction == -1) colors <- rev(colors)

  ggplot2::scale_fill_manual(values = colors)
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
    name_col_ref = col_code,
    quiet = FALSE
  ) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_string(col_fill, null.ok = TRUE)
  prettycheck:::assert_choice(col_fill, names(data), null.ok = TRUE)
  prettycheck:::assert_string(col_code)
  prettycheck:::assert_choice(col_code, names(data))
  prettycheck:::assert_string(name_col_value)
  prettycheck:::assert_string(name_col_ref)
  prettycheck:::assert_flag(quiet)

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
      ) |>
        rutils::shush(quiet)

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
    thematic = TRUE,
    thematic_direction = 1,
    viridis = "viridis",
    viridis_direction = 1,
    viridis_alpha = 1,
    color_brewer = "YlOrRd",
    color_low = NULL,
    color_high = NULL,
    color_na = NA,
    binary = FALSE,
    binned = FALSE,
    breaks = ggplot2::waiver(), # Just for continuous or binned scales.
    n_breaks = NULL,
    labels = ggplot2::waiver(),
    reverse = FALSE,
    limits = NULL,
    point = FALSE,
    transform = "identity",
    ...
  ) {
  prettycheck:::assert_flag(thematic)
  prettycheck:::assert_choice(thematic_direction, c(-1, 1))
  prettycheck:::assert_choice(viridis_direction, c(-1, 1))
  prettycheck:::assert_number(viridis_alpha, lower = 0, upper = 1)

  prettycheck:::assert_choice(
    color_brewer, RColorBrewer::brewer.pal.info |> row.names(), null.ok = TRUE
  )

  assert_color_options(color_low, color_high, viridis)
  prettycheck:::assert_color(color_na, na_ok = TRUE)
  prettycheck:::assert_flag(binary)
  prettycheck:::assert_flag(binned)
  prettycheck:::assert_multi_class(breaks, c("function", "numeric", "waiver"))
  prettycheck:::assert_integer_number(n_breaks, lower = 1, null.ok = TRUE)
  prettycheck:::assert_multi_class(labels, c("function", "numeric", "waiver"))
  prettycheck:::assert_flag(reverse)

  prettycheck:::assert_multi_class(
    limits, c("numeric", "function"), null.ok = TRUE
  )

  prettycheck:::assert_flag(point)
  prettycheck:::assert_multi_class(transform, c("character", "transform"))


  if (isTRUE(binary)) binned <- FALSE

  if (isTRUE(thematic)) {
    if (isTRUE(point)) {
      scale_color_brand_c(
        direction = thematic_direction,
        na.value = color_na,
        breaks = breaks,
        n.breaks = n_breaks,
        labels = labels,
        reverse = reverse,
        limits = limits,
        transform = transform,
        ...
      )
    } else if (isTRUE(binned)) {
      scale_fill_brand_b(
        direction = thematic_direction,
        na.value = color_na,
        breaks = breaks,
        n.breaks = n_breaks,
        labels = labels,
        reverse = reverse,
        limits = limits,
        transform = transform,
        ...
      )
    } else if (isTRUE(binary)) {
      scale_fill_brand_c(
        direction = thematic_direction,
        na.value = color_na,
        breaks = breaks,
        n.breaks = n_breaks,
        labels = labels,
        reverse = reverse,
        limits = limits,
        transform = transform,
        ...
      )
    } else {
      scale_fill_brand_c(
        direction = thematic_direction,
        na.value = color_na,
        breaks = breaks,
        n.breaks = n_breaks,
        labels = labels,
        reverse = reverse,
        limits = limits,
        transform = transform,
        ...
      )
    }
  } else if (is.null(viridis)) {
    if (isTRUE(point)) {
      viridis::scale_color_viridis(
        alpha = viridis_alpha,
        option = viridis,
        direction = viridis_direction,
        na.value = color_na,
        breaks = breaks,
        n.breaks = n_breaks,
        labels = labels,
        limits = limits,
        transform = transform,
        ...
      )
    } else if (isTRUE(binned)) {
      ggplot2::scale_fill_stepsn(
        colors = viridis::viridis(
          30,
          alpha = viridis_alpha,
          direction = viridis_direction,
          option = viridis
        ),
        na.value = color_na,
        breaks = breaks,
        n.breaks = n_breaks,
        labels = labels,
        limits = limits,
        transform = transform,
        ...
      )
    } else {
      viridis::scale_fill_viridis(
        alpha = viridis_alpha,
        option = viridis,
        direction = viridis_direction,
        na.value = color_na,
        breaks = breaks,
        n.breaks = n_breaks,
        labels = labels,
        limits = limits,
        transform = transform,
        ...
      )
    }
  } else if (is.null(color_brewer)) {
    if (is.null(color_low) || is.null(color_high)) {
      colors <- RColorBrewer::brewer.pal(5, color_brewer)

      if (direction == 1) {
        color_low <- dplyr::first(colors)
        color_high <- dplyr::last(colors)
      } else {
        color_low <- dplyr::last(colors)
        color_high <- dplyr::first(colors)
      }
    }

    if (isTRUE(point)) {
      ggplot2::scale_color_continuous(
        low = color_low,
        high = color_high,
        na.value = color_na,
        breaks = breaks,
        n.breaks = n_breaks,
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
        n.breaks = n_breaks,
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
        n.breaks = n_breaks,
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
        n.breaks = n_breaks,
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
    theme = "bw",
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
