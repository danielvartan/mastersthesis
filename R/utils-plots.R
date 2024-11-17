# library(hms)
# library(lubritime) # github.com/danielvartan/lubritime
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils

labels_hms <- function(x, type = "even") {
  classes <- c("numeric", "Duration", "difftime", "hms", "POSIXct",
               "POSIXlt", "Interval")

  prettycheck:::assert_multi_class(x, classes)

  if (hms::is_hms(x)) out <- lubritime:::fix_hms(x)

  out <-
    out |>
    hms::as_hms() |>
    substr(1, 5)

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

# library(here)
# library(hms)
# library(lubritime) # github.com/danielvartan/lubritime
library(magrittr)
# library(mctq)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils
# library(stats)

source(here::here("R/test_normality.R"))

summarise_inline <- function(data,
                             col,
                             x_label = col,
                             test_norm = FALSE) {
  classes <- c("numeric", "Duration", "difftime", "hms", "POSIXct",
               "POSIXlt", "Interval")

  prettycheck:::assert_tibble(data)
  prettycheck:::assert_choice(col, names(data))
  prettycheck:::assert_string(x_label)
  prettycheck:::assert_flag(test_norm)
  prettycheck:::assert_multi_class(data[[col]], classes)

  # R CMD Check variable bindings fix (see: https://bit.ly/3z24hbU) -----
  . <- NULL

  n <- paste0(
    "n = ",
    prettyNum(length(which(!(is.na(data[[col]])))), big.mark = ",")
  )

  if (prettycheck:::test_temporal(data[[col]], rm = "Period")) {
    data[[col]] <- mctq:::extract_seconds(data[[col]])
  }

  if (isTRUE(test_norm)) {
    test <- data |> test_normality(col)

    if ("test_shapiro" %in% names(test)) {
      value <-
        test$test_shapiro$p.value |>
        round(digits = 3) |>
        format(nsmall = 3, scientific = FALSE)

      if (test$test_shapiro$p.value > 0.05) {
        test <- paste0(
          "SW p-value \u2248 ",
          value,

          " (Can assume normality)"
          )
      } else {
        test <- paste0(
          "SW p-value \u2248 ",
          value,
          " (Cannot assume normality)"
          )
      }
    } else if ("test_lcks" %in% names(test)) {
      value <-
        test$test_lcks$p.value |>
        round(digits = 3) |>
        format(nsmall = 3, scientific = FALSE)

      if (test$test_lcks$p.value > 0.05) {
        test <- paste0(
          "LcKS p-value \u2248 ",
          value,

          " (Can assume normality)"
          )
      } else {
        test <- paste0(
          "LcKS p-value \u2248 ",
          value,

          " (Cannot assume normality)"
          )
      }
    }
  }

  mean <-
    data[[col]] |>
    mean(na.rm = TRUE) |>
    lubritime::cycle_time(lubridate::dhours(24)) |>
    round() |>
    hms::hms() %>% # Don't change the pipe.
    paste0("Mean = ", .)

  sd <-
    data[[col]] |>
    stats::sd(na.rm = TRUE) |>
    lubritime::cycle_time(lubridate::dhours(24)) |>
    round() |>
    hms::hms() %>% # Don't change the pipe.
    paste0("SD = ", .)

  if (isFALSE(test_norm) && !(is.na(mean)) && !(is.na(sd))) {
    paste0(x_label, ": ", n, " | ",  mean, " | ",  sd)
  } else if (!(is.na(mean)) && !(is.na(sd)) && !(is.na(test))) {
    paste0(x_label, ": ", n, " | ",  mean, " | ",  sd, " | ", test)
  } else {
    paste0(x_label, ": ", n)
  }
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

filter_points_on_land <- function(data, geom) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_subset(c("longitude", "latitude"), names(data))
  prettycheck:::assert_class(geom, "sf")

  box <-
    geom |>
    magrittr::extract2("geom") |>
    sf::st_bbox()

  points <-
    data |>
    dplyr::filter(
      longitude > box$xmin,
      longitude < box$xmax,
      latitude > box$ymin,
      latitude < box$ymax
    ) |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = sf::st_crs(geom))

  points <-
    points |>
    dplyr::mutate(on_land = lengths(sf::st_within(points, geom))) |>
    dplyr::filter(on_land == 1) |>
    dplyr::select(-on_land)

  points <-
    points |>
    sf::st_coordinates() |>
    dplyr::as_tibble() |>
    dplyr::bind_cols(points) |>
    dplyr::mutate(
      latitude = Y,
      longitude = X
    ) |>
    dplyr::select(-X, -Y)
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

# library(ggplot2)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(viridis)

add_color_scale <- function(
    viridis = "viridis",
    alpha = 1,
    direction = 1,
    color_brewer = "YlOrRd",
    color_low,
    color_high,
    color_na = NA,
    binned = FALSE,
    breaks = ggplot2::waiver(),
    limits = NULL,
    point = FALSE,
    transform = "identity",
    ...
  ) {
  assert_color_options(color_low, color_high, viridis)
  prettycheck:::assert_number(alpha, lower = 0, upper = 1)
  prettycheck:::assert_choice(direction, c(-1, 1))

  prettycheck:::assert_choice(
    color_brewer, RColorBrewer::brewer.pal.info |> row.names()
  )

  prettycheck:::assert_color(color_na, na_ok = TRUE)
  prettycheck:::assert_flag(binned)
  prettycheck:::assert_multi_class(breaks, c("waiver", "numeric"))

  prettycheck:::assert_multi_class(
    limits, c("numeric", "function"), null.ok = TRUE
  )

  prettycheck:::assert_flag(point)
  prettycheck:::assert_multi_class(transform, c("character", "transform"))


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
        limits = limits,
        transform = transform,
        ...
      )
    }
  }
}

# library(ggplot2)
# library(prettycheck) # github.com/danielvartan/prettycheck

add_labels <- function(
    title = NULL,
    subtitle = NULL,
    x_label = "Longitude",
    y_label = "Latitude",
    color_label = NULL,
    fill_label = NULL,
    size_label = NULL
  ) {
  prettycheck:::assert_string(title, null.ok = TRUE)
  prettycheck:::assert_string(subtitle, null.ok = TRUE)
  prettycheck:::assert_string(x_label, null.ok = TRUE)
  prettycheck:::assert_string(y_label, null.ok = TRUE)
  prettycheck:::assert_string(color_label, null.ok = TRUE)
  prettycheck:::assert_string(fill_label, null.ok = TRUE)
  prettycheck:::assert_string(size_label, null.ok = TRUE)

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

add_theme <- function(
    theme = "default",
    legend = TRUE,
    text_size = NULL,
    ...
  ) {
  theme_choices <- c(
    "default", "gray", "bw", "linedraw", "light", "dark", "minimal",
    "classic", "test", "void"
  )

  prettycheck:::assert_choice(theme, theme_choices, null.ok = TRUE)
  prettycheck:::assert_flag(legend)
  prettycheck:::assert_number(text_size, null.ok = TRUE)

  theme_fun <- switch(
    theme,
    default = ggplot2::theme,
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

  theme_fun(
    legend.position = ifelse(isTRUE(legend), "right", "none"),
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
