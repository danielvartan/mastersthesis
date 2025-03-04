# library(brandr) # github.com/danielvartan/brandr

set_brandr_options <- function() {
  brandr_options <- list(
    "BRANDR_COLOR_SEQUENTIAL" =
      brandr::get_brand_color(c("primary", "secondary")),
    "BRANDR_COLOR_DIVERGING" =
      brandr::get_brand_color(c("primary", "white", "secondary")),
    "BRANDR_COLOR_QUALITATIVE" = c(
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
    )

  for (i in seq_along(brandr_options)) {
    options(brandr_options[i])
  }
}

set_brandr_options()

scale_brand <- brandr::scale_brand
scale_color_brand_d <- brandr::scale_color_brand_d
scale_color_brand_c <- brandr::scale_color_brand_c
scale_color_brand_b <- brandr::scale_color_brand_b
scale_colour_brand_d <- brandr::scale_color_brand_d
scale_colour_brand_c <- brandr::scale_color_brand_c
scale_colour_brand_b <- brandr::scale_color_brand_b
scale_fill_brand_d <- brandr::scale_color_brand_d
scale_fill_brand_c <- brandr::scale_color_brand_c
scale_fill_brand_b <- brandr::scale_color_brand_b


# Move to `plotr` package -----

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

  checkmate::assert_multi_class(x, classes)

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
  checkmate::assert_character(x, pattern = "^\\d{2}:\\d{2}:\\d{2}$")

  x |>
    hms::as_hms() |>
    rutils:::label_jump(type = "even")
}

# library(prettycheck) # github.com/danielvartan/prettycheck

label_decimal_fix <- function(x){
  checkmate::assert_character(x)

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

  checkmate::assert_character(x, min.len = 1)
  checkmate::assert_choice(tz, OlsonNames())

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
  checkmate::assert_multi_class(x, c("character", "factor"))

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
  checkmate::assert_tibble(data)
  checkmate::assert_subset(c("longitude", "latitude"), names(data))
  checkmate::assert_class(geometry, "sfc_MULTIPOLYGON")

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
  checkmate::assert_tibble(data)
  checkmate::assert_string(col_fill, null.ok = TRUE)
  checkmate::assert_choice(col_fill, names(data), null.ok = TRUE)
  checkmate::assert_string(col_code)
  checkmate::assert_choice(col_code, names(data))
  checkmate::assert_string(name_col_value)
  checkmate::assert_string(name_col_ref)
  checkmate::assert_flag(quiet)

  if (is.null(col_fill)) {
    data |>
      dplyr::rename(!!as.symbol(name_col_ref) := !!as.symbol(col_code)) |>
      dplyr::select(!!as.symbol(name_col_ref)) |>
      tidyr::drop_na() |>
      dplyr::count(!!as.symbol(name_col_ref))
  } else {
    prettycheck::assert_numeric(data[[col_fill]], null_ok = TRUE)

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
  checkmate::assert_class(plot, "gg")

  # plot$layers[[3]]$constructor[[1]][[3]]
  # plot$layers[[3]] <- NULL

  plot |>
    gginnards::delete_layers("GeomScaleBar") |>
    gginnards::delete_layers("GeomNorthArrow")
}
