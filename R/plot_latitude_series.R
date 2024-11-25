# library(dplyr)
library(ggplot2)
# library(here)
# library(hms)
# library(latex2exp)
library(magrittr)
# library(prettycheck) # github.com/danielvartan/prettycheck
library(rlang)
# library(rutils) # github.com/danielvartan/rutils
# library(tidyr)

source(here::here("R/utils.R"))
source(here::here("R/utils-plots.R"))
source(here::here("R/utils-stats.R"))

plot_latitude_series <- function(
    data,
    col = "msf_sc",
    type = "boxplot",
    group_width = 1,
    linewidth = 2,
    size_point = 2,
    error_bar = TRUE,
    error_bar_width = 0.5,
    error_bar_linewidth = 0.5,
    date_breaks = "1 hour",
    minor_breaks = NULL,
    limits = NULL,
    title = NULL,
    subtitle = NULL,
    x_label = "Latitude",
    y_label = latex2exp::TeX("$MSF_{sc}$"), # "$MSF_{sc} \\pm SEM$"
    theme = "bw",
    text_size = NULL,
    print = TRUE,
    ...
  ) {
  col_classes <- c("numeric", "integer", "POSIXt", "hms", "Duration")

  prettycheck:::assert_tibble(data)
  prettycheck:::assert_string(col)
  prettycheck:::assert_choice(col, names(data))
  prettycheck:::assert_subset(c(col, "latitude"), names(data))
  prettycheck:::assert_numeric(data[["latitude"]])
  prettycheck:::assert_multi_class(data[[col]], col_classes)
  prettycheck:::assert_choice(type, c("boxplot", "point"))
  prettycheck:::assert_number(linewidth, lower = 0)
  prettycheck:::assert_number(size_point, lower = 0)
  prettycheck:::assert_flag(error_bar)
  prettycheck:::assert_number(error_bar_width, lower = 0)
  prettycheck:::assert_number(error_bar_linewidth, lower = 0)
  prettycheck:::assert_string(date_breaks)

  prettycheck:::assert_multi_class(
    minor_breaks, c("waiver", "numeric"), null.ok = TRUE
  )

  prettycheck:::assert_multi_class(
    limits, c("numeric", "function"), null.ok = TRUE
  )

  prettycheck:::assert_flag(print)

  if (is.null(y_label) && hms::is_hms(data[[col]])) {
    y_label = paste0("Local time (", col, " +- SEM)")
  }

  if (is.null(y_label) && prettycheck:::test_duration(data[[col]])) {
    y_label = paste0("Duration (", col, " +- SEM)")
  }

  data <-
    data |>
    dplyr::select(dplyr::all_of(c("latitude", col))) |>
    dplyr::filter(dplyr::between(latitude, -34, 3)) |>
    dplyr::mutate(
      latitude = cut_width(latitude, group_width),
      latitude = get_midpoint(latitude)
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::where(prettycheck:::test_duration),
        .fns = ~ hms::hms(as.numeric(.x))
      ),
      dplyr::across(
        .cols = dplyr::where(hms::is_hms),
        .fns = midday_trigger
      )
    ) |>
    tidyr::drop_na() |>
    dplyr::arrange(latitude)


  if (type == "boxplot") {
    plot <- data |> plot_latitude_series_boxplot(col)
  } else if (type == "point") {
    plot <- data |> plot_latitude_series_point(col, error_bar)
  }

  plot <-
    plot +
    ggplot2::geom_line(
      stat = "smooth",
      method = "lm",
      formula = y ~ x,
      linewidth = linewidth,
      color = "red",
      alpha = 0.75
    ) +
    ggplot2::scale_x_reverse(limits = limits,) +
    add_labels(
      title = title,
      subtitle = subtitle,
      x = x_label,
      y = y_label
    ) +
    add_theme(
      theme = theme,
      text_size = text_size,
      ...
    )

  if (inherits(data[[col]], c("POSIXt", "hms", "Duration"))) {
    plot <-
      plot +
      ggplot2::scale_y_datetime(
        date_breaks = date_breaks,
        minor_breaks = minor_breaks,
        date_labels = "%H:%M:%S",
      )
  }

  if (isTRUE(print)) print(plot) |> rutils::shush()

  invisible(plot)
}

plot_latitude_series_boxplot <- function(data, col) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_string(col)
  prettycheck:::assert_choice(col, names(data))
  prettycheck:::assert_subset(c(col, "latitude"), names(data))
  prettycheck:::assert_numeric(data[["latitude"]])

  data |>
    ggplot2::ggplot(
      ggplot2::aes(x = latitude, y = !!as.symbol(col))
    ) +
    ggplot2::geom_boxplot(
      ggplot2::aes(group = latitude),
      outlier.colour = "black",
      outlier.shape = 1,
      width = 0.75
    ) +
    ggplot2::stat_summary(
      fun = mean,
      geom = "point",
      shape = 4
    )
}

plot_latitude_series_point <- function(data, col, error_bar) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_string(col)
  prettycheck:::assert_choice(col, names(data))
  prettycheck:::assert_subset(c(col, "latitude"), names(data))
  prettycheck:::assert_numeric(data[["latitude"]])

  plot <-
    data |>
    dplyr::group_by(latitude) |>
    dplyr::summarise(
      std_error = std_error(!!as.symbol(col)),
      !!as.symbol(col) := mean(!!as.symbol(col), na.rm = TRUE),
    ) |>
    tidyr::drop_na() |>
    dplyr::arrange(latitude) |>
    ggplot2::ggplot(
      ggplot2::aes(x = latitude, y = !!as.symbol(col))
    ) +
    ggplot2::geom_point(
      size = size_point,
      color = "black"
    )

  if (isTRUE(error_bar)) {
    plot +
      ggplot2::geom_errorbar(
        ggplot2::aes(
          x = latitude,
          y = !!as.symbol(col),
          ymin = !!as.symbol(col) - std_error,
          ymax = !!as.symbol(col) + std_error
        ),
        show.legend = FALSE,
        inherit.aes = FALSE,
        width = error_bar_width,
        linewidth = error_bar_linewidth
      )
  } else {
    plot
  }
}
