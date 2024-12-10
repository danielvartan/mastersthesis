# library(dplyr)
# library(forcats)
library(ggplot2)
# library(here)
# library(hms)
# library(prettycheck) # github.com/danielvartan/prettycheck
library(rlang)
# library(rutils) # github.com/danielvartan/rutils
# library(tidyr)
# library(viridis)

source(here::here("R", "utils.R"))
source(here::here("R", "utils-plots.R"))

# # To do
#
# - Add option to cut-offs like Roenneberg et al. (2007).
#    See cut-offs above 60.
# - Create `plot_msf_sc series()`.

plot_series <- function(
    data,
    col_x = "age",
    col_y = "msf_sc",
    col_group = "sex",
    thematic = TRUE,
    viridis = "viridis",
    line_width = 2,
    boundary = 0.5,
    point_size = 1,
    error_bar_width = 0.5,
    error_bar_linewidth = 0.5,
    error_bar = TRUE,
    date_breaks = "15 mins",
    date_minor_breaks = NULL,
    reverse = FALSE,
    change_sign = FALSE,
    title = NULL,
    subtitle = NULL,
    x_label = "Age",
    y_label = latex2exp::TeX("$MSF_{sc}$"), # "$MSF_{sc} \\pm SEM$"
    color_label = "Sex",
    theme = "bw",
    text_size = NULL,
    print = TRUE
) {
  col_classes <- c("numeric", "integer", "POSIXt", "hms", "Duration")

  prettycheck:::assert_tibble(data)
  prettycheck:::assert_string(col_x)
  prettycheck:::assert_subset(col_x, names(data))
  prettycheck:::assert_multi_class(data[[col_x]], col_classes)
  prettycheck:::assert_string(col_y)
  prettycheck:::assert_subset(col_y, names(data))
  prettycheck:::assert_multi_class(data[[col_y]], col_classes)
  prettycheck:::assert_string(col_group)
  prettycheck:::assert_subset(col_group, names(data))
  prettycheck:::assert_class(data[[col_group]], "factor")
  prettycheck:::assert_flag(thematic)
  assert_color_options(viridis = viridis)
  prettycheck:::assert_multi_class(y_label, c("character", "latexexpression"))
  prettycheck:::assert_length(y_label, len = 1)
  prettycheck:::assert_number(line_width)
  prettycheck:::assert_number(boundary)
  prettycheck:::assert_number(point_size)
  prettycheck:::assert_number(error_bar_width)
  prettycheck:::assert_number(error_bar_linewidth)
  prettycheck:::assert_flag(error_bar)
  prettycheck:::assert_string(date_breaks)

  prettycheck:::assert_multi_class(
    date_minor_breaks, c("waiver", "numeric"), null.ok = TRUE
  )

  prettycheck:::assert_flag(reverse)
  prettycheck:::assert_flag(change_sign)
  prettycheck:::assert_flag(print)

  if (y_label == col_y && hms::is_hms(data[[col_y]])) {
    y_label = paste0("Local time (", col_y, " +- SEM)")
  }

  if (y_label == col_y && prettycheck:::test_duration(data[[col_y]])) {
    y_label = paste0("Duration (", col_y, " +- SEM)")
  }

  data <-
    data |>
    dplyr::select(dplyr::all_of(c(col_x, col_y, col_group))) |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::where(prettycheck:::test_duration),
      .fns = ~ hms::hms(as.numeric(.x))
    )) |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::where(hms::is_hms),
      .fns = midday_trigger
    )) |>
    dplyr::mutate(
      !!as.symbol(col_x) := ggplot2::cut_width(
        !!as.symbol(col_x), width = 1, boundary = boundary, closed = "right"
      )
    )

  if (isTRUE(reverse)) {
    data <-
      data |>
      dplyr::mutate(
        !!as.symbol(col_x) :=
          `levels<-`(
            !!as.symbol(col_x),
            cut_mean(levels(!!as.symbol(col_x))) |>
              change_sign(change_sign)
          ) |>
          forcats::fct_rev()
      )
  } else {
    data <-
      data |>
      dplyr::mutate(
        !!as.symbol(col_x) :=
          `levels<-`(
            !!as.symbol(col_x),
            cut_mean(levels(!!as.symbol(col_x))) |>
              change_sign(change_sign)
          )
      )
  }

  if (isTRUE(reverse)) {
    data <-
      data |>
      dplyr::mutate(
        !!as.symbol(col_x) :=
          `levels<-`(
            !!as.symbol(col_x),
            levels(!!as.symbol(col_x)) |> forcats::fct_rev()
          )
      )
  }

  data_by_col_x <-
    data |>
    dplyr::summarise(
      std_error = std_error(!!as.symbol(col_y)),
      !!as.symbol(col_y) := mean(!!as.symbol(col_y), na.rm = TRUE),
      .by = !!as.symbol(col_x)
    ) |>
    rutils::shush() |>
    tidyr::drop_na()

  data_by_col_x_and_col_group <-
    data |>
    dplyr::summarise(
      std_error = std_error(!!as.symbol(col_y)),
      !!as.symbol(col_y) := mean(!!as.symbol(col_y), na.rm = TRUE),
      .by = c(!!as.symbol(col_x), !!as.symbol(col_group))
    ) |>
    rutils::shush() |>
    tidyr::drop_na()

  plot <-
    ggplot2::ggplot() +
    ggplot2::geom_line(
      ggplot2::aes(x = !!as.symbol(col_x), y = !!as.symbol(col_y), group = 1),
      data = data_by_col_x,
      linewidth = line_width,
      color = "gray"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = !!as.symbol(col_x),
        y = !!as.symbol(col_y),
        color = !!as.symbol(col_group)
      ),
      data = data_by_col_x_and_col_group,
      size = point_size
    ) +
    ggplot2::scale_x_discrete(breaks = label_decimal_fix) +
    {
      if (isTRUE(thematic)) {
        scale_color_brand_d()
      } else {
        viridis::scale_color_viridis(
          begin = 0.5,
          end = 0.75,
          discrete = TRUE,
          option = viridis
        )
      }
    } +
    add_labels(
      x = x_label,
      y = y_label,
      title = title,
      subtitle = subtitle,
      color = color_label
    ) +
    add_theme(
      theme = theme,
      text_size = text_size
    )

  if (inherits(data[[col_x]], c("POSIXt", "hms", "Duration"))) {
    plot <-
      plot +
      ggplot2::scale_x_datetime(
        date_breaks = date_breaks,
        minor_breaks = date_minor_breaks,
        date_labels = "%H:%M:%S",
      )
  }

  if (inherits(data[[col_y]], c("POSIXt", "hms", "Duration"))) {
    plot <-
      plot +
      ggplot2::scale_y_datetime(
        date_breaks = date_breaks,
        minor_breaks = date_minor_breaks,
        date_labels = "%H:%M:%S",
      )
  }

  if (isTRUE(error_bar)) {
    plot <-
      plot +
      ggplot2::geom_errorbar(
        ggplot2::aes(
          x = !!as.symbol(col_x),
          y = !!as.symbol(col_y),
          ymin = !!as.symbol(col_y) - std_error,
          ymax=!!as.symbol(col_y) + std_error,
          color = !!as.symbol(col_group)
        ),
        data = data_by_col_x_and_col_group,
        show.legend = FALSE,
        inherit.aes = FALSE,
        width = error_bar_width,
        linewidth = error_bar_linewidth
      )
  }

  if (isTRUE(print)) print(plot)

  invisible(plot)
}
