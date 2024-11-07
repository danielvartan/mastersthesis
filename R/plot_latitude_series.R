# library(dplyr)
library(ggplot2)
# library(here)
# library(hms)
# library(latex2exp)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rlang)
# library(rutils) # github.com/danielvartan/rutils
# library(tidyr)

source(here::here("R/utils.R"))
source(here::here("R/utils-plots.R"))
source(here::here("R/utils-stats.R"))

plot_latitude_series <- function(
    data,
    col = "msf_sc",
    y_lab = col,
    line_width = 2,
    point_size = 1,
    error_bar_width = 0.5,
    error_bar_linewidth = 0.5,
    error_bar = TRUE,
    text_size = NULL
  ) {
  col_classes <- c("numeric", "integer", "POSIXt", "hms", "Duration")

  prettycheck:::assert_tibble(data)
  prettycheck:::assert_string(col)
  prettycheck:::assert_subset(col, names(data))
  prettycheck:::assert_subset(c("latitude", col), names(data))
  prettycheck:::assert_multi_class(data[[col]], col_classes)
  prettycheck:::assert_multi_class(y_lab, c("character", "latexexpression"))
  prettycheck:::assert_length(y_lab, len = 1)
  prettycheck:::assert_number(line_width)
  prettycheck:::assert_number(point_size)
  prettycheck:::assert_number(error_bar_width)
  prettycheck:::assert_number(error_bar_linewidth)
  prettycheck:::assert_flag(error_bar)
  prettycheck:::assert_number(text_size, null.ok = TRUE)

  if (y_lab == col && hms::is_hms(data[[col]])) {
    y_lab = paste0("Local time (", col, " +- SEM)")
  }

  if (y_lab == col && prettycheck:::test_duration(data[[col]])) {
    y_lab = paste0("Duration (", col, " +- SEM)")
  }

  data <-
    data |>
    dplyr::select(dplyr::all_of(c("latitude", col))) |>
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
    dplyr::group_by(latitude) |>
    dplyr::summarise(
      std_error = std_error(!!as.symbol(col)),
      !!as.symbol(col) := mean(!!as.symbol(col), na.rm = TRUE)
    ) |>
    rutils::shush() |>
    tidyr::drop_na()

  plot <-
    ggplot2::ggplot(
      ggplot2::aes(x = latitude, y = !!as.symbol(col)),
      data = data
      ) +
    ggplot2::geom_point(
      ggplot2::aes(x = latitude, y = !!as.symbol(col)),
      data = data,
      size = point_size,
      color = "black"
    ) +
    ggplot2::stat_smooth(
      method = "lm",
      formula = y ~ x,
      geom = "smooth",
      color = "red"
    ) +
    ggplot2::scale_x_reverse() +
    ggplot2::labs(x = "Latitude", y = y_lab) +
    ggplot2::theme(text = ggplot2::element_text(size = text_size))

  if (inherits(data[[col]], c("POSIXt", "hms", "Duration"))) {
    plot <-
      plot +
      ggplot2::scale_y_datetime(date_labels = "%H:%M:%S")
  }

  if (isTRUE(error_bar)) {
    plot <-
      plot +
      ggplot2::geom_errorbar(
        ggplot2::aes(
          x = latitude,
          y = !!as.symbol(col),
          ymin = !!as.symbol(col) - std_error,
          ymax=!!as.symbol(col) + std_error
        ),
        data = data,
        show.legend = FALSE,
        inherit.aes = FALSE,
        width = error_bar_width,
        linewidth = error_bar_linewidth
      )
  }

  print(plot)
  invisible(plot)
}
