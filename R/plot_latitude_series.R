# # TODO:
#
# * Document functions.

plot_latitude_series <- function(
    data, col = "msf_sc", y_lab = col, line_width = 2,
    point_size = 1, error_bar_width = 0.5, error_bar_linewidth = 0.5,
    error_bar = TRUE, text_size = NULL) {
  col_classes <- c("numeric", "integer", "POSIXt", "hms", "Duration")

  checkmate::assert_tibble(data)
  checkmate::assert_string(col)
  checkmate::assert_subset(col, names(data))
  checkmate::assert_subset(c("latitude", col), names(data))
  checkmate::assert_multi_class(data[[col]], col_classes)
  checkmate::assert_multi_class(y_lab, c("character", "latexexpression"))
  gutils:::assert_length_one(y_lab)
  checkmate::assert_number(line_width)
  checkmate::assert_number(point_size)
  checkmate::assert_number(error_bar_width)
  checkmate::assert_number(error_bar_linewidth)
  checkmate::assert_flag(error_bar)
  checkmate::assert_number(text_size, null.ok = TRUE)

  if (y_lab == col && hms::is_hms(data[[col]])) {
    y_lab = paste0("Local time (", col, " +- SEM)")
  }

  if (y_lab == col && gutils:::test_duration(data[[col]])) {
    y_lab = paste0("Duration (", col, " +- SEM)")
  }

  data <- data %>%
    dplyr::select(dplyr::all_of(c("latitude", col))) |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::where(gutils:::test_duration),
      .fns = ~ hms::hms(as.numeric(.x))
    )) |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::where(hms::is_hms),
      .fns = midday_trigger
    )) |>
    dplyr::group_by(latitude) |>
    dplyr::summarise(
      std_error = std_error(!!as.symbol(col)),
      !!as.symbol(col) := mean(!!as.symbol(col), na.rm = TRUE)
    ) |>
    gutils:::shush() |>
    tidyr::drop_na()

  plot <-
    ggplot2::ggplot(
      ggplot2::aes(x = latitude, y = !!as.symbol(col)), data = data
      ) +
    ggplot2::geom_point(
      ggplot2::aes(x = latitude, y = !!as.symbol(col)),
      data = data, size = point_size, color = "black"
    ) +
    ggplot2::stat_smooth(
      method = "lm", formula = y ~ x, geom = "smooth",
      color = "red"
    ) +
    ggplot2::scale_x_reverse() +
    ggplot2::labs(
      x = "Latitude",
      y = latex2exp::TeX("$MSF_{sc} \\pm SEM$")
      ) +
    ggplot2::theme(
      text = ggplot2::element_text(size = text_size)
    )

  if (inherits(data[[col]], c("POSIXt", "hms", "Duration"))) {
    plot <- plot +
      ggplot2::scale_y_datetime(date_labels = "%H:%M:%S")
  }

if (isTRUE(error_bar)) {
  plot <- plot +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        x = latitude, y = !!as.symbol(col),
        ymin = !!as.symbol(col) - std_error,
        ymax=!!as.symbol(col) + std_error
      ),
      data = data, show.legend = FALSE, inherit.aes = FALSE,
      width = error_bar_width, linewidth = error_bar_linewidth
    )
}

  print(plot)
  invisible(plot)
}
