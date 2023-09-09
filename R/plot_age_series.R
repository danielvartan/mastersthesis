## TODO:
##
## * Arrumar cuts igual Roenneberg et al. (2007). Ver cuts acima de 60.
## * Criar `plot_msf_sc series()`.

plot_age_series <- function(data, col = "msf_sc", y_lab = col, line_width = 2,
                            boundary = 0.5, point_size = 1,
                            error_bar_width = 0.5,
                            error_bar_linewidth = 0.5, error_bar = TRUE,
                            text_size = NULL) {
  col_classes <- c("numeric", "integer", "POSIXt", "hms", "Duration")

  checkmate::assert_tibble(data)
  checkmate::assert_string(col)
  checkmate::assert_subset(col, names(data))
  checkmate::assert_subset(c("sex", "age", col), names(data))
  checkmate::assert_multi_class(data[[col]], col_classes)
  checkmate::assert_string(y_lab)
  checkmate::assert_number(line_width)
  checkmate::assert_number(boundary)
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
    dplyr::select(dplyr::all_of(c("age", col, "sex"))) %>%
    dplyr::mutate(dplyr::across(
      .cols = dplyr::where(gutils:::test_duration),
      .fns = ~ hms::hms(as.numeric(.x))
    )) %>%
    dplyr::mutate(dplyr::across(
      .cols = dplyr::where(hms::is_hms),
      .fns = midday_trigger
    )) %>%
    dplyr::mutate(
      age = ggplot2::cut_width(
        age, width = 1, boundary = boundary, closed = "right"
      )
    ) %>%
    dplyr::mutate(age = `levels<-`(age, cut_mean(levels(age))))

  data_by_age <- data %>%
    dplyr::summarise(
      std_error = std_error(!!as.symbol(col)),
      !!as.symbol(col) := mean(!!as.symbol(col), na.rm = TRUE),
      .by = age
    ) %>%
    gutils:::shush() %>%
    tidyr::drop_na()

  data_by_age_and_sex <- data %>%
    dplyr::summarise(
      std_error = std_error(!!as.symbol(col)),
      !!as.symbol(col) := mean(!!as.symbol(col), na.rm = TRUE),
      .by = c(age, sex)
    ) %>%
    gutils:::shush() %>%
    tidyr::drop_na()

  plot <- ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(
      x = age, y = !!as.symbol(col), group = 1
    ),
    data = data_by_age, linewidth = line_width, color = "gray"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = age, y = !!as.symbol(col), color = sex
      ),
      data = data_by_age_and_sex, size = point_size
    ) +
    ggplot2::scale_x_discrete(
      breaks = label_decimal_fix
    ) +
    ggplot2::labs(x = "Age", y = y_lab) +
    viridis::scale_color_viridis(
      name = "Sex", begin = 0.25, end = 0.75, discrete = TRUE,
      option = "viridis"
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
          x = age, y = !!as.symbol(col),
          ymin = !!as.symbol(col) - std_error,
          ymax=!!as.symbol(col) + std_error,
          color = sex
        ),
        data = data_by_age_and_sex,
        show.legend = FALSE, inherit.aes = FALSE,
        width = error_bar_width, linewidth = error_bar_linewidth
      )
  }

  print(plot)
  invisible(plot)
}
