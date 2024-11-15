# library(dplyr)
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

# # TO DO
#
# - Add option to cut-offs like Roenneberg et al. (2007).
#    See cut-offs above 60.
# - Create `plot_msf_sc series()`.

plot_age_series <- function(data,
                            col = "msf_sc",
                            y_lab = col,
                            line_width = 2,
                            boundary = 0.5,
                            point_size = 1,
                            error_bar_width = 0.5,
                            error_bar_linewidth = 0.5,
                            error_bar = TRUE,
                            text_size = NULL) {
  col_classes <- c("numeric", "integer", "POSIXt", "hms", "Duration")

  prettycheck:::assert_tibble(data)
  prettycheck:::assert_string(col)
  prettycheck:::assert_subset(col, names(data))
  prettycheck:::assert_subset(c("sex", "age", col), names(data))
  prettycheck:::assert_multi_class(data[[col]], col_classes)
  prettycheck:::assert_multi_class(y_lab, c("character", "latexexpression"))
  prettycheck:::assert_length(y_lab, len = 1)
  prettycheck:::assert_number(line_width)
  prettycheck:::assert_number(boundary)
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
    dplyr::select(dplyr::all_of(c("age", col, "sex"))) |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::where(prettycheck:::test_duration),
      .fns = ~ hms::hms(as.numeric(.x))
    )) |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::where(hms::is_hms),
      .fns = midday_trigger
    )) |>
    dplyr::mutate(
      age = ggplot2::cut_width(
        age, width = 1, boundary = boundary, closed = "right"
      )
    ) |>
    dplyr::mutate(age = `levels<-`(age, cut_mean(levels(age))))

  data_by_age <-
    data |>
    dplyr::summarise(
      std_error = std_error(!!as.symbol(col)),
      !!as.symbol(col) := mean(!!as.symbol(col), na.rm = TRUE),
      .by = age
    ) |>
    rutils::shush() |>
    tidyr::drop_na()

  data_by_age_and_sex <-
    data |>
    dplyr::summarise(
      std_error = std_error(!!as.symbol(col)),
      !!as.symbol(col) := mean(!!as.symbol(col), na.rm = TRUE),
      .by = c(age, sex)
    ) |>
    rutils::shush() |>
    tidyr::drop_na()

  plot <-
    ggplot2::ggplot() +
    ggplot2::geom_line(
      ggplot2::aes(x = age, y = !!as.symbol(col), group = 1),
      data = data_by_age,
      linewidth = line_width,
      color = "gray"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = age, y = !!as.symbol(col), color = sex),
      data = data_by_age_and_sex,
      size = point_size
    ) +
    ggplot2::scale_x_discrete(breaks = label_decimal_fix) +
    ggplot2::labs(x = "Age", y = y_lab) +
    viridis::scale_color_viridis(
      name = "Sex",
      begin = 0.25,
      end = 0.75,
      discrete = TRUE,
      option = "viridis"
    ) +
    ggplot2::theme(text = ggplot2::element_text(size = text_size))

  if (inherits(data[[col]], c("POSIXt", "hms", "Duration"))) {
    plot <- plot + ggplot2::scale_y_datetime(date_labels = "%H:%M:%S")
  }

  if (isTRUE(error_bar)) {
    plot <-
      plot +
      ggplot2::geom_errorbar(
        ggplot2::aes(
          x = age,
          y = !!as.symbol(col),
          ymin = !!as.symbol(col) - std_error,
          ymax=!!as.symbol(col) + std_error,
          color = sex
        ),
        data = data_by_age_and_sex,
        show.legend = FALSE,
        inherit.aes = FALSE,
        width = error_bar_width,
        linewidth = error_bar_linewidth
      )
  }

  print(plot)
  invisible(plot)
}

# library(apyramid)
# library(dplyr)
library(ggplot2)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils
# library(tidyr)
# library(viridis)

plot_age_pyramid <- function(data,
                             interval = 10,
                             na_rm = TRUE,
                             text_size = NULL){
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_subset(c("sex", "age"), names(data))
  prettycheck:::assert_number(interval)
  prettycheck:::assert_flag(na_rm)
  prettycheck:::assert_number(text_size, null.ok = TRUE)

  ## TODO:
  ##
  ## * Create a `pretty_breaks()` function.

  plot <- rutils::shush(
    data |>
      dplyr::select(sex, age) |>
      dplyr::mutate(
        age_group = cut(
          age,
          breaks = pretty(age, n = interval),
          right = FALSE,
          include.lowest = TRUE
        )
      ) |>
      tidyr::drop_na() |>
      apyramid::age_pyramid(
        age_group = "age_group",
        split = "sex",
        na.rm = na_rm
      ) +
      ggplot2::labs(x = "Frequency", y = "Age group") +
      viridis::scale_fill_viridis(
        name = "Sex",
        begin = 0.5,
        end = 0.75,
        discrete = TRUE,
        option = "viridis"
      ) +
      ggplot2::theme(
        text = ggplot2::element_text(size = text_size)
      )
  )

  print(plot)
  invisible(plot)
}
