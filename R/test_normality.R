# library(checkmate, quietly = TRUE)
# library(cowplot, quietly = TRUE)
# library(fBasics, quietly = TRUE)
# library(here, quietly = TRUE)
# library(hms, quietly = TRUE)
# library(lubridate, quietly = TRUE)
# library(moments, quietly = TRUE)
# library(nortest, quietly = TRUE)
# library(rutils, quietly = TRUE)
# library(stats, quietly = TRUE)
# library(tseries, quietly = TRUE)

source(here::here("R/stats_sum.R"))
source(here::here("R/utils-stats.R"))
source(here::here("R/utils.R"))

test_normality <- function(x,
                           name = "x",
                           threshold = hms::parse_hms("12:00:00"),
                           remove_outliers = FALSE,
                           iqr_mult = 1.5,
                           log_transform = FALSE,
                           density_line = TRUE,
                           text_size = NULL,
                           print = TRUE) {
  classes <- c(
    "numeric", "Duration", "difftime", "hms", "POSIXt", "Interval"
  )

  checkmate::assert_atomic(x)
  checkmate::assert_multi_class(x, classes)
  checkmate::assert_string(name)
  rutils:::assert_hms(
    threshold, lower = hms::hms(0), upper = hms::parse_hms("23:59:59"),
    null.ok = TRUE
  )
  checkmate::assert_flag(remove_outliers)
  checkmate::assert_number(iqr_mult)
  checkmate::assert_flag(log_transform)
  checkmate::assert_flag(density_line)
  checkmate::assert_number(text_size, null.ok = TRUE)
  checkmate::assert_flag(print)

  n <- x |> length()
  class = x |> class()
  is_temporal <- x |> rutils:::test_temporal()
  tz <- ifelse(lubridate::is.POSIXt(x), lubridate::tz(x), "UTC")
  n_rm_na <- x |> length()

  if (rutils:::test_temporal(x)) {
    x <- x |> transform_time(threshold = threshold)
  }

  if (isTRUE(remove_outliers)) {
    x <- x |> remove_outliers(method = "iqr", iqr_mult = iqr_mult)
  }

  if (isTRUE(log_transform)) {
    x <-
      x |>
      log() |>
      drop_inf()
  }

  if (n_rm_na >= 7) {
    ad <- x |> nortest::ad.test()

    cvm <-
      x |>
      nortest::cvm.test() |>
      rutils:::shush()
  } else {
    ad <- NULL
    cmv <- NULL
  }

  bonett <- x |> moments::bonett.test()

  # See also `Rita::DPTest()` (just for Omnibus (K) tests).
  dagostino <-
    x |>
    fBasics::dagoTest() |>
    rutils:::shush()

  jarque_bera <-
    rutils:::drop_na(x) |>
    tseries::jarque.bera.test()

  if (n_rm_na >= 4) {
    lillie_ks <- x |> nortest::lillie.test()
  } else {
    lillie_ks <- NULL
  }

  pearson <- x |> nortest::pearson.test()

  if (n_rm_na >= 5 && n_rm_na <= 5000) {
    sf <- x |> nortest::sf.test()
  } else {
    sf <- NULL
  }

  if (n_rm_na >= 3 && n_rm_na <= 3000) {
    shapiro <- x |> stats::shapiro.test()
  } else {
    shapiro <- NULL
  }

  if (isTRUE(is_temporal) && isFALSE(log_transform)) {
    x <- x |> lubridate::as_datetime(tz = tz)
  }

  qq_plot <- x |> plot_qq(text_size = text_size, print = FALSE)

  hist_plot <-
    x |>
    plot_hist(
      x_lab = name,
      text_size = text_size,
      density_line = density_line,
      print = FALSE
      )

  grid_plot <- cowplot::plot_grid(hist_plot, qq_plot, ncol = 2, nrow = 1)

  out <- list(
    stats = stats_sum(
      x,
      threshold = NULL,
      na_rm = TRUE,
      remove_outliers = FALSE,
      hms_format = TRUE,
      print = print
    ),
    params = list(
      name = name,
      class = class,
      threshold = threshold,
      remove_outliers = remove_outliers,
      log_transform = log_transform,
      density_line = density_line
    ),

    ad = ad,
    bonett = bonett,
    cvm = cvm,
    dagostino = dagostino,
    jarque_bera = jarque_bera,
    lillie_ks = lillie_ks,
    pearson = pearson,
    sf = sf,
    shapiro = shapiro,

    hist_plot = hist_plot,
    qq_plot = qq_plot,
    grid_plot = grid_plot
  )

  if (isTRUE(print)) print(grid_plot)

  invisible(out)
}

# library(checkmate, quietly = TRUE)
# library(dplyr, quietly = TRUE)
library(ggplot2, quietly = TRUE)
# library(rutils, quietly = TRUE)

plot_qq <- function(x,
                    text_size = NULL,
                    na_rm = TRUE,
                    print = TRUE) {
  checkmate::assert_atomic(x)
  checkmate::assert_multi_class(x, c("numeric", "POSIXt"))
  checkmate::assert_number(text_size, null.ok = TRUE)
  checkmate::assert_flag(na_rm)
  checkmate::assert_flag(print)

  if (isTRUE(na_rm)) x <- x |> rutils:::drop_na()

  plot <-
    dplyr::tibble(y = x) |>
    ggplot2::ggplot(ggplot2::aes(sample = y)) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line(color = "red", linewidth = 1) +
    ggplot2::labs(
      x = "Theoretical quantiles (Std. normal)",
      y = "Sample quantiles"
    ) +
    ggplot2::theme(text = ggplot2::element_text(size = text_size))

  if (inherits(x, "POSIXt")) {
    plot <- plot + ggplot2::scale_y_datetime(date_labels = "%H:%M")
  }

  if (isTRUE(print)) print(plot)
  invisible(plot)
}

# library(checkmate, quietly = TRUE)
# library(dplyr, quietly = TRUE)
library(ggplot2, quietly = TRUE)
# library(rutils, quietly = TRUE)

plot_hist <- function(x,
                      x_lab = "x",
                      stat = "density",
                      text_size = NULL,
                      density_line = TRUE,
                      na_rm = TRUE,
                      print = TRUE) {
  checkmate::assert_atomic(x)
  checkmate::assert_multi_class(x, c("numeric", "POSIXt"))
  checkmate::assert_string(x_lab)
  checkmate::assert_choice(stat, c("count", "density"))
  checkmate::assert_number(text_size, null.ok = TRUE)
  checkmate::assert_flag(density_line)
  checkmate::assert_flag(na_rm)
  checkmate::assert_flag(print)

  if (isTRUE(na_rm)) x <- x |> rutils:::drop_na()
  y_lab <- ifelse("count", "Frequency", "Density")

  plot <-
    dplyr::tibble(y = x) |>
    ggplot2::ggplot(ggplot2::aes(x = y)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(!!as.symbol(stat))),
      bins = 30, color = "white"
    ) +
    ggplot2::labs(x = x_lab, y = y_lab) +
    ggplot2::theme(text = ggplot2::element_text(size = text_size))

  if (stat == "density" && isTRUE(density_line)) {
    plot <- plot + ggplot2::geom_density(color = "red", linewidth = 1)
  }

  if (inherits(x, "POSIXt")) {
    plot <- plot + ggplot2::scale_x_datetime(date_labels = "%H:%M:%S")
  }

  if (isTRUE(print)) print(plot)
  invisible(plot)
}
