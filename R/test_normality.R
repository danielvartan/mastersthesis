# library(fBasics)
# library(here)
# library(hms)
# library(lubridate)
# library(moments)
# library(nortest)
# library(patchwork)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils
# library(stats)
# library(tseries)

source(here::here("R", "plot_hist.R"))
source(here::here("R", "plot_qq.R"))
source(here::here("R", "stats_summary.R"))
source(here::here("R", "utils.R"))
source(here::here("R", "utils-stats.R"))

test_normality <- function(
    data,
    col,
    name = col,
    remove_outliers = FALSE,
    iqr_mult = 1.5,
    log_transform = FALSE,
    density_line = TRUE,
    threshold = hms::parse_hms("12:00:00"),
    print = TRUE
  ) {
  classes <- c(
    "numeric", "integer", "Duration", "difftime", "hms", "POSIXt", "Interval"
  )

  prettycheck:::assert_tibble(data)
  prettycheck:::assert_choice(col, names(data))
  prettycheck:::assert_multi_class(data[[col]], classes)
  prettycheck:::assert_string(name)
  prettycheck:::assert_flag(remove_outliers)
  prettycheck:::assert_number(iqr_mult, lower = 1)
  prettycheck:::assert_flag(log_transform)
  prettycheck:::assert_flag(density_line)
  prettycheck:::assert_flag(print)

  prettycheck:::assert_hms(
    threshold, lower = hms::hms(0), upper = hms::parse_hms("23:59:59"),
    null_ok = TRUE
  )

  x <- data |> dplyr::pull(col)
  n <- x |> length()
  class = x |> class()
  tz <- ifelse(lubridate::is.POSIXt(x), lubridate::tz(x), "UTC")
  n_rm_na <- x |> rutils:::drop_na() |> length()

  if (prettycheck:::test_temporal(x)) {
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
      rutils::shush()
  } else {
    ad <- NULL
    cmv <- NULL
  }

  bonett <- x |> moments::bonett.test()

  # See also `Rita::DPTest()` (just for Omnibus (K) tests).
  dagostino <-
    x |>
    fBasics::dagoTest() |>
    rutils::shush()

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

  if (isTRUE(prettycheck:::test_temporal(x)) && isFALSE(log_transform)) {
    x <- x |> lubridate::as_datetime(tz = tz)
  }

  qq_plot <-
    dplyr::tibble(x = x) |>
    plot_qq(
      col = "x",
      print = FALSE
    )

  hist_plot <-
    dplyr::tibble(x = x) |>
    plot_hist(
      col = "x",
      density_line = density_line,
      x_label = name,
      print = FALSE
      )

  grid_plot <- patchwork::wrap_plots(hist_plot, qq_plot,  ncol = 2)

  out <- list(
    stats =
      dplyr::tibble(x = x) |>
      stats_summary(
        "x",
        name = name,
        na_rm = TRUE,
        remove_outliers = FALSE,
        hms_format = TRUE,
        threshold = NULL,
        as_list = TRUE
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
