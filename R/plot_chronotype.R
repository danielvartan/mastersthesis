# # TO DO:
#
# * Index the local time vector in a timeline of 2 or 3 days in length and
#   scan data density in order to choose the densest data window. Extract
#   the statistics after.
#
#   The window must have an iterative length in order to obtain the desired
#   or best density.

# library(dplyr)
library(ggplot2)
# library(here)
# library(hms)
# library(latex2exp)
# library(lubridate)
# library(lubritime) # github.com/danielvartan/lubritime
# library(prettycheck) # github.com/danielvartan/prettycheck
library(rlang)
# library(rutils) # github.com/danielvartan/rutils
# library(stats)
# library(tidyr)

source(here::here("R", "get_chronotype_cutoffs.R"))
source(here::here("R", "utils.R"))
source(here::here("R", "utils-plots.R"))

# # Helpers
#
# # Possible bins
# out |>
#   dplyr::transmute(
#     interval = cut(
#       !!as.symbol(col_msf_sc),
#       breaks = seq( # Two-day timeline
#         from  = 0 + (15 * 60), # 15 minutes (left buffer)
#         to = ((60 * 60 * 24) * 2) + (15 * 60), # 48 hours + 15 minutes
#         by = 30 * 60 # 30 minutes
#       ),
#       dig.lab = 10
#     )
#   ) |>
#   dplyr::pull(interval) |>
#   levels() |>
#   cut_mean() |>
#   as.POSIXct(tz = "UTC")

plot_chronotype <- function(
    data,
    col_msf_sc = "msf_sc",
    col_width = 0.8,
    col_border = 0.1,
    thematic = TRUE,
    thematic_color_type = "div",
    direction = 1,
    reverse = FALSE,
    x_label = "Frequency (%)",
    y_label = latex2exp::TeX("Local time ($MSF_{sc}$)"),
    fill_label = NULL, # "Chronotype"
    theme = "bw",
    legend = TRUE,
    legend_position = "right",
    text_size = NULL,
    print = TRUE,
    ...
  ) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_choice(col_msf_sc, names(data))
  prettycheck:::assert_number(col_width, lower = 0)
  prettycheck:::assert_number(col_border, lower = 0)
  prettycheck:::assert_flag(thematic)
  prettycheck:::assert_choice(direction, c(-1, 1))
  prettycheck:::assert_flag(reverse)
  prettycheck:::assert_flag(print)

  if (is.null(y_label)) {
    if (hms::is_hms(data[[col_msf_sc]])) {
      y_label <- paste0("Local time (", col_msf_sc, ")")
    } else if (lubridate::is.duration(data[[col_msf_sc]])) {
      y_label <- paste0("Duration (", col_msf_sc, ")")
    } else {
      y_label <- col_msf_sc
    }
  }

  out <-
    data |>
    dplyr::select(!!as.symbol(col_msf_sc)) |>
    tidyr::drop_na() |>
    dplyr::mutate(
      !!as.symbol(col_msf_sc) := transform_time(!!as.symbol(col_msf_sc))
    )

  aes <-
    out |>
    dplyr::group_by(
      interval = cut( # Possibles bins.
        !!as.symbol(col_msf_sc),
        breaks = seq( # Two-day timeline: 1970-01-01--1970-01-02
          from  = 0 + (15 * 60), # 15 minutes (left buffer)
          to = ((60 * 60 * 24) * 2) + (15 * 60), # 48 hours + 15 minutes
          by = 30 * 60 # 30 minutes
        ),
        dig.lab = 10
      )
    ) |>
    dplyr::summarise(freq = dplyr::n()) |>
    dplyr::mutate(
      order = seq_along(interval),
      # Transform bins in `Interval` objects.
      interval = transform_cut_levels(as.character(interval)),
      freq = (freq / sum(freq)) * 100,
      label =
        lubritime:::int_mean(
          lubridate::int_start(interval),
          lubridate::int_end(interval)
        ) |>
        lubritime::round_time() |>
        lubritime:::fix_hms()
    )

  #            00:30           01:00           01:30
  # -----|---------------|---------------|---------------|------
  #    00:15           00:45           01:15           01:45

  fill <- data |> get_chronotype_cutoffs(col_msf_sc = "msf_sc", pretty = FALSE)

  out <-
    aes |>
    dplyr::mutate(
      ee = lubritime:::int_overlap(fill$interval[1], interval),
      me = lubritime:::int_overlap(fill$interval[2], interval),
      se = lubritime:::int_overlap(fill$interval[3], interval),
      int = lubritime:::int_overlap(fill$interval[4], interval),
      sl = lubritime:::int_overlap(fill$interval[5], interval),
      ml = lubritime:::int_overlap(fill$interval[6], interval),
      el = lubritime:::int_overlap(fill$interval[7], interval),
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(c("ee", "me", "se", "int", "sl", "ml", "el")),
        .fns = ~ hms::hms(as.numeric(.x))
      )
    ) |>
    dplyr::mutate(
      fill =
        mapply(
          FUN = (\(a, b, c, d, e, f, g) which.max(c(a, b, c, d, e, f, g))),
          ee, me, se, int, sl, ml, el
        ) |>
        factor(
          levels = seq(7),
          labels = c(
            "Extremely early", "Moderately early", "Slightly early",
            "Intermediate", "Slightly late", "Moderately late",
            "Extremely late"
            ),
          ordered = TRUE
        )
    ) |>
    dplyr::select(order, interval, freq, fill, label)

  plot <-
    out |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = freq,
        y = stats::reorder(label, -order),
        fill = fill
        )
    ) +
    ggplot2::geom_col(
      width = col_width,
      colour = "#000000",
      linewidth = col_border
    ) +
    ggplot2::scale_x_continuous(minor_breaks = NULL) +
    ggplot2::scale_y_discrete(labels = labels_char_hms) +
    {
      if (isTRUE(thematic)) {
        scale_fill_brand_d(
          color_type = thematic_color_type,
          direction = direction,
          reverse = reverse
        )
      } else {
        scale_fill_rainbow(direction = direction)
      }
    } +
    add_labels(
      x = x_label,
      y = y_label,
      fill = fill_label
    ) +
    add_theme(
      theme = theme,
      legend = TRUE,
      legend_position = legend_position,
      text_size = text_size,
      ...
    )

  if (isTRUE(print)) print(plot)

  invisible(plot)
}
