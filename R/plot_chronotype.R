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

source(here::here("R/utils.R"))
source(here::here("R/utils-plots.R"))

plot_chronotype <- function(
    data,
    col = "msf_sc",
    x_lab = "Frequency (%)",
    y_lab = col,
    col_width = 0.8,
    col_border = 0.6,
    text_size = NULL,
    legend_position = "right",
    chronotype_cuts = FALSE
  ) {
  prettycheck:::assert_tibble(data, min.rows = 1, min.cols = 1)
  prettycheck:::assert_choice(col, names(data))
  prettycheck:::assert_multi_class(x_lab, c("character", "latexexpression"))
  prettycheck:::assert_length(x_lab, len = 1)
  prettycheck:::assert_multi_class(y_lab, c("character", "latexexpression"))
  prettycheck:::assert_length(y_lab, len = 1)
  prettycheck:::assert_number(col_width)
  prettycheck:::assert_number(col_border)
  prettycheck:::assert_number(text_size, null.ok = TRUE)
  prettycheck:::assert_choice(legend_position, c("left","top", "right", "bottom"))
  prettycheck:::assert_flag(chronotype_cuts)

  if (is.null(y_lab)) {
    if (hms::is_hms(data[[col]])) {
      y_lab <- paste0("Local time (", col, ")")
    } else if (lubridate::is.duration(data[[col]])) {
      y_lab <- paste0("Duration (", col, ")")
    } else {
      y_lab <- col
    }
  }

  out <-
    data |>
    dplyr::select(!!as.symbol(col)) |>
    tidyr::drop_na() |>
    dplyr::mutate(!!as.symbol(col) := transform_time(!!as.symbol(col)))

  aes <-
    out |>
    dplyr::group_by(
      interval = cut(
        !!as.symbol(col),
        breaks = seq(
          0 + (15 * 60), ((60 * 60 * 24) * 2) + (15 * 60),
          by = 30 * 60
          ),
        dig.lab = 10
      )
    ) |>
    dplyr::summarise(freq = dplyr::n()) |>
    dplyr::mutate(
      order = seq_along(interval),
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

  ## See `ggplot2::cut_interval()`.
  probs <- c(
    0 / 3 / 3,
    1 / 3 / 3,
    2 / 3 / 3,
    3 / 3 / 3,
    6 / 3 / 3,
    7 / 3 / 3,
    8 / 3 / 3,
    9 / 3 / 3
  )

  fill <-
    out |>
    dplyr::group_by(
      interval = cut(
        !!as.symbol(col),
        breaks = quantile(
          transform_time(rutils:::drop_na(!!as.symbol(col))),
          probs
        ),
        dig.lab = 10,
        include.lowest = TRUE
      )
    ) |>
    dplyr::summarise(freq = dplyr::n()) |>
    dplyr::mutate(
      order = seq_along(interval),
      interval = transform_cut_levels(as.character(interval)),
      freq = (freq / sum(freq)) * 100,
      label = c(
        "Extremely early", "Moderately early", "Slightly early",
        "Intermediate", "Slightly late", "Moderately late", "Extremely late"
        )
    )

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
    # ggplot2::scale_fill_manual(
    #     values = c(
    #       "#fde725", "#90d743", "#35b779", "#21918c", "#31688e", "#443983",
    #       "#440154"
    #     )
    # ) +
    ggplot2::scale_fill_manual(
      values = c(
        "#FF1E00", "#FF7C00", "#FDD400", "#00CB00", "#009DF5", "#0040F7",
        "#981EAF"
      )
    ) +
    ggplot2::labs(x = x_lab, y = y_lab, fill = "Chronotype") +
    ggplot2::theme(
      # panel.grid.major.y = ggplot2::element_line(
      #     size = 0.6,
      #     linetype = "longdash",
      #     colour = "#C4C4C4"
      # ),
      # panel.grid.major.x = ggplot2::element_line(
      #     size = 0,
      #     linetype = "blank",
      #     colour = "white"
      # ),
      # panel.grid.minor.x = ggplot2::element_line(
      #     size = 0,
      #     linetype = "blank",
      #     colour = "white"
      # ),
      text = ggplot2::element_text(size = text_size),
      legend.position = legend_position
    )

  if (isTRUE(chronotype_cuts)) {
    invisible(fill)
  } else {
    print(plot)
    invisible(plot)
  }
}
