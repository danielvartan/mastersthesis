# library(dplyr)
library(GGally)
library(ggplot2)
# library(hms)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils
# library(tidyr)
# library(viridis)

source(here::here("R", "utils.R"))
source(here::here("R", "utils-plots.R"))

plot_ggally <- function(
    data,
    cols = names(data),
    mapping = NULL, # ggplot2::aes(colour = sex)
    axis_labels = "none",
    na_rm = TRUE,
    theme = "bw",
    text_size = NULL,
    print = TRUE,
    ...
  ) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_character(cols)
  prettycheck:::assert_subset(cols, names(data))
  prettycheck:::assert_class(mapping, "uneval", null.ok = TRUE)
  prettycheck:::assert_choice(axis_labels, c("show", "internal", "none"))
  prettycheck:::assert_flag(na_rm)
  prettycheck:::assert_flag(print)

  out <-
    data|>
    dplyr::select(dplyr::all_of(cols))|>
    dplyr::mutate(
      dplyr::across(
      .cols = dplyr::where(hms::is_hms),
      .fns = ~ midday_trigger(.x)
      ),
      dplyr::across(
        .cols = dplyr::where(
          ~ !is.character(.x) && !is.factor(.x) &&
            !is.numeric(.x) && !hms::is_hms(.x)
        ),
        .fns = ~ as.numeric(.x)
      )
    )

  if (isTRUE(na_rm)) out <- out |> tidyr::drop_na()

  if (is.null(mapping)) {
    plot <-
      out|>
      GGally::ggpairs(
        lower = list(continuous = "smooth"),
        axisLabels = axis_labels,
        ...
      )
  } else {
    plot <-
      out|>
      GGally::ggpairs(
        mapping = mapping,
        axisLabels = axis_labels,
        ...
      ) +
      viridis::scale_color_viridis(
        begin = 0.25,
        end = 0.75,
        discrete = TRUE,
        option = "viridis"
      ) +
      viridis::scale_fill_viridis(
        begin = 0.25,
        end = 0.75,
        discrete = TRUE,
        option = "viridis"
      )
  }

  plot <-
    plot +
    add_theme(
      theme = theme,
      text_size = text_size,
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )

  if (isTRUE(print)) print(plot)

  invisible(plot)
}
