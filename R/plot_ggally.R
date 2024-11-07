# library(dplyr)
library(GGally)
library(ggplot2)
# library(hms)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(tidyr)
# library(viridis)

source(here::here("R/utils.R"))

# ggplot2::aes(colour = sex)
plot_ggally <- function(data,
                        cols,
                        mapping = NULL,
                        axis_labels = "none",
                        na_rm = TRUE,
                        text_size = NULL) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_character(cols)
  prettycheck:::assert_subset(cols, names(data))
  prettycheck:::assert_class(mapping, "uneval", null.ok = TRUE)
  prettycheck:::assert_choice(axis_labels, c("show", "internal", "none"))
  prettycheck:::assert_flag(na_rm)
  prettycheck:::assert_number(text_size, null.ok = TRUE)

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

  if (isTRUE(na_rm)) {
    out <- out|> tidyr::drop_na(dplyr::all_of(cols))
  }

  if (is.null(mapping)) {
    plot <-
      out|>
      GGally::ggpairs(
        lower = list(continuous = "smooth"),
        axisLabels = axis_labels
      )
  } else {
    plot <-
      out|>
      GGally::ggpairs(
        mapping = mapping,
        axisLabels = axis_labels
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

  plot <- plot +
    ggplot2::theme(
      text = ggplot2::element_text(size = text_size)
    )

  print(plot)
  invisible(plot)
}
