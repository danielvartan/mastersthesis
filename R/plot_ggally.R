# library(dplyr)
library(GGally)
library(ggplot2)
# library(hms)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils
# library(tidyr)

source(here::here("R", "utils.R"))
source(here::here("R", "utils-plots.R"))

plot_ggally <- function(
    data,
    cols = names(data),
    mapping = NULL, # ggplot2::aes(colour = sex)
    axis_labels = "none",
    na_rm = TRUE,
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
        .fns = function(x) {
          x |>
            lubritime:::link_to_timeline(
              threshold = hms::parse_hms("12:00:00")
            ) |>
            as.numeric()
        }
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
      scale_color_brand_d() +
      scale_fill_brand_d()
  }

  plot <-
    plot +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  if (isTRUE(print)) print(plot)

  invisible(plot)
}
