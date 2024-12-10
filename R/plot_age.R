# library(apyramid)
# library(dplyr)
library(ggplot2)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils
# library(tidyr)
# library(viridis)

source(here::here("R", "utils.R"))
source(here::here("R", "utils-plots.R"))

## To do
##
## - Create a `pretty_breaks()` function.

plot_age_pyramid <- function(
    data,
    interval = 10,
    thematic = TRUE,
    viridis = "viridis",
    breaks = NULL,
    na_rm = TRUE,
    theme = "bw",
    text_size = NULL,
    print = TRUE
  ){
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_subset(c("sex", "age"), names(data))
  prettycheck:::assert_number(interval)
  prettycheck:::assert_flag(thematic)
  assert_color_options(viridis = viridis)
  prettycheck:::assert_numeric(breaks, null_ok = TRUE)
  prettycheck::assert_pick(interval, breaks, min_pick = 1)
  prettycheck:::assert_flag(na_rm)
  prettycheck:::assert_number(text_size, null.ok = TRUE)

  if (is.null(breaks)) breaks <- pretty(data$age, n = interval)

  plot <- rutils::shush(
    data |>
      dplyr::select(sex, age) |>
      dplyr::mutate(
        age_group = cut(
          age,
          breaks = breaks,
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
      {
        if (isTRUE(thematic)) {
          scale_fill_brand_d()
        } else {
          viridis::scale_fill_viridis(
            begin = 0.5,
            end = 0.75,
            discrete = TRUE,
            option = viridis
          )
        }
      } +
      add_labels(
        x = "Frequency",
        y = "Age group",
        fill = "Sex"
      ) +
      add_theme(
        theme = theme,
        text_size = text_size
      )
  )

  if (isTRUE(print)) print(plot)

  invisible(plot)
}
