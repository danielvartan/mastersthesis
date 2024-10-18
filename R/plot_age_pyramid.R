# library(apyramid)
# library(dplyr)
library(ggplot2)
# library(prettycheck) # https://github.com/danielvartan/prettycheck
# library(rutils) # https://github.com/danielvartan/rutils
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
