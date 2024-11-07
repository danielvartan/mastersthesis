# library(dplyr)
# library(ggcorrplot)
library(ggplot2)
# library(here)
# library(hms)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(stats)
# library(tidyr)

source(here::here("R/utils.R"))

plot_ggcorrplot <- function(data,
                            cols,
                            na_rm = TRUE,
                            text_size = NULL,
                            label = TRUE,
                            hc_order = TRUE) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_character(cols)
  prettycheck:::assert_subset(cols, names(data))
  prettycheck:::assert_flag(na_rm)
  prettycheck:::assert_number(text_size, null.ok = TRUE)
  prettycheck:::assert_flag(label)
  prettycheck:::assert_flag(hc_order)

  out <-
    data |>
    dplyr::select(dplyr::all_of(cols)) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::where(hms::is_hms),
        .fns = ~ midday_trigger(.x)
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::everything(),
        .fns = ~ as.numeric(.x))
      )

  if (isTRUE(na_rm)) {
    out <- out |> tidyr::drop_na(dplyr::all_of(cols))
  }

  corr <- stats::cor(out, use = "complete.obs")
  p_matrix <- ggcorrplot::cor_pmat(out)
  # round(ggcorrplot::cor_pmat(out), 5)

  plot <-
    ggcorrplot::ggcorrplot(
      corr = corr,
      type = "lower",
      ggtheme = ggplot2::theme_gray(),
      outline.color = "gray",
      hc.order = hc_order,
      lab = label,
      p.mat = p_matrix
    ) +
    ggplot2::theme(
      text = ggplot2::element_text(size = text_size),
      axis.text.x = ggplot2::element_text(size = text_size),
      axis.text.y = ggplot2::element_text(size = text_size)
    )

  print(plot)
  invisible(plot)
}
