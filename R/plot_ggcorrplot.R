# library(checkmate, quietly = TRUE)
# library(dplyr, quietly = TRUE)
# library(ggcorrplot, quietly = TRUE)
library(ggplot2, quietly = TRUE)
# library(here, quietly = TRUE)
# library(hms, quietly = TRUE)
# library(stats, quietly = TRUE)
# library(tidyr, quietly = TRUE)

source(here::here("R/utils.R"))

plot_ggcorrplot <- function(data,
                            cols,
                            na_rm = TRUE,
                            text_size = NULL,
                            label = TRUE,
                            hc_order = TRUE) {
  checkmate::assert_tibble(data)
  checkmate::assert_character(cols)
  checkmate::assert_subset(cols, names(data))
  checkmate::assert_flag(na_rm)
  checkmate::assert_number(text_size, null.ok = TRUE)
  checkmate::assert_flag(label)
  checkmate::assert_flag(hc_order)

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
