# library(dplyr)
# library(ggcorrplot)
library(ggplot2)
# library(here)
# library(hms)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(stats)
# library(tidyr)
# library(thematic)

source(here::here("R", "utils.R"))

plot_ggcorrplot <- function(
    data,
    cols,
    na_rm = TRUE,
    label = TRUE,
    hc_order = TRUE
  ) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_character(cols)
  prettycheck:::assert_subset(cols, names(data))
  prettycheck:::assert_flag(na_rm)
  prettycheck:::assert_flag(label)
  prettycheck:::assert_flag(hc_order)

  out <-
    data |>
    dplyr::select(dplyr::all_of(cols)) |>
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
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::everything(),
        .fns = ~ as.numeric(.x))
      )

  if (isTRUE(na_rm)) out <- out |> tidyr::drop_na(dplyr::all_of(cols))

  corr <- stats::cor(out, use = "complete.obs")
  p_matrix <- ggcorrplot::cor_pmat(out)
  # round(ggcorrplot::cor_pmat(out), 5)

  plot <-
    ggcorrplot::ggcorrplot(
      corr = corr,
      type = "lower",
      ggtheme = thematic::thematic_get_theme(),
      outline.color = "gray",
      hc.order = hc_order,
      lab = label,
      p.mat = p_matrix
    )

  print(plot)
  invisible(plot)
}
