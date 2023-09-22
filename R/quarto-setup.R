## Based on https://github.com/hadley/r4ds/blob/main/_common.R

set.seed(2023)

require(checkmate, quietly = TRUE)
require(here)
require(knitr, quietly = TRUE)
require(ggplot2, quietly = TRUE)

source(here::here("R/quarto_status.R"))
source(here::here("R/quarto_callout_block.R"))

knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  # cache = TRUE,
  fig.retina = 2,
  fig.width = 6,
  fig.asp = 2/3,
  fig.show = "hold"
)

options(
  dplyr.print_min = 6,
  dplyr.print_max = 6,
  pillar.max_footer_lines = 2,
  pillar.min_chars = 15,
  stringr.view_n = 6,
  # Temporarily deactivate cli output for quarto
  cli.num_colors = 0,
  cli.hyperlink = FALSE,
  pillar.bold = TRUE,
  width = 77 # 80 - 3 for #> comment
)

ggplot2::theme_set(ggplot2::theme_gray(12))
