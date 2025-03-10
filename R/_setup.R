## Based on <https://github.com/hadley/r4ds/blob/main/_common.R>.

# Load packages -----

# library(brandr) # github.com/danielvartan/brandr
library(downlit)
library(ggplot2)
# library(here)
library(httpgd)
# library(knitr)
library(lubridate)
library(magrittr)
library(ragg)
library(rlang)
# library(rutils) # github.com/danielvartan/rutils
library(showtext)
library(sysfonts)
library(targets)
library(vscDebugger)
library(xml2)
# library(yaml)

# Load functions -----

source(here::here("R", "utils.R"))

# Set general options -----

options(
  dplyr.print_min = 6,
  dplyr.print_max = 6,
  pillar.max_footer_lines = 2,
  pillar.min_chars = 15,
  scipen = 10,
  digits = 10,
  stringr.view_n = 6,
  pillar.bold = TRUE,
  width = 77 # 80 - 3 for #> comment
)

# Set variables -----

set.seed(2025)

env_vars <- yaml::read_yaml(here::here("_variables.yml"))
res_vars <- yaml::read_yaml(here::here("_results.yml"))

if (env_vars$format %in% c("html", "revealjs")) {
  base_size <- 11
} else {
  base_size <- 10
}

env_vars$base_size <- base_size

# Set knitr -----

knitr::clean_cache() |> rutils::shush()

knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  root.dir = here::here(),
  dev = "ragg_png",
  dev.args = list(bg = "transparent"),
  fig.showtext = TRUE
)

# Set `brandr` options -----

brandr_options <- list(
  "BRANDR_COLOR_SEQUENTIAL" =
    brandr::get_brand_color(c("primary", "secondary")),
  "BRANDR_COLOR_DIVERGING" =
    brandr::get_brand_color(c("primary", "white", "secondary")),
  "BRANDR_COLOR_QUALITATIVE" = c(
    "#EA7701", # orange (primary)
    "#142A32", # black (secondary)
    "#964D01", # brown
    "#D67C20",
    "#4F5556", # grey (tertiary)
    "#2B4A5E",
    "#F5BD83", # light-orange
    "#F08C3E",
    "#CC5A15",
    "#B23300"
    )
  )

for (i in seq_along(brandr_options)) {
  options(brandr_options[i])
}

# Set and load graph fonts -----

sysfonts::font_paths(
  c(
    here::here("ttf"),
    here::here("_extensions", "abnt", "otf"),
    here::here("_extensions", "abnt", "ttf")
  )
)

sysfonts::font_add(
  family = "dm-sans",
  regular = here::here("ttf", "dmsans-regular.ttf"),
  bold = here::here("ttf", "dmsans-bold.ttf"),
  italic = here::here("ttf", "dmsans-italic.ttf"),
  bolditalic = here::here("ttf", "dmsans-bolditalic.ttf"),
  symbol = NULL
)

showtext::showtext_auto()

# Set `ggplot2` theme -----

ggplot2::theme_set(
  ggplot2::theme_bw(
    base_size = base_size,
    base_line_size = base_size / 22,
    base_rect_size = base_size / 22
  ) +
    ggplot2::theme(
      text = ggplot2::element_text(
        color = brandr::get_brand_color("secondary"),
        family = "dm-sans",
        face = "plain"
      ),
      panel.background = ggplot2::element_rect(fill = "transparent"),
      plot.background = ggplot2::element_rect(
        fill = "transparent", color = NA
      ),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(fill = "transparent"),
      legend.box.background = ggplot2::element_rect(
        fill = "transparent", color = NA
      ),
      legend.frame = ggplot2::element_blank(),
      legend.ticks = ggplot2::element_line(color = "white")
    )
)
