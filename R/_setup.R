## Based on <https://github.com/hadley/r4ds/blob/main/_common.R>.

# Load libraries -----

library(downlit)
# library(extrafont)
library(ggplot2)
# library(here)
# library(knitr)
library(lubridate)
library(magrittr)
library(rlang)
# library(rutils) # github.com/danielvartan/rutils
library(targets)
# library(thematic)
library(xml2)
# library(yaml)

# Load functions -----

source(here::here("R", "utils.R"))
source(here::here("R", "utils-plots.R"))

# Set general options -----

options(
  dplyr.print_min = 6,
  dplyr.print_max = 6,
  pillar.max_footer_lines = 2,
  pillar.min_chars = 15,
  scipen = 10,
  digits = 5,
  stringr.view_n = 6,
  pillar.bold = TRUE,
  width = 77 # 80 - 3 for #> comment
)

# Set variables -----

set.seed(2024)

env_vars <- yaml::read_yaml(here::here("_variables.yml"))
results_vars <- yaml::read_yaml(here::here("_results.yml"))

if (env_vars$format == "html") {
  base_size <- 11
} else {
  base_size <- 10
}

env_vars$base_size <- base_size

# Load fonts -----

if (is.null(env_vars$sansfont)) {
  cli::cli_abort("Error while importing the environment variables.")
} else {
  extrafont::font_import(
    paths = NULL,
    recursive = TRUE,
    prompt = FALSE,
    pattern = paste0(
      "^(?i)", stringr::str_extract(env_vars$sansfont, "(?i)^.[a-zÀ-ÿ]+"), "*"
    )
  ) |>
    rutils::shush()

  extrafont::loadfonts(quiet = TRUE)
}

# Set knitr -----

knitr::clean_cache()

knitr::opts_chunk$set(
  # comment = "#>",
  # collapse = TRUE,
  root.dir = here::here()
)

# Set `ggplot2` -----

ggplot2::theme_set(
  ggplot2::theme_bw(
    base_size = base_size,
    base_family = env_vars$sansfont,
    base_line_size = base_size / 22,
    base_rect_size = base_size / 22
  )
)

thematic::thematic_set_theme(
  thematic::thematic_theme(
    bg = get_brand_color("white") |> paste0("00"),
    fg = get_brand_color("secondary"),
    accent = get_brand_color("primary"),
    font = thematic::font_spec(
      families = get_brand_font("base"),
      install = TRUE
    )
  ) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "transparent"),
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(fill = "transparent"),
      legend.box.background = ggplot2::element_rect(fill = "transparent"),
      legend.frame = ggplot2::element_blank()
    )
)
