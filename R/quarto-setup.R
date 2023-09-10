## Based on https://github.com/hadley/r4ds/blob/main/_common.R

set.seed(2023)

require(checkmate, quietly = TRUE)
require(knitr, quietly = TRUE)
require(ggplot2, quietly = TRUE)

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

# use results: "asis" when setting a status for a chapter
quarto_status <- function(type) {
  status <- switch(
    type,
    polishing = paste0(
      "should be readable but is currently undergoing final polishing"
      ),
    restructuring = paste0(
      "is undergoing heavy restructuring and may be confusing or incomplete"
      ),
    drafting = paste0(
      "is currently a dumping ground for ideas, and I don't recommend", " ",
      "reading it"
      ),
    complete = "is largely complete and just needs final proof reading",
    stop("Invalid `type`", call. = FALSE)
    )

  class <- switch(
    type,
    polishing = "note",
    restructuring = "important",
    drafting = "important",
    complete = "note"
  )

  cat(paste0(
    "\n",
    "::: {.callout-", class, "}", "\n",
    "You are reading the work-in-progress of this thesis.", " ",
    "This chapter ", status, ".", "\n",
    ":::",
    "\n"
  ))
}

quarto_callout_block <- function(text, type, options = "") {
  choices_type <- c("note", "warning", "important", "tip", "caution")

  checkmate:: assert_string(text)
  checkmate::assert_choice(type, choices_type)
  checkmate::assert_string(options)

  options <- ifelse(options == "", "", paste0(" ", options))

  cat(paste0(
    "\n",
    "::: {.callout-", type, options, "}", "\n",
    text, "\n",
    ":::",
    "\n"
  ))
}
