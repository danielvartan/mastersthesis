# library(here)
# library(quartor) # github.com/danielvartan/quartor

# Pre-render begin ----------

source(here::here("R", "_pre-render-begin.R"))

# Update Quarto files -----

swap_list <- list(
  index = list(
    from = here::here("qmd", "chapter-1.qmd"),
    to = here::here("index.qmd"),
    begin_tag = "%:::% index begin %:::%",
    end_tag = "%:::% index end %:::%",
    value = NULL,
    quarto_render = FALSE
  ),
  title_page = list(
    from = here::here("qmd", "_config.qmd"),
    to = here::here("tex", "include-in-header.tex"),
    begin_tag = "%:::% title-page body begin %:::%",
    end_tag = "%:::% title-page body end %:::%",
    value = NULL,
    quarto_render = FALSE
  ),
  cataloging_record = list(
    from = here::here("qmd", "_config.qmd"),
    to = here::here("tex", "include-before-body.tex"),
    begin_tag = "%:::% cataloging-record body begin %:::%",
    end_tag = "%:::% cataloging-record body end %:::%",
    value = NULL,
    quarto_render = FALSE
  ),
  approval_sheet = list(
    from = here::here("qmd", "_config.qmd"),
    to = here::here("tex", "include-before-body.tex"),
    begin_tag = "%:::% approval-sheet body begin %:::%",
    end_tag = "%:::% approval-sheet body end %:::%",
    value = NULL,
    quarto_render = FALSE
  ),
  acknowledgments_body = list(
    from = here::here("qmd", "acknowledgments.qmd"),
    to = here::here("tex", "include-before-body.tex"),
    begin_tag = "%:::% acknowledgments body begin %:::%",
    end_tag = "%:::% acknowledgments body end %:::%",
    value = NULL,
    quarto_render = TRUE
  ),
  epigraph_body = list(
    from = here::here("qmd", "epigraph.qmd"),
    to = here::here("tex", "include-before-body.tex"),
    begin_tag = "%:::% epigraph body begin %:::%",
    end_tag = "%:::% epigraph body end %:::%",
    value = NULL,
    quarto_render = FALSE
  ),
  vernacular_abstract_reference = list(
    from = here::here("qmd", "vernacular-abstract.qmd"),
    to = here::here("tex", "include-before-body.tex"),
    begin_tag = "%:::% vernacular-abstract reference begin %:::%",
    end_tag = "%:::% vernacular-abstract reference end %:::%",
    value = NULL,
    quarto_render = FALSE
  ),
  vernacular_abstract_body = list(
    from = here::here("qmd", "vernacular-abstract.qmd"),
    to = here::here("tex", "include-before-body.tex"),
    begin_tag = "%:::% vernacular-abstract body begin %:::%",
    end_tag = "%:::% vernacular-abstract body end %:::%",
    value = NULL,
    quarto_render = TRUE
  ),
  vernacular_abstract_keywords = list(
    from = here::here("qmd", "vernacular-abstract.qmd"),
    to = here::here("tex", "include-before-body.tex"),
    begin_tag = "%:::% vernacular-abstract keywords begin %:::%",
    end_tag = "%:::% vernacular-abstract keywords end %:::%",
    value = NULL,
    quarto_render = FALSE
  ),
  foreign_abstract_reference = list(
    from = here::here("qmd", "foreign-abstract.qmd"),
    to = here::here("tex", "include-before-body.tex"),
    begin_tag = "%:::% foreign-abstract reference begin %:::%",
    end_tag = "%:::% foreign-abstract reference end %:::%",
    value = NULL,
    quarto_render = FALSE
  ),
  foreign_abstract_body = list(
    from = here::here("qmd", "foreign-abstract.qmd"),
    to = here::here("tex", "include-before-body.tex"),
    begin_tag = "%:::% foreign-abstract body begin %:::%",
    end_tag = "%:::% foreign-abstract body end %:::%",
    value = NULL,
    quarto_render = TRUE
  ),
  foreign_abstract_keywords = list(
    from = here::here("qmd", "foreign-abstract.qmd"),
    to = here::here("tex", "include-before-body.tex"),
    begin_tag = "%:::% foreign-abstract keywords begin %:::%",
    end_tag = "%:::% foreign-abstract keywords end %:::%",
    value = NULL,
    quarto_render = FALSE
  ),
  other_in_header = list(
    from = here::here("qmd", "_config.qmd"),
    to = here::here("tex", "include-in-header.tex"),
    begin_tag = "%:::% other-in-header begin %:::%",
    end_tag = "%:::% other-in-header end %:::%",
    value = NULL,
    quarto_render = FALSE
  ),
  other_before_body = list(
    from = here::here("qmd", "_config.qmd"),
    to = here::here("tex", "include-before-body.tex"),
    begin_tag = "%:::% other-before-body begin %:::%",
    end_tag = "%:::% other-before-body end %:::%",
    value = NULL,
    quarto_render = FALSE
  ),
  other_after_body = list(
    from = here::here("qmd", "_config.qmd"),
    to = here::here("tex", "include-after-body.tex"),
    begin_tag = "%:::% other-after-body begin %:::%",
    end_tag = "%:::% other-after-body end %:::%",
    value = NULL,
    quarto_render = FALSE
  )
)

for (i in swap_list) {
  quartor:::swap_value_between_files(
    from = i$from,
    to = i$to,
    begin_tag = i$begin_tag,
    end_tag = i$end_tag,
    value = i$value,
    quarto_render = i$quarto_render,
    cite_method = "biblatex"
  )
}

# Add/update environment variables -----

var_list <- list(
  format = "pdf"
)

quartor:::add_env_var(
  var = var_list,
  yml_file = here::here("_variables.yml")
)

# Pre-render end ----------

source(here::here("R", "_pre-render-end.R"))
