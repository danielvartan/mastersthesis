# library(here)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils
library(sysfonts)
# lybrary(yaml)

# Pre-render begin ----------

source(here::here("R", "_pre-render-begin.R"))

# Update Quarto files -----

swap_list <- list(
  index = list(
    from = here::here("qmd", "_index-html.qmd"),
    to = here::here("index.qmd"),
    begin_tag = "%:::% index begin %:::%",
    end_tag = "%:::% index end %:::%",
    value = NULL,
    quarto_render = FALSE
  )
)

for (i in swap_list) {
  rutils:::swap_value_between_files(
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
  format = "html"
)

rutils:::add_or_update_env_var(
  var = var_list,
  yml_file = here::here("_variables.yml")
)

# Pre-render end ----------

source(here::here("R", "_pre-render-end.R"))
