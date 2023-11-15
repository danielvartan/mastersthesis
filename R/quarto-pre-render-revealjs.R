# library(here)
# library(rutils)
# lybrary(yaml)

# Pre-render begin ----------

source(here::here("R", "quarto-pre-render-begin.R"))

# Update Quarto files -----

swap_list <- list(
  introduction = list(
    from = here::here("qmd", "slides-introduction.qmd"),
    to = here::here("qmd", "_index-revealjs.qmd"),
    begin_tag = "%:::% slides-introduction begin %:::%",
    end_tag = "%:::% slides-introduction end %:::%",
    value = NULL,
    quarto_render = FALSE
  ),
  results = list(
    from = here::here("qmd", "slides-results.qmd"),
    to = here::here("qmd", "_index-revealjs.qmd"),
    begin_tag = "%:::% slides-results begin %:::%",
    end_tag = "%:::% slides-results end %:::%",
    value = NULL,
    quarto_render = FALSE
  ),
  index = list(
    from = here::here("qmd", "_index-revealjs.qmd"),
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
  format = "revealjs"
)

rutils:::add_or_update_env_var(
  var = var_list,
  yml_file = here::here("_variables.yml")
)

# Add invisible slide on top of `index.qmd` ----

data <- readLines(here::here("index.qmd"))

test <- grepl(
  "^## \\{visibility", data[1:grep("%:::% index begin %:::%", data)[1]]
)

if (!any(test, na.rm = TRUE)) {
  if (grepl("^---", data[1])) {
    data <- c(
      data[seq(1, grep("^---", data)[2] + 1)],
      '## {visibility="hidden"}',
      data[seq(grep("^---", data)[2] + 2, length(data))]
    )
  } else {
    data <- c('## {visibility="hidden"}', data)
  }
}

writeLines(data, here::here("index.qmd"))

# Pre-render end ----------

source(here::here("R", "quarto-pre-render-end.R"))
