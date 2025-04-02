# library(here)

# Post-render begin ----------

source(here::here("R", "_post-render-begin.R"))

# Clean top of `index.qmd` ----

data <- readLines(here::here("index.qmd"))

if (grepl("^---", data[1])) {
  data <- c(
    data[seq(1, grep("^---", data)[2])],
    "",
    data[seq(grep("%:::% index begin %:::%", data), length(data))]
  )
} else {
  data[seq(grep("%:::% index begin %:::%", data), length(data))]
}

# data <- data[-stringr::str_which(data, "%:::% index begin %:::%")]

writeLines(data, here::here("index.qmd"))

# Post-render end ----------

source(here::here("R", "_post-render-end.R"))
