# library(here)
# library(lubridate)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils
# library(showtext)
# lybrary(stringr)
library(sysfonts)
# lybrary(yaml)

# Load common render -----

source(here::here("R", "_render-common.R"))

# Copy images folder to `./qmd` -----

## *Solve issues related to relative paths.

dir_path <- here::here("qmd", "images")

if (!prettycheck:::test_directory_exists(dir_path)) {
  dir.create(dir_path) |> invisible()
}

for (i in rutils:::list_files(here::here("images"))) {
  rutils:::copy_file(
    from = i,
    to = file.path(dir_path, basename(i))
  )
}

# Create environment variables -----

env_vars <- list()

var_files <- c(
  "quarto_yml_extension_vars", "quarto_yml_vars", "quarto_yml_html_vars",
  "quarto_yml_pdf_vars"
)

var_patterns <- c(
  "academic-title", "academic-degree", "area-of-concentration", "author",
  "^book.url$", "cosupervisor", "date", "keyword", "language", "pdf.location$",
  "mainfont", "monofont", "program", "sansfont", "school", "supervisor",
  "^book.title$","type-of-work", "university", "version-note"
)

for (i in var_files){
  values <- unlist(get(i))

  for (j in var_patterns) {
    test <- grepl(j, names(values))

    if (any(test, na.rm = TRUE)) {
      if (grepl("^\\^", j) || grepl("\\$$", j)) {
        j <- sub("^\\^", "", j)
        j <- sub("\\$$", "", j)
      }

      if (grepl("\\.", j)) j <- sub("^.+\\.", "", j)

      if (j == "date") {
        if (!grepl("\\d{4}", values[test][1]) &&
            !any(values[test][1] == "today", na.rm = TRUE)) {
          next()
        } else if (any(values[test][1] == "today", na.rm = TRUE)) {
          env_vars[[j]] <- as.character(Sys.Date())
          env_vars[["year"]] <- as.character(lubridate::year(Sys.Date()))
          next()
        } else {
          env_vars[["year"]] <-
            as.character(stringr::str_extract(values[test][1], "\\d{4}"))
        }
      }

      if (j == "language" &&
          !grepl("^[a-z]{2}$|^[a-z]{2}\\-[a-zA-Z]{2}$", values[test][1])) {
        next()
      }

      if (j == "author") {
        if (!grepl(" ", values[test][1])) {
          env_vars[["author-surname"]] <- values[test][1]
          env_vars[["author-given-name"]] <- values[test][1]
        } else{
          env_vars[["author-surname"]] <-
            stringr::str_extract(values[test][1], "(?i)(?<= )[a-zÀ-ÿ]+$")

          env_vars[["author-given-name"]] <-
            stringr::str_extract(values[test][1], "^(?i)[a-zÀ-ÿ]+(?= )")

          env_vars[["author-minus-surname"]] <-
            stringr::str_extract(values[test][1], "^.+(?= )")
        }

        env_vars[["author-initials"]] <-
          rutils:::extract_initials(values[test][1])
      }

      env_vars[[j]] <- values[test][1] |> unname()
    }
  }
}

env_vars |> yaml::write_yaml(env_vars_file_path)

# Create `_results.yml` variables -----

source(here::here("R", "_pre-render-vars.R"))

# Scan Quarto files for citations and add them to references.bib -----

quarto_yml_pdf_path <- here::here("_quarto-pdf.yml")
quarto_yml_pdf_vars <- yaml::read_yaml(quarto_yml_pdf_path)

# (2024-08-25)
# This function should work with any version of BetterBibTeX (BBT) for Zotero.
# Verify if @wmoldham PR was merged in the `rbbt` package (see issue #47
# <https://github.com/paleolimbot/rbbt/issues/47>). If not, install `rbbt`
# from @wmoldham fork `renv::install("wmoldham/rbbt")`.

if (isTRUE(quarto_yml_pdf_vars$format$`abnt-pdf`$zotero)) {
  rutils:::bbt_write_quarto_bib(
    bib_file = "references.bib",
    dir = c("", "qmd", "tex"),
    pattern = c("\\.qmd$|\\.tex$"),
    wd = here::here()
  )
}
