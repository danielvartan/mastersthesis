# library(here)
# library(prettycheck) # https://github.com/danielvartan/prettycheck
# library(rutils) # https://github.com/danielvartan/rutils
# library(yaml)

# Post-render begin ----------

source(here::here("R", "_post-render-begin.R"))

# Copy PDF (if exists) to `output_dir_html` folder ----------

pdf_file <- list.files(output_dir_pdf, full.names = TRUE, pattern = ".pdf$")

if (length(pdf_file) == 1) {
  rutils:::copy_file(
    from = pdf_file,
    to = file.path(output_dir_html, "index.pdf")
  )
}

# Create robots.txt file ----------

robots_file <- file.path(output_dir_html, "robots.txt")

if (!prettycheck:::test_file_exists(robots_file)) {
  rutils:::create_file(robots_file)
}

# Change this part if you will not use GitHub Pages.
writeLines(
  text = c(
    "user-agent: *",
    "allow: /",
    "",
    paste0(
      "Sitemap: https://",
      github_user,
      ".github.io/",
      project_name,
      "/sitemap.xml"
    )
  ),
  con = robots_file
)

# Post-render end ----------

source(here::here("R", "_post-render-end.R"))
