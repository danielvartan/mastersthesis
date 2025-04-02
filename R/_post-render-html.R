# library(checkmate)
# library(fs)
# library(here)

# Post-render begin -----

source(here::here("R", "_post-render-begin.R"))

# Copy PDF (if exists) to `output_dir_html` folder -----

pdf_file <- list.files(output_dir_pdf, full.names = TRUE, pattern = ".pdf$")

if (length(pdf_file) == 1) {
  fs::file_copy(
    path = pdf_file,
    new_path = file.path(output_dir_html, "index.pdf"),
    overwrite = TRUE
  )
}

# Create robots.txt file -----

robots_file <- file.path(output_dir_html, "robots.txt")

if (!checkmate::test_file_exists(robots_file)) {
  fs::file_create(robots_file)
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

# Copy favicon.png file to the `docs` folder -----

favicon_file <- here::here("images", "favicon.png")

if (checkmate::test_file_exists(favicon_file)) {
  fs::file_copy(
    path = favicon_file,
    new_path = file.path(output_docs, "favicon.png"),
    overwrite = TRUE
  )
}

# Post-render end ----------

source(here::here("R", "_post-render-end.R"))
