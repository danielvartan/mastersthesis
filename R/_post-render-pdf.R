# library(fs)
# library(here)
# library(rutils) # github.com/danielvartan/rutils

# Post-render begin ----------

source(here::here("R", "_post-render-begin.R"))

# Copy and rename the PDF file (if exists) to `output_dir_pdf` folder -----

pdf_file <- list.files(output_dir_pdf, full.names = TRUE, pattern = "\\.pdf$")

if (length(pdf_file) == 1) {
  fs::file_copy(
    path = pdf_file,
    new_path = file.path(output_dir_pdf, "index.pdf"),
    overwrite = TRUE
  )

  fs::file_copy(
    path = pdf_file,
    new_path = file.path(output_dir_html, "index.pdf"),
    overwrite = TRUE
  )

  fs::file_delete(pdf_file)
}

# Copy and rename the TeX file (if exists) to `output_dir_pdf` folder -----

tex_file <- list.files(here::here(), full.names = TRUE, pattern = "\\.tex$")

if (length(tex_file) == 1) {
  fs::file_copy(
    path = tex_file,
    new_path = file.path(output_dir_pdf, "index.tex"),
    overwrite = TRUE
  )
}

# Copy other files (if they exist) to `output_dir_pdf` folder -----

files <- c("index.log", "index.bbl", "index.blg")

for (i in files) {
  log_file <- list.files(
    here::here(),
    full.names = TRUE,
    pattern = stringr::str_escape(i)
  )

  if (length(log_file) == 1) {
    fs::file_copy(
      path = log_file,
      new_path = file.path(output_dir_pdf, i),
      overwrite = TRUE
    )
  }
}

# Post-render end ----------

source(here::here("R", "_post-render-end.R"))
