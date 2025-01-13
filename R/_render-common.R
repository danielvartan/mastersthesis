# library(here)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils
# lybrary(yaml)

# Set variables -----

quarto_yml_path <- here::here("_quarto.yml")
quarto_yml_html_path <- here::here("_quarto-html.yml")
quarto_yml_pdf_path <- here::here("_quarto-pdf.yml")
quarto_yml_revealjs_path <- here::here("_quarto-revealjs.yml")
quarto_yml_extension_path <- here::here("_extensions", "abnt", "_extension.yml")

quarto_yml_vars <- yaml::read_yaml(quarto_yml_path)
quarto_yml_html_vars <- yaml::read_yaml(quarto_yml_html_path)
quarto_yml_pdf_vars <- yaml::read_yaml(quarto_yml_pdf_path)
quarto_yml_revealjs_vars <- yaml::read_yaml(quarto_yml_revealjs_path)
quarto_yml_extension_vars <- yaml::read_yaml(quarto_yml_extension_path)

github_user <- "danielvartan"
project_name <- here::here() |> basename() # rstudioapi::getActiveProject()

# Create output folders if they don't exist -----

output_dir_pdf <- here::here(quarto_yml_pdf_vars$project$`output-dir`)
output_dir_html <- here::here(quarto_yml_html_vars$project$`output-dir`)
output_dir_revealjs <- here::here(quarto_yml_revealjs_vars$project$`output-dir`)

output_docs <- here::here("docs")
output_docs_pdf <- here::here("docs", "print")
output_docs_html <- here::here("docs", "web")
output_docs_revealjs <- here::here("docs", "slides")

folders <- c(
  output_dir_pdf, output_dir_html, output_dir_revealjs,
  output_docs, output_docs_pdf, output_docs_html, output_docs_revealjs
)

for (i in folders) {
  if (!prettycheck:::test_directory_exists(i)) dir.create(i)
}

# Create `_variables.yml` if it doesn't exist -----

env_vars_file_path <- here::here("_variables.yml")

if (!prettycheck:::test_file_exists(env_vars_file_path)) {
  rutils:::create_file(env_vars_file_path)
}

# Create `_results.yml` if it doesn't exist -----

res_vars_file_path <- here::here("_results.yml")

if (!prettycheck:::test_file_exists(res_vars_file_path)) {
  rutils:::create_file(res_vars_file_path)
}

# Run the data pipeline if the `_targets` directory doesn't exist -----

if (!prettycheck:::test_directory_exists(here::here("_targets"))) {
  targets::tar_make()
}
