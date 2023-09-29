# # For development use only (comment the code after use (Ctrl + Shift + C)).
#
# .rs.restartR()
# .libPaths()
# cffr::cff_validate()
# knitr::clean_cache()
# knitr:::webshot_available()
# lintr::use_lintr()
# lintr::lint_dir("R")
# normalizePath(readClipboard(), "/", mustWork = FALSE)
# targets::tar_manifest()
# targets::tar_visnetwork(targets_only = TRUE)
# targets::tar_make()
# targets::tar_outdated()
# data <- targets::tar_read(geocoded_data)
# renv::init()
# utils::install.packages("renv", dependencies = TRUE)
# renv:::renv_download_method()
# options(renv.download.override = utils::download.file)
# renv::equip()
# renv::activate()
# renv::deactivate()
# renv::status()
# renv::dependencies("R/quarto-setup.R")
# renv::install()
# renv::update()
# renv::restore()
# renv::snapshot()
# rutils:::bbt_scan_citation_keys()
# rutils:::bbt_write_quarto_bib()
# rutils:::set_quarto_speel_check()
# rutils:::gather_words_from_spell_check()
# rutils:::spell_check_quarto()
# rutils:::update_quarto_wordlist()
# rutils:::clean_quarto_mess()
# rutils:::quarto_status("drafting")
# spelling::spell_check_files("index.qmd")
# urlchecker::url_check()
# urlchecker::url_update()

# # Pandoc
#
# rmarkdown::pandoc_convert(
#   input = "qmd/acknowledgments.qmd",
#   from = "markdown",
#   to = "latex",
#   wd = here::here()
# )

# # Quarto (see <https://quarto.org/docs/projects/quarto-projects.html>)
#
# quarto render
# quarto render --profile pdf
# quarto render --profile pdf --cache-refresh
# quarto render --profile html
# quarto render --profile html --cache-refresh
# quarto publish gh-pages

# # Google Cloud Storage
#
# library(googleCloudStorageR)
# googleCloudStorageR::gcs_setup()
# googleCloudStorageR::gcs_list_buckets("brchrono")
# googleCloudStorageR::gcs_get_bucket("brchrono")
# googleCloudStorageR::gcs_list_objects()
#
# parsed_download <- googleCloudStorageR::gcs_get_object("raw-data.zip.lockr")
#
# file <- tempfile(fileext = ".lockr")
# gcs_get_object("lookup.rda.lockr", saveToDisk = file)
# lockr::unlock_file(file, private_key  = ".ssh/id_rsa")
# file <- stringr::str_remove(file, "\\.lockr$")
# load(file)

# # Encryption
#
# lockr::rsa_keygen()
# lockr::lock_dir(dir = "temp", public_key  = ".ssh/id_rsa.pub")
# lockr::unlock_dir(dir = "temp", private_key  = ".ssh/id_rsa")
# lockr::lock_file(file.choose(), public_key  = ".ssh/id_rsa.pub")
# lockr::unlock_file(file.choose(), private_key  = ".ssh/id_rsa")

# # TODO
#
# * Document functions.
# * Finish version of 'tesesusp' Quarto format.
# * Document and promote quarto util functions on {rutils}
# * Create check_quarto_urls()
