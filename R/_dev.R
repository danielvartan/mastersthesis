# # For development use only (comment out the code after use)
#
# .libPaths()
# cffr::cff_validate()
# grDevices::dev.off()
# quartor:::bbt_scan_citation_keys()
# quartor:::bbt_write_quarto_bib()
# quartor:::clean_quarto_mess()
# renv::activate()
# renv::clean(actions = "unused.packages")
# renv::deactivate()
# renv::dependencies("R/_setup.R")
# renv::equip()
# renv::restore()
# renv::snapshot()
# renv::status()
# renv::update()
# tcltk::tk_choose.files()
# urlchecker::url_check()
# urlchecker::url_update()

# # {targets}
#
# targets::tar_manifest()
# targets::tar_visnetwork(targets_only = TRUE)
# targets::tar_make()
# targets::tar_outdated()
# data <- targets::tar_read(geocoded_data)

# # Quarto (see <https://quarto.org/docs/projects/quarto-projects.html>)
#
# source(here::here("R", "_pre-render-pdf.R"))
# source(here::here("R", "_pre-render-html.R"))
# source(here::here("R", "_pre-render-revealjs.R"))
#
# --cache
# --no-cache
# --cache-refresh
# quarto render
# quarto render --profile gfm
# quarto render --profile pdf # Source pre-render first.
# quarto render --profile html # Source pre-render first.
# quarto render --profile revealjs # Source pre-render first.

# # LaTeX
#
# \BeforeBeginEnvironment{}{} # {etoolbox}
# \AtBeginEnvironment{}{}
# \AtEndEnvironment{}{}
# \AfterEndEnvironment{}{}

# # OSF
#
# osf_pat <- Sys.getenv("OSF_PAT")
# password <- Sys.getenv("MASTERSTHESIS_PASSWORD")
#
# osfr::osf_auth(osf_pat)
#
# osf_id <- "https://osf.io/cbqsa"
# pattern <- "lookup.rda"
#
# file <-
#   osfr::osf_ls_files(
#     osfr::osf_retrieve_node(osf_id),
#     pattern = pattern
#   ) |>
#   osfr::osf_download(path = tempdir(), conflicts = "overwrite") |>
#   magrittr::extract2("local_path")
#
# lockr::unlock_file(file, private_key= private_key, password = password)
# file <- stringr::str_remove(file, "\\.lockr$")
# load(file)
# lockr::lock_file(file, public_key = public_key, remove_file = TRUE)

# # Encryption
#
# password <- Sys.getenv("MASTERSTHESIS_PASSWORD")
#
# lockr::rsa_keygen()
# lockr::lock_dir(dir = "temp", public_key = "_ssh/id_rsa.pub")
# lockr::unlock_dir(
#   dir = "temp", private_key = "_ssh/id_rsa", password = password
# )
# lockr::lock_file(
#   tcltk::tk_choose.files(), public_key = "_ssh/id_rsa.pub"
# )
# lockr::unlock_file(
#   tcltk::tk_choose.files(), private_key = "_ssh/id_rsa", password = password
# )
