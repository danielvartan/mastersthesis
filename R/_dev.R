# # For development use only (comment out the code after use)
#
# .rs.restartR()
# .libPaths()
# cffr::cff_validate()
# knitr::clean_cache()
# knitr:::webshot_available()
# lintr::use_lintr()
# lintr::lint_dir("R")
# normalizePath(readClipboard(), "/", mustWork = FALSE)
# renv::init()
# utils::install.packages("renv", dependencies = TRUE)
# renv:::renv_download_method()
# options(renv.download.override = utils::download.file)
# renv::equip()
# renv::activate()
# renv::deactivate()
# renv::status()
# renv::dependencies("R/_setup.R")
# renv::install()
# renv::update()
# renv::restore()
# renv::snapshot()
# renv::clean(actions = "unsed.packages")
# rutils:::bbt_scan_citation_keys()
# rutils:::bbt_write_quarto_bib()
# rutils:::set_quarto_speel_check()
# rutils:::gather_words_from_spell_check(pattern = "\\.qmd$|\\.Rmd$")
# rutils:::spell_check_quarto(pattern = "\\.qmd$|\\.Rmd$")
# rutils:::update_quarto_wordlist(pattern = "\\.qmd$|\\.Rmd$")
# rutils:::clean_quarto_mess()
# rutils:::quarto_status("drafting")
# spelling::spell_check_files("index.qmd")
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
#
# lockr::lock_dir(dir = "temp", public_key = "_ssh/id_rsa.pub")
#
# lockr::unlock_dir(
#   dir = "temp", private_key = "_ssh/id_rsa", password = password
# )
#
# lockr::lock_file(
#   rstudioapi::selectFile(), public_key = "_ssh/id_rsa.pub"
# )
#
# lockr::unlock_file(
#   rstudioapi::selectFile(), private_key = "_ssh/id_rsa", password = password
# )
