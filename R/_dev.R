# # For development use only (comment the code after use (Ctrl + Shift + C)).
#
# .rs.restartR()
# .libPaths()
# cffr::cff_validate()
# lintr::lint_dir("R")
# normalizePath(readClipboard(), "/", mustWork = FALSE)
# renv::activate()
# renv::deactivate()
# renv::status()
# renv::dependencies()
# renv::install()
# renv::update()
# renv::restore()
# renv::snapshot()
# roxygen2::roxygenise()
# spelling::spell_check_package()
# spelling::update_wordlist()
# urlchecker::url_check()

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
