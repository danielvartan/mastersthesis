# library(lockr) # github.com/danielvartan/lockr
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(readr)

save_and_lock <- function(
    x,
    file,
    type = "csv",
    public_key = here::here("_ssh", "id_rsa.pub"),
    ...
  ) {
  checkmate::assert_path_for_output(file, overwrite = TRUE)
  checkmate::assert_choice(type, c("csv", "rds"))

  if (type == "csv") {
    checkmate::assert_data_frame(x)

    readr::write_csv(x, file, ...)

    if (checkmate::test_file_exists(paste0(file, ".lockr"))) {
      file.remove(paste0(file, ".lockr"))
    }

    lockr::lock_file(file, public_key, remove_file = TRUE)
  } else {
    readr::write_rds(x, file, ...)

    if (checkmate::test_file_exists(paste0(file, ".lockr"))) {
      file.remove(paste0(file, ".lockr"))
    }

    lockr::lock_file(file, public_key, remove_file = TRUE)
  }

  paste0(file, ".lockr")
}
