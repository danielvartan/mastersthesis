# library(rutils, quietly = TRUE)
# library(here, quietly = TRUE)
# library(magrittr, quietly = TRUE)

# Copy output to `docs` folder -----

## TODO: Create function in `rutils` to delete all files and dirs from a folder.

library(magrittr)

env_vars <- yaml::read_yaml(env_vars_file_path)

output_dir_files <- list.files(
  path = get(paste0("output_dir_", env_vars$format)),
  recursive = TRUE,
  full.names = TRUE
)

output_dir_dirs <- list.dirs(
  path = get(paste0("output_dir_", env_vars$format))
  )[-1]

output_docs_old_files <- list.files(
  path = get(paste0("output_docs_", env_vars$format)),
  recursive = TRUE,
  full.names = TRUE
)

output_docs_old_dirs <- list.dirs(
  path = get(paste0("output_docs_", env_vars$format))
  )[-1]

output_docs_new_files <-
  list.files(
    path = get(paste0("output_dir_", env_vars$format)),
    recursive = TRUE
  ) %>%
  here::here(get(paste0("output_docs_", env_vars$format)), .)

output_docs_new_dirs <- list.dirs(
  path = get(paste0("output_dir_", env_vars$format)),
  full.names = FALSE
)[-1]  %>%
  here::here(get(paste0("output_docs_", env_vars$format)), .)

for (i in output_docs_old_files) {
  if (checkmate::test_file_exists(i)) rutils:::delete_file(i)
}

for (i in output_docs_old_dirs) {
  if (checkmate::test_directory_exists(i)) rutils:::delete_dir(i)
}

for (i in output_docs_new_dirs) {
  if (!checkmate::test_directory_exists(i)) dir.create(i)
}

for (i in seq_along(output_docs_new_files)) {
  rutils:::copy_file(
    from = output_dir_files[i],
    to = output_docs_new_files[i],
    overwrite = TRUE,
    recursive = TRUE
  )
}

# Delete unnecessary files and folders -----

dir_list <-
  c(".temp", "index_cache", "index_files", "site_libs") |>
  append(x = _, list.dirs("qmd")[-1])

rutils:::clean_quarto_mess(
  file = NULL,
  dir = dir_list,
  ext = c(
    "aux", "bbx", "cbx", "dbx", "fdb_latexmk", "lbx", "loa", "log", "otf",
    "pdf", "scss", "tex", "xdv"
  ),
  ignore = NULL,
  wd = here::here()
)
