# library(beepr)
# library(fs)
# library(here)
library(magrittr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(quartor) # github.com/danielvartan/quartor
# library(readr)
# library(rutils) # github.com/danielvartan/rutils
# library(stringr)

# Copy output to `docs` folder -----

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
)[-1] %>%
  here::here(get(paste0("output_docs_", env_vars$format)), .)

for (i in output_docs_old_files) {
  if (checkmate::test_file_exists(i)) fs::file_delete(i)
}

for (i in output_docs_old_dirs) {
  if (checkmate::test_directory_exists(i)) fs::file_delete(i)
}

for (i in output_docs_new_dirs) {
  if (!checkmate::test_directory_exists(i)) dir.create(i)
}

for (i in seq_along(output_docs_new_files)) {
  fs::file_copy(
    path = output_dir_files[i],
    new_path = output_docs_new_files[i],
    overwrite = TRUE
  )
}

# Stop `showtext` -----

showtext::showtext_auto(enable = FALSE)

# Update `LICENSE.md` year -----

file <- here::here("LICENSE.md")

data <-
  file |>
  readr::read_lines() |>
  stringr::str_replace_all(
    pattern = "20\\d{2}",
    replacement = as.character(Sys.Date() |> lubridate::year())
  )

data |> readr::write_lines(file)

# Delete unnecessary files and folders -----

dir_list <-
  c(".temp", "index_cache", "index_files", "site_libs") |>
  append(x = _, list.dirs("qmd")[-1])

quartor:::clean_quarto_mess(
  file = c(".luarc.json"),
  dir = dir_list,
  ext = c(
    "aux", "bbx", "bcf-SAVE-ERROR", "cbx", "dbx", "fdb_latexmk", "lbx", "loa",
    "log", "otf", "pdf", "scss", "tex", "xdv"
  ),
  ignore = NULL,
  wd = here::here()
)

# Check if all ran successfully -----

beepr::beep(1)

Sys.sleep(3)
