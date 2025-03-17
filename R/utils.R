# library(checkmate)
# library(prettycheck) # github.com/danielvartan/prettycheck

change_sign <- function(x, flag = TRUE) {
  prettycheck::assert_numeric(x)
  checkmate::assert_flag(flag)

  if (isTRUE(flag)) {
    x * (-1)
  } else {
    x
  }
}

# library(checkmate)
# library(here)
# library(stringr)

to_relative_path <- function(path, root = ".") {
  checkmate::assert_string(path)
  checkmate::assert_string(root)

  path <- stringr::str_remove(path, here::here())

  paste0(root, path)
}

# library(checkmate)

# TODO: Move to `rutils`.
count_not_na <- function(x) {
  checkmate::assert_atomic(x)

  length(which(!is.na(x)))
}

# library(checkmate)

# TODO: Move to `rutils`.
drop_inf <- function(x) {
  checkmate::assert_atomic(x)

  x[!(x == -Inf | x == Inf)]
}

# library(checkmate)
# library(dplyr)
# library(tidyr)

list_as_tibble <- function(list) {
  checkmate::assert_list(list)

  list |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::everything(),
        .fns = as.character
      )
    ) |>
    tidyr::pivot_longer(cols = dplyr::everything())
}

# library(checkmate)
library(yaml)

write_in_results_yml <- function(
    x, #nolint
    path = here::here("_results.yml"),
    digits = 30
  ) {
  checkmate::assert_list(x)
  checkmate::assert_file_exists(path)

  out <- yaml::read_yaml(path)

  for (i in seq_along(x)) {
    if (is.numeric(x[[i]])) {
      x[[i]] <- x[[i]] |> signif(digits)
    }

    if (names(x)[i] %in% names(out)) {
      out[[names(x)[i]]] <- x[[i]]
    } else {
      out <- c(out, x[i])
    }
  }

  yaml::write_yaml(out, path)
}

# library(checkmate)
library(magrittr)
# library(prettycheck) # github.com/danielvartan/prettycheck

format_to_md_latex <- function(
    x, #nolint
    after = NULL,
    before = NULL,
    max_digits = 3,
    decimal_mark = ".",
    big_mark = ",",
    key = "$"
  ) {
  checkmate::assert_atomic(x)
  checkmate::assert_string(after, null.ok = TRUE)
  checkmate::assert_string(before, null.ok = TRUE)
  checkmate::assert_string(decimal_mark)
  checkmate::assert_string(big_mark)
  checkmate::assert_int(max_digits, lower = 0)
  checkmate::assert_string(key)

  if (is.numeric(x)) {
    x |>
      round(max_digits) |>
      format(
        decimal.mark = decimal_mark,
        big.mark = big_mark,
        scientific = FALSE
      ) %>% # Don't change the pipe!
      paste0(key, before, ., after, key) |>
      I()
  } else {
    x |>
      as.character() %>% # Don't change the pipe!
      paste0(key, before, ., after, key) |>
      I()
  }
}

# library(checkmate)
library(magrittr)
# library(prettycheck) # github.com/danielvartan/prettycheck

inverse_log_max <- function(x, base = exp(1)) {
  prettycheck::assert_numeric(x)
  checkmate::assert_number(base)

  x |>
    log(base) |>
    max(na.rm = TRUE) |>
    ceiling() %>% # Don't change the pipe!
    `^`(base, .)
}

# library(checkmate)

cli_test_fun <- function(test) {
  checkmate::assert_flag(test)

  if (isTRUE(test)) {
    cli::col_blue
  } else {
    cli::col_red
  }
}
