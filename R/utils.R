long_string <- function(x) {
  checkmate::assert_string(x)

  x |>
    strwrap() |>
    paste0(collapse = " ") |>
    gsub(x = _, pattern = "\\s+", replacement = " ")
}

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


# library(here)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(stringr)

to_relative_path <- function(path, root = ".") {
  checkmate::assert_string(path)
  checkmate::assert_string(root)

  path <- stringr::str_remove(path, here::here())

  paste0(root, path)
}

# library(hms)
# library(lubritime) # github.com/danielvartan/lubritime
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils

# TODO: Refactor and move to `lubritime`.
transform_time <- function(x, threshold = hms::parse_hms("12:00:00")) {
  checkmate::assert_atomic(x)
  prettycheck::assert_hms(
    threshold,
    lower = hms::hms(0),
    upper = hms::parse_hms("23:59:59"),
    null_ok = TRUE
  )

  classes <- c("Duration", "difftime", "hms", "POSIXt", "Interval")

  if (hms::is_hms(x) && !is.null(threshold)) {
    x |>
      lubritime:::link_to_timeline(threshold = threshold) |>
      as.numeric()
  } else if (checkmate::test_multi_class(x, classes)) {
    x |> lubritime:::extract_seconds()
  } else {
    x
  }
}

# library(prettycheck) # github.com/danielvartan/prettycheck

# TODO: Move to `rutils`.
count_not_na <- function(x) {
  checkmate::assert_atomic(x)

  length(which(!is.na(x)))
}

# library(prettycheck) # github.com/danielvartan/prettycheck

# TODO: Move to `rutils`.
drop_inf <- function(x) {
  checkmate::assert_atomic(x)

  x[!(x == -Inf | x == Inf)]
}

# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck
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

# library(prettycheck) # github.com/danielvartan/prettycheck
library(yaml)

write_in_results_yml <- function(
    x,
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

# library(prettycheck) # github.com/danielvartan/prettycheck

is_in_scientific_notation <- function(x) {
  prettycheck::assert_numeric(x)

  x |>
    format() |>
    grepl("e", x = _)
}

# library(prettycheck) # github.com/danielvartan/prettycheck

left_digits <- function(x, count_zero = TRUE) {
  prettycheck::assert_numeric(x)
  checkmate::assert_flag(count_zero)

  out <-
    x |>
    abs() |>
    trunc()

  if (out == 0) {
    ifelse(isTRUE(count_zero), 1, 0)
  } else {
    out |> nchar()
  }
}

library(magrittr)
# library(prettycheck) # github.com/danielvartan/prettycheck

right_digits <- function(x, max_digits = 15) {
  prettycheck::assert_numeric(x)
  checkmate::assert_int(max_digits, lower = 1, upper = 15)

  digit_options <- getOption("digits")
  options(digits = max_digits)

  out <- vapply(
    x,
    function(x) {
      if (is.na(x)){
        NA_real_
      } else {
        if (abs(x - round(x)) > .Machine$double.eps^0.5) {
          x |>
            format() |>
            strsplit("\\.") |>
            magrittr::extract2(1) |>
            magrittr::extract(2) |>
            nchar()
        } else {
          0
        }
      }
    },
    FUN.VALUE = numeric(1)
  )

  options(digits = digit_options)
  out
}

# library(prettycheck) # github.com/danielvartan/prettycheck

digits <- function(x, left = FALSE, right = FALSE) {
  prettycheck::assert_numeric(x)
  checkmate::assert_flag(left)
  checkmate::assert_flag(right)

  if (isTRUE(left)) {
    left_digits(x)
  } else if (isTRUE(right)) {
    right_digits(x)
  } else {
    left_digits(x) + right_digits(x)
  }
}

library(magrittr)
# library(prettycheck) # github.com/danielvartan/prettycheck

format_to_md_latex <- function(
    x,
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

# library(prettycheck) # github.com/danielvartan/prettycheck

to_ascii <- function(x, from = "UTF-8") {
  checkmate::assert_character(x)
  checkmate::assert_string(from)

  x |> iconv(to = "ASCII//TRANSLIT")
}

# library(prettycheck) # github.com/danielvartan/prettycheck

to_ascii_and_lower <- function(x, from = "UTF-8") {
  checkmate::assert_character(x)
  checkmate::assert_string(from)

  x |> to_ascii(from) |> tolower()
}

# library(rutils) # github.com/danielvartan/rutils

vector_to_c <- rutils::vector_to_c

# library(prettycheck) # github.com/danielvartan/prettycheck

cli_test_fun <- function(test) {
  checkmate::assert_flag(test)

  if (isTRUE(test)) {
    cli::col_blue
  } else {
    cli::col_red
  }
}

# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(stringr)

# x <- c("BRT", "EST", "BRT")
# paired_vector <- c("EST" = "EST", "BRT" = "America/Sao_Paulo")
# look_and_replace_chr(x, paired_vector)
look_and_replace_chr <- function(x, paired_vector) {
  checkmate::assert_atomic(x)
  checkmate::assert_atomic(paired_vector)
  checkmate::assert_character(names(paired_vector), null.ok = FALSE)

  x <- stringr::str_squish(x)

  dplyr::case_when(
    x %in% names(paired_vector) ~
      paired_vector[match(x, names(paired_vector))],
    TRUE ~ NA
  ) |>
    unname()
}

# library(rutils) # github.com/danielvartan/rutils

grab_fun_par <- rutils::grab_fun_par

# library(prettycheck) # github.com/danielvartan/prettycheck

rm_caps <- function(x, start = TRUE, end = TRUE) {
  checkmate::assert_flag(start)
  checkmate::assert_flag(end)

  if (isTRUE(start)) x <- x[-1]
  if (isTRUE(end)) x <- x[-length(x)]

  x
}

# library(prettycheck) # github.com/danielvartan/prettycheck

clean_arg_list <- function(list) {
  checkmate::assert_multi_class(list, c("list", "pairlist"))
  checkmate::assert_list(as.list(list), names = "named")

  list <- list |> nullify_list()

  out <- list()

  for (i in seq_along(list)) {
    if (!names(list[i]) %in% names(out)) {
      out <- c(out, list[i])
    }
  }

  out
}

# library(prettycheck) # github.com/danielvartan/prettycheck

nullify_list <- function(list) {
  checkmate::assert_multi_class(list, c("list", "pairlist"))
  checkmate::assert_list(as.list(list), names = "named")

  for (i in names(list)) {
    if (!is.null(list[[i]]) && is.atomic(list[[i]])) {
      if (any(list[[i]] == "", na.rm = TRUE)) {
        list[i] <- list(NULL)
      }
    }
  }

  list
}

# library(rutils) # github.com/danielvartan/rutils

normalize_names <- rutils::normalize_names
