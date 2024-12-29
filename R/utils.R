# library(prettycheck) # github.com/danielvartan/prettycheck

change_sign <- function(x, flag = TRUE) {
  prettycheck:::assert_numeric(x)
  prettycheck:::assert_flag(flag)

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
  prettycheck:::assert_string(path)
  prettycheck:::assert_string(root)

  path <- stringr::str_remove(path, here::here())

  paste0(root, path)
}

# library(hms)
# library(lubritime) # github.com/danielvartan/lubritime
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils

# TODO: Refactor and move to `lubritime`.
transform_time <- function(x, threshold = hms::parse_hms("12:00:00")) {
  prettycheck:::assert_atomic(x)
  prettycheck:::assert_hms(
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
  } else if (prettycheck:::test_multi_class(x, classes)) {
    x |> lubritime:::extract_seconds()
  } else {
    x
  }
}

# library(hms)
# library(lubridate)
# library(lubritime) # github.com/danielvartan/lubritime
# library(rutils) # github.com/danielvartan/rutils

# TODO: Try to integrate this with `lubritime:::link_to_timeline()`.
midday_trigger <- function(x, trigger = hms::parse_hm("22:00")) {
  prettycheck:::assert_hms(trigger)

  if (hms::is_hms(x) && any(x > trigger, na.rm = TRUE)) {
    lubritime:::link_to_timeline(
      x, threshold = hms::parse_hms("12:00:00")
    )
  } else if (hms::is_hms(x) && !any(x > trigger, na.rm = TRUE)) {
    lubridate::as_datetime(x, tz = "UTC")
  } else {
    x
  }
}

# library(prettycheck) # github.com/danielvartan/prettycheck

# TODO: Move to `rutils`.
count_not_na <- function(x) {
  prettycheck:::assert_atomic(x)

  length(which(!is.na(x)))
}

# library(prettycheck) # github.com/danielvartan/prettycheck

# TODO: Move to `rutils`.
drop_inf <- function(x) {
  prettycheck:::assert_atomic(x)

  x[!(x == -Inf | x == Inf)]
}

# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(tidyr)

list_as_tibble <- function(list) {
  prettycheck:::assert_list(list)

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
    digits = 10
  ) {
  prettycheck:::assert_list(x)
  prettycheck:::assert_file_exists(path)

  out <- yaml::read_yaml(path)

  for (i in seq_along(x)) {
    if (is.numeric(x[[i]])) {
      x[[i]] <- x[[i]] |> signif(10)
    }

    if (names(x)[i] %in% names(out)) {
      out[[names(x)[i]]] <- x[[i]]
    } else {
      out <- c(out, x[i])
    }
  }

  yaml::write_yaml(out, path)
}

library(magrittr)
# library(prettycheck) # github.com/danielvartan/prettycheck

format_to_md_latex <- function(
    x,
    after = NULL,
    before = NULL,
    round = 3,
    decimal_mark = ".",
    big_mark = ",",
    key = "$"
  ) {
  prettycheck:::assert_numeric(x)
  prettycheck:::assert_string(after, null.ok = TRUE)
  prettycheck:::assert_string(before, null.ok = TRUE)
  prettycheck:::assert_string(decimal_mark)
  prettycheck:::assert_string(big_mark)
  prettycheck:::assert_number(round, lower = 0)
  prettycheck:::assert_string(key)

  if (is.null(after)) after <- ""

  x |>
    round(round) |>
    format(
      decimal.mark = decimal_mark,
      big.mark = big_mark,
      scientific = FALSE
    ) %>% # Don't change the pipe!
    paste0(key, before, ., after, key) |>
    I()
}

library(magrittr)
# library(prettycheck) # github.com/danielvartan/prettycheck

inverse_log_max <- function(x, base = exp(1)) {
  prettycheck:::assert_numeric(x)
  prettycheck:::assert_number(base)

  x |>
    log(base) |>
    max(na.rm = TRUE) |>
    ceiling() %>% # Don't change the pipe!
    `^`(base, .)
}

# library(prettycheck) # github.com/danielvartan/prettycheck

to_ascii <- function(x, from = "UTF-8") {
  prettycheck:::assert_character(x)
  prettycheck:::assert_string(from)

  x |> iconv(to = "ASCII//TRANSLIT")
}

# library(prettycheck) # github.com/danielvartan/prettycheck

to_ascii_and_lower <- function(x, from = "UTF-8") {
  prettycheck:::assert_character(x)
  prettycheck:::assert_string(from)

  x |> to_ascii(from) |> tolower()
}

# library(clipr)
# library(prettycheck) # github.com/danielvartan/prettycheck

vector_to_c <- function(x, quote = TRUE, clipboard = TRUE) {
  prettycheck:::assert_atomic(x)

  if (isTRUE(quote)) x <- paste0('"', x, '"')

  out <- paste0("c(", paste(x, collapse = ", "), ")")

  if (isTRUE(clipboard)) {
    cli::cli_alert_info("Copied to clipboard.")

    out |> clipr::write_clip()
  }

  cat(out)

  invisible(out)
}

# library(prettycheck) # github.com/danielvartan/prettycheck

cli_test_fun <- function(test) {
  prettycheck:::assert_flag(test)

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
  prettycheck:::assert_atomic(x)
  prettycheck:::assert_atomic(paired_vector)
  prettycheck:::assert_character(names(paired_vector), null.ok = FALSE)

  x <- stringr::str_squish(x)

  dplyr::case_when(
    x %in% names(paired_vector) ~
      paired_vector[match(x, names(paired_vector))],
    TRUE ~ NA
  ) |>
    unname()
}

# Credits: B. Christian Kamgang
# Source: Adapted from
# <https://stackoverflow.com/questions/66329835/
# how-to-get-all-parameters-passed-into-a-function-with-their-values>

grab_fun_par <- function() {
  args_names <- ls(envir = parent.frame(), all.names = TRUE, sorted = FALSE)

  if ("..." %in% args_names) {
    dots <- eval(quote(list(...)), envir = parent.frame())
  } else {
    dots = list()
  }

  args_names <- sapply(setdiff(args_names, "..."), as.name)

  if(length(args_names)) {
    not_dots <- lapply(args_names, eval, envir = parent.frame())
  } else {
    not_dots <- list()
  }

  out <- c(not_dots, dots)

  out[names(out) != ""]
}

# library(prettycheck) # github.com/danielvartan/prettycheck

rm_caps <- function(x, start = TRUE, end = TRUE) {
  prettycheck:::assert_flag(start)
  prettycheck:::assert_flag(end)

  if (isTRUE(start)) x <- x[-1]
  if (isTRUE(end)) x <- x[-length(x)]

  x
}

# library(prettycheck) # github.com/danielvartan/prettycheck

clean_arg_list <- function(list) {
  prettycheck:::assert_multi_class(list, c("list", "pairlist"))
  prettycheck:::assert_list(as.list(list), names = "named")

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
  prettycheck:::assert_multi_class(list, c("list", "pairlist"))
  prettycheck:::assert_list(as.list(list), names = "named")

  for (i in names(list)) {
    if (!is.null(list[[i]]) && is.atomic(list[[i]])) {
      if (any(list[[i]] == "", na.rm = TRUE)) {
        list[i] <- list(NULL)
      }
    }
  }

  list
}

# library(clipr)
library(magrittr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(stringr)

normalize_names <- function(
    path = clipr::read_clip(),
    exceptions = c(
      "^README", "^OFL.txt$", "^DESCRIPTION", "^Google README.txt$"
    ),
    just_dirs = FALSE
  ) {
  prettycheck:::assert_string(path)
  prettycheck:::assert_directory_exists(path)
  prettycheck:::assert_character(exceptions)
  prettycheck:::assert_flag(just_dirs)

  dirs <- path |> list.dirs()
  if (length(dirs) == 0) dirs <- path

  if (!length(dirs) == 0 && isTRUE(just_dirs)) {
    dirs <- dirs |> magrittr::extract(-1)

    new_dir_name <-
      dirs |>
      basename() |>
      tolower() |>
      stringr::str_replace_all(" - ", "_") |>
      stringr::str_replace_all(" ", "-") |>
      stringr::str_squish() %>%
      file.path(dirname(dirs), .)

    for (i in rev(seq_along(dirs))) {
      if (!dirs[i] == new_dir_name[i]) {
        paste(
          "mv",
          glue::single_quote(dirs[i]),
          glue::single_quote(new_dir_name[i])
        ) |>
          system()
      }
    }

    dirs <-
      path |>
      list.dirs() |>
      magrittr::extract(-1)
  } else {
    for (i in dirs) {
      files <- list.files(i)

      for (j in exceptions) {
        files <- files |> stringr::str_subset(j, negate = TRUE)
      }

      if (length(files) == 0) {
        next
      } else {
        new_name <-
          files |>
          tolower() |>
          stringr::str_replace_all(" - ", "_") |>
          stringr::str_replace_all(" ", "-") |>
          stringr::str_squish()

        file.rename(file.path(i, files), file.path(i, new_name))
      }
    }
  }

  invisible()
}
