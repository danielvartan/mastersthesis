# Load functions -----

single_quote <- function(x) paste0("'", x, "'")
double_quote <- function(x) paste0('"', x, '"')

require_pkg <- function(...) {
  out <- list(...)

  if (length(out) == 0) stop("'...' cannot be empty.")

  lapply(
    X = out,
    FUN = function(x, pattern) grepl(pattern, x),
    pattern = "^[A-Za-z][A-Za-z0-9.]+[A-Za-z0-9]$"
  )

  if (!identical(unique(unlist(out)), unlist(out))) {
    stop("'...' cannot have duplicated values.")
  }

  pkg <- unlist(out)

  namespace <- vapply(
    X = pkg,
    FUN = requireNamespace,
    FUN.VALUE = logical(1),
    quietly = TRUE,
    USE.NAMES = FALSE
  )

  if (!all(namespace, na.rm = TRUE)) {
    pkg <- pkg[!namespace]

    if (length(pkg) > 1) {
      str_1 <- "packages"
      str_2 <- "them"
      str_3 <- "c("
      str_4 <- ")"
      str_5 <- "all"
    } else {
      str_1 <- "package"
      str_2 <- "it"
      str_3 <- ""
      str_4 <- ""
      str_5 <- "the"
    }

    stop(
      paste0(
        "\n\n",
        "'.Rprofile' requires the following ", str_1, " to run:", "\n\n",
        paste0(single_quote(pkg), collapse = " "), "\n\n",
        "You can install ", str_2, " by running:", "\n\n",
        "install.packages(", str_3,
        paste(double_quote(pkg), collapse = ", "),
        str_4, ")", "\n\n",
        "Restart R (Ctrl+Shift+F10) after installing ", str_5,
        " required ", str_1, "."
      ),
      call. = FALSE
    )
  }

  invisible()
}

rprofile_message <- function(x, id) {
  if (Sys.getenv(paste0("RPROFILE_MESSAGE_", id)) == "") {
    do.call(
      what = Sys.setenv,
      args = list(TRUE) |> magrittr::set_names(paste0("RPROFILE_MESSAGE_", id))
    )
    cli::cli_alert(x, wrap = TRUE)
  }

  invisible()
}

# Assert required packages -----

require_pkg(c("cli", "here" ,"magrittr", "ragg", "renv", "stats", "stringr"))

# Load packages -----

library(magrittr)
library(ragg)

# Activate `renv` -----

rprofile_message("`renv` activation settings:", 1)

source(here::here("renv", "activate.R"))

cat("\n")

# Set options -----

options(scipen = 999)

# Warn about setting 'AGG' as the graphic device backend -----

rprofile_message(
  paste0(
    "If you haven't already set it, configure {.strong AGG} ",
    "as the RStudio graphic device backend. Learn more in ",
    "<https://ragg.r-lib.org/#use-ragg-in-rstudio>."
  ),
  2
)

cat("\n")

# Set system locale -----

source(here::here("R", "set_locale.R"))

set_locale()

# Clean the global environment -----

rm(list = ls())

# .Rprofile end -----

cat("\n")
