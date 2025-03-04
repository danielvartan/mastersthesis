# Set session ID -----

session_id <- Sys.time() |> as.character()

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

rprofile_cat_line <- function(session_id, env_var = "RPROFILE_MESSAGES") {
  if (Sys.getenv(env_var) == session_id) cat("\n")

  invisible()
}

rprofile_message <- function(
    x,
    session_id,
    env_var = "RPROFILE_MESSAGES",
    cat_line = TRUE,
    info = FALSE
  ) {
  if (Sys.getenv(env_var) %in% c("", session_id)) {
    do.call(
      what = Sys.setenv,
      args =
        list(session_id) |>
        magrittr::set_names(env_var)
    )

    if (isTRUE(info)) {
      cli::cli_alert_info(x, wrap = TRUE)
    } else {
      cli::cli_alert(x, wrap = TRUE)
    }

    if (isTRUE(cat_line)) cat("\n")
  }

  invisible()
}

# Assert required packages -----

require_pkg(
  c(
    "cli", "here", "httpgd", "magrittr", "ragg", "renv", "stats", "stringr",
    "vscDebugger"
  )
)

# Load packages -----

library(magrittr)
library(ragg)

# Show session message -----

rprofile_message(
  x = "The messages below are shown only once per R session.",
  session_id = session_id,
  info = TRUE
)

# Activate `renv` -----

rprofile_message("`renv` activation settings:", session_id)

source(here::here("renv", "activate.R"))

rprofile_cat_line(session_id)

# Set options -----

options(scipen = 999)

# Warn about setting 'AGG' as the graphic device backend -----

rprofile_message(
  paste0(
    "If you haven't already set it, configure {.strong AGG} ",
    "as the RStudio graphic device backend. Learn more at ",
    "<https://ragg.r-lib.org/#use-ragg-in-rstudio>."
  ),
  session_id
)

# Set system locale -----

source(here::here("R", "set_locale.R"))

set_locale(session_id)

# End line -----

rprofile_cat_line(session_id = session_id, env_var = "SET_LOCALE_MESSAGES")

# Clean the global environment -----

rm(list = ls())
