extract_from_summary <- function(x, element = "r.squared") {
  checkmate::assert_list(x)
  checkmate::assert_string(element)

  summary <- summary(x)

  if (is.null(names(summary))) {
    cli::cli_abort(paste0(
      "{.strong {cli::col_red('summary(x)')}} has no names."
    ))
  }

  if (!element %in% names(summary)) {
    cli::cli_abort(paste0(
      "There is no element named {.strong {cli::col_red(element)}} in ",
      "{.strong summary(x)}."
    ))
  }

  summary(x)[[element]]
}

r_squared <- function(x) extract_from_summary(x, "r.squared")
adj_r_squared <- function(x) extract_from_summary(x, "adj.r.squared")
f_statistic <- function(x) extract_from_summary(x, "fstatistic")

std_error <- function(x){
  checkmate::assert_numeric(x)

  stats::sd(x, na.rm = TRUE) / sqrt(length(x))
}
