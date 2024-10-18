# # TODO:
#
# * Document functions.
# * Add to {scaler} package.

# See https://doi.org/10.1037/0033-2909.112.1.155
#     https://doi.org/10.3389/fpsyg.2012.00111
# to learn more.

# library(dplyr)
# library(prettycheck) # https://github.com/danielvartan/prettycheck

cohens_f_squared <- function(base_r_squared, new_r_squared = NULL) {
  prettycheck:::assert_number(base_r_squared, lower = 0, upper = 1)
  prettycheck:::assert_number(
    new_r_squared, lower = 0, upper = 1, null.ok = TRUE
  )

  if (is.null(new_r_squared)) {
    base_r_squared / (1 - base_r_squared)
  } else {
    (new_r_squared - base_r_squared) / (1 - new_r_squared)
  }
}

# library(dplyr)
# library(prettycheck) # https://github.com/danielvartan/prettycheck

cohens_f_squared_effect_size <- function(f_squared) {
  prettycheck:::assert_number(f_squared, lower = - 1, upper = 1)

  dplyr::case_when(
    abs(f_squared) >= 0.35 ~ "Large",
    abs(f_squared) >= 0.15 ~ "Medium",
    abs(f_squared) >= 0.02 ~ "Small",
    TRUE ~ "Negligible"
  )
}

# library(dplyr)
# library(prettycheck) # https://github.com/danielvartan/prettycheck

cohens_f_squared_summary <- function(base_r_squared, new_r_squared = NULL) {
  prettycheck:::assert_number(base_r_squared, lower = 0, upper = 1)
  prettycheck:::assert_number(
    new_r_squared, lower = 0, upper = 1, null.ok = TRUE
  )

  f_squared <- cohens_f_squared(base_r_squared, new_r_squared)
  category <- cohens_f_squared_effect_size(f_squared)

  dplyr::tibble(
    name = c("f_squared", "effect_size"),
    value = c(f_squared, category)
  )
}
