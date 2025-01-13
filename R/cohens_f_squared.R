# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck

# # TODO:
#
# - Add to {scaler} package.

# See <https://doi.org/10.1037/0033-2909.112.1.155> and
# <https://doi.org/10.3389/fpsyg.2012.00111> to learn more.

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
# library(prettycheck) # github.com/danielvartan/prettycheck

cohens_f_squared_effect_size <- function(f_squared) {
  prettycheck:::assert_number(f_squared, lower = 0)

  dplyr::case_when(
    f_squared >= 0.35 ~ "Large",
    f_squared >= 0.15 ~ "Medium",
    f_squared >= 0.02 ~ "Small",
    TRUE ~ "Negligible"
  )
}

# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck

cohens_f_squared_summary <- function(
    base_r_squared,
    new_r_squared = NULL
  ) {
  if (is.atomic(base_r_squared)) {
    prettycheck:::assert_number(base_r_squared, lower = 0, upper = 1)
    prettycheck:::assert_number(
      new_r_squared, lower = 0, upper = 1, null.ok = TRUE
    )

    f_squared <- cohens_f_squared(base_r_squared, new_r_squared)

    list(
      f_squared = f_squared,
      effect_size = cohens_f_squared_effect_size(f_squared)
    )
  } else {
    col_check <- c("Rsq", "SErsq", "LCL", "UCL") # psychometric::CI.Rsq()

    prettycheck:::assert_data_frame(base_r_squared)
    prettycheck:::assert_set_equal(names(base_r_squared), col_check)
    prettycheck:::assert_data_frame(new_r_squared)
    prettycheck:::assert_set_equal(names(new_r_squared), col_check)

    f_values <- c(
      cohens_f_squared(base_r_squared$UCL, new_r_squared$UCL),
      cohens_f_squared(base_r_squared$UCL, new_r_squared$LCL),
      cohens_f_squared(base_r_squared$LCL, new_r_squared$UCL),
      cohens_f_squared(base_r_squared$LCL, new_r_squared$LCL)
    )

    min_f <- ifelse(min(f_values) < 0, 0, min(f_values))
    max_f <- ifelse(max(f_values) < 0, 0, max(f_values))

    list(
      f_squared = cohens_f_squared(base_r_squared$Rsq, new_r_squared$Rsq),
      lower_ci_limit = min_f,
      upper_ci_limit = max_f,
      effect_size = cohens_f_squared_effect_size(min_f)
    )
  }
}
