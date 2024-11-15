library(magrittr)
# library(prettycheck) # github.com/danielvartan/prettycheck

source(here::here("R", "test_outlier.R"))

remove_outliers <- function(
    x,
    method = "iqr",
    iqr_mult = 1.5,
    sd_mult = 3
  ) {
  prettycheck:::assert_numeric(x)
  prettycheck:::assert_choice(method, c("iqr", "sd"))
  prettycheck:::assert_number(iqr_mult, lower = 1)
  prettycheck:::assert_number(sd_mult, lower = 0)

  x |>
    test_outlier(
      method = method,
      iqr_mult = iqr_mult,
      sd_mult = sd_mult
    ) %>% # Don't change the pipe.
    `!`() %>% # Don't change the pipe.
    magrittr::extract(x, .)
}
