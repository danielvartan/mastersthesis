# # TODO:
#
# * Document functions.

## See <https://gt.rstudio.com/> & <https://gt.albert-rapp.de/>.

# library(cli)
# library(dplyr)
# library(gt)
# library(here)
# library(prettycheck) # github.com/danielvartan/prettycheck

source(here::here("R/test_normality.R"))

exploratory_table <- function(x, big_mark = ",", decimal_mark = ".",  ...) {
  prettycheck:::assert_atomic(x)
  prettycheck:::assert_choice(big_mark, c(".", ","))
  prettycheck:::assert_choice(decimal_mark, c(".", ","))

  if (big_mark == decimal_mark) {
    cli::cli_abort(paste0(
      "{.strong {cli::col_blue('big_mark')}} and ",
      "{.strong {cli::col_blue('decimal_mark')}} cannot have the same value."
    ))
  }

  stats <- test_normality(x, print = FALSE, ...)

  if (is.numeric(x)) {
    dplyr::tibble(
      Statistic = c(
        "Valid $n$",
        "Mean $\\pm$ SD",
        "Minimum (Q0)",
        "1st Quartile (Q1)",
        "Median (Q2)",
        "3rd Quartile (Q3)",
        "Maximum (Q4)",
        "Skewness",
        "Kurtosis"
      ),
      Value = c(
        paste0(
          "$",
          format(
            round(stats$stats$n_rm_na, 3), big.mark = big_mark,
            decimal.mark = decimal_mark
          ),
          "$"
        ),
        paste0(
          "$",
          format(
            round(stats$stats$mean, 3), big.mark = big_mark,
            decimal.mark = decimal_mark
          ),
          " \\pm ",
          format(
            round(stats$stats$sd, 3), big.mark = big_mark,
            decimal.mark = decimal_mark
          ),
          "$"
        ),
        paste0(
          "$",
          format(
            round(stats$stats$min, 3), big.mark = big_mark,
            decimal.mark = decimal_mark
          ),
          "$"
        ),
        paste0(
          "$",
          format(
            round(stats$stats$q_1, 3), big.mark = big_mark,
            decimal.mark = decimal_mark
          ),
          "$"),
        paste0(
          "$",
          format(
            round(stats$stats$median, 3), big.mark = big_mark,
            decimal.mark = decimal_mark
          ),
          "$"
        ),
        paste0(
          "$",
          format(
            round(stats$stats$q_3, 3), big.mark = big_mark,
            decimal.mark = decimal_mark
          ),
          "$"
        ),
        paste0(
          "$",
          format(
            round(stats$stats$max, 3), big.mark = big_mark,
            decimal.mark = decimal_mark
          ),
          "$"
        ),
        paste0(
          "$",
          format(
            round(stats$stats$skewness, 3), big.mark = big_mark,
            decimal.mark = decimal_mark
          ),
          "$"
        ),
        paste0(
          "$",
          format(
            round(stats$stats$kurtosis, 3), big.mark = big_mark,
            decimal.mark = decimal_mark
          ),
          "$"
        )
      ),
      "Nomality test" = c(
        "Anderson-Darling",
        "Bonett-Seier test of Geary's kurtosis",
        "Cramer-von Mises",
        "D'Agostino",
        "Jarque–Bera",
        "Lilliefors (Kolmogorov-Smirnov)",
        "Pearson $\\chi^{2}$",
        "Shapiro-Francia",
        "Shapiro-Wilk"
      ),
      "Test statistic" = c(
        paste0(
          "$A = ",
          format(
            round(stats$ad$statistic, 3), big.mark = big_mark,
            decimal.mark = decimal_mark
          ),
          "$"
        ),
        paste0(
          "$\\tau = ",
          format(
            round(stats$bonett$statistic[1], 3), big.mark = big_mark,
            decimal.mark = decimal_mark
          ),
          "$", "\n",
          "$z = ",
          format(
            round(stats$bonett$statistic[2], 3), big.mark = big_mark,
            decimal.mark = decimal_mark
          ),
          "$"
        ),
        paste0(
          "$W = ",
          format(
            round(stats$cvm$statistic, 3), big.mark = big_mark,
            decimal.mark = decimal_mark
          ),
          "$"
        ),
        paste0(
          "$Z3 = ",
          format(
            round(attr(stats$dagostino, "test")$statistic[2], 3),
            big.mark = big_mark, decimal.mark = decimal_mark
          ),
          "$"
        ),
        paste0(
          "$\\chi^{2} = ",
          format(
            round(stats$jarque_bera$statistic, 3), big.mark = big_mark,
            decimal.mark = decimal_mark
          ),
          "$", "\n",
          "$df = ",
          format(
            round(stats$jarque_bera$parameter, 3), big.mark = big_mark,
            decimal.mark = decimal_mark
          ),
          "$"
        ),
        paste0(
          "$D = ",
          format(
            round(stats$lillie_ks$statistic, 3), big.mark = big_mark,
            decimal.mark = decimal_mark
          ),
          "$"
        ),
        paste0(
          "$P = ",
          format(
            round(stats$pearson$statistic, 3), big.mark = big_mark,
            decimal.mark = decimal_mark
          ),
          "$"
        ),
        ifelse(
          is.null(stats$shapiro), "NA",
          paste0(
            "$W = ",
            format(
              round(stats$shapiro$statistics, 3), big.mark = big_mark,
              decimal.mark = decimal_mark
            ),
            "$"
          )
        ),
        ifelse(
          is.null(stats$sf), "NA",
          paste0(
            "$W = ",
            format(
              round(stats$sf$statistics, 3), big.mark = big_mark,
              decimal.mark = decimal_mark
            ),
            "$"
          )
        )
      ),
      "p-value" = c(
        paste0(
          "$",
          format(
            round(stats$ad$p.value, 5), big.mark = big_mark,
            decimal.mark = decimal_mark, nsmall = 5
          ),
          sig_level_asterisks(stats$ad$p.value),
          "$"
        ),
        paste0(
          "$",
          format(
            round(stats$bonett$p.value, 5), big.mark = big_mark,
            decimal.mark = decimal_mark, nsmall = 5
          ),
          sig_level_asterisks(stats$bonett$p.value),
          "$"
        ),
        paste0(
          "$",
          format(
            round(stats$cvm$p.value, 5), big.mark = big_mark,
            decimal.mark = decimal_mark, nsmall = 5
          ),
          sig_level_asterisks(stats$cvm$p.value),
          "$"
        ),
        paste0(
          "$",
          format(
            round(attr(stats$dagostino, "test")$p.value[2], 5),
            big.mark = big_mark, decimal.mark = decimal_mark, nsmall = 5
          ),
          sig_level_asterisks(attr(stats$dagostino, "test")$p.value[2]),
          "$"
        ),
        paste0(
          "$",
          format(
            round(stats$jarque_bera$p.value, 5), big.mark = big_mark,
            decimal.mark = decimal_mark, nsmall = 5
          ),
          sig_level_asterisks(stats$jarque_bera$p.value),
          "$"
        ),
        paste0(
          "$",
          format(
            round(stats$lillie_ks$p.value, 5), big.mark = big_mark,
            decimal.mark = decimal_mark, nsmall = 5
          ),
          sig_level_asterisks(stats$lillie_ks$p.value),
          "$"
        ),
        paste0(
          "$",
          format(
            round(stats$pearson$p.value, 5), big.mark = big_mark,
            decimal.mark = decimal_mark, nsmall = 5
          ),
          sig_level_asterisks(stats$pearson$p.value),
          "$"
        ),
        ifelse(
          is.null(stats$shapiro), "NA",
          paste0(
            "$A = ",
            format(
              round(stats$shapiro$p.value, 3), big.mark = big_mark,
              decimal.mark = decimal_mark
            ),
            sig_level_asterisks(stats$shapiro$p.value),
            "$"
          )
        ),
        ifelse(
          is.null(stats$sf), "NA",
          paste0(
            "$",
            format(
              round(stats$sf$p.value, 5), big.mark = big_mark,
              decimal.mark = decimal_mark, nsmall = 5
            ),
            sig_level_asterisks(stats$sf$p.value),
            "$"
          )
        )
      )
    ) |>
      gt::gt()
  } else{
    NULL
  }
}

# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck

sig_level_asterisks <- function(x) {
  prettycheck:::assert_number(x)

  dplyr::case_when(
    x < 0.0001 ~ "****",
    x < 0.001 ~ "***",
    x < 0.01 ~ "**",
    x < 0.05 ~ "*",
    TRUE ~ ""
  )
}

# library(exams)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(readr)
# library(stringr)

# Code from <https://gt.albert-rapp.de/case_studies.html#latex-formulas>.
latex_to_svg <- function(x, color = "black") {
  prettycheck:::assert_string(x)
  prettycheck:::assert_choice(color, c("black", "white"))

  temp_dir <- tempdir()
  temp_name <- basename(tempfile())

  exams::tex2image(
    tex = x,  format = "svg", dir = temp_dir, name = temp_name
  )

  svg_formula_black <-
    readr::read_lines('formula.svg') |>
    stringr::str_flatten()

  svg_formula_white <-
    svg_formula_black |>
    # Overline color uses stroke, rest uses fill
    stringr::str_replace_all('stroke:rgb\\(0%,0%,0%\\)', 'stroke:#FFFFFF') |>
    stringr::str_replace_all('fill:rgb\\(0%,0%,0%\\)', 'fill:#FFFFFF')

  if (color == "black") {
    svg_formula_black
  } else {
    svg_formula_white
  }
}
