library(ggplot2)
# library(gridExtra)
# library(prettycheck) # https://github.com/danielvartan/prettycheck
library(rlang)

plot_nominal_vars <- function(data) {
  nominal_vars <- c(
    "sex", "sleep_drugs", "sleep_disorder", "medication",
    "snore", "work", "study", "no_work_or_study","alarm_w", "alarm_f"
  )

  prettycheck:::assert_tibble(data)
  prettycheck:::assert_subset(nominal_vars, names(data))

  for (i in nominal_vars) {
    plot <-
      data |>
      ggplot2::ggplot(ggplot2::aes(x = !!as.symbol(i))) +
      ggplot2::geom_bar(
        ggplot2::aes(y = ggplot2::after_stat(count / sum(count))),
        na.rm = TRUE
      ) +
      ggplot2::labs(x = i, y = NULL)

    assign(paste0("p_", i), plot)
  }

  p_sex |> print()

  gridExtra::grid.arrange(
    p_sleep_drugs,
    p_sleep_disorder,
    p_medication, p_snore,
    ncol = 2,
    nrow = 2
  ) |>
    print()

  gridExtra::grid.arrange(
    p_work,
    p_study,
    p_no_work_or_study,
    ncol = 2,
    nrow = 2
  ) |>
    print()

  gridExtra::grid.arrange(p_alarm_w, p_alarm_f, ncol = 2) |> print()

  # gridExtra::grid.arrange(
  #     p_sex, p_sleep_drugs, p_sleep_disorder, p_medication, p_snore,
  #     p_work, p_study, p_no_work_or_study, p_alarm_w, p_alarm_f,
  #     ncol = 3, nrow = 4
  # ) |>
  #     print()

  invisible(NULL)
}
