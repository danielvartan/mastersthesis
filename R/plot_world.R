# library(dplyr)
library(ggplot2)
# library(maps)
# library(prettycheck) # github.com/danielvartan/prettycheck
library(rlang)

source(here::here("R", "utils-plots.R"))

# # Helpers
#
# geocoded_data <- targets::tar_read("geocoded_data")
# geocoded_data |> plot_world_countries()
#
# geocoded_data |>
#   dplyr::mutate(
#     country = dplyr::case_match(
#       country,
#       "Cabo Verde" ~ "Cape Verde",
#       "Czechia" ~ "Czech Republic",
#       "Russian Federation" ~ "Russia",
#       "United Kingdom" ~ "UK",
#       "United States" ~ "USA",
#       .default = country
#     )
#   ) |>
#   tidyr::drop_na() |>
#   dplyr::pull(country) |>
#   unique() |>
#   setdiff(world_data$region |> unique())

plot_world_countries <- function(
    data,
    col_fill = NULL,
    col_country = "country",
    transform = "log10", # See ?ggplot2::scale_fill_gradient
    direction = 1,
    binned = TRUE,
    breaks = ggplot2::waiver(),
    n_breaks = NULL,
    labels = ggplot2::waiver(),
    reverse = TRUE,
    limits = NULL,
    print = TRUE,
    quiet = FALSE
  ) {
  checkmate::assert_tibble(data)
  checkmate::assert_subset("country", names(data))
  checkmate::assert_string(col_fill, null.ok = TRUE)
  checkmate::assert_choice(col_fill, names(data), null.ok = TRUE)
  checkmate::assert_string(col_country)
  checkmate::assert_choice(col_country, names(data))
  checkmate::assert_character(data[[col_country]])
  checkmate::assert_multi_class(transform, c("character", "transform"))
  checkmate::assert_choice(direction, c(-1, 1))
  checkmate::assert_flag(binned)
  checkmate::assert_multi_class(breaks, c("function", "numeric", "waiver"))
  checkmate::assert_int(n_breaks, lower = 1, null.ok = TRUE)
  checkmate::assert_multi_class(labels, c("function", "numeric", "waiver"))
  checkmate::assert_flag(reverse)
  checkmate::assert_flag(print)
  checkmate::assert_flag(quiet)

  checkmate::assert_multi_class(
    limits, c("numeric", "function"), null.ok = TRUE
  )

  data <-
    data |>
    dplyr::mutate(
      !!as.symbol(col_country) := dplyr::case_match(
        !!as.symbol(col_country),
        "Cabo Verde" ~ "Cape Verde",
        "Czechia" ~ "Czech Republic",
        "Russian Federation" ~ "Russia",
        "United Kingdom" ~ "UK",
        "United States" ~ "USA",
        .default = !!as.symbol(col_country)
      )
    ) |>
    get_map_fill_data(
      col_fill = col_fill,
      col_code = col_country,
      name_col_ref = "ID",
      quiet = quiet
    ) |>
    dplyr::right_join(
      maps::map("world", plot = FALSE, fill = TRUE) |>
        sf::st_as_sf(),
      by = "ID"
    )

  plot <-
    data |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(
      ggplot2::aes(geometry = geom, fill = n),
      color = "gray75",
      linewidth = 0.1,
      fill = "white"
    ) +
    ggplot2::geom_sf(
      ggplot2::aes(geometry = geom,fill = n),
      color = NA
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(-150, 150, 50)
      # limits = c(-180, 180)
    )

  if (data |> tidyr::drop_na() |> nrow() == 1) binned <- FALSE

  plot <-
    plot +
    scale_brand(
      aesthetics = "fill",
      scale_type = ifelse(isTRUE(binned), "binned", "continuous"),
      direction = direction,
      breaks = breaks,
      n.breaks = n_breaks,
      labels = labels,
      reverse = reverse,
      limits = limits,
      transform = transform
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      fill = NULL
    )

  if (isTRUE(print)) print(plot)

  invisible(plot)
}
