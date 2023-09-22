# # TODO:
#
# * Document functions.

require(checkmate, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(geobr, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(gutils, quietly = TRUE)
require(stringr, quietly = TRUE)
require(viridis, quietly = TRUE)

plot_brazil_uf_map <- function(data, option = "viridis", text_size = NULL) {
  option_choices <- c(
    "magma", "A", "inferno", "B", "plasma", "C", "viridis", "D",
    "cividis", "E", "rocket", "F", "mako", "G", "turbo", "H"
  )

  checkmate::assert_tibble(data)
  checkmate::assert_subset("state", names(data))
  checkmate::assert_choice(option, option_choices, null.ok = TRUE)
  checkmate::assert_number(text_size, null.ok = TRUE)
  gutils:::assert_internet()

  brazil_uf_map <-
    geobr::read_country(year = 2020, showProgress = FALSE) |>
    gutils:::shush() |>
    dplyr::mutate(
      name_state = stringr::str_replace_all(name_state, " Do ", " do "),
      name_state = stringr::str_replace_all(name_state, " De ", " de ")
    )

  uf_list <- unique(brazil_uf_map$name_state)

  out <-
    data |>
    dplyr::count(state) |>
    dplyr::rename(name_state = state)

  plot <-
    brazil_uf_map |>
    dplyr::left_join(out, by = "name_state") |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = n)) +
    ggplot2::labs(x = "Longitude", y = "Latitude") +
    ggplot2::theme(text = ggplot2::element_text(size = text_size))

  if (!is.null(option)) {
    plot <- plot + viridis::scale_fill_viridis(option = option)
  }

  print(plot)
  invisible(plot)
}
