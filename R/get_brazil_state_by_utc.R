# library(here)
# library(prettycheck) # github.com/danielvartan/prettycheck

source(here::here("R", "get_brazil_state.R"))

# Based on the 2024b dataset (Released 2024-09-04) from the
# Internet Assigned Numbers Authority (IANA).
# See <https://www.iana.org/time-zones> to learn more.

get_brazil_state_by_utc <- function(utc = -3, type = "fu") {
  prettycheck:::assert_choice(utc, -5:-2)
  prettycheck:::assert_choice(type, c("fu", "state"))

  if (utc == -2) {
    # PE -> Except Atlantic islands -> Fernando de Noronha
    out <- "PE"

    if (type == "fu") out else get_brazil_state(out)
  } else if (utc == -3) {
    out <- c(
      "AL", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "PA", "PB", "PR",
      "PE", "PI", "RJ", "RN", "RS", "SC", "SP", "SE", "TO"
    )

    if (type == "fu") out else get_brazil_state(out)
  } else if (utc == -4) {
    # AM -> East (Except far west)
    out <- c("AM", "MT", "MS", "RO", "RR")

    if (type == "fu") out else get_brazil_state(out)
  } else if (utc == -5) {
    # AM -> West (Far west)
    out <- c("AC", "AM")

    if (type == "fu") out else get_brazil_state(out)
  }
}
