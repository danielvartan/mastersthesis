# library(cli)
# library(encryptr)
# library(here)
# library(lockr) # github.com/danielvartan/lockr
# library(prettycheck) # github.com/danielvartan/prettycheck

#' Encrypt variables of `validate_data()` output
#'
#' @description
#'
#' `lock_data()` encrypt variables of `validate_data()` output` in order to
#' preserve the anonymity of the study participants.
#'
#' @param data A [`tibble`][tibble::tibble()] with the `validate_data()`
#'   output.
#' @param public_key (optional) an [`openssl`][openssl::rsa_keygen()] RSA
#'   public key or a string specifying the public key path. See
#'   [`rsa_keygen()`][lockr::rsa_keygen] to learn how to create an RSA key
#'   pair (default: `here::here("_ssh/id_rsa.pub")`).
#'
#' @return An invisible [`tibble`][dplyr::tibble()] with a validated dataset
#'   with some variables encrypted.
#'
#' @family data munging functions
#' @noRd
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("utils")) {
#'  data <-
#'   get_data() |>
#'   tidy_data() |>
#'   validate_data() |>
#'   lock_data()
#'
#'  utils::View(data)
#' }
#' }
lock_data <- function(
    data,
    public_key_path = here::here("_ssh/id_rsa.pub")
  ) {
  prettycheck:::assert_tibble(data)
  lockr:::assert_public_key(public_key)

  cli::cli_progress_step("Locking data")

  out <-
    data |>
    encryptr::encrypt(
      track, name, email, birth_date, gender_identity,
      sexual_orientation, country, state, city, postal_code,
      public_key_path = public_key_path
    )

  invisible(out)
}
