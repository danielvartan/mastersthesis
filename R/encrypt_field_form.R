#' Encrypt variables of the `validate_field_form()` output
#'
#' @description
#'
#' `encrypt_field_form()` encrypt variables of the `validate_field_form()`
#' output` in order to preserve the anonymity of the study participants.
#'
#' @param data a [`tibble`][tibble::tibble()] with the `validate_field_form()`
#'   output.
#' @param public_key (optional) a string specifying the public key path. See
#'   [lockr::rsa_keygen()] to learn how to create an RSA key pair (default:
#'   `"./inst/ssh/id_rsa.pub"`).
#'
#' @return An invisible [`tibble`][dplyr::tibble()] with a validated
#'   `field_form` dataset with some variables encrypted.
#'
#' @inheritParams read_field_form
#' @family data wrangling functions
#' @importFrom dplyr %>%
#' @export
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("utils", quietly = TRUE)) {
#'  data <- read_field_form() %>%
#'      tidy_field_form() %>%
#'      validate_field_form() %>%
#'      encrypt_field_form()
#'
#'  utils::View(data)
#' }
#' }
encrypt_field_form <- function(
    data,
    write = FALSE,
    public_key_path = here::here("inst/ssh/id_rsa.pub")
    ) {
  checkmate::assert_tibble(data)
  checkmate::assert_flag(write)

  cli::cli_progress_step("Encrypting data")

  field_form <- data %>%
    encryptr::encrypt(
      track, name, email, birth_date, gender_identity,
      sexual_orientation, country, state, city, postal_code,
      public_key_path = public_key_path
    )

  if (isTRUE(write)) usethis::use_data(field_form, overwrite = TRUE)

  invisible(field_form)
}
