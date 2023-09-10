#' Encrypt variables of `validate_data()` output
#'
#' @description
#'
#' `lock_data()` encrypt variables of `validate_data()` output` in order to
#' preserve the anonymity of the study participants.
#'
#' @param data A [`tibble`][tibble::tibble()] with the `validate_data()`
#'   output.
#'
#' @return An invisible [`tibble`][dplyr::tibble()] with a validated dataset
#'   with some variables encrypted.
#'
#' @template param_public_key
#' @family data wrangling functions
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("utils", quietly = TRUE)) {
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
    public_key_path = here::here(".ssh/id_rsa.pub")
    ) {
  checkmate::assert_tibble(data)
  lockr:::assert_public_key(public_key)

  cli::cli_progress_step("Locking data")

  out <- data |>
    encryptr::encrypt(
      track, name, email, birth_date, gender_identity,
      sexual_orientation, country, state, city, postal_code,
      public_key_path = public_key_path
    )

  invisible(out)
}
