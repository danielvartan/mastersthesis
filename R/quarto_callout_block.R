require(checkmate, quietly = TRUE)

# use '#| output: asis'
quarto_callout_block <- function(text, type, options = "") {
  choices_type <- c("note", "warning", "important", "tip", "caution")

  checkmate:: assert_string(text)
  checkmate::assert_choice(type, choices_type)
  checkmate::assert_string(options)

  options <- ifelse(options == "", "", paste0(" ", options))

  cat(paste0(
    "\n",
    "::: {.callout-", type, options, "}", "\n",
    text, "\n",
    ":::",
    "\n"
  ))
}
