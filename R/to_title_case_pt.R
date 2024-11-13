# Author: José de Jesus Filho
# Source: <https://gist.github.com/jjesusfilho/454192db8356eb9c486a02698338221a>
# Comments: Adapted.

# library(stringr)

#' Convert to title case with lower case for some classes of words
#'
#' @description
#'
#' In written Portuguese (PT), when converting to title case, it is not usual
#' to keep in title case some words, like prepositions, conjunctions,
#' articles and some kinds of pronouns. This functions locates those
#' cases and converts them to lower case.
#'
#' @param string A [`character`][base::character()] vector to be converted.
#' @param articles A [`logical`][base::logical()] flag indicating if articles
#'   should be converted (default: `TRUE`).
#' @param conjuctions A [`logical`][base::logical()] flag indicating if
#'  conjuctions should be converted (default: `TRUE`).
#' @param oblique_pronouns A [`logical`][base::logical()] flag indicating if
#'   oblique pronouns should be converted (default: `TRUE`).
#' @param prepositions A [`logical`][base::logical()] flag indicating if
#'  prepositions should be converted (default: `TRUE`).
#' @param custom_rules A [`character`][base::character()] vector with custom
#'  rules to be applied. The syntax is `c("regex" = "replacement")`. The
#'  default is `c("(.)\\bD(el)\\b" = "\\1d\\2")`, which converts _Del_ to
#'  _del_.
#'
#' @details
#'
#' The list presented in this function is far from complete or exaustive, mainly
#' due to the absence of the accidental prepositions and conjuntions, which are
#' words that are not originally prepositions or conjunctions, but can play
#' those roles in some contexts, like, for instance _segundo_, which can mean
#' either the numeral _second_ or the prepositional expression _acording to_.
#'
#' @return A [`character`][base::character()] vector with the same dimension of
#'   `string`.
#'
#' @author José de Jesus Filho
#'
#' @examples
#'
#' to_title_case_pt("Abadia De Goiás")
#' #> [1] "Abadia de Goiás" # Expected
#'
#' to_title_case_pt("Desterro de Entre Rios")
#' #> [1] "Desterro de entre Rios" # Expected
#'
#' to_title_case_pt("São João Del Rei")
#' #> [1] "São João del Rei" # Expected
#'
#' to_title_case_pt("Sant'ana do Livramento")
#' #> [1] "Sant'Ana do Livramento" # Expected
#'
#' to_title_case_pt("Alta Floresta d'Oeste")
#' #> [1] "Alta Floresta D'Oeste" # Expected
to_title_case_pt <- function(
    string,
    articles = TRUE,
    conjuctions = TRUE,
    oblique_pronouns = TRUE,
    prepositions = TRUE,
    custom_rules = c("(.)\\bD(el)\\b" = "\\1d\\2") # Del
  ) {
  prettycheck:::assert_character(string)
  prettycheck:::assert_flag(articles)
  prettycheck:::assert_flag(conjuctions)
  prettycheck:::assert_flag(oblique_pronouns)
  prettycheck:::assert_flag(prepositions)
  prettycheck:::assert_character(custom_rules, null.ok = TRUE)

  prettycheck::assert_pick(
    articles,
    conjuctions,
    oblique_pronouns,
    prepositions,
    min_pick = 1
  )

  # Using `c("regex" = "replacement")` syntax
  rules <- c(custom_rules)

  if (isTRUE(articles)) {
    rules <- c(
      rules,
      # A | As
      "(.)\\bA(s)?\\b" = "\\1a\\2",
      # O | Os
      "(.)\\bO(s)?\\b" = "\\1o\\2",
      # Um | Uns | Uma | Umas
      "(.)\\bU((m(a(s)?)?)|ns)\\b" = "\\1u\\2"
    )
  }

  if (isTRUE(conjuctions)) {
    rules <- c(
      rules,
      # Conforme | Conquanto | Contudo
      "(.)\\bC(on(forme|quanto|tudo))\\b" = "\\1c\\2",
      # Durante
      "(.)\\bD(urante)\\b" = "\\1D\\2",
      # E | Embora | Enquanto | Então | Entretanto | Exceto
      "(.)\\bE((mbora|n(quanto|t(ão|retanto))|xceto)?)\\b" = "\\1e\\2",
      # Logo
      "(.)\\bL(ogo)\\b" = "\\1l\\2",
      # Mas
      "(.)\\bM(as)\\b" = "\\1m\\2",
      # Nem
      "(.)\\bN(em)\\b" = "\\1n\\2",
      # Ou | Ora
      "(.)\\bO(u|ra)\\b" = "\\1o\\2",
      # Pois | Porém | Porque | Porquanto | Portanto
      "(.)\\bP(o(is|r(ém|qu(e|anto)|tanto)))\\b" = "\\1p\\2",
      # Quando | Quanto | Que
      "(.)\\bQ(u(an[dt]o|e))\\b" = "\\1q\\2",
      # Se | Senão
      "(.)\\bS(e(não)?)\\b" = "\\1s\\2",
      # Todavia
      "(.)\\bT(odavia)\\b" = "\\1t\\2"
    )
  }

  if (isTRUE(oblique_pronouns)) {
    rules <- c(
      rules,
      # Lhe | Lhes
      "(.)\\bL(he(s)?)\\b" = "\\1l\\2",
      # Me | Meu | Meus | Mim | Minha | Minhas
      "(.)\\bM((e(u(s)?)?)|(i(m|(nha(s)?))))\\b" = "\\1m\\2",
      # Nos | Nosso | Nossa | Nossos | Nossas
      "(.)\\bN(os(s[ao](s)?)?)\\b" = "\\1n\\2",
      # Seu | Seus | Sua | Suas
      "(.)\\bS((e(u(s)?)?)|(ua(s)?))\\b" = "\\1s\\2",
      # Te | Teu | Teus | Ti | Tua | Tuas
      "(.)\\bT((e(u(s)?)?)|i|(ua(s)?))\\b" = "\\1t\\2",
      # Vos | Vosso | Vossa | Vossos | Vossas
      "(.)\\bV(os(s[ao](s)?)?)\\b" = "\\1v\\2"
    )
  }

  if (isTRUE(prepositions)) {
    rules <- c(
      rules,
      # Ao | Aos | Ante | Até | Após
      "(.)\\bA((o)(s)?|nte|té|pós)\\b" = "\\1a\\2",
      # Às
      "(.)\\bÀ(s)?\\b" = "\\1à\\2",
      # Com | Contra
      "(.)\\bC(om|ontra)\\b" = "\\1c\\2",
      # Da | Das | Do | Dos | De | Desde
      "(.)\\bD(((a|o)(s)?)|(e(sde)?))\\b" = "\\1d\\2",
      # Em | Entre
      "(.)\\bE(m|ntre)\\b" =  "\\1e\\2",
      # Na | Nas | No | Nos
      "(.)\\bN((a|o)(s)?)\\b" = "\\1n\\2",
      # Para | Perante | Pela | Pelas | Pelo | Pelos | Por
      "(.)\\bP(ara|(e((l(a|o)(s)?)|rante))|or)\\b" = "\\1p\\2",
      # Sem | Sob | Sobre
      "(.)\\bS(em|(ob(re)?))\\b" = "\\1s\\2",
      # Trás
      "(.)\\bT(rás)\\b" = "\\1t\\2",
      # Del
      "(.)\\bD(el)\\b" = "\\1d\\2"
    )
  }

  out <-
    string |>
    stringr::str_to_title() |>
    stringr::str_replace_all(rules)

  # Deals with contractions like "Copo D'Água" and "Sant'Ana do Livramento".
  out |>
    stringr::str_replace_all(
      "(\\b\\p{L}')\\p{Ll}|(\\p{Ll}')\\p{Ll}",
      function(m) paste0(substr(m, 1, 2), toupper(substr(m, 3, 3)))
    )
}
