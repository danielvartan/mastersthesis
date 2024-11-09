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
#'
#' @details
#'
#' The current list of words converted to lower case is:
#' - articles
#'   - o(s), a(s), um, uma(s), uns
#' - pronouns
#'   - me, mim, meu(s), minha(s)
#'   - te, ti, teu(s), tua(s)
#'   - lhe(s), seu(s), sua(s)
#'   - nos, nosso(a)(s)
#'   - vos, vosso(a)(s)
#' - prepositions, their contractions and combinations
#'   - prepositions
#'     - a, ante, até, após, com, contra, de, desde, em, entre, para,
#'       perante, por, sem, sob, sobre, trás
#'   - contractions
#'     - à(s), do(a)(s), no(a)(s), pelo(a)(s), pro(a)(s) (informal language)
#'   - combinations
#'     - ao(s)
#' - conjunctions
#'   - conforme, conquanto, contudo, durante, embora, enquanto, então,
#'     entretanto, exceto, logo, mas, nem, ou, ora, pois, porém, porque,
#'     porquanto, portanto, quando, quanto, que, se, senão, todavia
#'
#' The above list is far from complete or exaustive, mainly due to the absence
#' of the accidental prepositions and conjuntions, which are words that are not
#' originally prepositions or conjunctions, but can play those roles in some
#' contexts, like, for instance _segundo_, which can mean either the numeral
#' _second_ or the prepositional expression _acording to_.
#'
#' @return A [`character`][base::character()] vector with the same dimension of
#'   `string`.
#'
#' @examples
#'
## to_title_case_pt("Abadia De Goiás")
## #> [1] "Abadia de Goiás" # Expected
to_title_case_pt <- function(string) {
  prettycheck:::assert_character(string)

  string |>
    stringr::str_to_title() |>
    stringr::str_replace_all( # using `c("regex" = "replacement")` syntax
      c(
        ## Articles

        "(.)\\bA(s)?\\b" = "\\1a\\2",
        "(.)\\bO(s)?\\b" = "\\1o\\2",
        "(.)\\bU((m(a(s)?)?)|ns)\\b" = "\\1u\\2",

        ## Oblique pronouns

        "(.)\\bL(he(s)?)\\b" = "\\1l\\2",
        "(.)\\bM((e(u(s)?)?)|(i(m|(nha(s)?))))\\b" = "\\1m\\2",
        "(.)\\bN(os(s[ao](s)?)?)\\b" = "\\1n\\2",
        "(.)\\bS((e(u(s)?)?)|(ua(s)?))\\b" = "\\1s\\2",
        "(.)\\bT((e(u(s)?)?)|i|(ua(s)?))\\b" = "\\1t\\2",
        "(.)\\bV(os(s[ao](s)?)?)\\b" = "\\1v\\2",

        ##  Prepositions

        "(.)\\bA((o)(s)?|nte|té|pós)\\b" = "\\1a\\2",
        "(.)\\bÀ(s)?\\b" = "\\1à\\2",
        "(.)\\bC(om|ontra)\\b" = "\\1c\\2",
        "(.)\\bD(((a|o)(s)?)|(e(sde)?))\\b" = "\\1d\\2",
        "(.)\\bE(m|ntre)\\b" =  "\\1e\\2",
        "(.)\\bN((a|o)(s)?)\\b" = "\\1n\\2",
        "(.)\\bP(ara|(e((l(a|o)(s)?)|rante))|or)\\b" = "\\1p\\2",
        "(.)\\bS(em|(ob(re)?))\\b" = "\\1s\\2",
        "(.)\\bT(rás)\\b" = "\\1t\\2",

        # Conjunctions

        "(.)\\bC(on(forme|quanto|tudo))\\b" = "\\1c\\2",
        "(.)\\bD(urante)\\b" = "\\1D\\2",
        "(.)\\bE((mbora|n(quanto|t(ão|retanto))|xceto)?)\\b" = "\\1e\\2",
        "(.)\\bL(ogo)\\b" = "\\1l\\2",
        "(.)\\bM(as)\\b" = "\\1m\\2",
        "(.)\\bN(em)\\b" = "\\1n\\2",
        "(.)\\bO(u|ra)\\b" = "\\1o\\2",
        "(.)\\bP(o(is|r(ém|qu(e|anto)|tanto)))\\b" = "\\1p\\2",
        "(.)\\bQ(u(an[dt]o|e))\\b" = "\\1q\\2",
        "(.)\\bS(e(não)?)\\b" = "\\1s\\2",
        "(.)\\bT(odavia)\\b" = "\\1t\\2"
      )
    ) |>
    stringr::str_replace_all( # Deals with contractions like "Copo D'Água".
      "(\\b\\p{L}')\\p{Ll}",
      function(m) paste0(substr(m, 1, 2), toupper(substr(m, 3, 3)))
    )
}
