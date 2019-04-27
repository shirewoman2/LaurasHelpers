#' Add stars to an appropriately rounded p value to indicate significance
#'
#' \code{starSig} checks whether the p value input is below a certain threshold,
#' rounds to the determined number of sig figs, and marks it with an asterisk or
#' asterisks as appropriate to its significance level. Output is a character
#' string.
#'
#' @param pval A numeric vector of p values
#' @param sig The number of significant digits to include
#' @param starlevels A vector of the three values to use for significance marked
#'   with ***, **, or *. Output: character vector of rounded numbers with
#'   asterisks, e.g., c("0.03\*", "0.0002**", "0.9", "0.00008***")
#'
#' @examples
#' starSig(0.0235)
#' starSig(0.0235, 2)
#' starSig(0.5)
#' starSig(0.0004235)

starSig <- function(pval, sig = 1,
                    starlevels = c(0.001, 0.01, 0.05)) {
      star <- ifelse(pval < starlevels[1], "\\*\\*\\*",
                     ifelse(pval < starlevels[2], "\\*\\*",
                            ifelse(pval < starlevels[3], "\\*", "")))
      return(paste0(signif(pval, sig), star))
}

