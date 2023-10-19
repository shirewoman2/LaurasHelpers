#' Add stars to an appropriately rounded p value to indicate significance
#'
#' \code{starSig} checks whether the p value input is below a certain threshold,
#' rounds to the determined number of sig figs, and marks it with an asterisk or
#' asterisks as appropriate to its significance level.
#'
#' @param pval A numeric vector of p values
#' @param sig The number of significant digits to include
#' @param starlevels A vector of the three values to use for significance marked
#'   with ***, **, or * in that order. Output: character vector of rounded
#'   numbers with asterisks, e.g., c("0.03*", "0.0002**", "0.9", "0.00008***")
#' @return Returns a character string
#' @examples
#' starSig(0.0235)
#' starSig(0.0235, 2)
#' starSig(0.5)
#' starSig(0.0004235)
#' starSig(c(0.0003, 0.04, 0.09))
#' # Output will be: "0.0003***" "0.04*" "0.09"
#' starSig(c( 0.0003, 0.04, 0.09), starlevels = c(0.01, 0.05, 0.1))
#' # Output will be: "0.0003***" "0.04**" "0.09*"
#'
#' @export

starSig <- function(pval,
                    sig = 1,
                    starlevels = c(0.001, 0.01, 0.05)){

      starSig_subfun <- function(pval){

            pval <- signif(pval, sig)

            if(is.na(pval)){
                  star <- ""
            } else if(pval <= starlevels[1]){
                  star <- "\\*\\*\\*"
            } else if(pval <= starlevels[2]){
                  star <- "\\*\\*"
            } else if(pval <= starlevels[3]){
                  star <- "\\*"
            } else {
                  star <- ""
            }

            return(paste0(ifelse(is.na(pval), "", pval), star))

      }

      return(unlist(lapply(X = pval, FUN = starSig_subfun)))

}

