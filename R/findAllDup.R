#' Find locations of all duplicates, not just the first
#'
#' Using the base-R function \code{duplicated} will find the 2nd, 3rd, etc.
#' instance of a duplicate but not \emph{all} instances of a duplicate.
#' \code{findAllDup} returns the locations of all duplicates.
#'
#' @examples
#' findAllDup(c("Lightning", "Theo", "Kiki", "Lightning"))
#'
#' @param x A vector
#' @export
#'

findAllDup <- function(x) {

      output <-  which(duplicated(x) | duplicated(x, fromLast = TRUE))
      return(output)
}



