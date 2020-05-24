#' Find locations of all duplicates, not just the first
#'
#' Using \code{duplicated}() will find the 2nd instance of a duplicate but not
#' all instances of a duplicate. \code{anyDup} returns the locations of all
#' duplicates.
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



