#' Find all duplicates, not just the first
#'
#' Using \code{duplicated}() will find the 2nd instance of a duplicate but not
#' all instances of a duplicate. \code{anyDup} returns all duplicates.
#'
#' @param x A vector
#' @export
#'

anyDup <- function(x) {

      output <-  x[duplicated(x) | duplicated(x, fromLast = TRUE)]
      return(output)
}



