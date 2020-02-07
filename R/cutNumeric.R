#' Cut a set of numeric data into bins and then set the new value to be the left
#' or right side of the bin
#'
#'
#' @param x A numeric string
#' @param breaks the desired breaks in that string; these are the same as for
#'   the base function "cut"
#'
#' @export
#'

cutNumeric <- function(x, breaks){

      Out <- x
      Out <- cut(Out, breaks)
      Out <- stringr::str_extract(Out, "[0-9]{1,}\\]|0\\.[0-9]{1,}\\]")
      Out <- as.numeric(sub("\\]", "", Out))
      Out[which(x == 0)] <- 0
      return(Out)

}


