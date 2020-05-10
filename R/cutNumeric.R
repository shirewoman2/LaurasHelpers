#' Cut numeric data into bins with vector output
#'
#' This function cuts a vector of numbers just like the base R function
#' \code{\link[base]{cut}} except that the output will be a vector of numbers
#' rather than that weird factor that's the default. The number listed in the
#' output will be the lower boundary of the bin for the number that was used in
#' the input.
#'
#' @param x A numeric string
#' @param breaks the desired breaks in that string; these are the same as for
#'   the base function "cut"
#'
#' @return Returns a vector of numeric data
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


