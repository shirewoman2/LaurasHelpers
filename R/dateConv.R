#' Convert dates from numbers to Date
#'
#' Since data imported from Excel often changes date and time data to numeric,
#' \code{dateConv} converts dates from decimal numbers to Date.
#'
#' @param x A numeric string
#' @export
#'

dateConv <- function(x) {

      output <- as.Date(as.numeric(x), origin = "1899-12-30")
      return(output)
}



