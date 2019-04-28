#' Convert dates and times from numbers to POSIXct
#'
#' Since data imported from Excel often changes date and time data to numeric,
#' \code{dateConv} converts dates and times from decimal numbers to POSIXct.
#'
#' @param x A numeric string
#'

dateConv <- function(x) {

      output <- as.Date(as.numeric(x), origin = "1899-12-30")
      return(output)
}



