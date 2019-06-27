#' Convert times from numbers to POSIXct
#'
#' Since data imported from Excel often changes date and time data to numeric,
#' \code{timeConv} converts dates from decimal numbers to Date with a time zone
#' of "UTC" unless otherwise specified.
#'
#' @param x A numeric string
#' @export
#'

timeConv <- function(x, tz = "UTC") {

      library(lubridate)
      output <- ymd_hms(format(as.POSIXct(as.Date(x, origin = "1899-12-30")),
                               tz = tz))

      return(output)
}


