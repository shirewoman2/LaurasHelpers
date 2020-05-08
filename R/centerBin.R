#' Cut a set of numeric data into bins based on the middle value of the bin
#'
#' This function cuts a vector of numeric data such that the breaks are set
#' using the middle value of a given bin rather than setting upper or lower
#' boundaries for each bin. For example, say you've got a vector of blood draw
#' times that weren't drawn at *exactly* the nominal time but were close to it.
#' Set the bins here to the nominal times, and the data will be filed into the
#' slot that's closest to the nominal time at which that sample should have been
#' drawn.
#'
#' @param x A numeric string
#' @param breaks the desired breaks in that string set by what the middle value
#'   should be
#'
#' @return Returns a vector of numeric data
#' @export
#'


centerBin <- function(x, breaks){

      CenterBins <- data.frame(LfBin = c(NA, breaks),
                               RtBin = c(breaks, NA),
                               stringsAsFactors = FALSE)
      CenterBins$LfBin[1] <- breaks[1]
      CenterBins$RtBin[length(breaks) + 1] <- breaks[length(breaks)]
      CenterBins$Mid <- (CenterBins$RtBin - CenterBins$LfBin)/2 +
            CenterBins$LfBin

      Outbin <- x
      for(i in seq_along(x)){
            Outbin[i] <- CenterBins$RtBin[which.min(abs(CenterBins$Mid - x[i]))][1]
      }
      return(Outbin)
}
