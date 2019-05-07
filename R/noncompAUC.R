#' Calculate the AUC using the trapezoidal rule
#'
#' Given a data.frame of concentration-time data, \code{noncompAUC} calculates
#' the area-under-the-concentration-time curve for the times included in the
#' input data.frame. This function does \emph{not} do any extrapolation to
#' infinity.
#'
#' @param DF Input data.frame with concentration-time data.
#' @param concentration The name of the column containing drug concentrations (character).
#' @param time The name of the column containing time data (character).
#' @export
#'

noncompAUC <- function(DF, concentration = "Concentration",
                       time = "Time") {
      # DF = a data.frame including a column Time and a column of the
      # concentrations of the analyte of interest. To analyze a data.frame
      # MyData with columns "Time.min" and
      # "Norbup.Conc", use the syntax:
      #     noncompAUC(MyData,"Norbup.Conc", "Time.min")

      library(tidyverse)

      names(DF)[names(DF) == concentration] <- "CONC"
      names(DF)[names(DF) == time] <- "TIME"

      DFmean <- DF %>% select(TIME, CONC) %>%
            filter(complete.cases(CONC)) %>%
            group_by(TIME) %>%
            summarize(CONC = mean(CONC)) %>%
            arrange(TIME)

      AUC <- sum(0.5*((DFmean$TIME[2:length(DFmean$TIME)] -
                             DFmean$TIME[1:(length(DFmean$TIME)-1)]) *
                            (DFmean$CONC[2:length(DFmean$CONC)] +
                                   DFmean$CONC[1:(length(DFmean$CONC)-1)])))

      return(AUC)
}
