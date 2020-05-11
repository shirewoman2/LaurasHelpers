#' Calculate the AUC using the trapezoidal rule
#'
#' Given a data.frame of concentration-time data, \code{noncompAUC} calculates
#' the area-under-the-concentration-time curve for the times included in the
#' input data.frame using the linear trapezoidal rule. This function does
#' \emph{not} do any extrapolation to infinity.
#'
#' @param DF Input data.frame with concentration-time data.
#' @param concentration The name of the column containing drug concentrations
#'   (character).
#' @param time The name of the column containing time data (character).
#' @param type The type of trapezoidal rule to use. Options are "LULD" (default)
#'   for "linear up, log down" or "linear".
#' @details \strong{Warning:} Because I'm not yet proficient at nonstandard evaluation, you must
#'   enter the names of the columns containing concentration and time data as
#'   character strings, and there must not be any other columns named "CONC" or
#'   "TIME" or this will not work properly.
#' @return Returns a number
#' @examples
#'
#' data(ConcTime)
#' noncompAUC(ConcTime, time = "Time_hr")
#' noncompAUC(ConcTime, time = "Time_hr", type = "LULD")
#' noncompAUC(ConcTime, time = "Time_hr", type = "linear")

#'
#' @export
#'

noncompAUC <- function(DF, concentration = "Concentration",
                       time = "Time", type = "LULD") {

      if(type %in% c("linear", "LULD") == FALSE){
            stop("The only options for type of AUC calculation are 'LULD' for 'linear up, log down' or 'linear'.")
      }

      names(DF)[names(DF) == concentration] <- "CONC"
      names(DF)[names(DF) == time] <- "TIME"

      DFmean <- DF %>% dplyr::select(TIME, CONC) %>%
            dplyr::filter(complete.cases(CONC)) %>%
            dplyr::group_by(TIME) %>%
            dplyr::summarize(CONC = mean(CONC)) %>%
            dplyr::arrange(TIME)

      if(type == "linear"){
            AUC <- sum(0.5*((DFmean$TIME[2:length(DFmean$TIME)] -
                                   DFmean$TIME[1:(length(DFmean$TIME)-1)]) *
                                  (DFmean$CONC[2:length(DFmean$CONC)] +
                                         DFmean$CONC[1:(length(DFmean$CONC)-1)])))

      } else {
            tmax <- DFmean$TIME[which.max(DFmean$CONC)]

            DFup <- DFmean %>% dplyr::filter(TIME <= tmax)
            DFdown <- DFmean %>% dplyr::filter(TIME >= tmax)

            AUCup <- sum(0.5*((DFup$TIME[2:length(DFup$TIME)] -
                                     DFup$TIME[1:(length(DFup$TIME)-1)]) *
                                    (DFup$CONC[2:length(DFup$CONC)] +
                                           DFup$CONC[1:(length(DFup$CONC)-1)])))
            AUCdown <- sum(
                  # C1 - C2
                  ((DFdown$CONC[1:(length(DFdown$CONC)-1)] -
                          DFdown$CONC[2:length(DFdown$CONC)]) /
                         # ln(C1) - ln(C2)
                         (log(DFdown$CONC[1:(length(DFdown$CONC)-1)]) -
                                log(DFdown$CONC[2:length(DFdown$CONC)])) ) *
                        # t2 - t1
                        (DFdown$TIME[2:length(DFdown$TIME)] -
                               DFdown$TIME[1:(length(DFdown$TIME)-1)])
            )

            # If AUCup is NA, e.g., when it's an IV bolus so there's no point
            # where the concentration is increasing, then AUCup will be NA. Need
            # to account for that with na.rm = T.
            AUC <- sum(AUCup, AUCdown, na.rm = TRUE)

      }

      return(AUC)
}


