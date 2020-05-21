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
#' @param backExtrap Back extrapolate the curve to 0? TRUE or FALSE. This is
#'   useful when the dose was administered IV and you know you're missing a
#'   significant portion of the curve from t0 to the first sampling time.
#' @param backExtrap_coefs If you've already performed a nonlinear regression to
#'   a monoexponential, biexponential, or triexponential, \code{noncompAUC} will
#'   use those for back extrapolation to t0. Provide a named list of:
#'   \describe{\item{\code{coefs}}{the coefficients from a \code{nls}
#'   regression. Format must be the standard output from
#'   \code{summary(nls(...))[["coefficients"]]}. It doesn't matter what you name
#'   the coefficients, but this function assumes that they will be listed with
#'   the y-intercept for the first term in the first row, the rate constant for
#'   the first term in the next row, the y intercept for the second term in the
#'   next row, the rate constant for the second term in the next row after that,
#'   etc.} \item{\code{tmax}}{The time to start fitting the elimination curve.
#'   This will be used to determine how far back in time t0 is.} }
#' @param backExtrap_model If \code{backExtrap} is TRUE and you haven't supplied
#'   your own fitted coefficients, choose whether to fit a
#'   \code{monoexponential} or \code{biexponential} decay equation. Inputting
#'   anything other than "monoexponential" or "biexponential" will result in the
#'   model being a biexponential.
#' @param backExtrap_times If \code{backExtrap} is TRUE, specify the time range
#'   to use for fitting the exponential decay as \code{c(minTime, maxTime)}.
#' @details \strong{Warning:} Because I'm not yet proficient at nonstandard
#'   evaluation, you must enter the names of the columns containing
#'   concentration and time data as character strings, and there must not be any
#'   other columns named "CONC" or "TIME" or this will not work properly.
#'
#'   \strong{Note:} If there are two consecutive time points with the same
#'   measured concentration, that results in an undefined value for the log
#'   trapezoidal rule. To deal with this, anytime the user has opted for the
#'   linear up/log down trapezoidal rule but there are also consecutive time
#'   points with the same concentration, those individual trapezoids will be
#'   calculated linearly rather than using a log function and all AUCs will be
#'   added together at the end.
#'
#' @return Returns a number
#' @examples
#'
#' data(ConcTime)
#' IV1 <- ConcTime %>% dplyr::filter(SubjectID == 101 & Drug == "A" &
#'                                  DoseRoute == "IV")
#' noncompAUC(IV1, time = "TimeHr")
#' noncompAUC(IV1, time = "TimeHr", type = "LULD")
#' noncompAUC(IV1, time = "TimeHr", type = "linear")
#' noncompAUC(IV1, time = "TimeHr", backExtrap = TRUE,
#'            backExtrap_model = "monoexponential",
#'            backExtrap_times = c(0.5, 12))
#'
#' # For supplying your own coefficients to back extrapolate with:
#' tmax <- 0.5
#' # The elimination phase begins at t = 0.5 hrs here, so don't start fitting at
#' # t0; you'll get bad estimates.
#'
#' Fit <- nls(Concentration ~ A * exp(-k * (TimeHr - tmax)),
#'           data = IV1 %>% filter(TimeHr >= tmax),
#'           start = list(A = max(IV1$Concentration), k = 0.01))
#' MyCoefs <- summary(Fit)[["coefficients"]]
#' noncompAUC(IV1, time = "TimeHr", backExtrap = TRUE,
#'            backExtrap_coefs = list(coefs = MyCoefs,
#'                                  tmax = tmax))
#'
#' @export
#'

noncompAUC <- function(DF, concentration = "Concentration",
                       time = "Time", type = "LULD",
                       backExtrap = FALSE,
                       backExtrap_coefs = NULL,
                       backExtrap_model = "monoexponential",
                       backExtrap_times = c(NA, NA)) {

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

      if(backExtrap){

            if(is.null(backExtrap_coefs)){
                  StartTime <-
                        ifelse(is.na(backExtrap_times[1]),
                               min(DFmean$TIME, na.rm = TRUE),
                               DFmean %>% filter(TIME >= backExtrap_times[1]) %>%
                                     pull(TIME) %>% min)
                  EndTime <-
                        ifelse(is.na(backExtrap_times[2]),
                               max(DFmean$TIME, na.rm = TRUE),
                               DFmean %>% filter(TIME <= backExtrap_times[2]) %>%
                                     pull(TIME) %>% max)

                  Tmax <- ifelse(is.na(backExtrap_times[1]),
                                 NA, StartTime)

                  Fit <- terminalFit(DFmean %>% filter(TIME >= StartTime &
                                                             TIME <= EndTime),
                                     concentration = "CONC",
                                     time = "TIME",
                                     tmax = Tmax,
                                     modelType = backExtrap_model)

                  if(Fit[[1]][1] == "Cannot fit to model"){
                        stop(paste("Attempts to fit a",
                                   backExtrap_model,
                                   "equation to the data failed to converge. Try a different model."))
                  }

                  if(backExtrap_model == "monoexponential"){
                        C0 <- Fit["A", "Estimate"] * exp(-Fit["k", "Estimate"] * -Tmax)
                  } else {
                        C0 <- Fit["A", "Estimate"] * exp(-Fit["alpha", "Estimate"] * -Tmax) +
                              Fit["B", "Estimate"] * exp(-Fit["beta", "Estimate"] * -Tmax)
                  }
            } else {

                  MyCoefs <- as.data.frame(backExtrap_coefs[["coefs"]])
                  tmax <- backExtrap_coefs[["tmax"]]

                  if(nrow(MyCoefs) == 2){
                        C0 <- MyCoefs$Estimate[1] *
                              exp(-MyCoefs$Estimate[2] * -tmax)
                  }

                  if(nrow(MyCoefs) == 4){
                        C0 <- MyCoefs$Estimate[1] *
                              exp(-MyCoefs$Estimate[2] * -tmax) +
                              MyCoefs$Estimate[3] *
                              exp(-MyCoefs$Estimate[4] * -tmax)
                  }

                  if(nrow(MyCoefs) == 6){
                        C0 <- MyCoefs$Estimate[1] *
                              exp(-MyCoefs$Estimate[2] * -tmax) +
                              MyCoefs$Estimate[3] *
                              exp(-MyCoefs$Estimate[4] * -tmax) +
                              MyCoefs$Estimate[5] *
                              exp(-MyCoefs$Estimate[6] * -tmax)
                  }

            }

            DFmean$CONC[DFmean$TIME == 0] <- C0
      }


      # function for linear trapezoidal rule
      lintrap <- function(DFmean){
            sum(0.5*((DFmean$TIME[2:length(DFmean$TIME)] -
                            DFmean$TIME[1:(length(DFmean$TIME)-1)]) *
                           (DFmean$CONC[2:length(DFmean$CONC)] +
                                  DFmean$CONC[1:(length(DFmean$CONC)-1)])))
      }

      if(type == "linear"){
            AUC <- lintrap(DFmean)

      } else {
            TMAX <- DFmean$TIME[which.max(DFmean$CONC)]

            DFup <- DFmean %>% dplyr::filter(TIME <= TMAX)
            DFdown <- DFmean %>% dplyr::filter(TIME >= TMAX)

            AUCup <- lintrap(DFup)

            # function for log trapezoidal rule
            logtrap <- function(DFdown){
                  sum(# C1 - C2
                        ((DFdown$CONC[1:(length(DFdown$CONC)-1)] -
                                DFdown$CONC[2:length(DFdown$CONC)]) /
                               # ln(C1) - ln(C2)
                               (log(DFdown$CONC[1:(length(DFdown$CONC)-1)]) -
                                      log(DFdown$CONC[2:length(DFdown$CONC)])) ) *
                              # t2 - t1
                              (DFdown$TIME[2:length(DFdown$TIME)] -
                                     DFdown$TIME[1:(length(DFdown$TIME)-1)]) )
            }

            # If any values for concentration are the same for two time points,
            # which WILL happen randomly sometimes due to inherent limitations
            # in measurements, use the linear trapezoidal rule to add that
            # trapezoid to the total AUC. To do that, I'll need to break those
            # up into multiple DFs.
            if(any(DFdown$CONC[1:(length(DFdown$CONC)-1)] ==
                   DFdown$CONC[2:length(DFdown$CONC)])){

                  # Noting which are problematic.
                  ProbPoints <- which(DFdown$CONC[1:(length(DFdown$CONC)-1)] ==
                                            DFdown$CONC[2:length(DFdown$CONC)])
                  AUCsToAdd <- c()
                  RowsToUse <- sort(unique(c(1, ProbPoints, ProbPoints + 1,
                                             nrow(DFdown))))
                  for(k in 1:(length(RowsToUse) - 1)){

                        if(RowsToUse[k] %in% ProbPoints){
                              tempDF <- DFdown[RowsToUse[k]:(RowsToUse[k] + 1), ]
                              AUCsToAdd[k] <- lintrap(tempDF)
                        } else {
                              tempDF <- DFdown[RowsToUse[k]:RowsToUse[k + 1], ]
                              AUCsToAdd[k] <- logtrap(tempDF)
                        }
                        rm(tempDF)
                  }

                  AUCdown <- sum(AUCsToAdd)

            } else {
                  AUCdown <- logtrap(DFdown)
            }

            # Adding up and down portions of the curve

            # If AUCup is NA, e.g., when it's an IV bolus so there's no
            # point where the concentration is increasing, then AUCup will
            # be NA. Need to account for that with na.rm = T.
            AUC <- sum(AUCup, AUCdown, na.rm = TRUE)
      }

      return(AUC)

}



      # to do:
      # Make a forward extrapolation option.

