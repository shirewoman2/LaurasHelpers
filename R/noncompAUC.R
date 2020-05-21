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
#'   to use for fitting the exponential decay as \code{c(minTime,
#'   maxTime)}.
#' @details \strong{Warning:} Because I'm not yet proficient at nonstandard
#'   evaluation, you must enter the names of the columns containing
#'   concentration and time data as character strings, and there must not be any
#'   other columns named "CONC" or "TIME" or this will not work properly.
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
#'           backExtrap_model = "monoexponential",
#'           backExtrap_times = c(0.5, 12))
#'
#' # For supplying your own coefficients to back extrapolate with:
#' tmax <- 0.5
#' # The elimination phase begins at t = 0.5 hrs here, so don't start fitting at
#' # t0; you'll get bad estimates.
#'
#' Fit <- nls(Concentration ~ A * exp(-k * (TimeHr - tmax)),
#'           data = IV1 %>% filter(TimeHr >= tmax),
#'           start = list(A = max(IV1$Concentration), k = 0.01))
#' MyCoeffs <- summary(Fit)[["coefficients"]]
#' noncompAUC(IV1, time = "TimeHr", backExtrap = TRUE,
#'           backExtrap_coefs = list(coeffs = MyCoeffs,
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

                  MyCoeffs <- as.data.frame(backExtrap_coefs[["coeffs"]])

                  if(nrow(MyCoeffs) == 2){
                        C0 <- MyCoeffs$Estimate[1] *
                              exp(-MyCoeffs$Estimate[2] * -tmax)
                  }

                  if(nrow(MyCoeffs) == 4){
                        C0 <- MyCoeffs$Estimate[1] *
                              exp(-MyCoeffs$Estimate[2] * -tmax) +
                              MyCoeffs$Estimate[3] *
                              exp(-MyCoeffs$Estimate[4] * -tmax)
                  }

                  if(nrow(MyCoeffs) == 6){
                        C0 <- MyCoeffs$Estimate[1] *
                              exp(-MyCoeffs$Estimate[2] * -tmax) +
                              MyCoeffs$Estimate[3] *
                              exp(-MyCoeffs$Estimate[4] * -tmax) +
                              MyCoeffs$Estimate[5] *
                              exp(-MyCoeffs$Estimate[6] * -tmax)
                  }

            }

            DFmean$CONC[DFmean$TIME == 0] <- C0
      }
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



# to do:
# Make a forward extrapolation option.

