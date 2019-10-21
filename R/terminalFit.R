#' Calculate the elimination rate of the terminal portion of a
#' concentration-time curve
#'
#' \code{terminalFit} fits concentration-time data to an exponential equation of
#' the form f(t) = A*exp(-k * t) where A is Cmax, k is the terminal elimination
#' rate constant, and t is time.
#'
#' @param DF The data.frame with the concentration-time data
#' @param startValues list of starting values for A and k, NA if the start
#'   values should be determined automatically
#' @param concentration A character string of the column name in DF that
#'   contains concentration data
#' @param time A character string of the column name in DF that contains time
#'   data
#' @param tmax The putative tmax, which is used for estimating the terminal
#'   elimination rate. Time points \emph{before} tmax will be omitted from the
#'   fit.
#' @param modelType The mathematical model to use for fitting the data, either
#'   "monoexponential" or "biexponential".
#' @param returnDataUsed Should the data used be returned? I wrote this script
#'   initially for bootstrapping, where it can be useful to see what data were
#'   used as input. For that reason, I'm including the option of returning the
#'   data that were used.
#' @param weights Any weighting to be used for the nonlinear regression
#'
#' @export

terminalFit <- function(DF, startValues = NA,
                        concentration = "Concentration",
                        time = "Time",
                        tmax = NA, modelType = "monoexponential",
                        returnDataUsed = FALSE,
                        weights = NULL){

      if(modelType %in% c("monoexponential", "biexponential") == FALSE){
            return("Acceptable model types are 'monoexponential' or 'biexponential'.")
      }

      DFinit <- DF

      names(DF)[names(DF) == concentration] <- "CONC"
      names(DF)[names(DF) == time] <- "TIME"

      DF <- DF[complete.cases(DF$CONC) &
                     complete.cases(DF$TIME), ]
      DF <- dplyr::arrange(DF, TIME)

      if(is.na(tmax)){
            tmax <- DF$TIME[which.max(DF$CONC)]
      }

      DF <- DF[DF$TIME >= tmax, ]

      # Accounting for the offset in time since tmax is often not at t0.
      DF$Time.offset <- DF$TIME - tmax

      if(nrow(DF) < 2 | length(unique(DF$TIME)) < 2){
            if(returnDataUsed){
                  Result <- list(DataUsed = DFinit,
                                 Estimates = "Insufficient data to create model")
                  return(Result)
            } else {
                  return("Insufficient data to create model")
            }
      }

      if(is.na(startValues[[1]])){
            # Determining good starting values for the fit if the user
            # didn't already supply them
            startValues.A <- max(DF$CONC)
            tfirstlast <- DF[c(1, nrow(DF)), ]
            startValues.k <- -1*((log(tfirstlast$CONC[2]) - log(tfirstlast$CONC[1]))/
                                 (tfirstlast$Time.offset[2] -
                                        tfirstlast$Time.offset[1]))
            if(startValues.k == Inf | is.nan(startValues.k)){
                  startValues.k <- 0.01
            }

            if(modelType == "monoexponential"){
                  startValues <- list(A = startValues.A,
                                      k = startValues.k)
            } else {
                  startValues <- list(A = startValues.A,
                                      alpha = startValues.k,
                                      B = startValues.A/2,
                                      beta = 0.1) # Just guessing that this value might work
            }
      }

      Fit <- NULL
      if(length(unique(DF$TIME)) > 2){

            if(modelType == "monoexponential"){

                  Fit <- tryCatch(nls(CONC ~ A*exp(-k * Time.offset),
                                      data = DF,
                                      start = startValues,
                                      weights = weights),
                                  error = function(x) return("Cannot fit to model"))

            } else {

                  Fit <- tryCatch(nls(CONC ~ A*exp(-alpha * Time.offset) +
                                            B*exp(-beta * Time.offset),
                                      data = DF,
                                      start = startValues,
                                      weights = weights),
                                  error = function(x) return("Cannot fit to model"))

            }

                  if(!is.null(Fit)){
                        if(class(Fit) == "character"){
                              Result <- list(
                                    DataUsed = DFinit,
                                    Estimates = Fit)
                        } else {

                              Result <- list(
                                    DataUsed = DFinit,
                                    Estimates = as.data.frame(summary(Fit)[["coefficients"]]))
                        }
                  } else {
                        DataUsed <- DF
                        Estimates <- data.frame(Estimate = NA,
                                                SE = NA,
                                                tval = NA,
                                                pval = NA)
                        names(Estimates) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

                  Result <- list(DataUsed = DataUsed, Estimates = Estimates)
            }

      } else {
            if(length(unique(DF$TIME)) == 2){
                  Result <- list(
                        DataUsed = DFinit,
                        Estimates = data.frame(Estimate = c(A = startValues$A,
                                                            k = startValues$k))
                  )
                  Fit <- "fitted"
            }
      }

      if(is.null(Fit)){
            Estimates <- data.frame(Estimate = NA,
                                    SE = NA,
                                    tval = NA,
                                    pval = NA)
            names(Estimates) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

            Result <- list(
                  DataUsed = DFinit,
                  Estimates = Estimates)
      }

      if(returnDataUsed == FALSE){
            Result <- Result[["Estimates"]]
      }

      return(Result)

}
