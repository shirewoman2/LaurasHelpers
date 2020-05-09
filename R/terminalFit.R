#' Calculate the elimination rate of the terminal portion of a
#' concentration-time curve
#'
#' \code{terminalFit} fits an exponential equation to concentration-time data.
#' The equation should be of the form \eqn{f(t) = concentration = A * exp(-kt)}
#' where A is ~Cmax, k is the terminal elimination rate constant, and t is time,
#' and the model can be monoexponential, biexponential, or triexponential decay.
#'
#' @param DF The data.frame with the concentration-time data
#' @param startValues list of starting values for A and k, NA if the start
#'   values should be determined automatically. \itemize{
#'
#'   \item Coefficients for the monoexponential fit that will need values: A, k.
#'
#'   \item Coefficients for the biexponential fit: A, alpha, B, and beta.
#'
#'   \item For the triexponential: A, alpha, B, beta, G, gamma.
#'
#'   \item \emph{An alternative approach:} After having a lot of trouble with
#'   finding decent starting values for triexponential decays using \code{nls}
#'   behind the scenes here, I've now changed this function to optionally use
#'   the function \code{nls2}, which does a more rigorous search for starting
#'   values than \code{nls}. For triexponential fits, if you submit a list of
#'   starting values like usual, this function will use the regular \code{nls}
#'   function. However, to use \code{nls2} instead, set startValues to a two row
#'   data.frame with columns for each coefficient in which the first row is the
#'   minimum possible value to start using and the 2nd row is the maximum value
#'   to start using for that coefficient. \strong{A warning:} Because nls2
#'   searches more possible starting values, it can be appreciably slower.}
#' @param concentration A character string of the column name in DF that
#'   contains concentration data
#' @param time A character string of the column name in DF that contains time
#'   data
#' @param tmax The putative tmax, which is used for estimating the terminal
#'   elimination rate. Time points \emph{before} tmax will be omitted from the
#'   fit.
#' @param modelType The mathematical model to use for fitting the data; options
#'   are "monoexponential", "biexponential", or "triexponential".
#' @param returnDataUsed Should the data used be returned? I wrote this script
#'   initially for bootstrapping, where it can be useful to see what data were
#'   used as input. For that reason, I'm including the option of returning the
#'   data that were used.
#' @param weights Any weighting to be used for the nonlinear regression
#' @param returnRSS TRUE or FALSE for whether to resturn the residual sum of
#'   squares. If set to TRUE, this will be the last column of the output
#'   data.frame where all rows = the residual sum of squares. (I wanted the
#'   output to still be a data.frame, so that's the place I could think of to
#'   put it.)
#' @param useNLS_outnames TRUE or FALSE for whether to use the standard output
#'   coeffecient names that come with the nls or nls2 functions, e.g.,
#'   "Estimate", "Std. Error", "t value", and "Pr(>|t|)". These names are
#'   annoying to work with for output data.frames b/c they don't follow standard
#'   column-naming practices (they contain spaces and symbols). If set to FALSE,
#'   the names of the output coefficient data.frame will be "Estimate", "SE",
#'   "tvalue" and "pvalue".
#'
#' @return Returns a data.frame of the coefficients or a list containing a
#'   data.frame of the input data and a data.frame of the estimated coefficients
#'
#' @examples
#' # Example data to work with:
#' data(ConcTime)
#' Subj101 <- ConcTime %>% filter(SubjectID == 101 & DoseRoute == "IV" &
#'                                      Drug == "A") %>%
#'       select(SubjectID, TimeHr, Concentration)
#'
#' # Automatically select the start values
#' terminalFit(Subj101, concentration = "Concentration", time = "TimeHr",
#'             modelType = "monoexponential")
#'
#' # Set the start values yourself
#' terminalFit(Subj101, concentration = "Concentration", time = "TimeHr",
#'             tmax = 0, startValues = list(A = 30, k = 0.01),
#'             modelType = "monoexponential")
#'
#' # Use the more robuse nls2 function to do the regression by selecting a range of
#' # values to search.
#' terminalFit(Subj101, concentration = "Concentration", time = "TimeHr",
#'             tmax = 0,
#'             startValues = data.frame(A = c(5, 50), k = c(0.0001, 0.05)),
#'             modelType = "monoexponential")
#'
#' # Weight by 1/y
#' terminalFit(Subj101, concentration = "Concentration", time = "TimeHr",
#'             tmax = 0,
#'             weight = 1/Subj101$Concentration,
#'             modelType = "monoexponential")
#'
#' # Get the residual sum of squares
#' terminalFit(Subj101, concentration = "Concentration", time = "TimeHr",
#'             modelType = "monoexponential", returnRSS = TRUE)
#'
#' # Use better names for the columns in the output
#' terminalFit(Subj101, concentration = "Concentration", time = "TimeHr",
#'             modelType = "monoexponential", useNLS_outnames = FALSE)
#'
#'
#' @export

terminalFit <- function(DF, startValues = NA,
                        concentration = "Concentration",
                        time = "Time",
                        tmax = NA, modelType = "monoexponential",
                        returnDataUsed = FALSE,
                        weights = NULL, returnRSS = FALSE,
                        useNLS_outnames = TRUE){

      # Catching inappropriate model input
      if(modelType %in% c("monoexponential", "biexponential",
                          "triexponential") == FALSE) {
            return("Acceptable model types are 'monoexponential', 'biexponential', or 'triexponential'.")
      }

      # Setting up the input data.frame
      DFinit <- DF
      names(DF)[names(DF) == concentration] <- "CONC"
      names(DF)[names(DF) == time] <- "TIME"
      DF <- DF[complete.cases(DF$CONC) & complete.cases(DF$TIME), ]
      DF <- dplyr::arrange(DF, TIME)

      # Setting tmax if it wasn't already.
      if(is.na(tmax)) {
            tmax <- DF$TIME[which.max(DF$CONC)]
      }
      DF <- DF[DF$TIME >= tmax, ]

      # Accounting for the offset in time since tmax is often not at t0.
      DF$Time.offset <- DF$TIME - tmax

      # Catching inappropriate input with < 2 rows
      if(nrow(DF) < 2 | length(unique(DF$TIME)) <= 2) {
            if(returnDataUsed) {
                  Result <- list(DataUsed = DFinit,
                                 Estimates = "Insufficient data to create model")
                  return(Result)
            }
            else {
                  return("Insufficient data to create model")
            }
      }

      # Determining good starting values for the fit if the user didn't
      # already supply them
      if(is.na(startValues[[1]][1])) {
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
            }

            if(modelType == "biexponential"){
                  startValues <- list(A = startValues.A,
                                      alpha = startValues.k,
                                      B = startValues.A/2,
                                      beta = 0.1) # Just guessing that this value might work
            }

            if(modelType == "triexponential"){
                  startValues <- list(A = startValues.A,
                                      alpha = startValues.k,
                                      B = startValues.A/2,
                                      beta = 0.1,
                                      G = startValues.A/3,
                                      gamma = 0.01) # Just guessing that this value might work
            }

      }


      # Fitting
      Fit <- NULL
      if(modelType == "monoexponential") {
            # Determining whether to use nls or nls2 and then fitting
            if(class(startValues) == "list") {
                  Fit <- tryCatch(nls(CONC ~ A * exp(-k * Time.offset),
                                      data = DF, start = startValues, weights = weights),
                                  error = function(x) return("Cannot fit to model"))
            } else {
                  if(nrow(startValues) != 2 |
                     all(c("A", "k") %in%
                         names(startValues)) == FALSE) {
                        stop("If you submit a data.frame with possible starting values for a monoexponential model, there must be two rows for each coefficient -- A and k -- to be fit.")
                  }

                  Fit <- tryCatch(nls2::nls2(CONC ~ A * exp(-k * Time.offset),
                                             data = DF, start = startValues,
                                             weights = weights),
                                  error = function(x) return("Cannot fit to model"))
            }
      }

      if(modelType == "biexponential") {
            # Determining whether to use nls or nls2 and then fitting
            if(class(startValues) == "list") {
                  Fit <- tryCatch(nls(
                        CONC ~ A * exp(-alpha * Time.offset) +
                              B * exp(-beta * Time.offset),
                        data = DF,
                        start = startValues,
                        weights = weights),
                        error = function(x) return("Cannot fit to model"))
            } else {
                  if(nrow(startValues) != 2 |
                     all(c("A", "alpha", "B", "beta") %in%
                         names(startValues)) == FALSE) {
                        stop("If you submit a data.frame with possible starting values for a biexponential model, there must be two rows for each coefficient -- A, alpha, B, and beta -- to be fit.")
                  }

                  Fit <- tryCatch(nls2::nls2(
                        CONC ~ A * exp(-alpha * Time.offset) +
                              B * exp(-beta * Time.offset),
                        data = DF,
                        start = startValues,
                        weights = weights),
                        error = function(x) return("Cannot fit to model"))
            }
      }

      if(modelType == "triexponential") {
            # Determining whether to use nls or nls2
            if(class(startValues) == "list") {
                  Fit <- tryCatch(nls(
                        CONC ~ A * exp(-alpha * Time.offset) +
                              B * exp(-beta * Time.offset) +
                              G * exp(-gamma * Time.offset),
                        data = DF,
                        start = startValues,
                        weights = weights),
                        error = function(x) return("Cannot fit to model"))
            } else {
                  if(nrow(startValues) != 2 |
                     all(c("A", "alpha", "B", "beta", "G", "gamma") %in%
                         names(startValues)) == FALSE) {
                        stop("If you submit a data.frame with possible starting values for a triexponential model, there must be two rows for each coefficient -- A, alpha, B, beta, G, and gamma -- to be fit.")
                  }

                  Fit <- tryCatch(nls2::nls2(
                        CONC ~ A * exp(-alpha * Time.offset) +
                              B * exp(-beta * Time.offset) +
                              G * exp(-gamma * Time.offset),
                        data = DF,
                        start = startValues,
                        weights = weights),
                        error = function(x) return("Cannot fit to model"))
            }
      }

      # Checking whether the fit worked at all
      if(!is.null(Fit)) {
            # If it did work, setting the output appropriately for whether the
            # fit converged.
            if(class(Fit) == "character") {
                  Result <- list(DataUsed = DFinit,
                                 Estimates = Fit)
            } else {
                  Result <- list(DataUsed = DFinit,
                                 Estimates = as.data.frame(
                                       summary(Fit)[["coefficients"]]))
            }
      } else {
            # If it did not work at all, returning a data.frame of NA values.
            DataUsed <- DF
            Estimates <- data.frame(Estimate = NA, SE = NA, tvalue = NA,
                                    pvalue = NA)
            names(Estimates) <- c("Estimate", "Std. Error", "t value",
                                  "Pr(>|t|)")
            Result <- list(DataUsed = DataUsed,
                           Estimates = Estimates)
      }

      # Here's what to do if the fit didn't work at all. I can't remember what
      # caused this to happen, but I know I needed to catch this.
      if(is.null(Fit)) {
            Estimates <- data.frame(Estimate = NA, SE = NA, tvalue = NA,
                                    pvalue = NA)
            names(Estimates) <- c("Estimate", "Std. Error", "t value",
                                  "Pr(>|t|)")
            Result <- list(DataUsed = DFinit,
                           Estimates = Estimates)
      }

      # If the user wants to use better names for the output data.frame, setting
      # those here.
      if(useNLS_outnames == FALSE & class(Fit) == "nls"){
            names(Result[["Estimates"]]) <- c("Estimate", "SE", "tvalue",
                                              "pvalue")
      }

      # If the user wanted to have an estimate of the residual sum of squares,
      # adding that to the output data.frame.
      if(returnRSS & !is.null(Fit) & class(Fit) == "nls") {
            Result[["Estimates"]]$RSS <- as.numeric(Fit$m$deviance())
      }

      # If the only thing the user wants is the estimates and not the input
      # data, make "Result" only include the estimates.
      if(returnDataUsed == FALSE) {
            Result <- Result[["Estimates"]]
      }

      return(Result)

}
