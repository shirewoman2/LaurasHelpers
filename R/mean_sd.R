#' Calculate mean +/- sd or other parameters with appropriate sig figs
#'
#' \code{mean_sd} takes as input either a) a numeric vector or b) a number for
#' the mean and a second number for the sd, and it outputs the mean plus or
#' minus the standard deviation of that vector with an appropriate number of sig
#' figs. If a vector was supplied, optionally also obtain the median, range, CV,
#' and/or 95% confidence interval. Output is \emph{always} of class character.
#'
#' @param x A vector of numbers \emph{or} a single mean
#' @param stdev.x The sd when you want to provide it (leave as NULL if providing
#'   a vector of numbers)
#' @param calcRange Should the range be reported? (logical) (TRUE or FALSE)
#' @param numDigRange Number of digits to report for the range
#' @param calcCV Should the coefficient of variation be reported? (logical)
#' @param calcMedian Should the median be reported? (logical)
#' @param calc95CI Should the 95\% confidence interval be reported? (logical) If
#'   calc95CI is set to TRUE, it will return the 95\% confidence interval as
#'   calculated as \emph{{\out{mean(x) +/- t<sub>n-1, 1-alpha/2</sub> *
#'   sd(x)/sqrt(n)}}}
#' @param reportn Should the number of observations be reported? (logical)
#' @param ndig Optionally set the number of sig figs to use if you don't want
#'   that calculated automatically
#' @param na.rm Should NA values be removed when calculating the mean, standard
#'   deviation, etc.? (logical) They'll still be included in the count of
#'   observations if \code{reportn} is TRUE.
#' @details Example of ultimate output with all possible options set to TRUE:
#'
#'   5.1 (5) +/- 0.2 (4.7 to 5.3, 3.7\%, 95\%CI: 4.6 to 5.2, n = 18)
#'
#'   mean (median) +/- sd (range.min to range.max, CV\%, 95\%CI: lower to upper,
#'   n = n).
#'
#'   A few important notes: \itemize{\item{I have not set this up to deal with
#'   mean values in scientific notation.} \item{If \code{sd(x) == 0}, output will
#'   be unique(x).}\item{If you want to supply the mean and standard deviation
#'   and have this function return that with appropriate sig figs, you
#'   \emph{cannot} supply a vector of values for x and stdev.x. That does not
#'   work. If x is a vector of length > 1, the function will calculate mean,
#'   standard deviation, etc. rather than taking those values from a second
#'   vector. It will result in output, just not the \emph{correct} output.}}
#'
#' @return Returns a character string
#'
#' @examples
#' mean_sd(rnorm(10, 5, 1))
#' mean_sd(rnorm(10, 5, 1), calcRange = TRUE)
#' mean_sd(rnorm(10, 5, 1), calcCV = TRUE)
#' mean_sd(rnorm(10, 5, 1), calcMedian = TRUE)
#' mean_sd(rnorm(10, 5, 1), calc95CI = TRUE)
#' mean_sd(rnorm(10, 5, 1), reportn = TRUE)
#' mean_sd(rnorm(10, 5, 1), calcRange = TRUE, calcCV = TRUE, reportn = TRUE)
#'
#' @export

mean_sd <- function(x, stdev.x = NULL,
                    calcRange = FALSE,
                    numDigRange = NA,
                    calcCV = FALSE,
                    calcMedian = FALSE,
                    calc95CI = FALSE,
                    reportn = FALSE,
                    ndig = NA,
                    na.rm = TRUE){

      if(all(is.na(x))){
            if(reportn == TRUE){
                  return(paste0("NA (n = ", length(x), ")"))
            } else {
                  return(as.character(NA))
            }
      }

      if(length(x) == 0){
            return(as.character(NA))
      }

      if(length(x) == 1 & is.null(stdev.x)){
            if(is.na(ndig)){
                  ndig <- 3
            }

            if(reportn == TRUE){
                  return(paste(signif(x, ndig),
                               "(n = 1)"))
            } else {
                  return(as.character(signif(x, ndig)))
            }
      }

      # If there are any infinite values, return Inf, which is what the mean of
      # a vector contain ing Inf would be, and issue warning message. Not truly
      # meaningful to calculate sd, 95% CI, etc., when x contains infinity, and
      # this function really just isn't designed for this purpose.
      if(any(is.infinite(x))){
            message("The vector supplied contains infinite values. This function was not designed to accommodate this scenario, so 'Inf' is all that will be returned.")
            return(Inf)
      }


      if(sd(x, na.rm = T) == 0 & is.null(stdev.x) |
         is.na(sd(x, na.rm = T)) & is.null(stdev.x)){
            if(is.na(ndig)){
                  ndig <- 3
            }

            if(reportn == TRUE){
                  return(paste0(signif(sort(unique(x)), ndig),
                                " (n =", length(x), ")"))
            } else {
                  return(as.character(signif(sort(unique(x)), ndig)))
            }
      }

      if(na.rm == FALSE & (all(complete.cases(x)) == FALSE |
                           all(is.finite(x)) == FALSE)){
            return(as.character(NA))
      }

      n <- length(x)

      x <- x[complete.cases(x) & is.finite(x)]

      if(length(x) == 0){
            return(as.character(NA))
      }

      if(is.null(stdev.x)){
            mean.x <- mean(x)
            stdev.x <- sd(x)
      } else {
            if(is.na(stdev.x)){
                  return(as.character(NA))
            } else {
                  mean.x <- x
            }
      }

      # Check for whether mean.x is negative b/c this doesn't
      # work for negative numbers.
      IsNeg <- mean.x < 0

      if(IsNeg){
            mean.x <- mean.x * -1
      }

      Digits <- data.frame(
            MinValue = 10^seq(-7, 7, 1),
            MaxValue = 10^seq(-6, 8, 1),
            Place = log10(10^seq(-6, 8, 1)))

      # How many places should we go back to find the decimal
      PlacesSD <- Digits$Place[which(signif(stdev.x, 1) >= Digits$MinValue &
                                           signif(stdev.x, 1) < Digits$MaxValue)]
      if(signif(stdev.x, 1) == 0){
            if(stringr::str_detect(as.character(mean.x), "\\.")){
                  PlacesSD <- 0
            } else {
                  PlacesSD <- 2
            }
      }

      if(is.na(ndig)){

            if(PlacesSD < 1){ # sd value is less than 1, i.e., has a decimal point in it

                  # Find the position of the 1st non-zero character after the decimal
                  DecLocSD <- stringr::str_locate(as.character(format(signif(stdev.x, 1),
                                                                      scientific = FALSE)),
                                                  "\\.(0){0,20}[1-9]")[2]
                  if(is.na(DecLocSD)){ # This will happen if sd(x) == 0 and there are digits after a decimal in mean(x)
                        nsmall <- 3 # Arbitrarily setting this.
                  } else {
                        nsmall <- DecLocSD - 2 # It's "-2" b/c you need one character for the "0" and one for the "."
                  }

                  stdev.x.sig <- prettyNum(signif(stdev.x, 1), big.mark = ",",
                                           scientific = FALSE, nsmall = nsmall)
                  mean.x.sig <- prettyNum(round(mean.x, nsmall),
                                          big.mark = ",",
                                          nsmall = nsmall)

            } else {

                  # Find which place the 1st digit is for the sd
                  # Apend a decimal if there isn't one.
                  if(stringr::str_detect(as.character(stdev.x), "\\.") == FALSE){
                        stdev.x <- paste0(stdev.x, ".0")
                  }
                  if(stringr::str_detect(as.character(mean.x), "\\.") == FALSE){
                        mean.x <- paste0(mean.x, ".0")
                  }

                  DecLocSD <- stringr::str_locate(as.character(stdev.x), "\\.")[2] - 1
                  DecLocMean <- stringr::str_locate(as.character(mean.x), "\\.")[2] - 1
                  SigFig <- DecLocMean - DecLocSD + 1

                  if(SigFig > 0){
                        mean.x.sig <- prettyNum(signif(as.numeric(mean.x),
                                                       SigFig),
                                                big.mark = ",")
                        stdev.x.sig <- prettyNum(signif(as.numeric(stdev.x), 1),
                                                 big.mark = ",")
                  } else {
                        mean.x.sig <- prettyNum(signif(as.numeric(mean.x), 1),
                                                big.mark = ",")
                        stdev.x.sig <- prettyNum(signif(as.numeric(stdev.x), 1),
                                                 big.mark = ",")
                  }
            }

            if(calcRange == TRUE){
                  if(length(x) < 2){
                        xrange <- "cannot calculate range"
                  } else {

                        if(complete.cases(numDigRange)){
                              xrange <- paste(prettyNum(round(min(x), numDigRange),
                                                        big.mark = ",",
                                                        nsmall = numDigRange),
                                              "to",
                                              prettyNum(round(max(x), numDigRange),
                                                        big.mark = ",",
                                                        nsmall = numDigRange))
                        } else {

                              if(PlacesSD < 1){
                                    xrange <- paste(prettyNum(round(min(x),
                                                                    nsmall),
                                                              big.mark = ",",
                                                              nsmall = nsmall),
                                                    "to", prettyNum(round(max(x),
                                                                          nsmall),
                                                                    big.mark = ",",
                                                                    nsmall = nsmall))
                              } else {

                                    xrange <- paste(prettyNum(signif(min(x),
                                                                     SigFig),
                                                              big.mark = ",",
                                                              nsmall = 0),
                                                    "to", prettyNum(signif(max(x),
                                                                           SigFig),
                                                                    big.mark = ",",
                                                                    nsmall = 0))
                              }
                        }
                  }
            }

            if(calc95CI == TRUE){
                  if(length(x) < 2){
                        CIhigh <- "cannot calculate 95% confidence interval"
                  } else {
                        if(PlacesSD < 1){
                              CIlow <- round(as.numeric(mean(x) - qt(0.975, length(x) - 1) *
                                                              sd(x)/sqrt(length(x))),
                                             nsmall)
                              CIhigh <- round(as.numeric(mean(x) + qt(0.975, length(x) - 1) *
                                                               sd(x)/sqrt(length(x))),
                                              nsmall)
                              xCI95 <- paste(prettyNum(CIlow, big.mark = ",",
                                                       nsmall = nsmall),
                                             "to",
                                             prettyNum(CIhigh, big.mark = ",",
                                                       nsmall = nsmall))
                        } else {
                              CIlow <- signif(as.numeric(mean(x) - qt(0.975, length(x) - 1) *
                                                               sd(x)/sqrt(length(x))),
                                              SigFig)
                              CIhigh <- signif(as.numeric(mean(x) + qt(0.975, length(x) - 1) *
                                                                sd(x)/sqrt(length(x))),
                                               SigFig)
                              xCI95 <- paste(prettyNum(CIlow, big.mark = ",",
                                                       nsmall = 0),
                                             "to",
                                             prettyNum(CIhigh, big.mark = ",",
                                                       nsmall = 0))
                        }
                  }
            }

            if(calcMedian == TRUE){
                  if(length(x) < 2){
                        xMed <- "cannot calculate median"
                  } else {
                        if(PlacesSD < 1){
                              xMed <- prettyNum(round(as.numeric(median(x)),
                                                      nsmall),
                                                big.mark = ",", nsmall = nsmall)
                        } else {
                              xMed <- prettyNum(signif(as.numeric(median(x)),
                                                       SigFig),
                                                big.mark = ",", nsmall = 0)
                        }
                  }
            }



      } else {
            mean.x.sig <- formatC(mean.x, big.mark = ",", format = "f", digits = ndig)

            stdev.x.sig <- formatC(stdev.x, big.mark = ",", format = "f", digits = ndig)

            if(calcRange == TRUE){
                  if(length(x) < 2){
                        xrange <- "cannot calculate range"
                  } else {
                        xrange <- paste(prettyNum(round(min(x),
                                                        ndig),
                                                  big.mark = ",",
                                                  nsmall = ndig),
                                        "to", prettyNum(round(max(x),
                                                              ndig),
                                                        big.mark = ",",
                                                        nsmall = ndig))
                  }
            }

            if(calcMedian == TRUE){
                  if(length(x) < 2){
                        xMed <- "cannot calculate median"
                  } else {
                        xMed <- prettyNum(round(as.numeric(median(x)),
                                                ndig),
                                          big.mark = ",", nsmall = ndig)
                  }
            }

            if(calc95CI == TRUE){
                  if(length(x) < 2){
                        CIhigh <- "cannot calculate 95% confidence interval"
                  } else {
                        CIlow <- round(as.numeric(mean(x) - qt(0.975, length(x) - 1) *
                                                        sd(x)/sqrt(length(x))),
                                       ndig)
                        CIhigh <- round(as.numeric(mean(x) + qt(0.975, length(x) - 1) *
                                                         sd(x)/sqrt(length(x))),
                                        ndig)
                        xCI95 <- paste(prettyNum(CIlow, big.mark = ",",
                                                 nsmall = nsmall),
                                       "to",
                                       prettyNum(CIhigh, big.mark = ",",
                                                 nsmall = ndig))
                  }
            }
      }

      if(IsNeg){
            mean.x.sig <- paste0("-", mean.x.sig)
      }

      if(calcCV == TRUE){
            xCV <- paste0(signif(100*as.numeric(stdev.x)/as.numeric(mean.x), 2), "%")
            if(as.numeric(stdev.x) == 0){xCV <- 0}
            if(xCV == "NaN%"){xCV <- "NaN"}
      }

      if(reportn){
            xn <- paste("n =",n)
      }

      # If both 95% CI and range are TRUE, it may be confusing to have two
      # sets of numbers that say "x to y". Adding "95% CI:" if both are TRUE.
      if(calc95CI & calcRange){
            xCI95 <- paste("95% CI:", xCI95)
      }

      # Setting output to include median, range, 95% CI, n, and/or CV as appropriate
      # Including commas as necessary.
      xrange <- ifelse(calcRange, xrange, "")
      xCV <- ifelse(calcCV, xCV, "")
      xCI95 <- ifelse(calc95CI, xCI95, "")
      xn <- ifelse(reportn, xn, "")

      # Median is listed right after the mean, so that value is inside its
      # own set of parentheses.
      xMed <- ifelse(calcMedian, paste0(" (", xMed, ")"), "") # Adding spaces and parentheses

      # All the others are within the last set of parentheses.
      if(any(calcRange, calcCV, calc95CI, reportn)){
            if(calcRange & any(calcCV, calc95CI, reportn)){
                  xrange <- paste0(xrange, ", ")
            }

            if(calcCV & any(calc95CI, reportn)){
                  xCV <- paste0(xCV, ", ")
            }

            if(calc95CI & any(reportn)){
                  xCI95 <- paste0(xCI95, ", ")
            }

            Parenth2 <- paste0(" (", xrange, xCV, xCI95, xn, ")")
      } else {
            Parenth2 <- ""
      }

      output <- paste0(mean.x.sig, xMed,
                       " \u00B1 ", stdev.x.sig, Parenth2)

      return(output)
}

