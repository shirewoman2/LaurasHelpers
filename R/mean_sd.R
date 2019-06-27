#' Calculate mean +/- sd or other parameters with appropriate sig figs
#'
#' \code{mean_sd} takes as input a numeric vector \emph{or} the mean and sd values and
#' outputs the mean plus or minus the standard deviation of that vector with an
#' appropriate number of sig figs. Output is ALWAYS of class character.
#'
#' Example of ultimate output with all possible options set to TRUE:
#'
#' 5.1 (5) +/- 0.2 (4.7 to 5.3, 3.7\%, 95\%CI: 4.6 to 5.2, n = 18)
#'
#' mean (median) +/- sd (range.min to range.max, CV\%, 95\%CI: lower to upper, n
#' = n).
#'
#' NB: I have not set this up to deal with mean values in scientific notation.
#'
#' NB: If sd(x) == 0, output will be unique(x).
#'
#' @param x A vector of numbers OR a single mean
#' @param stdev.x The sd of the mean provided (leave as NULL if providing a
#'   vector of numbers)
#' @param calcRange Should the range be reported? (logical) (TRUE or FALSE)
#' @param numDigRange Number of digits to report for the range
#' @param calcCV Should the coefficient of variation be reported? (logical)
#' @param calcMedian Should the median be reported? (logical)
#' @param calc95CI Should the 95\% confidence interval be reported? (logical) If
#'   calc95CI is set to TRUE, it will return the 95\% confidence interval as
#'   calculated as mean(x) +/- t~n-1, 1-alpha/2~ * sd(x)/sqrt(n)
#' @param calcQuantiles Should the quantiles be reported? (logical)
#' @param reportn Should the number of observations be reported? (logical)
#' @param ndig Optionally set the number of sig figs to use if you want to set
#'   to a specific value
#' @param na.rm Should NA values be removed? (logical)
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
                    calcRange = FALSE, numDigRange = NA,
                    calcCV = FALSE,
                    calcMedian = FALSE,
                    calc95CI = FALSE,
                    calcQuantiles = FALSE,
                    reportn = FALSE,
                    ndig = NA, na.rm = TRUE){
      require(stringr)

      if(all(is.na(x))){
            return(as.character(NA))
      }

      if(length(x) == 0){
            return(as.character(NA))
      }

      if(length(x) == 1 & is.null(stdev.x)){
            return(as.character(x))
      }

      if(sd(x, na.rm = T) == 0 & is.null(stdev.x) |
         is.na(sd(x, na.rm = T))){
            return(as.character(unique(x)))
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
            MinValue = 10^seq(-5, 5, 1),
            MaxValue = 10^seq(-4, 6, 1),
            Place = log10(10^seq(-4, 6, 1)))

      # How many places should we go back to find the decimal
      PlacesSD <- Digits$Place[which(signif(stdev.x, 1) >= Digits$MinValue &
                                           signif(stdev.x, 1) < Digits$MaxValue)]
      if(signif(stdev.x, 1) == 0){
            if(str_detect(as.character(mean.x), "\\.")){
                  PlacesSD <- 0
            } else {
                  PlacesSD <- 2
            }
      }

      if(is.na(ndig)){

            if(PlacesSD < 1){ # sd value is less than 1, i.e., has a decimal point in it

                  # Find the position of the 1st non-zero character after the decimal
                  DecLocSD <- str_locate(as.character(format(signif(stdev.x, 1),
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
                  if(str_detect(as.character(stdev.x), "\\.") == FALSE){
                        stdev.x <- paste0(stdev.x, ".0")
                  }
                  if(str_detect(as.character(mean.x), "\\.") == FALSE){
                        mean.x <- paste0(mean.x, ".0")
                  }

                  DecLocSD <- str_locate(as.character(stdev.x), "\\.")[2] - 1
                  DecLocMean <- str_locate(as.character(mean.x), "\\.")[2] - 1
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
                        CI95 <- "cannot calculate 95% confidence interval"
                  } else {
                        if(PlacesSD < 1){
                              CI5 <- round(as.numeric(mean(x) - qt(0.975, length(x) - 1) * sd(x)/sqrt(length(x))),
                                           nsmall)
                              CI95 <- round(as.numeric(mean(x) + qt(0.975, length(x) - 1) * sd(x)/sqrt(length(x))),
                                            nsmall)
                              xCI95 <- paste(prettyNum(CI5, big.mark = ",",
                                                       nsmall = nsmall),
                                             "to",
                                             prettyNum(CI95, big.mark = ",",
                                                       nsmall = nsmall))
                        } else {
                              CI5 <- signif(as.numeric(mean(x) - qt(0.975, length(x) - 1) * sd(x)/sqrt(length(x))),
                                            SigFig)
                              CI95 <- signif(as.numeric(mean(x) + qt(0.975, length(x) - 1) * sd(x)/sqrt(length(x))),
                                             SigFig)
                              xCI95 <- paste(prettyNum(CI5, big.mark = ",",
                                                       nsmall = 0),
                                             "to",
                                             prettyNum(CI95, big.mark = ",",
                                                       nsmall = 0))
                        }
                  }
            }

            if(calcQuantiles == TRUE){
                  if(length(x) < 2){
                        Q95 <- "cannot calculate 95% confidence interval"
                  } else {
                        if(PlacesSD < 1){
                              Q5 <- round(as.numeric(quantile(x, c(0.025))),
                                           nsmall)
                              Q95 <- round(as.numeric(quantile(x, c(0.975))),
                                            nsmall)
                              xQ95 <- paste(prettyNum(Q5, big.mark = ",",
                                                       nsmall = nsmall),
                                             "to",
                                             prettyNum(Q95, big.mark = ",",
                                                       nsmall = nsmall))
                        } else {
                              Q5 <- signif(as.numeric(quantile(x, c(0.025))),
                                            SigFig)
                              Q95 <- signif(as.numeric(quantile(x, c(0.975))),
                                             SigFig)
                              xQ95 <- paste(prettyNum(Q5, big.mark = ",",
                                                       nsmall = 0),
                                             "to",
                                             prettyNum(Q95, big.mark = ",",
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
            mean.x.sig <- prettyNum(signif(as.numeric(mean.x),
                                           ndig),
                                    big.mark = ",")
            stdev.x.sig <- prettyNum(signif(as.numeric(stdev.x), ndig),
                                     big.mark = ",")

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

            if(calc95CI == TRUE){
                  if(length(x) < 2){
                        CI95 <- "cannot calculate 95% confidence interval"
                  } else {
                        CI5 <- round(as.numeric(quantile(x, c(0.025))),
                                     ndig)
                        CI95 <- round(as.numeric(quantile(x, c(0.975))),
                                      ndig)
                        xCI95 <- paste(prettyNum(CI5, big.mark = ",",
                                                 nsmall = ndig),
                                       "to",
                                       prettyNum(CI95, big.mark = ",",
                                                 nsmall = ndig))
                  }
            }

            if(calcQuantiles == TRUE){
                  if(length(x) < 2){
                        Q95 <- "cannot calculate 95% confidence interval"
                  } else {
                        Q5 <- round(as.numeric(quantile(x, c(0.025))),
                                     ndig)
                        Q95 <- round(as.numeric(quantile(x, c(0.975))),
                                      ndig)
                        xQ95 <- paste(prettyNum(Q5, big.mark = ",",
                                                 nsmall = ndig),
                                       "to",
                                       prettyNum(Q95, big.mark = ",",
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
      xQ95 <- ifelse(calcQuantiles, xQ95, "")

      # Median is listed right after the mean, so that value is inside its
      # own set of parentheses.
      xMed <- ifelse(calcMedian, paste0(" (", xMed, ")"), "") # Adding spaces and parentheses

      # All the others are within the last set of parentheses.
      if(any(calcRange, calcCV, calc95CI, calcQuantiles, reportn)){
            if(calcRange & any(calcCV, calc95CI, calcQuantiles, reportn)){
                  xrange <- paste0(xrange, ", ")
            }

            if(calcCV & any(calc95CI, calcQuantiles, reportn)){
                  xCV <- paste0(xCV, ", ")
            }

            if(calc95CI & any(calcQuantiles, reportn)){
                  xCI95 <- paste0(xCI95, ", ")
            }

            if(calcQuantiles & reportn){
                  xQ95 <- paste0(xQ95, ", ")
            }

            Parenth2 <- paste0(" (", xrange, xCV, xCI95, xQ95, xn, ")")
      } else {
            Parenth2 <- ""
      }

      output <- paste0(mean.x.sig, xMed,
                       " \u00B1 ", stdev.x.sig, Parenth2)

      return(output)
}

