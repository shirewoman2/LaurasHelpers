#' Fit data to a standard curve
#'
#' \code{stdCurve}, which was designed with mass spectrometry data in mind, fits
#' concentration and signal data to a standard curve and returns the calculated
#' betas, a plot of the standard curve, the data that were used to generate the
#' fitted line, and the original data that were used to generate the standard
#' curve.
#'
#' @param DF The input data.frame with columns containing the nominal analyte
#'   concentration and the instrument response
#' @param rawPeak The unadjusted instrument response column name (character).
#'   Ignore this if data are already normalized by internal standard.
#' @param rawIS The internal standard column name (character). Ok to ignore if
#'   data are already normalized by internal standard.
#' @param normPeak The name of the column containing instrument response
#'   normalized by internal standard. Use this if the data are peak heights or
#'   areas already divided by the IS peak height or area.
#' @param nominal The column with the nominal concentrations or masses
#'   (character).
#' @param poly Should the data be fit to a 1st or 2nd order polynomial? Options:
#'   "1st" or "2nd".
#' @param weights A vector of weights to use for the regression. If left as NA,
#'   no weighting scheme will be used. Be careful that you don't have any
#'   infinite values!
#' @param colorBy What column to color the points by in the standard curve
#'   graph. If left as NA, points will all be black.
#'
#' @return Output is a list of the following named objects:
#'
#'   1. Fit - A list of the fitted parameters
#'
#'   2. CurvePlot - A plot of the data and the fitted line,
#'
#'   3. Curve.DF - A data.frame of the points used for graphing the fitted line.
#'
#'   4. Data - The original data with a column calculating the percent
#'   difference between the calculated and the nominal concentrations or masses
#'
#' @export
#'


stdCurve <- function(DF, rawPeak, rawIS, normPeak = NA,
                     nominal, poly = "1st", weights = NULL,
                     colorBy = NA) {

      if(is.na(normPeak)){
            # Normalized peak height or area is not given, only raw
            names(DF)[names(DF) == rawPeak] <- "rawPeak"
            names(DF)[names(DF) == rawIS] <- "rawIS"
            DF$normPeak <- DF$rawPeak/DF$rawIS
      } else {
            names(DF)[names(DF) == normPeak] <- "normPeak"
      }
      names(DF)[names(DF) == nominal] <- "nominal"

      Maxnominal <- max(DF$nominal, na.rm = TRUE)

      if(poly == "1st"){
            Fit <- lm(DF$normPeak ~ DF$nominal, weights = weights)
            beta0 <- summary(Fit)$coef["(Intercept)", "Estimate"]
            beta1 <- summary(Fit)$coef["DF$nominal", "Estimate"]

            Curve <- data.frame(nominal = seq(0, Maxnominal, length.out = 1000))
            Curve$normPeak <- beta1 * Curve$nominal + beta0
      }

      if(poly == "2nd"){
            Fit <- nls(normPeak ~ beta2*nominal^2 + beta1*nominal + beta0,
                       data = DF,
                       start = list(beta2 = 0.01,
                                    beta1 = summary(lm(
                                          DF$normPeak ~ DF$nominal))$coef[
                                                "DF$nominal", "Estimate"],
                                    beta0 = 0),
                       weights = weights)
            beta0 <- summary(Fit)$coef["beta0", "Estimate"]
            beta1 <- summary(Fit)$coef["beta1", "Estimate"]
            beta2 <- summary(Fit)$coef["beta2", "Estimate"]

            Curve <- data.frame(nominal = seq(0, Maxnominal, length.out = 1000))
            Curve$normPeak <- beta2*Curve$nominal^2 + beta1*Curve$nominal + beta0
      }

      if(is.na(colorBy)){

            CurvePlot <- ggplot(DF, aes(x = nominal, y = normPeak)) +
                  geom_point() +
                  geom_line(data = Curve, aes(x = nominal, y = normPeak)) +
                  xlab(nominal) + ylab(normPeak)

      } else {

            names(DF)[names(DF) == colorBy] <- "COLOR"

            CurvePlot <- ggplot(DF, aes(x = nominal, y = normPeak,
                                        color = COLOR)) +
                  geom_point() +
                  geom_line(data = Curve, aes(x = nominal, y = normPeak),
                            inherit.aes = FALSE) +
                  labs(color = colorBy) +
                  xlab(nominal) + ylab(normPeak)

      }

      names(Curve)[names(Curve) == "nominal"] <- nominal
      names(Curve)[names(Curve) == "normPeak"] <- normPeak

      if(poly == "1st"){
            DF$Calculated <- (DF$normPeak - beta0)/beta1
      }

      if(poly == "2nd"){
            DF$Calculated <- (-beta1 + sqrt(beta1^2 - 4*beta2*(beta0 - DF$normPeak)))/
                  (2*beta2)
      }

      DF$PercentDifference <- (DF$Calculated - DF$nominal)/DF$nominal
      DF$PercentDifference[DF$nominal == 0] <- NA
      DF <- plyr::rename(DF, c("nominal" = nominal,
                               "normPeak" = normPeak))

      CurveResults <- list(Fit, CurvePlot, Curve, DF)
      names(CurveResults) <- c("Fit", "CurvePlot", "Curve.DF", "Data")

      return(CurveResults)

}
