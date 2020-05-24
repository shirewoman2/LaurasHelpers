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
#' @param rawPeak The unadjusted instrument response column name. Ignore this if
#'   data are already normalized by internal standard.
#' @param rawIS The internal standard column name. This is optional if
#'   \code{normPeak} is provided.
#' @param normPeak The name of the column containing instrument response
#'   normalized by internal standard. Use this if the data are peak heights or
#'   areas already divided by the IS peak height or area.
#' @param nominal The column with the nominal concentrations or masses.
#' @param poly Should the data be fit to a 1st or 2nd order polynomial? Options:
#'   "1st" or "2nd".
#' @param weights A vector of weights to use for the regression. If left as NA,
#'   no weighting scheme will be used. Be careful that you don't have any
#'   infinite values or this will fail!
#' @param colorBy What column to color the points by in the standard curve
#'   graph. If left as NA, points will all be black.
#'
#' @return Output is a list of the following named objects:\describe{
#'
#'   \item{Fit}{A list of the fitted parameters}
#'
#'   \item{CurvePlot}{A plot of the data and the fitted line}
#'
#'   \item{Curve.DF}{A data.frame of the points used for graphing the fitted
#'   line.}
#'
#'   \item{Data}{The original data with a column calculating the percent
#'   difference between the calculated and the nominal concentrations or
#'   masses}}
#' @examples
#'
#' data(ExStdCurve)
#' stdCurve(ExStdCurve, rawPeak = MET.area,
#'          rawIS = d6MET.area,
#'          nominal = MET.nominalmass, poly = "2nd")
#'
#' stdCurve(ExStdCurve, normPeak = MET.peakarearatio,
#'          nominal = MET.nominalmass, poly = "1st",
#'          weights = 1/ExStdCurve$MET.nominalmass)
#'
#' @export
#'


stdCurve <- function(DF, rawPeak, rawIS, normPeak = NA,
                     nominal, poly = "1st", weights = NULL,
                     colorBy = NA) {

      nominal <- enquo(nominal)

      if(is.na(normPeak)){
            rawPeak <- enquo(rawPeak)
            rawIS <- enquo(rawIS)
      } else {
            normPeak <- enquo(normPeak)
      }

      if(complete.cases(colorBy)){
            colorBy <- enquo(colorBy)
      }

      # When normalized peak height or area is not given, only raw, calculate
      # that.
      if(is.na(normPeak)){

            # If "colorBy" was provided, we need to retain that column, but we
            # *can't* included it in the select call if it's not present. Need
            # to catch that.
            if(complete.cases(colorBy)){
                  colorBy <- enquo(colorBy)

                  DF <- DF %>% select(!! rawPeak, !! rawIS, !! nominal,
                                      !! colorBy) %>%
                        rename(rawPeak = !! rawPeak,
                               rawIS = !! rawIS,
                               nominal = !! nominal,
                               COLOR = !! colorBy) %>%
                        mutate(normPeak = rawPeak/rawIS)

            } else {
                  DF <- DF %>% select(!! rawPeak, !! rawIS, !! nominal) %>%
                        rename(rawPeak = !! rawPeak,
                               rawIS = !! rawIS,
                               nominal = !! nominal) %>%
                        mutate(normPeak = rawPeak/rawIS)
            }

      } else {

            # If "colorBy" was provided, we need to retain that column, but we
            # *can't* included it in the select call if it's not present. Need
            # to catch that.
            if(complete.cases(colorBy)){
                  colorBy <- enquo(colorBy)

                  DF <- DF %>% select(!! normPeak, !! nominal, !! colorBy) %>%
                        rename(normPeak = !! normPeak,
                               nominal = !! nominal,
                               COLOR = !! colorBy)
            } else {
                  DF <- DF %>% select(!! normPeak, !! nominal) %>%
                        rename(normPeak = !! normPeak,
                               nominal = !! nominal)
            }
      }

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

            CurvePlot <- ggplot2::ggplot(DF, aes(x = nominal, y = normPeak)) +
                  geom_point() +
                  geom_line(data = Curve, aes(x = nominal, y = normPeak)) +
                  xlab(enquo(nominal)) + ylab(enquo(normPeak))

      } else {

            CurvePlot <- ggplot2::ggplot(DF, aes(x = nominal, y = normPeak,
                                                 color = COLOR)) +
                  geom_point() +
                  geom_line(data = Curve, aes(x = nominal, y = normPeak),
                            inherit.aes = FALSE) +
                  labs(color = enquo(colorBy)) +
                  xlab(enquo(nominal)) + ylab(enquo(normPeak))

      }

      if(poly == "1st"){
            DF$Calculated <- (DF$normPeak - beta0)/beta1
      }

      if(poly == "2nd"){
            DF$Calculated <- (-beta1 + sqrt(beta1^2 - 4*beta2*(beta0 - DF$normPeak)))/
                  (2*beta2)
      }

      DF$PercentDifference <- (DF$Calculated - DF$nominal)/DF$nominal
      DF$PercentDifference[DF$nominal == 0] <- NA
      DF <- rename(DF, c(Nominal = nominal,
                         NormPeak = normPeak))

      CurveResults <- list(Fit, CurvePlot, Curve, DF)
      names(CurveResults) <- c("Fit", "CurvePlot", "Curve.DF", "Data")

      return(CurveResults)

}
