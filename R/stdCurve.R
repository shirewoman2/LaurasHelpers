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
#' @param rawPeak The unadjusted instrument response column. Ignore this if
#'   data are already normalized by internal standard.
#' @param rawIS The internal standard column name. This is ignored if
#'   \code{normPeak} is provided.
#' @param normPeak The column containing instrument response normalized by
#'   internal standard. Use this if the data are peak heights or areas already
#'   divided by the IS peak height or area.
#' @param nominal The column with the nominal concentrations or masses.
#' @param poly Should the data be fit to a 1st or 2nd order polynomial? Options:
#'   "1st" or "2nd".
#' @param weights A vector of weights to use for the regression. If left as
#'   NULL, no weighting scheme will be used. Be careful that you don't have any
#'   infinite values or this will fail!
#' @param IDcol Optional column with sample IDs
#' @param colorBy What column to color the points by in the standard curve
#'   graph. If not set, all points will be black.
#'
#' @return Output is a list of the following named objects:\describe{
#'
#'   \item{Fit}{A list of the fitted parameters}
#'
#'   \item{CurvePlot}{A plot of the data and the fitted line}
#'
#'   \item{CurveDF}{A data.frame of the points used for graphing the fitted
#'   line.}
#'
#'   \item{Data}{The original data with a column calculating the percent
#'   difference between the calculated and the nominal concentrations or
#'   masses}}
#' @examples
#' data(ExStdCurve)
#'
#' # Using a peak ratio that's already been calculated
#' stdCurve(ExStdCurve,
#'          normPeak = MET.peakarearatio,
#'          nominal = MET.nominalmass,
#'          poly = "2nd")
#'
#' # Having 'stdCurve' calculate the peak ratio
#' stdCurve(ExStdCurve,
#'          rawPeak = MET.area,
#'          rawIS = d6MET.area,
#'          nominal = MET.nominalmass,
#'          poly = "2nd",
#'          IDcol = SampleID)
#'
#' # Using weights in the nonlinear regression
#' stdCurve(ExStdCurve,
#'          normPeak = MET.peakarearatio,
#'          nominal = MET.nominalmass,
#'          poly = "1st",
#'          weights = 1/ExStdCurve$MET.nominalmass)
#'
#' # Coloring by some variable
#' stdCurve(ExStdCurve %>% mutate(Group = c(rep("A", 5), rep("B", 6))),
#'          normPeak = MET.peakarearatio,
#'          nominal = MET.nominalmass,
#'          colorBy = Group,
#'          poly = "2nd")
#'
#' @export
#'


stdCurve <- function(DF,
                     rawPeak,
                     rawIS,
                     normPeak,
                     nominal,
                     poly = "1st",
                     weights = NULL,
                     IDcol = NA,
                     colorBy) {

      nominal <- enquo(nominal)
      rawPeak <- enquo(rawPeak)
      rawIS <- enquo(rawIS)
      normPeak <- enquo(normPeak)
      IDcol <- enquo(IDcol)
      colorBy <- enquo(colorBy)

      DForig <- DF

      # If normPeak is NOT supplied, calculate it.
      if(as_label(normPeak) %in% names(DForig) == FALSE){
            DF <- DF %>% mutate(NormPeak = !!rawPeak / !!rawIS)

            # Now, only keep NormPeak, nominal, and, if present, colorBy and
            # IDcol. Rename them to work with more easily later in the function.
            if(as_label(colorBy) %in% names(DForig)){
                  DF <- DF %>% select(any_of(c(as_label(nominal), "NormPeak",
                                               as_label(colorBy),
                                               as_label(IDcol)))) %>%
                        rename(Nominal = !!nominal,
                               ColorBy = !!colorBy)
            } else {
                  DF <- DF %>%
                        select(any_of(c(as_label(nominal), "NormPeak"))) %>%
                        rename(Nominal = !!nominal)
            }

            # Setting the y label for the graphs based on whether normPeak was
            # supplied.
            Ylabel <- paste0(as_label(rawPeak), "/", as_label(rawIS))

      } else {
            # If normPeak *is* supplied, keep that. Check for colorBy. Rename
            # everything to make life easier farther down in the function.
            if(as_label(colorBy) %in% names(DForig)){
                  DF <- DF %>% select(any_of(c(as_label(nominal),
                                               as_label(normPeak),
                                               as_label(colorBy),
                                               as_label(IDcol)))) %>%
                        rename(Nominal = !!nominal,
                               NormPeak = !!normPeak,
                               ColorBy = !!colorBy)
            } else {
                  DF <- DF %>% select(any_of(c(as_label(nominal),
                                               as_label(normPeak),
                                               as_label(IDcol)))) %>%
                        rename(Nominal = !!nominal,
                               NormPeak = !!normPeak)
            }

            # Setting the y label for the graphs based on whether normPeak was
            # supplied.
            Ylabel <- as_label(normPeak)
      }

      MaxNominal <- max(DF$Nominal, na.rm = TRUE)

      if(poly == "1st"){
            Fit <- lm(DF$NormPeak ~ DF$Nominal, weights = weights)
            beta0 <- summary(Fit)$coef["(Intercept)", "Estimate"]
            beta1 <- summary(Fit)$coef["DF$Nominal", "Estimate"]

            Curve <- data.frame(Nominal = seq(0, MaxNominal, length.out = 1000))
            Curve$NormPeak <- beta1 * Curve$Nominal + beta0
      }

      if(poly == "2nd"){
            Fit <- nls(NormPeak ~ beta2*Nominal^2 + beta1*Nominal + beta0,
                       data = DF,
                       start = list(beta2 = 0.01,
                                    beta1 = summary(lm(
                                          DF$NormPeak ~ DF$Nominal))$coef[
                                                "DF$Nominal", "Estimate"],
                                    beta0 = 0),
                       weights = weights)
            beta0 <- summary(Fit)$coef["beta0", "Estimate"]
            beta1 <- summary(Fit)$coef["beta1", "Estimate"]
            beta2 <- summary(Fit)$coef["beta2", "Estimate"]

            Curve <- data.frame(Nominal = seq(0, MaxNominal, length.out = 1000))
            Curve$NormPeak <- beta2*Curve$Nominal^2 + beta1*Curve$Nominal + beta0
      }

      if(as_label(colorBy) %in% names(DForig)){

            CurvePlot <- ggplot2::ggplot(DF, aes(x = Nominal, y = NormPeak,
                                                 color = ColorBy)) +
                  geom_point() +
                  geom_line(data = Curve, aes(x = Nominal, y = NormPeak),
                            inherit.aes = FALSE) +
                  labs(color = as_label(colorBy)) +
                  xlab(as_label(nominal)) + ylab(Ylabel)

      } else {
            CurvePlot <- ggplot2::ggplot(DF, aes(x = Nominal, y = NormPeak)) +
                  geom_point() +
                  geom_line(data = Curve, aes(x = Nominal, y = NormPeak)) +
                  xlab(as_label(nominal)) + ylab(Ylabel)
      }

      if(poly == "1st"){
            DF$Calculated <- (DF$NormPeak - beta0)/beta1
      }

      if(poly == "2nd"){
            DF$Calculated <- (-beta1 + sqrt(beta1^2 - 4*beta2*(beta0 - DF$NormPeak)))/
                  (2*beta2)
      }

      DF <- DF %>%
            mutate(PercentDifference = (Calculated - Nominal)/Nominal,
                   PercentDifference = ifelse(Nominal == 0,
                                              NA, PercentDifference),
                   Nominal = signif(Nominal, 3),
                   NormPeak = signif(NormPeak, 3),
                   Calculated = round(Calculated, 2),
                   PercentDifference = round(PercentDifference, 2)) %>%
            select(any_of(c(as_label(IDcol), "ColorBy", "Nominal", "NormPeak",
                            "Calculated", "PercentDifference")))

      if(as_label(normPeak) %in% names(DForig)){
            names(DF)[names(DF) == "Nominal"] <- as_label(nominal)
            names(DF)[names(DF) == "NormPeak"] <- as_label(normPeak)
            names(Curve)[names(Curve) == "Nominal"] <- as_label(nominal)
            names(Curve)[names(Curve) == "NormPeak"] <- as_label(normPeak)

      } else {
            names(DF)[names(DF) == "Nominal"] <- as_label(nominal)
            names(DF)[names(DF) == "NormPeak"] <-
                  paste0(as_label(rawPeak), "/", as_label(rawIS))
            names(Curve)[names(Curve) == "Nominal"] <- as_label(nominal)
            names(Curve)[names(Curve) == "NormPeak"] <-
                  paste0(as_label(rawPeak), "/", as_label(rawIS))

      }

      if(as_label(colorBy) %in% names(DForig)){
            names(DF)[names(DF) == "ColorBy"] <- as_label(colorBy)
            names(Curve)[names(Curve) == "ColorBy"] <- as_label(colorBy)
      }

      CurveResults <- list(Fit, CurvePlot, Curve, DF)
      names(CurveResults) <- c("Fit", "CurvePlot", "CurveDF", "Data")

      return(CurveResults)

}
