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
#' @param rawPeak The unadjusted instrument response column. Ignore this if data
#'   are already normalized by internal standard.
#' @param rawIS The internal standard column name. This is ignored if
#'   \code{normPeak} is provided.
#' @param normPeak The column containing instrument response normalized by
#'   internal standard. Use this if the data are peak heights or areas already
#'   divided by the IS peak height or area.
#' @param nominal The column with the nominal concentrations or masses.
#' @param poly Should the data be fit to a 1st or 2nd order polynomial? Options:
#'   "1st" or "2nd".
#' @param weights Weighting scheme to use for the regression. User may supply a
#'   numeric vector of weights to use or choose from "1/x", "1/x^2", "1/y" or
#'   "1/y^2". If left as NULL, no weighting scheme will be used. Be careful that
#'   you don't have any infinite values or this will fail!
#' @param omit An index of which, if any, samples to omit from the curve. These
#'   samples will be depicted as red open circles in the graph but will not be
#'   included in the regression. The red color for omitted points overrides any
#'   other choices for \code{colorBy}.
#' @param IDcol Optional column with sample IDs
#' @param colorBy What column to color the points by in the standard curve
#'   graph. If not set, all points will be black.
#' @param useNLS_outnames TRUE or FALSE for whether the object "Fit" should be
#'   the standard, list output from \code{nls} or \code{nls2}. If set to FALSE,
#'   the output will be a data.frame of the coefficients with column names
#'   "Beta", "Estimate", "SE", "tvalue" and "pvalue".
#'
#' @return Output is a list of the following named objects:\describe{
#'
#'   \item{Fit}{The fitted parameters}
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
#' # Having 'stdCurve' calculate the peak ratio and making the fitted
#' # coefficients a data.frame rather than a list
#' stdCurve(ExStdCurve,
#'          rawPeak = MET.area,
#'          rawIS = d6MET.area,
#'          nominal = MET.nominalmass,
#'          poly = "2nd",
#'          IDcol = SampleID,
#'          useNLS_outnames = FALSE)
#'
#' # Using weights in the nonlinear regression
#' stdCurve(ExStdCurve,
#'          normPeak = MET.peakarearatio,
#'          nominal = MET.nominalmass,
#'          poly = "1st",
#'          weights = "1/x")
#'
#' # Omitting certain points from the regression but showing them on the graph
#' stdCurve(ExStdCurve,
#'          normPeak = MET.peakarearatio,
#'          nominal = MET.nominalmass,
#'          poly = "2nd",
#'          omit = which(ExStdCurve$MET.nominalmass > 10 &
#'                             ExStdCurve$MET.nominalmass < 20),
#'          useNLS_outnames = FALSE)
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
                     omit = NA,
                     IDcol = NA,
                     colorBy,
                     useNLS_outnames = TRUE) {

      nominal <- dplyr::enquo(nominal)
      rawPeak <- dplyr::enquo(rawPeak)
      rawIS <- dplyr::enquo(rawIS)
      normPeak <- dplyr::enquo(normPeak)
      IDcol <- dplyr::enquo(IDcol)
      colorBy <- dplyr::enquo(colorBy)

      DForig <- DF

      # If the user supplied a value for "omit" but that value doesn't fall
      # within DF, issue a warning but keep going.
      if(any(complete.cases(omit)) & any(omit %in% 1:nrow(DF) == FALSE) |
         length(omit) == 0){
            message("One or more of the values supplied for 'omit' do not fall within the range of the data.frame supplied. All of the points supplied were included in the regression.")
      }

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

            # Removing any rows the user requests
            if(any(complete.cases(omit)) & any(omit %in% 1:nrow(DF))){
                  DFomit <- DF %>% slice(omit)
                  DF <- DF %>% slice(-omit)
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

            # Removing any rows the user requests
            if(any(complete.cases(omit)) & any(omit %in% 1:nrow(DF))){
                  DFomit <- DF %>% slice(omit)
                  DF <- DF %>% slice(-omit)
            }

            # Setting the y label for the graphs based on whether normPeak was
            # supplied.
            Ylabel <- as_label(normPeak)
      }

      MaxNominal <- max(DF$Nominal, na.rm = TRUE)

      # Setting up the weights to use
      if(class(weights) == "character"){

            WeightOptions <- DF %>%
                  select(Nominal, NormPeak) %>%
                  mutate(One_x = 1/Nominal,
                         One_x2 = 1/Nominal^2,
                         One_y = 1/NormPeak,
                         One_y2 = 1/NormPeak^2)

            weights <- tolower(weights)

            MyWeights <- c("1/x" = "One_x", "1/x^2" = "One_x2",
                           "1/y" = "One_y", "1/y^2" = "One_y2")

            weights <- WeightOptions %>% pull(MyWeights[weights])
      }

      if(any(is.infinite(weights))){
            stop("The weights used for the regression cannot include infinite numbers. Please change the weighting scheme to avoid this. If the problem is that you've included a point with a nominal mass or concentration of 0, that shouldn't be part or your curve anyway since it is below the LLOQ; remove it.")
      }

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

      # If the user wants to use better names for the output data.frame, setting
      # those here for 2nd order polynomials...
      if(useNLS_outnames == FALSE & class(Fit) == "nls"){
            Fit <- as.data.frame(summary(Fit)[["coefficients"]])
            names(Fit) <- c("Estimate", "SE", "tvalue", "pvalue")
            Fit$Beta <- row.names(Fit)
            Fit <- Fit %>% select(Beta, Estimate, SE, tvalue, pvalue) %>%
                  arrange(desc(Beta))
      }
      # ... and for 1st order polynomials.
      if(useNLS_outnames == FALSE & class(Fit) == "lm"){
            Fit <- as.data.frame(summary(Fit)[["coefficients"]])
            names(Fit) <- c("Estimate", "SE", "tvalue", "pvalue")
            Fit$Beta <- c("beta0", "beta1")
            Fit <- Fit %>% select(Beta, Estimate, SE, tvalue, pvalue) %>%
                  arrange(desc(Beta))
      }

      if(as_label(colorBy) %in% names(DForig)){

            if(theme_get()$panel.background$fill == "grey92"){
                  ColorsToUse <- c("black",  "green")
            } else {
                  ColorsToUse <- c("black", "#5ECCF3")
            }

            CurvePlot <- ggplot2::ggplot(DF, aes(x = Nominal, y = NormPeak,
                                                 fill = ColorBy, color = ColorBy,
                                                 shape = ColorBy)) +
                  geom_point(size = 2, shape = 21) +
                  geom_line(data = Curve, ggplot2::aes(x = Nominal, y = NormPeak),
                            inherit.aes = FALSE, color = "gray60") +
                  labs(color = as_label(colorBy), fill = as_label(colorBy),
                       shape = as_label(colorBy)) +
                  xlab(as_label(nominal)) +
                  ylab(Ylabel) +
                  scale_shape_manual(values = c(16, 17))

            if(length(unique(DF$ColorBy)) == 2){
                  CurvePlot <- CurvePlot +
                        scale_fill_manual(values = ColorsToUse) +
                        scale_color_manual(values = c("black", "#005883"))
            }

      } else {
            CurvePlot <- ggplot2::ggplot(DF, aes(x = Nominal, y = NormPeak)) +
                  geom_point() +
                  geom_line(data = Curve, aes(x = Nominal, y = NormPeak),
                            color = "gray60") +
                  xlab(as_label(nominal)) +
                  ylab(Ylabel)
      }

      if(any(complete.cases(omit)) & any(omit %in% 1:nrow(DF))){
            CurvePlot <- CurvePlot +
                  geom_point(data = DFomit, aes(x = Nominal, y = NormPeak),
                             color = "red", inherit.aes = FALSE, shape = "o", size = 2)
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
                            "Calculated", "PercentDifference"))) %>%
            arrange(Nominal)

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
