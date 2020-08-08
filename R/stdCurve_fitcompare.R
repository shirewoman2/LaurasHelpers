#' Compare two fitted standard curves
#'
#' To use this function, use \code{\link{stdCurve}} to fit the same data.frame
#' two different ways. For example, for one fit, include all data points and for
#' the other, omit troublesome data points to see their effect on the curve.
#' Other examples: Try comparing different weighting schemes or comparing 1st-
#' vs. 2nd-order polynomials. Input to this function is the output from
#' \code{\link{stdCurve}} the two different ways.
#'
#'
#' @param stdCurve1 The list output from \code{\link{stdCurve}}
#' @param stdCurve2 The list output from a slight variation on
#'   \code{\link{stdCurve}} on the same data.frame
#' @param fitNames A preferably succinct character vector with two items
#'   describing what the differences are between \code{stdCurve1} and
#'   \code{stdCurve2} (optional)
#'
#' @return Output is a list of the following named objects:\describe{
#'
#'   \item{CurvePlot}{A plot of the data and the fitted lines. The two fits will
#'   be in different colors.}
#'
#'   \item{Data}{The input data including the nominal concentration or mass,
#'   columns of the calculated concentration for each of the two fit methods,
#'   and colums of the percent differences between the calculated and the
#'   nominal concentrations or masses.}}
#'
#'
#' @examples
#' data(ExStdCurve)
#' stdCurve1 <- stdCurve(ExStdCurve,
#'                       normPeak = MET.peakarearatio,
#'                       nominal = MET.nominalmass,
#'                       poly = "2nd")
#'
#' stdCurve2 <- stdCurve(ExStdCurve,
#'                       normPeak = MET.peakarearatio,
#'                       nominal = MET.nominalmass,
#'                       poly = "2nd", weights = 1/ExStdCurve$MET.nominalmass^2)
#'
#' stdCurve_fitcompare(stdCurve1, stdCurve2)
#' stdCurve_fitcompare(stdCurve1, stdCurve2, fitNames = c("no weighting", "1/x2 weighting"))
#'
#'
#' stdCurve1 <- stdCurve(ExStdCurve %>% filter(SampleID != "standard 100 A"),
#'                       normPeak = MET.peakarearatio,
#'                       nominal = MET.nominalmass,
#'                       poly = "2nd")
#'
#' stdCurve2 <- stdCurve(ExStdCurve,
#'                       normPeak = MET.peakarearatio,
#'                       nominal = MET.nominalmass,
#'                       poly = "2nd")
#'
#' stdCurve_fitcompare(stdCurve1, stdCurve2)
#' stdCurve_fitcompare(stdCurve1, stdCurve2,
#'                     fitNames = c("omit standard 100", "all points"))
#'
#'
#'
#' @export
#'
#'
#'
stdCurve_fitcompare <- function(stdCurve1, stdCurve2, fitNames = NA){
      OrigNames1 <- names(stdCurve1$Data)
      OrigNames2 <- names(stdCurve1$Data)

      if(any(OrigNames1 != OrigNames2) |
         ncol(stdCurve1$Data) != ncol(stdCurve2$Data)){
            stop("The names of the data columns don't match. The columns must be the same for this function to work appropriately.")
      }

      # If they didn't include IDcol or colorBy, then there should be 4 columns.
      if(ncol(stdCurve1$Data) == 4){
            names(stdCurve1$Data) <- c("Nominal", "NormPeak", "Calculated_A", "PercDiff_A")
            names(stdCurve2$Data) <- c("Nominal", "NormPeak", "Calculated_B", "PercDiff_B")
      }

      # If they included IDcol but not colorBy OR if they included colorBy but
      # not IDcol, then these should be the names:
      if(ncol(stdCurve1$Data) == 5){
            names(stdCurve1$Data) <- c("IDcol", "Nominal", "NormPeak", "Calculated_A", "PercDiff_A")
            names(stdCurve2$Data) <- c("IDcol", "Nominal", "NormPeak", "Calculated_B", "PercDiff_B")
      }

      # If they included both IDcol AND colorBy, then these should be the names:
      if(ncol(stdCurve1$Data) == 6){
            names(stdCurve1$Data) <- c("IDcol", "colorBy", "Nominal", "NormPeak", "Calculated_A", "PercDiff_A")
            names(stdCurve2$Data) <- c("IDcol", "colorBy", "Nominal", "NormPeak", "Calculated_B", "PercDiff_B")
      }

      OutData <- stdCurve1$Data %>% full_join(stdCurve2$Data) %>%
            select(any_of(c("IDcol", "colorBy", "Nominal",
                            "Calculated_A", "Calculated_B",
                            "PercDiff_A", "PercDiff_B"))) %>%
            arrange(Nominal)

      # Making graph
      PlotData <- stdCurve1$Data %>% mutate(Fit = "Fit_A") %>%
            select(Nominal, NormPeak, Fit) %>%
            bind_rows(stdCurve2$Data %>% mutate(Fit = "Fit_B") %>%
                            select(Nominal, NormPeak, Fit))

      CurveData <- stdCurve1$CurveDF %>% mutate(Fit = "Fit_A") %>%
            bind_rows(stdCurve2$CurveDF %>% mutate(Fit = "Fit_B"))
      names(CurveData) <- c("Nominal", "NormPeak", "Fit")

      if(all(complete.cases(fitNames)) & length(fitNames == 2)){
            PlotData <- PlotData %>%
                  mutate(Fit = recode(Fit, "Fit_A" = fitNames[1],
                                      "Fit_B" = fitNames[2]))
            CurveData <- CurveData %>%
                  mutate(Fit = recode(Fit, "Fit_A" = fitNames[1],
                                      "Fit_B" = fitNames[2]))

            names(OutData)[2:5] <- sub("_A", paste0("_", fitNames[1]),
                                       names(OutData)[2:5])
            names(OutData)[2:5] <- sub("_B", paste0("_", fitNames[2]),
                                       names(OutData)[2:5])
      }

      CurvePlot <- ggplot2::ggplot(PlotData, aes(x = Nominal, y = NormPeak,
                                                 color = Fit, shape = Fit)) +
            geom_point() +
            geom_line(data = CurveData, aes(x = Nominal, y = NormPeak,
                                            color = Fit, linetype = Fit)) +
            xlab(OrigNames1[1]) + ylab(OrigNames1[2])

      names(OutData)[1] <- OrigNames1[1]

      Out <- list(CurvePlot, OutData)
      names(Out) <- c("CurvePlot", "Data")

      return(Out)
}

