#' Compare two fitted standard curves
#'
#' To use this function, use \code{\link{stdCurve}} to fit the same data.frame
#' two different ways. For example, for one fit, include all data points and for
#' the other, omit troublesome data points to see their effect on the curve.
#' Other examples: Try comparing different weighting schemes or comparing 1st-
#' vs. 2nd-order polynomials. Input to this function is the output from
#' \code{\link{stdCurve}} from each of the two different ways.
#'
#'
#' @param stdCurve1 The list output from \code{\link{stdCurve}}
#' @param stdCurve2 The list output from a variation on \code{\link{stdCurve}}
#'   on the same data.frame or on a data.frame with the same columns
#' @param fitNames A preferably succinct character vector with two items
#'   describing what the differences are between \code{stdCurve1} and
#'   \code{stdCurve2} (optional)
#'
#' @return Output is a list of the following named objects:\describe{
#'
#'   \item{CurvePlot}{A plot of the data and the fitted lines. The two fits will
#'   be in different colors.}
#'
#'   \item{Data}{The input data, including the nominal concentration or mass,
#'   columns of the calculated concentration for each of the two fit methods,
#'   and colums of the percent differences between the calculated and the
#'   nominal concentrations or masses.}}
#'
#'
#' @examples
#' data(ExStdCurve)
#'
#' # Comparing no weights vs. weighting by 1/x^2
#' stdCurve1 <- stdCurve(ExStdCurve,
#'                       normPeak = MET.peakarearatio,
#'                       nominal = MET.nominalmass,
#'                       poly = "2nd")
#'
#' stdCurve2 <- stdCurve(ExStdCurve,
#'                       normPeak = MET.peakarearatio,
#'                       nominal = MET.nominalmass,
#'                       poly = "2nd", weights = "1/x^2")
#'
#' stdCurve_fitcompare(stdCurve1, stdCurve2)
#' stdCurve_fitcompare(stdCurve1, stdCurve2, fitNames = c("no weighting", "1/x2 weighting"))
#'
#'
#' # Comparing all points vs. omitting one point.
#' stdCurve1 <- stdCurve(ExStdCurve,
#'                       normPeak = MET.peakarearatio,
#'                       nominal = MET.nominalmass,
#'                       poly = "2nd", IDcol = SampleID)
#'
#' stdCurve2 <- stdCurve(ExStdCurve %>% filter(SampleID != "standard 200"),
#'                       normPeak = MET.peakarearatio,
#'                       nominal = MET.nominalmass,
#'                       poly = "2nd", IDcol = SampleID)
#'
#' stdCurve_fitcompare(stdCurve1, stdCurve2)
#' stdCurve_fitcompare(stdCurve1, stdCurve2,
#'                     fitNames = c("all points", "omit standard 200"))
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
            stop("The names of the data columns don't match. The columns must be the same for this function to work appropriately. Note: If you used 'colorBy' for one fit and did NOT use 'colorBy' for the other, that would cause the names of the columns to not match.")
      }

      # If they didn't include IDcol or colorBy, then there should be 4 columns.
      if(ncol(stdCurve1$Data) == 4){
            names(stdCurve1$Data) <- c("Nominal", "NormPeak", "Calculated_A", "PercDiff_A")
            names(stdCurve2$Data) <- c("Nominal", "NormPeak", "Calculated_B", "PercDiff_B")
            Xlab <- OrigNames1[1]
            Ylab <- OrigNames1[2]
      }

      # If they included IDcol but not colorBy OR if they included colorBy but
      # not IDcol, then these should be the names:
      if(ncol(stdCurve1$Data) == 5){
            names(stdCurve1$Data) <- c("IDcol", "Nominal", "NormPeak", "Calculated_A", "PercDiff_A")
            names(stdCurve2$Data) <- c("IDcol", "Nominal", "NormPeak", "Calculated_B", "PercDiff_B")
            Xlab <- OrigNames1[2]
            Ylab <- OrigNames1[3]
      }

      # If they included both IDcol AND colorBy, then these should be the names:
      if(ncol(stdCurve1$Data) == 6){
            names(stdCurve1$Data) <- c("IDcol", "colorBy", "Nominal", "NormPeak", "Calculated_A", "PercDiff_A")
            names(stdCurve2$Data) <- c("IDcol", "colorBy", "Nominal", "NormPeak", "Calculated_B", "PercDiff_B")
            Xlab <- OrigNames1[3]
            Ylab <- OrigNames1[4]

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

      # Removing any replicates and arranging out data.
      if("IDcol" %in% names(OutData)){
            if(anyDuplicated(OutData$IDcol)){
                  OutData <- OutData %>%
                        group_by(IDcol, Nominal) %>%
                        summarize(Nominal = mean(Nominal, na.rm = T),
                                  Calculated_A = mean(Calculated_A, na.rm = T),
                                  Calculated_B = mean(Calculated_B, na.rm = T),
                                  PercDiff_A = mean(PercDiff_A, na.rm = T),
                                  PercDiff_B = mean(PercDiff_B, na.rm = T))
                  ungroup()
            }
      }

      OutData <- OutData %>% arrange(Nominal)

      if(all(complete.cases(fitNames)) & length(fitNames == 2)){
            PlotData <- PlotData %>%
                  mutate(Fit = recode(Fit, "Fit_A" = fitNames[1],
                                      "Fit_B" = fitNames[2]))
            CurveData <- CurveData %>%
                  mutate(Fit = recode(Fit, "Fit_A" = fitNames[1],
                                      "Fit_B" = fitNames[2]))

            names(OutData) <- sub("_A", paste0("_", fitNames[1]),
                                       names(OutData))
            names(OutData) <- sub("_B", paste0("_", fitNames[2]),
                                       names(OutData))
      }

      CurvePlot <- ggplot2::ggplot(PlotData, aes(x = Nominal, y = NormPeak,
                                                 color = Fit, shape = Fit)) +
            geom_point() +
            geom_line(data = CurveData, aes(x = Nominal, y = NormPeak,
                                            color = Fit)) +
            # scale_color_manual(values = c("dodgerblue3", "#3E8853")) +
            xlab(Xlab) + ylab(Ylab)

      names(OutData)[1] <- OrigNames1[1]

      Out <- list(CurvePlot, OutData)
      names(Out) <- c("CurvePlot", "Data")

      return(Out)
}

