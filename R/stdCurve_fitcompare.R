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
#'   and columns of the calculated concentration or mass as a percent of nominal
#'   concentration or mass for each of the two fits.}}
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
#' stdCurve2 <- stdCurve(ExStdCurve %>%  dplyr::filter(SampleID != "standard 200"),
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

      # Defining pipe operator and bang bang
      `%>%` <- magrittr::`%>%`
      `!!` <- rlang::`!!`

      OrigNames1 <- names(stdCurve1$Data)
      OrigNames2 <- names(stdCurve1$Data)

      if(any(OrigNames1 != OrigNames2) |
         ncol(stdCurve1$Data) != ncol(stdCurve2$Data)){
            stop("The names of the data columns don't match. The columns must be the same for this function to work appropriately. Note: If you used 'colorBy' for one fit and did NOT use 'colorBy' for the other, that would cause the names of the columns to not match.")
      }

      # If they didn't include IDcol or colorBy, then there should be 4 columns.
      if(ncol(stdCurve1$Data) == 4){
            names(stdCurve1$Data) <- c("Nominal", "NormPeak", "Calculated_A", "PercOfNom_A")
            names(stdCurve2$Data) <- c("Nominal", "NormPeak", "Calculated_B", "PercOfNom_B")
            Xlab <- OrigNames1[1]
            Ylab <- OrigNames1[2]
      }

      # If they included IDcol but not colorBy OR if they included colorBy but
      # not IDcol, then these should be the names:
      if(ncol(stdCurve1$Data) == 5){
            names(stdCurve1$Data) <- c("IDcol", "Nominal", "NormPeak", "Calculated_A", "PercOfNom_A")
            names(stdCurve2$Data) <- c("IDcol", "Nominal", "NormPeak", "Calculated_B", "PercOfNom_B")
            Xlab <- OrigNames1[2]
            Ylab <- OrigNames1[3]
      }

      # If they included both IDcol AND colorBy, then these should be the names:
      if(ncol(stdCurve1$Data) == 6){
            names(stdCurve1$Data) <- c("IDcol", "colorBy", "Nominal", "NormPeak", "Calculated_A", "PercOfNom_A")
            names(stdCurve2$Data) <- c("IDcol", "colorBy", "Nominal", "NormPeak", "Calculated_B", "PercOfNom_B")
            Xlab <- OrigNames1[3]
            Ylab <- OrigNames1[4]

      }

      # If the names of the samples differ between stdCurve1$Data and
      # stdCurve2$Data but any of them have the same nominal mass, combine those
      # so that they're on the same row. That requires renaming the IDcol.

      OutData <- dplyr::full_join(
            stdCurve1$Data %>%
                  rename(IDcol_A = IDcol) %>%
                  dplyr::select(any_of(c("IDcol_A", "colorBy", "Nominal",
                                         "Calculated_A", "Calculated_B",
                                         "PercOfNom_A", "PercOfNom_B"))),
            stdCurve2$Data %>%
                  rename(IDcol_B = IDcol) %>%
                  dplyr::select(any_of(c("IDcol_B", "colorBy", "Nominal",
                                         "Calculated_A", "Calculated_B",
                                         "PercOfNom_A", "PercOfNom_B"))) %>%
                  dplyr::ungroup()) %>%
            dplyr::select(any_of(c("IDcol_A", "IDcol_B", "IDcol", "colorBy",
                                   "Nominal",
                                   "Calculated_A", "Calculated_B",
                                   "PercOfNom_A", "PercOfNom_B"))) %>%
            dplyr::arrange(Nominal)

      if(all(OutData$IDcol_A == OutData$IDcol_B, na.rm = T)){
            OutData$IDcol <- OutData$IDcol_A
            OutData$IDcol_A <- NULL
            OutData$IDcol_B <- NULL
      }

      OutData <- OutData %>%
            dplyr::select(any_of(c("IDcol_A", "IDcol_B", "IDcol", "colorBy",
                                   "Nominal",
                                   "Calculated_A", "Calculated_B",
                                   "PercOfNom_A", "PercOfNom_B")))

      # Making graph
      PlotData <- stdCurve1$Data %>% dplyr::mutate(Fit = "Fit_A") %>%
            dplyr::select(Nominal, NormPeak, Fit) %>%
            dplyr::bind_rows(stdCurve2$Data %>% dplyr::mutate(Fit = "Fit_B") %>%
                                   dplyr::select(Nominal, NormPeak, Fit))

      CurveData <- stdCurve1$CurveDF %>% dplyr::mutate(Fit = "Fit_A") %>%
            dplyr::bind_rows(stdCurve2$CurveDF %>% dplyr::mutate(Fit = "Fit_B"))
      names(CurveData) <- c("Nominal", "NormPeak", "Fit")

      # Removing any replicates and arranging out data.
      if("IDcol" %in% names(OutData)){
            if(anyDuplicated(OutData$IDcol)){
                  OutData <- OutData %>%
                        dplyr::group_by(IDcol, Nominal) %>%
                        dplyr::summarize(Calculated_A = mean(Calculated_A, na.rm = T),
                                         Calculated_B = mean(Calculated_B, na.rm = T),
                                         PercOfNom_A = mean(PercOfNom_A, na.rm = T),
                                         PercOfNom_B = mean(PercOfNom_B, na.rm = T)) %>%
                        dplyr::ungroup()
            }
      }

      if("IDcol_A" %in% names(OutData)){
            if(anyDuplicated(OutData$IDcol_A) | anyDuplicated(OutData$IDcol_B)){
                  OutData <- OutData %>%
                        dplyr::group_by(IDcol_A, IDcol_B, Nominal) %>%
                        dplyr::summarize(Calculated_A = mean(Calculated_A, na.rm = T),
                                         Calculated_B = mean(Calculated_B, na.rm = T),
                                         PercOfNom_A = mean(PercOfNom_A, na.rm = T),
                                         PercOfNom_B = mean(PercOfNom_B, na.rm = T)) %>%
                        dplyr::ungroup()
            }
      }

      OutData <- OutData %>% dplyr::arrange(Nominal)

      if(all(complete.cases(fitNames)) & length(fitNames == 2)){
            PlotData <- PlotData %>%
                  dplyr::mutate(Fit = dplyr::recode(Fit, "Fit_A" = fitNames[1],
                                             "Fit_B" = fitNames[2]))
            CurveData <- CurveData %>%
                  dplyr::mutate(Fit = dplyr::recode(Fit, "Fit_A" = fitNames[1],
                                             "Fit_B" = fitNames[2]))

            names(OutData) <- sub("_A", paste0("_", fitNames[1]),
                                  names(OutData))
            names(OutData) <- sub("_B", paste0("_", fitNames[2]),
                                  names(OutData))
      }

      CurvePlot <- ggplot2::ggplot(PlotData, ggplot2::aes(x = Nominal, y = NormPeak,
                                                          color = Fit, shape = Fit)) +
            ggplot2::geom_point(size = 2) +
            ggplot2::geom_line(data = CurveData, ggplot2::aes(x = Nominal, y = NormPeak,
                                                              color = Fit)) +
            # ggplot2::scale_color_manual(values = c("dodgerblue3", "#3E8853")) +
            ggplot2::xlab(Xlab) + ggplot2::ylab(Ylab)

      names(OutData) <- sub("IDcol", OrigNames1[1], names(OutData))

      Out <- list(CurvePlot, OutData)
      names(Out) <- c("CurvePlot", "Data")

      return(Out)
}

