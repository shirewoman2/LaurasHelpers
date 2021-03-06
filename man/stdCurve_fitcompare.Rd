% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stdCurve_fitcompare.R
\name{stdCurve_fitcompare}
\alias{stdCurve_fitcompare}
\title{Compare two fitted standard curves}
\usage{
stdCurve_fitcompare(stdCurve1, stdCurve2, fitNames = NA)
}
\arguments{
\item{stdCurve1}{The list output from \code{\link{stdCurve}}}

\item{stdCurve2}{The list output from a variation on \code{\link{stdCurve}}
on the same data.frame or on a data.frame with the same columns}

\item{fitNames}{A preferably succinct character vector with two items
describing what the differences are between \code{stdCurve1} and
\code{stdCurve2} (optional)}
}
\value{
Output is a list of the following named objects:\describe{

  \item{CurvePlot}{A plot of the data and the fitted lines. The two fits will
  be in different colors.}

  \item{Data}{The input data, including the nominal concentration or mass,
  columns of the calculated concentration for each of the two fit methods,
  and columns of the calculated concentration or mass as a percent of nominal
  concentration or mass for each of the two fits.}}
}
\description{
To use this function, use \code{\link{stdCurve}} to fit the same data.frame
two different ways. For example, for one fit, include all data points and for
the other, omit troublesome data points to see their effect on the curve.
Other examples: Try comparing different weighting schemes or comparing 1st-
vs. 2nd-order polynomials. Input to this function is the output from
\code{\link{stdCurve}} from each of the two different ways.
}
\examples{
data(ExStdCurve)

# Comparing no weights vs. weighting by 1/x^2
stdCurve1 <- stdCurve(ExStdCurve,
                      normPeak = MET.peakarearatio,
                      nominal = MET.nominalmass,
                      poly = "2nd")

stdCurve2 <- stdCurve(ExStdCurve,
                      normPeak = MET.peakarearatio,
                      nominal = MET.nominalmass,
                      poly = "2nd", weights = "1/x^2")

stdCurve_fitcompare(stdCurve1, stdCurve2)
stdCurve_fitcompare(stdCurve1, stdCurve2, fitNames = c("no weighting", "1/x2 weighting"))


# Comparing all points vs. omitting one point.
stdCurve1 <- stdCurve(ExStdCurve,
                      normPeak = MET.peakarearatio,
                      nominal = MET.nominalmass,
                      poly = "2nd", IDcol = SampleID)

stdCurve2 <- stdCurve(ExStdCurve \%>\%  dplyr::filter(SampleID != "standard 200"),
                      normPeak = MET.peakarearatio,
                      nominal = MET.nominalmass,
                      poly = "2nd", IDcol = SampleID)

stdCurve_fitcompare(stdCurve1, stdCurve2)
stdCurve_fitcompare(stdCurve1, stdCurve2,
                    fitNames = c("all points", "omit standard 200"))



}
