% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stdCurve.R
\name{stdCurve}
\alias{stdCurve}
\title{Fit data to a standard curve}
\usage{
stdCurve(
  DF,
  rawPeak,
  rawIS,
  normPeak,
  nominal,
  poly = "1st",
  weights = NULL,
  omit = NA,
  IDcol = NA,
  colorBy,
  useNLS_outnames = TRUE
)
}
\arguments{
\item{DF}{The input data.frame with columns containing the nominal analyte
concentration and the instrument response}

\item{rawPeak}{The unadjusted instrument response column. Ignore this if data
are already normalized by internal standard.}

\item{rawIS}{The internal standard column name. This is ignored if
\code{normPeak} is provided.}

\item{normPeak}{The column containing instrument response normalized by
internal standard. Use this if the data are peak heights or areas already
divided by the IS peak height or area.}

\item{nominal}{The column with the nominal concentrations or masses.}

\item{poly}{Should the data be fit to a 1st or 2nd order polynomial? Options:
"1st" or "2nd".}

\item{weights}{Weighting scheme to use for the regression. User may supply a
numeric vector of weights to use or choose from "1/x", "1/x^2", "1/y" or
"1/y^2". If left as NULL, no weighting scheme will be used. Be careful that
you don't have any infinite values or this will fail!}

\item{omit}{An index of which, if any, samples to omit from the curve. These
samples will be depicted as red open circles in the graph but will not be
included in the regression. The red color for omitted points overrides any
other choices for \code{colorBy}.}

\item{IDcol}{Optional column with sample IDs}

\item{colorBy}{What column to color the points by in the standard curve
graph. If not set, all points will be black.}

\item{useNLS_outnames}{TRUE or FALSE for whether the object "Fit" should be
the standard, list output from \code{nls} or \code{nls2}. If set to FALSE,
the output will be a data.frame of the coefficients with column names
"Beta", "Estimate", "SE", "tvalue" and "pvalue".}
}
\value{
Output is a list of the following named objects:\describe{

  \item{Fit}{The fitted parameters}

  \item{CurvePlot}{A plot of the data and the fitted line}

  \item{CurveDF}{A data.frame of the points used for graphing the fitted
  line.}

  \item{Data}{The original data with a column of the calculated concentration
  or mass as a percent of the nominal.}}
}
\description{
\code{stdCurve}, which was designed with mass spectrometry data in mind, fits
concentration and signal data to a standard curve and returns the calculated
betas, a plot of the standard curve, the data that were used to generate the
fitted line, and the original data that were used to generate the standard
curve.
}
\examples{
data(ExStdCurve)

# Using a peak ratio that's already been calculated
stdCurve(ExStdCurve,
         normPeak = MET.peakarearatio,
         nominal = MET.nominalmass,
         poly = "2nd")

# Having 'stdCurve' calculate the peak ratio and making the fitted
# coefficients a data.frame rather than a list
stdCurve(ExStdCurve,
         rawPeak = MET.area,
         rawIS = d6MET.area,
         nominal = MET.nominalmass,
         poly = "2nd",
         IDcol = SampleID,
         useNLS_outnames = FALSE)

# Using weights in the nonlinear regression
stdCurve(ExStdCurve,
         normPeak = MET.peakarearatio,
         nominal = MET.nominalmass,
         poly = "1st",
         weights = "1/x")

# Omitting certain points from the regression but showing them on the graph
stdCurve(ExStdCurve,
         normPeak = MET.peakarearatio,
         nominal = MET.nominalmass,
         poly = "2nd",
         omit = which(ExStdCurve$MET.nominalmass > 10 &
                            ExStdCurve$MET.nominalmass < 20),
         useNLS_outnames = FALSE)

# Coloring by some variable
stdCurve(ExStdCurve \%>\% dplyr::mutate(Group = c(rep("A", 5), rep("B", 6))),
         normPeak = MET.peakarearatio,
         nominal = MET.nominalmass,
         colorBy = Group,
         poly = "2nd")

}
