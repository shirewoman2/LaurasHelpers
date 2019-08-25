#' King County contributions to candidates and political committees in 2018
#'
#' A dataset containing the amount of money donated to candidates for political
#' office in King County, WA in 2018. For a complete list of all variables,
#' please see the data source.
#'
#' @format A data.frame with 39192 rows and 37 variables:
#' \describe{
#'   \item{filer_name}{Name of the candidate}
#'   \item{office}{The office they're running for}
#'   \item{legislative_district}{The legislative district of that office}
#'   \item{party}{The political party with which they're affiliated}
#'   \item{amount}{The dollar amount of the contribution}
#'   \item{contributor_name}{The name of the person who made the donation}
#'   ...
#' }
#' @source \url{http://www.pdc.wa.gov}
"Candidates"

#' Example data for converting data.frames from long to wide format
#'
#' Example mass spec data for a run of the cotinine assay
#'
#' @format A data.frame with 44 rows and 9 variables:
#' \describe{
#'   \item{SampleID}{Sample ID}
#'   \item{File}{MS data file}
#'   \item{DateTime}{Date and time of the acquisition}
#'   \item{Method}{MS method file used}
#'   \item{Position}{Well position}
#'   \item{Analyte}{Analyte measured}
#'   \item{RT}{Retention time (min)}
#'   \item{Area}{Peak area (counts)}
#'   \item{Height}{Peak height (counts)}
#' }
"Cotinine"


#' Example data for data manipulation, graphing
#'
#' Example mass spec data for a run of the metformin assay
#'
#' @format A data.frame with 37 rows and 6 variables:
#' \describe{
#'   \item{SampleID}{Sample ID}
#'   \item{Matrix}{biological matrix sample was in}
#'   \item{Metformin.in.working.solution..ng.uL.}{Concentration of metformin in working solution (ng/uL)}
#'   \item{Volume.working.solution.added..uL.}{Volume of working solution added to sample (uL)}
#'   \item{metformin.peak.area}{Metformin peak area (counts)}
#'   \item{d6.metformin.peak.area}{d6-metformin peak area (counts)}
#' }
"Metformin"



#' Example data for data manipulation, graphing
#'
#' Example oxycodone concentration-time data
#'
#' @format A data.frame with 12 rows and 3 variables:
#' \describe{
#'   \item{Time}{Time sample drawn post-dose (min)}
#'   \item{Conc1}{Concentration for 1st subject (ng/mL)}
#'   \item{Conc2}{Concentration for 2nd subject (ng/mL)}
#' }
"Oxycodone"


#' Example data for joining data.frames
#'
#' Students' name and the number of pets they have for joining with 'Students'
#' data.frame.
#'
#' @format A data.frame with 29 rows and 2 variables:
#' \describe{
#'   \item{Name}{Student's name}
#'   \item{Number.of.pets}{Number of pets that student has}
#' }
"Pets"


#' Example data for using stdCurve function
#'
#' Example mass spec data for a run of the metformin assay
#'
#' @format A data.frame with 37 rows and 6 variables:
#' \describe{
#'   \item{SampleID}{Sample ID}
#'   \item{MET.nominalmass}{Nominal mass of metformin in the sample (ng)}
#'   \item{MET.area}{Metformin peak area (counts)}
#'   \item{d6MET.area}{d6-metformin peak area (counts)}
#'   \item{MET.peakarearatios}{Peak area ratio of MET/d6-MET}
#'   \item{Weight.1overx}{Weights to use for a 1-over-x weighting scheme (1/MET nominal mass)}
#' }
"ExStdCurve"




#' Example data for data manipulation, data checking, etc.
#'
#' Students in the UW Medicinal Chemistry or Pharmaceutics Departments and
#' some made-up data on their imagined habits.
#'
#' @format A data.frame with 37 rows and 6 variables:
#' \describe{
#'   \item{Name}{First name}
#'   \item{Department}{Department}
#'   \item{Gender}{Gender}
#'   \item{Cookie}{Favorite type of cookie}
#'   \item{VampTV.hr}{Amount of time spent watching trashy vampire TV last week (hr)}
#'   \item{Sleep.hr}{Amount of sleep last night (hr)}
#' }
"Students"



#' Salaries of University of Washington employees from 2013-2017 (public record)
#'
#' The pre-tax salaries of all UW employees from 2013 to 2017 in US dollars. (NB:
#' Not all employees worked a full year or worked a full year in a given position.)
#' Why am I including this in this package? Because it's a great data set and also because 1. you have a right to
#' know as a taxpayer and 2. if you're a UW employee (and most of the people who will use this package are), you should know
#' that this information is part of the public record. Same thing for your
#' UW email, fyi. If you're interested in a titillating tale, ask me about a
#' *former* Spokane County Public Works Department supervisor who had an affair on company
#' time and wrote emails to his paramour, a supervisee, using his Spokane County
#' email. And then had his emails requested as part of a public record search by
#' the local newspaper.
#'
#' @format A data.frame with ~124,000 rows and 7 variables:
#' \describe{
#'   \item{Name}{Employee name}
#'   \item{JobTitle}{Job title}
#'   \item{Sal2013}{Salary in 2013}
#'   \item{Sal2014}{Salary in 2014}
#'   \item{Sal2015}{Salary in 2015}
#'   \item{Sal2016}{Salary in 2016}
#'   \item{Sal2017}{Salary in 2017}
#' }
"UWsalaries"






