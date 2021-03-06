% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/countWords.R
\name{countWords}
\alias{countWords}
\title{Count the number of words in each item in a vector or list}
\usage{
countWords(x)
}
\arguments{
\item{x}{A vector or list. This can be any type of data.}
}
\value{
Returns a vector of numbers that is the number of words present
}
\description{
This function determines how many words are in a string; it assumes that
words are separated by either a space or a hyphen.
}
\examples{

countWords("My cats' names are Lightning and Theo.")
countWords(list("My cats' names are Lightning and Theo.",
              "mein hindee seekh rahi hoon", 4.5, TRUE,
              "Pharmacogenetic variation in drug-metabolizing enzymes"))

}
