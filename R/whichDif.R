#' Determine which columns differ in a two-row data.frame
#'
#' This function takes a two-row data.frame and determines which cells differ.
#' For example, say you've got a data.frame of all the subjects in your study
#' and one subject is listed twice, even after you type \code{unique(MyDF)}.
#' There are dozens of columns in \code{MyDF}, so finding the column{s} that
#' differs would be tedious. This function will do that for you.
#'
#' @param x A two-row data.frame
#' @param ignore.na TRUE or FALSE for whether to ignore NA values. Default is
#'   TRUE. If \code{ignore.na = TRUE}, then one column with a missing value and
#'   another column with a complete case will be counted as a match and the
#'   missing value will be filled in with the complete one. If \code{ignore.na =
#'   FALSE}, then one missing value and one complete case always counts as a
#'   mismatch.
#'
#' @return a character vector of the column names where value differ
#' @export
#'
#' @examples
#' data(Pets)
#' whichDif(Pets[Pets$Name == "Eddie", ])
#'
#'
whichDif <- function(x, ignore.na = TRUE) {
      Dif <- rep(NA, ncol(x))

      for(j in 1:ncol(x)){
            if(ignore.na){
                  Dif[j] <- (x[1, j] == x[2, j] |
                                   all(is.na(x[, j])))
            } else {
                  Dif[j] <- (x[1, j] == x[2, j] &
                                   complete.cases(x[1, j]) & complete.cases(x[2, j]) |
                                   all(is.na(x[, j])))
            }

      }
      names(Dif) <- names(x)
      Dif <- Dif[complete.cases(Dif)]
      Dif <- Dif[Dif == FALSE]
      return(names(Dif))
}



