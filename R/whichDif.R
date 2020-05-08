#' Determine which columns differ in a two-row data.frame
#'
#' This function takes a two-row data.frame and determines which cells differ.
#' For example, say you've got a data.frame of all the subjects in your study
#' and one subject is listed twice, even after you type \code{unique(MyDF)}.
#' There are dozens of columns in \code{MyDF}, so finding the column{s} that
#' differs would be tedious. This function will do that for you.
#'
#' @param x
#' @param ignore.na
#'
#' @return
#' @export
#'
#' @examples
#'
#' whichDif(MyDF[MyDF$SubjectID == "Subject623", ])
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



# function for checking on which column differs in values

# Input is a 2-row data.frame.
# If ignore.na = TRUE, then one column with a missing value and another
# column with a complete case will be counted as a match. If ignore.na = FALSE,
# then one missing value and one complete case always counts as a mismatch.
