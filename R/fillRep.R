#' Make a unique data.frame by combining NA values
#'
#' This function takes a data.frame, looks for rows that are replicates that
#' differ only because one row has missing values where the other has values,
#' fills in all missing values and returns the unique (no replicates) data.frame
#' whenever possible. When replicates cannot be removed because there are
#' differences, fillRep returns a data.frame of those differences. Whenever the
#' function encounters a set of rows that will \emph{not} reduce, it prints the
#' problematic fields as a data.frame. Optionally, you can keep this data.frame
#' of problems by setting \code{returnDifs} to TRUE.
#'
#' @param DF a data.frame that you want to remove replicates from
#' @param dlplyBy a character vector of the columns in DF that you want to use
#'   to break down DF; it's what you're supplying to \code{plyr::dlply} as the
#'   \code{.variables} parameter.
#' @param concatFields a character vector of columns that are likely to contain
#'   replicates that you want to concatenate. For example, say you've got two
#'   rows in a merged data.frame where one row has the value "File1" listed for
#'   the column "DataSource", and the other has the value "File2", but
#'   everything else is the same. If you list "DataSource" for
#'   \code{concatFields}, then the result will condense those two original rows
#'   into one row where the column "DataSource" contains the concatenated names
#'   of the original two files separated by a space, e.g., "File1 File2".
#' @param returnDifs If set to TRUE, returns a list with the same name as the
#'   original data.frame and where the contents of the list are: \enumerate{
#'   \item a data.frame called "differences" that shows differences between all
#'   the subsets of data that \code{fillRep} was not able to reduce. It will
#'   have 0 rows if there are no problematic data. \item a data.frame called
#'   "new data.frame" that is the new, unique data.frame.} If set to FALSE, the
#'   default value, the differences will be printed but the output object will
#'   be only the new, unique data.frame.
#' @importFrom plyr dlply
#' @return Returns a unique data.frame or, if \code{returnDifs} is TRUE, a list
#'   of two data.frames.
#' @examples
#'
#' # Concatenating anything that's in the column "SourceFile" if that's the
#' # only thing that differs:
#' fillRep(MyDF, dlplyby = c("SubjectID", "StudyDay"),
#'         concatFields = "SourceFile", returnDifs = TRUE)
#'
#' # Not concatenating any fields; any non-unique values will result in
#' # multiple rows in the output data.frame:
#' fillRep(MyDF, dlplyby = "SubjectID")
#' @export
#'

fillRep <- function(DF, dlplyBy, concatFields = NULL,
                    returnDifs = FALSE){

      colNames <- names(DF)[!names(DF) %in% concatFields]
      # Having trouble with bind_rows converting Date or POSIX format data to
      # numeric. Will fix that lower in function.
      myclass <- function(x) {
            class(x)[[1]]
      }
      DateCol <- which(sapply(DF, myclass) %in% c("Date"))
      DateCol <- names(DF)[DateCol]
      TimeCol <- which(sapply(DF, myclass) %in% c("POSIXct"))
      TimeCol <- names(DF)[TimeCol]

      if(length(c(DateCol, TimeCol)) > 0){
            for(i in c(DateCol, TimeCol)){
                  DF[, i] <- as.character(DF[, i])
            }
      }

      DF <- plyr::dlply(DF, dlplyBy)
      Difs <- list()

      for(i in names(DF)){

            Difs[[i]] <- list()

            for(j in colNames){

                  NewValue <- sort(unique(DF[[i]][, j]))
                  if(length(NewValue) > 1 & returnDifs == TRUE){
                        Difs[[i]][[j]] <-
                              data.frame(Data.frame = i,
                                         DifCol = j)
                        rm(NewValue)
                        next
                  }

                  if(length(NewValue) > 1 & returnDifs == FALSE){
                        print(paste(i, j))
                        rm(NewValue)
                        next
                  }

                  DF[[i]][, j] <- ifelse(length(NewValue) == 1,
                                         NewValue, NA)
                  rm(NewValue)
            }

            if(length(Difs[[i]]) == 0){
                  Difs[[i]] <- NULL
            } else {
                  Difs[[i]] <- bind_rows(Difs[[i]])
            }

            for(j in concatFields){
                  DF[[i]][, j] <- str_c(sort(unique(DF[[i]][, j])),
                                        collapse = " ")
            }
            DF[[i]] <- unique(DF[[i]])
            if(nrow(DF[[i]]) > 1 & returnDifs == FALSE){
                  print(paste(i))
            }
      }

      if(length(Difs) > 0){
            Difs <- bind_rows(Difs)
      }

      DF <- unique(bind_rows(DF))

      if(length(DateCol) > 0){
            for(i in DateCol){
                  DF[, i] <- ymd(DF[, i])
            }
      }

      if(length(TimeCol) > 0){
            for(i in TimeCol){
                  DF[, i] <- ymd_hms(DF[, i])
            }
      }

      if(returnDifs == TRUE){
            Result <- list(DF, Difs)
            names(Result) <- c("new data.frame",
                               "differences")
            return(Result)
      } else {
            return(DF)
      }
}



