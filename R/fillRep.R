#' Make a unique data.frame by combining NA values
#'
#' This function takes a data.frame, looks for rows that are replicates that
#' differ only because one row has missing values where the other has values,
#' fills in all missing values and returns the unique (no replicates) data.frame
#' whenever possible. When replicates cannot be removed because there are
#' differences, fillRep returns a data.frame of those differences. Whenever the
#' function encounters a set of rows that will *not* reduce, it prints the
#' problematic fields as a data.frame. Optionally, you can keep this data.frame
#' of problems by setting `returnDifs` to TRUE.
#'
#' @param DF a data.frame that you want to remove replicates from
#' @param dlplyBy a character string of the columns in DF that you want to use
#'   to break down DF; it's what you're supplying to "plyr::dlply" as the
#'   ".variables" parameter.
#' @param concatFields a string of columns that are likely to contain replicates
#'   that you want to concatenate. For example, say you've got two rows in a
#'   merged data.frame where one row has the value "File1" listed for the column
#'   "DataSource", and the other has the value "File2", but everything else is
#'   the same. If you list "DataSource" for "concatFields", then the result will
#'   be one row where the column "DataSource" contains the concatenated names of
#'   the original two files.
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



