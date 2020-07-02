#' Split a string of text by a specified pattern and return a vector of the
#' split text
#'
#' This function works similarly to \code{stringr::str_split} except that it
#' returns a vector rather than a list of vectors. Also, you can have it return
#' only one item in that vector instead of the entire vector, which works better
#' when you're trying to extract a specified piece of text and fill it into a
#' column of a data.frame or tibble.
#'
#' @param string The string of text to be split into pieces
#' @param pattern The pattern to use for splitting the text. The pattern will
#'   not be retained in the output unless \code{retainPattern = TRUE}.
#' @param n The index of the output vector to be returned. If left as \code{NA},
#'   this will return all items in the vector. If set to a specific number n, it
#'   will return only the nth item of that vector. If set to "last", it will
#'   return only the last item of the vector.
#' @param retainPattern TRUE or FALSE for whether to retain the pattern that was
#'   used to separate the input text in the output vector. Warning: This was set
#'   up for a situation in which the user is only looking to match a single,
#'   exact pattern and \emph{not} a pattern with regular expressions in it. The
#'   function will still work if you use regex, but the output won't be what
#'   you're likely expecting.
#'
#' @return Returns a vector
#' @export
#'
#' @examples
#'
#' # Applied to a vector of length 1:
#' MyString <- c("First filename.xlsx Second filename.xlsx")
#' str_split_alt(MyString, ".xlsx")
#' str_split_alt(MyString, ".xlsx", 1)
#' str_split_alt(MyString, ".xlsx", "last")
#' str_split_alt(MyString, ".xlsx", retainPattern = TRUE)
#'
#' # Applied to a longer vector, e.g., a column in a data.frame:
#' MyDF <- data.frame(ColA = paste("Item", 1:4),
#'                    ColB = c("This is a string to be split.",
#'                             "This one, too",
#'                             "Also split this",
#'                             "Last, split this one."))
#'
#' MyDF$ColC <- str_split_alt(MyDF$ColB, " ", "last")
#' MyDF$ColD <- str_split_alt(MyDF$ColB, " ", 1)
#' MyDF$ColE <- str_split_alt(MyDF$ColB, " ", 1, retainPattern = TRUE)
#'
#'
str_split_alt <- function(string,
                          pattern,
                          n = NA,
                          retainPattern = FALSE){
      library(stringr)

      # If the supplied input is a vector of length 1, then this is pretty
      # straightforward.
      if(length(string) == 1){
            S <- str_split(string, pattern)[[1]]
            S <- S[-which(S == "")]
            S <- str_trim(S)

            if(complete.cases(n)){
                  if(n == "last"){
                        S <- S[length(S)]
                  } else {
                        S <- S[n]
                  }
            }

            if(retainPattern == TRUE){
                  S <- paste0(S, pattern)
            }

      } else {
            # Otherwise, for vectors of length > 1, e.g., columns in a
            # data.frame, then apply this sequentially.

            mySplit <- function(x){
                  str_trim(str_split(x, pattern)[[1]])
            }

            S <- sapply(string, mySplit)

            if(complete.cases(n)){
                  if(n == "last"){
                        IndexToRetain <- sapply(S, length)
                  } else {
                        IndexToRetain <- rep(n, length(S))
                  }

                  for(i in 1:length(S)){
                        S[[i]] <- S[[i]][IndexToRetain[i]]
                  }

                  S <- as.character(S)

            }

            if(retainPattern == TRUE){
                  myPaste <- function(x) paste0(x, pattern)
                  S <- sapply(S, myPaste)
            }
      }

      return(S)
}


MyDF <- data.frame(ColA = paste("Item", 1:4),
                   ColB = c("This is a string to be split.",
                            "This one, too",
                            "Also split this",
                            "Last, split this one."))

MyDF$ColC <- str_split_alt(MyDF$ColB, " ", "last")
MyDF$ColD <- str_split_alt(MyDF$ColB, " ", 1)
MyDF$ColE <- str_split_alt(MyDF$ColB, " ", 1, retainPattern = TRUE)
