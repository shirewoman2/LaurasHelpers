#' Split a string of text by a specified pattern and return a vector of the
#' split text. Optionally, return only a specified index of that split.
#'
#' This function works similarly to \code{stringr::str_split} except that it
#' returns a vector rather than a list of vectors. Also, you can have it return
#' only one piece of that vector instead of the entire vector, which works
#' better when you're trying to extract a specified piece of text and fill it
#' into a column of a data.frame or tibble.
#'
#' @param string The string of text to be split into pieces
#' @param pattern The pattern to use for splitting the text. The pattern will
#'   not be retained in the output unless /code{retainPattern = TRUE}.
#' @param n The index of the output vector to be returned. If left at
#'   /code{Inf}, this will return all pieces of the vector. If set to a specific
#'   number n, it will return only the nth piece of that vector.
#' @param retainPattern TRUE or FALSE for whether to retain the pattern that was
#'   used to separate the input text in the output vector. Warning: This was set
#'   up for a situation in which the user is only looking to match a single,
#'   exact pattern, so not a pattern with regular expressions in it. The
#'   function will still work, but the output won't be what you're likely
#'   expecting.
#'
#' @return Returns a vector
#' @export
#'
#' @examples
#'
#' MyString <- c("First filename.xlsx Second filename.xlsx")
#' str_split_alt(MyString, ".xlsx")
#' str_split_alt(MyString, ".xlsx", 1)
#' str_split_alt(MyString, ".xlsx", retainPattern = TRUE)
#'
#'
str_split_alt <- function(string,
                          pattern,
                          n = Inf,
                          retainPattern = FALSE){
      library(stringr)
      S <- str_split(string, pattern)[[1]]
      S <- S[-which(S == "")]
      S <- str_trim(S)

      if(is.finite(n)){
            S <- S[n]
      }

      if(retainPattern == TRUE){
            S <- paste0(S, pattern)
      }

      return(S)
}



