#' Calculate the geometric 95% confidence interval
#'
#' \code{gm_95CI} takes as input a numeric vector and returns the geometric 95%
#' confidence interval
#'
#' Because this calculation log-transforms the input data, 1. values of zero
#' must be either omitted (zero.propagate = FALSE) or included (zero.propagate =
#' TRUE) with the result being NA, and 2. the function is undefined for negative
#' numbers.
#'
#' @param x A vector of numbers
#' @param na.rm Should NA values be removed? (logical)
#' @param zero.propagate Should zeroes be propagated? (logical)
#'
#' @examples
#' gm_95CI(rnorm(10, 5, 1))
#'
#' @export

gm_95CI <- function(x, na.rm=TRUE, zero.propagate = FALSE){

      # If any values are negative, return NaN.
      if(any(x < 0, na.rm = TRUE)){
            return(NaN)
      }

      if(zero.propagate){
            if(any(x == 0, na.rm = TRUE)){
                  return(NA)
            }
      } else { # If you don't want to propagate zeroes, then remove them from the vector.
            x <- x[x > 0]
      }

      ErrTerm <- 1.96 * sd(log(x))/sqrt(length(x))
      Xbar <- mean(log(x))

      Result <- list(exp(Xbar) - exp(ErrTerm),
                     exp(Xbar) + exp(ErrTerm))
      names(Result) <- c("lower", "upper")

      return(Result)

}

