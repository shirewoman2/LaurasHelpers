#' Calculate the geometric mean
#' 
#' \code{gm_mean} takes as input a numeric vector and returns the geometric mean
#' 
#' When there are no zeroes in a vector of numbers, then the geometric mean is 
#' defined as the nth root of the product of all the values \emph{and is 
#' equivalently defined as} exp(1/n * the sum of the logs of each value). 
#' However, those two are not equivalent if there are zeroes!
#' 
#' This function gives the option of propagating zeroes. If you want to get a 0 
#' when the vector includes 0 values, that's the definition of the nth root of 
#' the product of all values, so that would be zero.propagate = TRUE.
#' 
#' Alternatively, if you want to just ignore 0 values, you can choose 
#' zero.propagate = FALSE. Then, either definition works, and, since R has a 
#' built-in sum(...) function but not a built-in product(...) function, you use 
#' the definition where the geometric mean = exp(1/n * the sum of the logs of 
#' each value) and that will give you an actual number if you ignore the zeroes.
#' 
#' Note that the geometric mean is undefined for negative numbers.
#' 
#' Geometric mean: $$ \text{geometric mean} = \mu_g =
#' \Bigg(\displaystyle\prod_{i=1}^{n} x_i\Bigg)^\frac{1}{n}
= \sqrt[n]{x_1 \times x_2  \times x_3  \times \dots  \times x_n}$$
#'
#' which, when there are no zero values, is equivalent to $$\text{geometric mean} =
            \text{exp}\bigg(\frac{1}{n} \sum_{i=1}^{n} ln(x_i)\bigg) = e^{\frac{1}{n} log(x_1 + x_2
                                                                                          + x_3  + \dots  + x_n)} = \text{exp}(\text{mean}(ln(x)))$$
#'
#' @param x A vector of numbers
#' @param na.rm Should NA values be removed? (logical)
#' @param zero.propagate Should zeroes be propagated? (logical)
#'
#' @examples
#' gm_mean(rnorm(10, 5, 1))
#'
#' @export

gm_mean <- function(x, na.rm=TRUE, zero.propagate = FALSE){

      # If any values are negative, return NaN.
      if(any(x < 0, na.rm = TRUE)){
            return(NaN)
      }

      if(zero.propagate){
            if(any(x == 0, na.rm = TRUE)){
                  return(0)
            }
      } else { # If you don't want to propagate zeroes, then remove them from the vector.
            x <- x[x > 0]
      }

      return(exp(mean(log(x), na.rm = na.rm)))

}


#' Calculate the geometric standard deviation
#' 
#' \code{gm_sd} takes as input a numeric vector and returns the geometric
#' standard deviation
#' 
#' Per Wikipedia's entry on the geometric standard deviation,
#' \href{https://en.wikipedia.org/wiki/Geometric_standard_deviation}{https://en.wikipedia.org/wiki/Geometric_standard_deviation},
#' the geometric standard deviation is dimensionless. See that entry for the
#' formula used.
#' 
#' Also, note that the way you determine the range of the data for the geometric
#' mean +/- one geometric standard deviation is NOT geometric mean value +/- the
#' geometric standard deviation! Instead, it is geometric mean / geometric
#' standard deviation to geometric mean * geometric standard deviation!
#' 
#' Geometric standard deviation: $$\text{geometric standard deviation} =
\sigma_g = exp \sqrt{ \frac{ \sum_{i=1}^{n} \big(log \frac{x_i}{\mu_g}\big)^2 }{n-1} } $$
#' 
#' @param x A vector of numbers
#' @param na.rm Should NA values be removed? (logical)
#' @param zero.propagate Should zeroes be propagated? (logical)
#'   
#' @examples
#' gm_sd(rnorm(10, 5, 1))
#' @export

gm_sd <- function(x, na.rm = TRUE, zero.propagate = FALSE) {

      if(na.rm){
            x <- x[complete.cases(x)]
      }

      # If any values are negative, return NaN.
      if(any(x < 0, na.rm = TRUE)){
            return(NaN)
      }

      if(zero.propagate){
            if(any(x == 0, na.rm = TRUE)){
                  return(0)
            }
      } else { # If you don't want to propagate zeroes, then remove them from the vector.
            x <- x[x > 0]
      }

      # Now, proceed with whatever your vector is after removing zeroes (or not
      # removing them if zero.propagate was TRUE but there weren't any zeroes
      # to start with anyway.)
      ToSum <- rep(NA, length(x))
      # There's probably a way to do this next bit without actually writing
      # a loop, but I'm not sure how.
      for(i in 1:length(x)){
            ToSum[i] <- (log(x[i]/gm_mean(x)))^2
      }

      return(exp(sqrt(sum(ToSum)/(length(x))))) # This is population geometric
      # standard deviation. For *sample* geometric sd, you'd want the denominator
      # to be N-1 instead of N.
}



#' Calculate the geometric 95% confidence interval
#'
#' See \href{https://stats.stackexchange.com/questions/285173/how-to-calculate-confidence-interval-for-a-geometric-mean}{https://stats.stackexchange.com/questions/285173/how-to-calculate-confidence-interval-for-a-geometric-mean} 
#'
#' @param x A vector of numbers
#' @param na.rm Should NA values be removed? (logical)
#' @param zero.propagate Should zeroes be propagated? (logical)
#'
#' @export
#'
#' @examples gm_95CI(rnorm(100, 100, 20))
#' 
#' 
gm_95CI <- function(x, na.rm = TRUE, zero.propagate = FALSE) {
      
      # If any values are negative, return NaN.
      if(any(x < 0, na.rm = TRUE)){
            return(NaN)
      }
      
      # Ref: https://stats.stackexchange.com/questions/285173/how-to-calculate-confidence-interval-for-a-geometric-mean
      
      zterm <- qt(0.975, length(x) - 1) *
            gm_mean(x, na.rm = na.rm, zero.propagate = zero.propagate) * 
            gm_sd(x, na.rm = na.rm, zero.propagate = zero.propagate)/sqrt(length(x))
      
      CI5 <- gm_mean(x, na.rm = na.rm, zero.propagate = zero.propagate) - zterm
      CI95 <- gm_mean(x, na.rm = na.rm, zero.propagate = zero.propagate) + zterm
      
      return(c(CI5, CI95))
      
}
