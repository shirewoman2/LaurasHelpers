#' Select colors evenly spaced around the color wheel
#'
#' \code{gg_color_hue} takes as input the number of colors desired and outputs
#' colors such as the defaults in ggplot2 graphs. NB: I did not write this
#' function, and I don't recall where I got it.
#'
#' @param n The number of colors desired
#' @export

gg_color_hue <- function(n) {
      hues = seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
}
