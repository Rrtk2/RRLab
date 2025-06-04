#' RRFilt
#'
#' This function indicates how big the difference is in a numeric vector.
#' It returns `abs(mean(x) - median(x)) / sd(x)`, which is close to zero for
#' symmetric distributions and larger when the mean and median diverge.
#'
#' @param x Numeric vector
#'
#' @return An indication of 'difference' in the given vector. Higher is better diff.
#' @examples
#' apply(iris[,-5],2,RRFilt)
#' @export

RRFilt = function(x) {
  abs(mean(x) - median(x)) / sd(x)
}
