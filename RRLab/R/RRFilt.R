#' RRFilt
#'
#' This function gives an indication how big the difference is in selected variable. When applying normally distributed features with no effect results in abs(x-mean(x))/sd(x), resulting near to 0. When applying normally distributed features with high effect the mean != median, thus abs(x-mean(x))/sd(x). This should result in higher than 0 number, higher the better difference in 
#'
#' @param x Numeric vector
#'
#' @return An indication of 'difference' in the given vector. Higher is better diff.
#' @examples
#' apply(iris[,-5],2,RRFilt)
#' @export

RRFilt = function(x){

function(x){abs(sum((x-median(x)))/sd(x))}
 
 }