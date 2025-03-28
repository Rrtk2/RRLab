#' RRnorm
#'
#' This function normalizes dataset and offests/scales if needed.
#'
#' @param X Object that shoulb be normalized.
#' @param Scalar Object that shoulb be scaled by.
#' @param Offset Object that shoulb be Offseted by.
#' 
#' @return Normalized object
#' @examples
#' Object=seq(from = -1,to = 1,by = 0.1)^2
#' NormObject = RRnorm(Object, Scalar = 2, Offset = -1)
#' plot(NormObject, col = "red")
#' points(Object, col="blue")
#' @export

RRnorm = function(X, Scalar=1, Offset=0){
	normObj = (((X-min(X))/(max(X)-min(X)))*Scalar)+Offset
	return(normObj)
}