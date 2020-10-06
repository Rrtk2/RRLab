#' RRmatch
#'
#' This function will get the matched info.
#'
#' @param FindVector Vector to be found.
#' @param IntableVector Vector that should contain the things that need to be found
#' @param Resultframe Vector/dataframe of info that will be returned
#' @return A vector/dataframe of matched items (matches Resultframe).
#' @examples
#' RRmatch(FindVector = c(1,100,150), IntableVector = rownames(iris), Resultframe = iris)
#' @export

RRmatch = function(FindVector, IntableVector, Resultframe) {

	Resultframe = as.data.frame(Resultframe)
	Resultframe[match(FindVector,IntableVector),]	
	
}