#' Rcor
#'
#' This function requires a dataset with variables able to be transformed to numeric (not character).
#'
#' @param dataset Input the dataset object (data.frame).
#' @param th threshold the correlations given a cutoff; will be shrunk to 0.
#'
#' @return A correlation matrix; containing all columns able to be transformed to numeric.
#' @examples
#' Rcor(mtcars)
#' @export

Rcor = function(df = NA, th = 0.3){
	df2 = apply(df,2,as.numeric)
	df3 = df2[,!colSums(is.na(df2))>0]
	
	removecols = NULL
	for(i in 1: dim(df3)[2]){
		for(o in 1: dim(df3)[2]){
			if(i>=o){next}
			
			if(identical(df3[,o],df3[,i])){
			removecols = c(removecols,o)
			}
		}
	}
	
	df3 = df3[,-removecols]
	df3 = round(cor(df3),2)
	df3[abs(df3 )<th] = 0
	diag(df3)=0
	# remove things not really relevant
	df4 = df3[!(colSums(df3) == 0 & rowSums(df3) == 0),!(colSums(df3) == 0 & rowSums(df3) == 0)]
	
	
	return(df4)
}
