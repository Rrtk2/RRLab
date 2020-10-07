#' Znorm
#'
#' This function performs Z-score normalisation on input. Dataframe input will be processed per row.
#'
#' @param X Object that should be normalized.
#' @param NormalizePerRow Normalize per row if TRUE, per column if FALSE
#' 
#' @return Normalized object
#' @examples
#' Znorm(iris[,-5])
#' @export

Znorm = function(X,NormalizePerRow=TRUE){

	if(class(X)=="integer"){
		Z = X - mean(X)
		Y = Z / sd(X)
		return(Y)
	}
	if(class(X)=="data.frame"){
			
		if(NormalizePerRow){
			for(i in 1:(dim(X)[1])){
				X2 = as.numeric(X[i,])
				Z = X2 - mean(X2)
				temp_Zvalue = Z / sd(X2)

				X[i,] = temp_Zvalue
				
			}
		}else{
			for(i in 1:(dim(X)[2])){
				X2 = as.numeric(X[,i])
				Z = X2 - mean(X2)
				temp_Zvalue = Z / sd(X2)

				X[,i] = temp_Zvalue
				
			}
		}
		return(X)
	}
}