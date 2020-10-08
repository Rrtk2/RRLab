#' RRZnorm
#'
#' This function performs Z-score normalisation on input. Dataframe input will be processed per row.
#'
#' @param Dataset Object that should be normalized.
#' @param NormalizePerRow Normalize per row if TRUE, per column if FALSE
#' 
#' @return Normalized object
#' @examples
#' NormObject = Znorm(iris[,-5])
#' @export

RRZnorm = function(Dataset,NormalizePerRow=TRUE){

	if(class(Dataset)=="integer"){
		Z = Dataset - mean(Dataset)
		Dataset = Z / sd(Dataset)
	}
	if(class(Dataset)=="data.frame"){
			
		if(NormalizePerRow){
			for(i in 1:(dim(Dataset)[1])){
				Dataset2 = as.numeric(Dataset[i,])
				Z = Dataset2 - mean(Dataset2)
				temp_Zvalue = Z / sd(Dataset2)

				Dataset[i,] = temp_Zvalue
			}
		}else{
			for(i in 1:(dim(Dataset)[2])){
				Dataset2 = as.numeric(Dataset[,i])
				Z = Dataset2 - mean(Dataset2)
				temp_Zvalue = Z / sd(Dataset2)

				Dataset[,i] = temp_Zvalue
				
			}
		}
	}
	return(Dataset)
}