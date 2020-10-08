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

	if(!class(Dataset)=="data.frame"){
		Z = Dataset - mean(Dataset)
		out = Z / sd(Dataset)
		#return(out)
	}
	if(class(Dataset)=="data.frame"){
		out = Dataset
			
		if(NormalizePerRow){
			for(i in 1:(dim(Dataset)[1])){
				Dataset2 = as.numeric(Dataset[i,])
				Z = Dataset2 - mean(Dataset2)
				temp_Zvalue = Z / sd(Dataset2)

				out[i,] = temp_Zvalue
				#return(out)
			}
		}else{
			for(i in 1:(dim(Dataset)[2])){
				Dataset2 = as.numeric(Dataset[,i])
				Z = Dataset2 - mean(Dataset2)
				temp_Zvalue = Z / sd(Dataset2)

				out[,i] = temp_Zvalue
				#return(out)
			}
		}
	}
	out
}