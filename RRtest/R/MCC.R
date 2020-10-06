#' MCC
#'
#' This function required a dataset with column 1 as observations and column 2 with predictions. Two-class only! 
#'
#' @param data Input the dataset object (column 1 = OBS; column 2 = PRED).
#' @param lev NULL
#' @param model NULL
#' @param showCM Show output of confusion matrix & metrics as cat.
#'
#' @return Several metrics to indicate machine learning performance ("MCC","Accuracy","Recall","Precision","F1","CombinedScore")
#' @examples
#' MCC(data.frame(obs=c(1,1,1,2,2,2),pred=c(1,1,1,1,2,2)))
#' @export

MCC = function(data, lev=NULL, model=NULL, showCM = FALSE){
	df = data.frame(obs=as.character(data$obs),pred=as.character(data$pred))
	
	CM = table(df)
	
	#data$obs <- factor(data$obs, levels = lev)
	if(names(labels(CM)[1]) == "obs"){CM = t(CM)}
	
	if((dim(CM)[1] == 2 & dim(CM)[2] == 2)){
		
		if(showCM){
			cat("Confusion matrix:\n")
			print(CM)
			cat("\n")
		}
		
		
		
		
		TP = CM[1,1]
		FP = CM[1,2]
		TN = CM[2,2]
		FN = CM[2,1]
		
		# Convert types to double for better precision
		TP <- as.double(TP)
		FP <- as.double(FP)
		TN <- as.double(TN)
		FN <- as.double(FN)
		
		# Calculate MCC
		numerator <- (TP * TN - FP * FN)
		denominator <- sqrt((TP + FP)*(TP + FN)*(TN + FP)*(TN + FN))
		if(denominator == 0) denominator <- 1
		MCCval <- numerator/denominator
		
		#names(out) <- c("MCC")
		AccuracyVal		=	((TP+TN)/((TP+TN)+(FP+FN)))
		RecallVal		=	(TP)/(TP+ FP)
		PrecisionVal	=	(TP) / (TP + FP)
		F1val			=	2* ( (PrecisionVal*RecallVal) / (PrecisionVal+RecallVal) )
		CombinedScVal 	= 	(MCCval + F1val)/2
		
	
		
		out = c(MCCval,AccuracyVal,RecallVal, PrecisionVal,  F1val, CombinedScVal)
	}else{
		out = c(0,0,0, 0, 0, 0,0,0,0)
	}	
	names(out) = c("MCC","Accuracy","Recall","Precision","F1","CombinedScore")
	if(showCM){print(out)}
	return(out)
}