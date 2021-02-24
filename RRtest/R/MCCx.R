#' MCCx
#'
#' This function required a dataset with column 1 as observations and column 2 with predictions. Multiclass!
#'
#' @param data Input the dataset object (column 1 = OBS; column 2 = PRED).
#' @param lev NULL
#' @param model NULL
#' @param showCM Show output of confusion matrix & metrics as cat.
#'
#' @return Several metrics to indicate machine learning performance ("MCC","Accuracy","Recall","Precision","F1","CombinedScore")
#' @examples
#' MCCx(data.frame(obs=c(1,1,1,2,2,2,3,3,3),pred=c(1,1,1,1,2,2,3,3,3)))
#' @export

MCCx = function(data, lev=NULL, model=NULL, showCM = FALSE){
	# Get the actual classes
	obs = data$obs
	pred = data$pred
	
	Levels = unique(data[,"obs"])
	
	TrueRes = data.frame(True=matrix(NA,nrow=length(Levels),ncol=1))
	FalseRes = data.frame(False=matrix(NA,nrow=length(Levels),ncol=1))
	
	for(i in 1:length(Levels)){
		Class = Levels[i]
		TrueRes[i,1] = sum(obs==Class&pred==Class)
		FalseRes[i,1] = sum(!obs==Class & pred==Class)
	}
	
	TrueProd = prod(TrueRes)
	FalseProd = prod(FalseRes)
	
	# difference in products
	numerator = TrueProd - FalseProd
	
	# now get the denominator
	# This is the product of all the possible combinations of (Truex + Falsey) (where x and y are all combinations of levels)
	denominator = 1
	for( i in 1:length(Levels)){
		for( o in 1:length(Levels)){
		a=TrueRes[i,1]+FalseRes[o,1]
		#print(a)
		denominator = denominator* a
		}
	}
	
	# then take the 'amount of levels'-th root
	denominator = denominator^(1/length(Levels))
	MCCval <- numerator/denominator
	
	# if not to be computed, set to -1 (maximum value from ideal value)
	if(is.nan(MCCval)){
		MCCval = -1
	}
	# MCC will become way harder to get 'right' as the amount of classes increase, which feels natural. Instead of saying pick one of 2 you're saying pick one which is not this or that. 
	#MCCval
	
	#
	#	# Calculate MCC
	#	numerator <- (TP * TN - FP * FN)
	#	denominator <- sqrt((TP + FP)*(TP + FN)*(TN + FP)*(TN + FN))
	#	if(denominator == 0) denominator <- 1
	#	MCCval <- numerator/denominator
	#	
	#	#names(out) <- c("MCC")
	#	AccuracyVal		=	((TP+TN)/((TP+TN)+(FP+FN)))
	#	RecallVal		=	(TP)/(TP+ FP)
	#	PrecisionVal	=	(TP) / (TP + FP)
	#	F1val			=	2* ( (PrecisionVal*RecallVal) / #(PrecisionVal+RecallVal) )
	#	CombinedScVal 	= 	(MCCval + F1val)/2
	#	
	#
	#	
	#	out = c(MCCval,AccuracyVal,RecallVal, PrecisionVal, F1val, #CombinedScVal)
	#}else{
	#	out = c(-1,0,0, 0, 0, -1)
	#}	
	#names(out) = #c("MCC","Accuracy","Recall","Precision","F1","CombinedScore")
	#if(showCM){print(out)}
	
	out = c(MCCval)
	names(out) <- c("MCC")
	
	return(out)
}