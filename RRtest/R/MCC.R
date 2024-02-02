#' MCC
#'
#' This function required a dataset with column 1 as observations and column 2 with predictions. Two-class only! 
#'
#' @param data Input the dataset object (column 1 = OBS; column 2 = PRED). Additonal 2 columns predictions for class 1 then class 2; see example
#' @param lev NULL
#' @param model NULL
#' @param showCM Show output of confusion matrix & metrics as cat.
#'
#' @return Several metrics to indicate machine learning performance ("MCC","Accuracy","Recall","Precision","F1","CombinedScore")
#' @examples
#' MCC(data.frame(obs=c(1,1,1,2,2,2),pred=c(1,1,1,1,2,2)))
#' MCC(data.frame(obs=c("a","a","a","b","b","b"),pred=c("a","a","a","a","b","b"),a=c(0.9,0.8,0.9,0.2,0.3,0.9),b=c(0.1, 0.2, 0.1, 0.8, 0.7, 0.1)),lev=c("a","b"))
#' # Example high accurate
#' MCC(data.frame(obs=c(rep(1,1000),rep(2,1000)),pred=c(rep(1,1000),rep(2,1000))))
#' # Example Inverse accurate
#' MCC(data.frame(obs=c(rep(2,1000),rep(1,1000)),pred=c(rep(1,1000),rep(2,1000))))
#' # Example random 
#' MCC(data.frame(obs=c(round(runif(200000,0,1))),pred=c(round(runif(200000,0,1)))))
#' # Example high accurate inbalance
#' MCC(data.frame(obs=c(rep(1,1000),rep(2,10)),pred=c(rep(1,1000),rep(2,10))))
#' # Example high accurate EXTREME inbalance
#' MCC(data.frame(obs=c(rep(1,1000),rep(2,10)),pred=c(rep(1,1010))))
#' # Example moderate accurate inbalance
#' MCC(data.frame(obs=c(rep(1,1000),round(runif(10,1,2))),pred=c(rep(1,1000),rep(2,10))))
#' @export

MCC = function(data, lev=NULL, model=NULL, showCM = FALSE,Verbose=FALSE){
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
		
		# Calculate the additional assisiting metrics
		P = TP + FN					#Actual positive
		N = FP + TN					#Actual Negative
		PP = TP + FP					#Predicted positive
		PN = FN + TN					#Predicted negative
		TPR = TP / P					#True Positive Rate #Sensitivity				
		FPR = FP / N					#True Negative Rate
		FNR = FN / P					#False Negative Rate
		TNR = TN / N					#True Negative Rate #specificity
		PPV = TP / PP					#Positive predictive value #Precision
		FDR = FP / PP					#False discovery rate
		FOR = FN / PN					#False omission rate
		NPV = TN / PN					#Negative predictive value 
		PLR = TPR / FPR					#Positive likelihood ratio
		NLR = FNR / TNR					#Negative likelihood ratio
		MK = PPV + NPV -1				#Markedness
		DOR = PLR / NLR					#Diagnostic odds ratio
		BA = (TPR + TNR)/2				#Balanced Accuracy
		F1 = (2*TP)/((2*TP)+FP+FN)		#F1
		FMI = sqrt(PPV * TPR)			#Fowlkes–Mallows index
		MCC = (TP * TN - FP * FN) / sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN)) #Matthews correlation coefficient
		TS = TP / (TP + FN + FP)		#Threat score 
		Prevalence = (P/(P+N))			#Prevalence
		Prevalence_THR = (sqrt(TPR*FPR)-FPR) / (TPR-FPR)				#Prevalence Threshold
		ACC = (TP + TN) / (P + N)	#Accuracy
		#Balanced_ACC = (TPR + TNR)/2	#Balance Accuracy
		Informedness = TPR + TNR - 1	#Informedness
		Total_Pop = P + N				#Total population
		YoudenJ = ((TP / (TP + FN)) + (TN / (TN + FP))) - 1#Youden's J statistic

		# AUC
   		rocObject <- try(pROC::roc(data$obs, data[, lev[1]], direction = ">", 
        quiet = TRUE), silent = TRUE)
      	rocAUC <- if (inherits(rocObject, "try-error")){ 
			NA
			}else{ 
			as.numeric(rocObject$auc)
      	}
      	AUC = rocAUC
		
		# INFO:
		# MK, MCC, Informedness, YoudenJ range from -1 to 1
		
		out = c(TPR=TPR,FPR=FPR,FNR=FNR,TNR=TNR,PPV=PPV,FDR=FDR,FOR=FOR,NPV=NPV,PLR=PLR,NLR=NLR,MK=MK,DOR=DOR,BA=BA,F1=F1,
		FMI=FMI,MCC=MCC,TS=TS,Prevalence=Prevalence,Prevalence_THR=Prevalence_THR,ACC=ACC,Informedness=Informedness,YoudenJ=YoudenJ,AUC=AUC)
		
		# round on 4 dec.
		out = round(out,4)
		
		# Check for 0 values
		if(T%in%(out==0)&Verbose){
			cat(paste0("Some values are zero:\n"))
			cat(paste0(names(out)[out==0],""),sep = "\n")
		}
		
		# Check for inf values
		if(T%in%is.infinite(out )&Verbose){
			cat(paste0("Some values could not compute. Infinite values detected:\n"))
			cat(paste0(names(out)[is.infinite(out)],""),sep = "\n")
		}
		

		
	}else{
		# These are ref values at RANDOM. because opposide prediction is actually worse than random in terms of optimization
		out = c(TPR=0.5,FPR=0.5,FNR=0.5,TNR=0.5,PPV=0.5,FDR=0.5,FOR=0.5,NPV=0.5,PLR=1.0,NLR=1.0,MK=0.0,DOR=1.0,BA=0.5,F1=0.5,FMI=0.5,
		MCC=0.0,TS=0.3,Prevalence=0.5,Prevalence_THR=0.5,ACC=0.5,Informedness=0.0,YoudenJ=0.0,AUC=0.5)
	}	
		
	if(showCM){print(out)}
	return(out)
}