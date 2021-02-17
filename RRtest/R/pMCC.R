#' pMCC
#'
#' This function required a dataset with column 1 as observations and column 2 with predictions. Two-class only! 
#'
#' @param data Input the dataset object (column 1 = OBS; column 2 = PRED).
#' @param lev NULL
#' @param model NULL
#' @param showCM Show output of confusion matrix & metrics as cat.
#' @param lev/level1 The actual label/number of the first (to be compared to) 'class'. NEEDS FIXING @RRR
#' @param s_TH TH for setting classes form probabilities.
#'
#' @return Several metrics to indicate machine learning performance ("MCC","Accuracy","Recall","Precision","F1","CombinedScore")
#' @examples
#' pMCC(data.frame(obs=c(1,1,1,2,2,2),pred=c(0.1,0.1,0.4999,0.5001,0.9,0.9)))
#' pMCC(data.frame(obs=c(1,1,1,2,2,2),pred=c(0.1,0.1,0.2,0.0.8,0.9,0.9)))
#' @export

pMCC = function (data, lev = NULL, model = NULL, showCM = FALSE,level1=NA,s_TH = 0.5) 
{

# OK so in classification situation the 'data' parsed form caret is as folowed:
#pred(class), obs(class), [nameclass0](probability), [nameclass1](probability), rowindex(number)

# OK so in classification situation the 'lev' parsed form caret is as folowed:
#[nameclass0], [nameclass1]
# FALSE? unknown
# is stored with c()


#	if(is.na(level0)){
#		level0 = min(unique(data$obs))
#	}
#assign("a",data,envir = .GlobalEnv)
#assign("b",lev,envir = .GlobalEnv)

	if(is.null(lev)){
		lev[1] = unique(data$obs)[1]
	}
	

  temp_obs = (data$obs==lev[1])+0
  temp_pred_p = data[,lev[1]]
  temp_pred_c = (temp_pred_p>=s_TH)+0
  
  # check if p is in good direction
  if(sum(temp_obs==temp_pred_c)/length(temp_obs)<0.5){
	warning("Accuracy is lower than 0.5, please check if entered level is corresponding probability value OR levels are switched")
  }
  
  # MCC
  df2 = data.frame(obs = as.character(temp_obs), pred = as.character(temp_pred_c))
  CM = table(df2)
  if (names(labels(CM)[1]) == "obs") {
    CM = t(CM)
  }
  if ((dim(CM)[1] == 2 & dim(CM)[2] == 2)) {
    if (showCM) {
      cat("Confusion matrix:\n")
      print(CM)
      cat("\n")
    }
    TP = CM[1, 1]
    FP = CM[1, 2]
    TN = CM[2, 2]
    FN = CM[2, 1]
    TP <- as.double(TP)
    FP <- as.double(FP)
    TN <- as.double(TN)
    FN <- as.double(FN)
    numerator <- (TP * TN - FP * FN)
    denominator <- sqrt((TP + FP) * (TP + FN) * (TN + FP) * 
      (TN + FN))
    if (denominator == 0) 
      denominator <- 1
    MCCval <- numerator/denominator
	
	
	#pMCC
	#p TP get sum error, class TRUE POS is expected '0', so take value as is.
	pTP = sum(temp_pred_p[temp_obs==0])
	if(is.nan(pTP)){pTP=0}
	
	#p TN get sum error, class TRUE NEG is expected '1', so take abs(value-1).
	pTN = sum(abs(temp_pred_p[temp_obs==1]-1))
	if(is.nan(pTN)){pTN=0}
	
	#p FP get sum error, class FALSE POS is expected '1', so take abs(value-1)
	FPindex = temp_pred_c==0
	FPindex[temp_obs==0] = FALSE
	pFP = sum(abs(temp_pred_p[FPindex]-1))
	if(is.nan(pFP)){pFP=0}
	
	#p FN get sum error, class FALSE NEGATIVE is expected '0', so take value as is
	FPindex = temp_pred_c==1
	FPindex[temp_obs==1] = FALSE
	pFN = sum(temp_pred_p[FPindex])
	if(is.nan(pFN)){pFN=0}

    pTP <- as.double(pTP)
    pFP <- as.double(pFP)
    pTN <- as.double(pTN)
    pFN <- as.double(pFN)
	#numerator <- ((TP-pTP) + TN-pTN)
    #denominator <- (TP + TN+ FP + FN)
	
	
	numerator <- ((TP-pTP) * (TN-pTN) - (FP-pFP) * (FN-pFN))
    denominator <- sqrt((TP + FP) * (TP + FN) * (TN + FP) * 
      (TN + FN))
	  
	  
    if (denominator == 0) 
      denominator <- 1
    pMCCval <- numerator/denominator
	
   
    out = c(MCCval,pMCCval)
	
	
  }
  else {
    out = c(-1,-1)
  }
  names(out) = c("MCC","pMCC")
  if (showCM) {
    print(out)
  }
  return(out)
  
  ## notes below
	#	  
	#	#pMCC(data.frame(obs=c(1,1,1,2,2,2),pred=c(0.1,0.1,0.4999,0.5001,0.9,0.9)#))
	#
	#		#pMCC(data.frame(obs=c(1,1,1,2,2,2),pred=c(0.1,0.1,0.14999,0.95001,0.9,0.9)))
	#
	#		result_df = data.frame(noise=NA,MCC=NA,pMCC=NA)
	#
	#		samples=100
	#		for( i in 1:1000){
	#			noise = 0.001*i
	#			#scores = runif(samples,0,1)
	#			scores = abs(c(runif(n = samples/2,0,0.01),runif(n = #samples/2,0.99,1)))
	#			#hist(scores)
	#			Trueclass = round(scores)
	#			Classp = (scores)+runif(n = samples,min = 0,max = noise+0)
	#			df = data.frame(obs=Trueclass,pred=Classp)
	#			result_df[i,1] = noise
	#			result_df[i,2:3] = pMCC(df)
	#		}
	#
	#		#plot(result_df$noise,result_df$MCC,main="mcc")
	#
	#		#plot(result_df$noise,result_df$pMCC,main="pmcc")
	#
	#		#plot(result_df$MCC,result_df$pMCC,abline(1,1))
	#
	#		plot(result_df$noise,result_df$MCC,main="mcc",col=2,ylim=c(0,1))
	#		points(result_df$noise,result_df$pMCC,col=3)
}

