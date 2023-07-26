#' RMostContibutingFeatures
#'
#' This function requires a PCA object of a dataset.
#'
#' @param PCA Input the PCA object (data.frame).
#' @param maxPCs (Default 10) The amount of PCs to be evaluated, but will select less if prcomp has less components. 
#' @param Quantile_TH_range (Default 0.9) The quantile range to detemine the data to be selected in modelling the average increase. 
#' @param use_ABS (Default FALSE) Use the signed (no ABS) loadings or unsigned (ABS) loadings.
#' @param plotfigure (Default TRUE) Plot the loadings in decreasing order, shows the model and 3x the sd of residual of model; then plots the features above threshold.
#'
#' @return A list of features driving the variance of selected components, with the relative variance explained in the main PC. Can be used to reduce data, select features related to biggest obseved variance.
#' @examples
#' RMostContibutingFeatures(prcomp(mtcars))
#' @export

RMostContibutingFeatures = function(PCA = NA, maxPCs = 10, Quantile_TH_range = 0.9, use_ABS = FALSE, plotfigure = TRUE){
	# check class
	if(class(PCA)!="prcomp"){stop("Class of object is not prcomp! Is this a PCA object?")}
	
	
	PCA_loadings = PCA$rotation
	PCA_var = round(pca$sdev^2 / sum(pca$sdev^2),4)*100
	
	featurePCs = list()
	for(PC in 1:min(maxPCs,dim(PCA_loadings)[1])){
		# get important 
		if(use_ABS){
			temp_loadings = abs(PCA_loadings[,PC])
			}else{
			temp_loadings = PCA_loadings[,PC]
		}
		
		temp_order = rank(-temp_loadings )
		
		# make df
		temp_data = data.frame(loadings = temp_loadings, order = temp_order)
		
		# get TH and THdata 
		temp_TH_HIGH = quantile(temp_loadings,0.5+(Quantile_TH_range/2))
		temp_TH_LOW = quantile(temp_loadings,0.5-(Quantile_TH_range/2))
		temp_data_TH = temp_data[temp_data$loadings<temp_TH_HIGH & temp_data$loadings>temp_TH_LOW,]

		
		# make model
		temp_lmmodel = lm(temp_data_TH)
		temp_model_sd = sd(temp_lmmodel$residuals)
		
		# adjust model with 3PCAsd (intercept)
		temp_lmmodel_adj = temp_lmmodel
		temp_lmmodel_adj$coefficients[1] = as.numeric(coef(temp_lmmodel)[1]+c(3*temp_model_sd))
		
		# Predict using 80% lowest
		temp_pred = predict(temp_lmmodel_adj, temp_data)
		
		# test wchih are above TH (3*sd)
		temp_feature_names = names(which(temp_data$loadings > temp_pred  ))
		
		# plot the figure and model
		if(plotfigure){
			plot(y= temp_loadings ,x = temp_order,main=paste0("PC ",PC," (",PCA_var[PC],"%)"))
			abline(temp_lmmodel)
			abline(temp_lmmodel_adj,col="red")
			points(y=temp_pred,x=temp_data$order,col="red",pch=20)
			abline(v = temp_data$order[which(temp_data$loadings > temp_pred  )],col="black",lty=2)
			points(x=temp_data$order[which(temp_data$loadings > temp_pred  )],y=temp_data$loadings[which(temp_data$loadings > temp_pred  )],col="blue",pch=19)
		}
		featurePCs[[PC]] = list(names = temp_feature_names, PCvariance = PCA_var[PC])
	}

	return(featurePCs)


}