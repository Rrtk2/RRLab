#' RMostContibutingFeatures2
#' 
#' @usage
#' This function requires a Dat object of a dataset, based on a vector of importances.
#'
#' @param Dat Input the Dat object (vector).
#' @param amountSDTH (Default 3) The amount SDs to be passed before considering the value to be relevant
#' @param Quantile_TH_range (Default 0.9) The quantile range to detemine the data to be selected in modelling the average increase. 
#' @param plotfigure (Default TRUE) Plot the loadings in decreasing order, shows the model and 3x the sd of residual of model; then plots the features above threshold.
#'
#' @return The names of the most important features.
#' @examples
#' Dat = data.frame(Vals = c(rnorm(100),(1.0+(1:10)/10)^3)); rownames(Dat) = paste0("Feature",rownames(Dat))
#' Results = RMostContibutingFeatures2(Dat)
#' @export

RMostContibutingFeatures2 = function(Dat = NA, amountSDTH = 3, Quantile_TH_range = 0.9, plotfigure = TRUE){
	
    if(class(Dat)!="data.frame"){stop("Dat should be a 1 column dataframe, with rownames")}

	temp_order = rank(-Dat )
	
	# make df
	temp_data = data.frame(loadings = Dat, order = temp_order)
    colnames(temp_data) = c("loadings","order")
	
	# get TH and THdata 
	# this will extract the inner 90% of all values, using this data to build the linear model on!
	# This assumes the values of interest are in the outer 10% (upper 5%)
	temp_TH_HIGH = quantile(temp_data$loadings,0.5+(Quantile_TH_range/2))
	temp_TH_LOW = quantile(temp_data$loadings,0.5-(Quantile_TH_range/2))
	temp_data_TH = temp_data[temp_data$loadings<temp_TH_HIGH & temp_data$loadings>temp_TH_LOW,]

	
	# make model
    # (...2) make robost model (using median rather than mean)!
	#temp_lmmodel = lm(temp_data_TH)
    temp_lmmodel2 = robustbase::lmrob(temp_data_TH)
    
	#temp_model_sd = sd(temp_lmmodel$residuals)
    temp_model_sd2 = sd(temp_lmmodel2$residuals)
	
	# adjust model with 3Datsd (intercept)
	#temp_lmmodel_adj = temp_lmmodel
	#temp_lmmodel_adj$coefficients[1] = as.numeric(coef(temp_lmmodel)[1]+c(amountSDTH*temp_model_sd))
	temp_lmmodel_adj2 = temp_lmmodel2
	temp_lmmodel_adj2$coefficients[1] = as.numeric(coef(temp_lmmodel2)[1]+c(amountSDTH*temp_model_sd2))


	# Predict using 90% lowest
	#temp_pred = predict(temp_lmmodel_adj, temp_data)
	temp_pred2 = predict(temp_lmmodel_adj2, temp_data)


	# test wchih are above TH (3*sd)
	#temp_feature_names = names(which(temp_data$loadings > temp_pred  ))
	temp_feature_names2 = names(which(temp_data$loadings > temp_pred2  ))

	# plot the figure and model
	if(plotfigure){
		plot(y= temp_data$loadings ,x = temp_order)
		#abline(temp_lmmodel)
    	#abline(temp_lmmodel_adj,col="red")
        #points(y=temp_pred,x=temp_data$order,col="red",pch=20)
           
		abline(temp_lmmodel2)
    	abline(temp_lmmodel_adj2,col="red")
        points(y=temp_pred2,x=temp_data$order,col="red",pch=20)
           
		abline(v = temp_data$order[which(temp_data$loadings > temp_pred2  )],col="black",lty=2)
		points(x=temp_data$order[which(temp_data$loadings > temp_pred2  )],y=temp_data$loadings[which(temp_data$loadings > temp_pred2  )],col="blue",pch=19)
	}

	return(list(relevant_features = temp_feature_names2, model = temp_lmmodel2, sd = temp_model_sd2, upper_QUANT_TH = temp_TH_HIGH, lower_QUANT_TH = temp_TH_LOW))

}