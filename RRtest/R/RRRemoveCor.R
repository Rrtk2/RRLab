#' RRRemoveCor
#'
#' This function required a dataset with rows as samples and columns as features.
#' All features should be continues, not categorical or ordinal as the func cannot deal with those at this time.
#' The fuction will assess the pearson corrlation between all the given features, and identify the most correlated ones.
#' Then the corrlated features will be linearly modelled by each other, extracting fitted and residuals in the process.
#' Internally it selects the most likely model based on normally disributed residuals.
#' The concept is that corrlated variables share information, and the only unique information between them are the residuals.
#' This function will result a modified dataframe, with updated values for the correlated features.
#'
#' @param dataset Input the dataset in dataframe format (row=sample, col=feature).
#' @param TH ABS Threshold correlation (defauls to 0.7)
#'
#' @return Dataframe with modified values for correlated features.
#' @examples
#' Updated_iris = RRRemoveCor(iris[,-5])
#' @export


RRRemoveCor = function(dataset,TH=0.7){
	#---------------#
	# 	CHECKS		#
	#---------------#
	# Test dataset format
	if(!is.data.frame(dataset)){
		warning("Data is not in data.frame format, converting!")
		dataset = as.data.frame(dataset)
	}

	# Test Th within limits
	if(TH>1|TH<0){
		warning("TH cannot be bigger than 1 or smaller than 0")
		break()

	}

	#---------------#
	# 	MAIN		#
	#---------------#
	# Get correlation matrix
	d= cor(dataset,method="pearson")
	results = data.frame(v1=character(0), v2=character(0), cor=numeric(0), stringsAsFactors=FALSE)

	# Find correlating features
	diag(d) <- 0
	while (sum(d!=0)>1) {
		maxval <- max(abs(d))
		max <- which(abs(d)==maxval, arr.ind=TRUE)[1,]
		results <- rbind(results, data.frame(v1=rownames(d)[max[1]], v2=colnames(d)[max[2]], cor=d[max[1],max[2]]))
		d[max[1],] <- 0
		d[,max[1]] <- 0
		d[max[2],] <- 0
		d[,max[2]] <- 0
		#print(d)
	}

	# filter on TH
	results = results[abs(results$cor)>TH,]
	print(results)

	# Case escape
	if(dim(results)[1]==0){
		message("No strong correlations found, returning original dataframe!")
		return(dataset)
	}

	# Modelling variables
	b=dataset
	# Check for every corrlated item
	for( i in 1:dim(results)[1]){
		# Make formulas
		temp_form1 = formula(paste0(results[i,1],"~",results[i,2]))
		temp_form2 = formula(paste0(results[i,2],"~",results[i,1]))

		# make models
		temp_lm1 = lm(temp_form1,dataset)
		temp_lm2 = lm(temp_form2,dataset)

		# check which model has better residual fit; using shapirp
		Better_model = shapiro.test(residuals(summary(temp_lm1)))$p < shapiro.test(residuals(summary(temp_lm2)))$p

		# select case
		if(Better_model){
			#TRUE ->  1 ~ 2
			temp_lm_res = residuals(temp_lm1) # residuals
			temp_lm_fit = temp_lm1$fitted.values #fitted vals

			# store fitted and residual info
			b[,colnames(b)==results[i,1]] = temp_lm_res
			b[,colnames(b)==results[i,2]] = temp_lm_fit

		}else{
			#FALSE ->  2 ~ 1
			temp_lm_res = residuals(temp_lm2) # residuals
			temp_lm_fit = temp_lm2$fitted.values #fitted vals

			# store new vars
			b[,colnames(b)==results[i,2]] = temp_lm_res
			b[,colnames(b)==results[i,1]] = temp_lm_fit
		}

	}

	return(b)
}
