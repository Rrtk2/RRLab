#' OutlierCheck
#'
#' This function required a dataset with rows as features and columns as samples.
#'
#' @param data Input the dataset object (row=feature; col=sample).
#' @param Type Type indicates sample outliers or feature outliers to be resulted. Defaults to 1 (SAMPLES), can be set to 2 (FEATURES).
#'
#' @return A PCA plot indicative of outlier samples based a set of tests.
#' @examples
#' OutlierCheck(t(mtcars),1)
#' @export


OutlierCheck = function(data,Type=1){

	#### State DATA
		if(class(data)!="data.frame"){
			cat(paste0("Data needs to be in data.frame. Forcing now.","\n"))
			data = data.frame(data)
		}
		
		if(class(data)!="data.frame"){
			stop("Forcing failed. Stopping\n\n")
		}
		
		# data ROW should be FEATURES
		# data COL should be SAMPLES
		cat(paste0("No samples (Cols): ",dim(data)[2],"\n"))
		cat(paste0("No features (Rows): ",dim(data)[1],"\n"))
		

	#### Check Type
		if(Type==1){
			cat(paste0("Getting *SAMPLE* outliers\n"))
			data = t(data)
			}else{
			cat(paste0("Getting *FEATURE* outliers\n"))
		}



	
	
	#### State FUNCTIONS
		test_Kurt = function(x){sum(((x-mean(x))/sd(x))^4)/length(x)}
		test_RMSE = function(x){sqrt((sum((x-mean(x))^2)/length(x)))}
		test_Skewness = function(x){3*(mean(x)-median(x))/sd(x)} #Pearson median skewness
		test_variance = function(x){sd(x)} 
		
		# Normality test, Jarque–Bera test:
		# depends on skewness and kurt
		test_JB =  function(x){(length(x)/6)*((test_Skewness(x)^2)+((0.25*(test_Kurt(x)-3))^2))}
		
		# Normality test, Shapiro–Wilk test
		#test_shapiro = function(x){as.numeric(shapiro.test(x)$statistic)}
		
		# uniform test (QQ unif dist, RMSE)
		test_QQunifRMSE = function(x){sqrt(sum((quantile(x,(1:length(x))/length(x)) - quantile(x = runif(10000,min = min(x),max = max(x)),(1:length(x))/length(x)))^2)/length(x))}
		
		# norm test (QQ norm dist, RMSE)
		test_QQnormRMSE = function(x){sqrt(sum((quantile(x,(1:length(x))/length(x)) - quantile(x = rnorm(10000,mean = mean(x),sd = sd(x)),(1:length(x))/length(x)))^2)/length(x))}

		# cluster test
	
	
	#### generate metrics per feature
	res = t(data.frame(apply(data,1,function(x){
		c(test_Kurt(x),
		test_RMSE(x),
		test_Skewness(x),
		test_variance(x),
		test_JB(x),
		#test_shapiro(x),
		test_QQunifRMSE(x),
		test_QQnormRMSE(x))
	})))
	
	colnames(res) = c("Kurt","RMSE","Skew","Var","JB","QQunif","QQnorm")
	
	rownames(res) = rownames(data)
	
	
	
	PCA = prcomp((res),scale. = T)
	#plot(t(PCA$rotation),main="Metrics")
	
	biplot_name = ifelse(Type==1,"Samples","Features")
	
	#print(plot(PCA$x,main=biplot_name))
	biplot(PCA,main=biplot_name)
	
	# final res
	return(res)
}