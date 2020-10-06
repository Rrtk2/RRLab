#' RRDistribution
#'
#' This function will get a estimate of the distribution of entered input, and indicates what distribution might fit better.
#'
#' @param Data Unlisted vector of numbers. 
#' @return A distribution estimate with plots.
#' @examples
#' RRDistribution(Data= iris[,-5])
#' @export

RRDistribution =  function(Data){
#-----------------------------------------------------------------------------------------------------#
#							Distribution fitting
#-----------------------------------------------------------------------------------------------------#
	# Simple data handling; ONLY NUMERIC INPUT!
	if(class(Data)!="numeric"){Data = unlist(Data)}
	
	ResultObject = list()
	fitdistrplus::descdist(Data)

	result = data.frame()
	methodsdist = c("norm", "lnorm", "pois", "exp", "gamma", "nbinom", "geom", "beta", "unif", "logis")
		#binomial			(link = "logit")
		#gaussian			(link = "identity")
		#Gamma				(link = "inverse")
		#inverse.gaussian	(link = "1/mu^2")
		#poisson			(link = "log")
		#quasi				(link = "identity", variance = "constant")
		#quasibinomial		(link = "logit")
		#quasipoisson		(link = "log")

	for( i in 1:length(methodsdist) ) {

		# Select one of the methods
		methodselect = methodsdist[i]
		
		# Fit the method 
		fit = try(fitdistrplus::fitdist(Data, methodselect, method = "mme"),silent = TRUE)
		
		# if fails, next
		if(class(fit) == "try-error"){next}

		# If not, store results
		result[i,1] = methodselect
		result[i,2] = fit$aic
	}

	# Remove NAs from results
	result = result[rowSums(is.na(result)+0)==0,]
	# Remove inf from results
	result = result[is.infinite(result[,2])+0==0,]

	# Rank results
	result[,3] = rank(abs(result[,2]))

	# Extract best minimal loss distirubution
	OptimalMethod = result[result[,3]==1,1]
	fit=fitdistrplus::fitdist(Data, OptimalMethod, method = "mme")
	plot(fit)
	cat(paste0("Best distribution fit: ",OptimalMethod,"\n"))
	
	# put results in object
	ResultObject$Scoreresult = result
	ResultObject$Fit = fit
	
	return(ResultObject)
}

