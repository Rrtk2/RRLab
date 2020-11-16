#' ProjectTimePCA
#'
#' This function will creata a PCA of data at time 0, then data from time 2 will be projected in the same PCA. The biggest difference between groups is calculated thoughout all PCs. The PC with the highest difference is the PC of interest, therefore the weighted coefs are shown for that PC; indicative of the contibutors of change.
#'
#' @param Data_t0 Input the dataset object from time-point 0 (row=sample; col=feature).
#' @param Data_t1 Input the dataset object from time-point 1 (row=sample; col=feature).
#' @param ClassColumnID Index number of class column (eg. for iris dataset classcolumn ID is 5 (species)).
#'
#' @return A list of prcomp objects, distances and plots.
#' @examples
#' # make PC plot
#' a=prcomp(iris[,-5])
#' 
#' # show plot
#' autoplot(a, data = iris, colour = 'Species')
#' 
#' # make modification within one feature (as time point 2)
#' irisMod = iris
#' irisMod[irisMod$Species=="virginica",c("Sepal.Length")] = irisMod[irisMod$Species=="setosa",c("Sepal.Length")] + 1
#' 
#' # make PC plot
#' b=prcomp(irisMod[,-5])
#' 
#' # show time point 2 plot
#' autoplot(b, data = irisMod, colour = 'Species')
#' 
#' # store as time-points
#' Data_t0 = iris
#' Data_t1 = irisMod
#' 
#' # run function
#' ProjectTimePCA(Data_t0 = Data_t0,Data_t1 = Data_t1,ClassColumnID = 5)
#' @export


ProjectTimePCA = function(Data_t0=NA, Data_t1=NA,ClassColumnID=NA){
	library(ggfortify)

	ResultObject = list()
	# Get PCA object
	a=prcomp(Data_t0[,-ClassColumnID])
	ResultObject[["Initial_PCA"]] = a
	
	# plot original PCA plot
	p1 = ggplot2::autoplot(a, data = Data_t0, colour = names(Data_t0)[ClassColumnID],main = "Initial PC plot",loadings=T,loadings.label=T)
	print(p1)
	ResultObject[["Plot_Initial_PCA"]] = p1
	
	# project new data onto the PCA space (new prcompObj$x)
	Projection = scale(Data_t1[,-5], a$center, a$scale) %*% a$rotation 
	ResultObject[["Projected_values"]] = Projection

	# create prcomp object, then replace samples with new samples
	b = a
	b$x = Projection
	ResultObject[["Projected_PCA"]] = b
	
	# plot the samples on initial PC plot
	p2 = ggplot2::autoplot(b, data = Data_t1, colour = names(Data_t1)[ClassColumnID],main = "Projected samples on initial PC plot")
	print(p2)
	ResultObject[["Plot_Projected_PCA"]] = p2
	
	# Get distance between samples (t0 and t1) based on sample space
	distance = data.frame(Projection-a$x)
	ResultObject[["Distance_projection"]] = distance

	# sum distance per col 
	coldist = colSums(abs(distance))
	#coldist
	ResultObject[["Distance_projection_colsums"]] = coldist

	# where is the most distance?
	WhichPC=which(max(coldist) == coldist)
	maxcoldist = coldist[WhichPC]
	#maxcoldist
	ResultObject[["Distance_projection_maxdist"]] = maxcoldist
	ResultObject[["Distance_projection_maxdist_which"]] = WhichPC
	# found that PC 2 has most dist; should look into that!

	# get info on relevant PC
	ResultObject[["Relevant_PC"]] = a$rotation[,WhichPC]
	
	# do matrix multiplication to get summed weigthed coefs based on coldist
	ResultObject[["Weighted_coefs"]] = abs(a$rotation)%*%coldist
	return(ResultObject)
	
}

