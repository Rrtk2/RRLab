#' RRPCAplot
#'
#' This function will get the matched info.
#'
#' @param PCA PCA object from prcomp()
#' @param PCAcontrastX What PC for X
#' @param PCAcontrastY What PC for Y
#' @return A plot indicative of lodings and loading importance.
#' @examples
#' RRPCAplot(PCA = prcomp(iris[,-5]), PCAcontrastX = 1, PCAcontrastY = 2)
#' @export

RRPCAplot = function(dataset = NA, Class= NA, PCAcontrastX = 1, PCAcontrastY = 2){
	# settings 
	#PCAcontrastX = 1
	#PCAcontrastX = 2

	if(!is.na(Class)){
		dataset$Class = Class
	}

	PCA = prcomp(dataset)
	
	# Get relevant numbers from PCA
	PCAx = PCA$x[,PCAcontrastX]
	PCAy = PCA$x[,PCAcontrastY]

	# Loadings object; scaled to max varaiance of PC
	PCALoadingX = PCA$rotation[,PCAcontrastX]*IQR(PCAx)
	PCALoadingY = PCA$rotation[,PCAcontrastY]*IQR(PCAy)
	Loadings = data.frame(x = PCALoadingX, y = PCALoadingY)

	# Get variance
	TotVar = round((PCA$sdev^2/sum(PCA$sdev^2))*100,2)

	# PCASamplePlot
	PCASamplePlot  = ggplot() + 
	theme(legend.position="bottom")+
	geom_point(dataset, mapping = aes(PCAx, PCAy, colour = Class)) +
	xlim(min(PCAx)*1.1, max(PCAx)*1.1) +
	ylim(min(PCAy)*1.1, max(PCAy)*1.1) +
	labs( x = paste0("PC",PCAcontrastX," (",TotVar[PCAcontrastX],"%)"), y = paste0("PC",PCAcontrastY," (",TotVar[PCAcontrastY],"%)")) + 
	ggtitle("Optimal unsupervised contrast") 


	# Loadings plot
	PCALoadingsPlot  = ggplot() + 
	theme(legend.position="bottom")+
	xlim(min(PCAx)*1.1, max(PCAx)*1.1) +
	ylim(min(PCAy)*1.1, max(PCAy)*1.1) +
	geom_text(data = Loadings, aes(x, y, label = rownames(Loadings))) + 
	labs( x = paste0("PC",PCAcontrastX," (",TotVar[PCAcontrastX],"%)"), y = paste0("PC",PCAcontrastY," (",TotVar[PCAcontrastY],"%)")) + 
	ggtitle("Optimal unsupervised contrast") 


	# Combined
	SelectedLodings = ceiling(dim(Loadings)[1]*0.05)
	LoadingPC1 = Loadings[order(abs(Loadings[,1]),decreasing = T),]
	LoadingPC2 = Loadings[order(abs(Loadings[,2]),decreasing = T),]

	# select important loadings
	SelectedLoadingNames = unique(c(rownames(LoadingPC1)[1:SelectedLodings],rownames(LoadingPC2)[1:SelectedLodings]))
	ImportantsLoadings = which(SelectedLoadingNames==rownames(Loadings))

	# use important loadings
	Loadings_select = Loadings[ImportantsLoadings,]
	Loadings_deselect = Loadings[-ImportantsLoadings,]

	# the actual plot
	PCASampleLoadingsPlot = ggplot() + 
	theme(legend.position="bottom")+
	geom_point(dataset, mapping = aes(PCAx, PCAy, colour = Class)) +
	xlim(min(PCAx)*1.1, max(PCAx)*1.1) +
	ylim(min(PCAy)*1.1, max(PCAy)*1.1) +
	geom_text(data = Loadings_select, aes(x, y, label = rownames(Loadings_select)),col='red',size = 4) + 
	geom_text(data = Loadings_deselect, aes(x, y, label = rownames(Loadings_deselect)),col='tomato1',size = 2.5) +
	labs( x = paste0("PC",PCAcontrastX," (",TotVar[PCAcontrastX],"%)"), y = paste0("PC",PCAcontrastY," (",TotVar[PCAcontrastY],"%)")) + 
	ggtitle("Optimal unsupervised contrast") 

	return(PCASampleLoadingsPlot)
}
