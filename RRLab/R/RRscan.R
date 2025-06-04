#' RRscan
#'
#' This function will generate information and images about inputted data, getting fundamental properties of entered data.
#'
#' @param data Data object in dataframe with samples as rows and features as columns.
#' @param color A vector indicating the colors per sample.
#' @param LogTransform Transform data using log2(x+1).
#' @param ImageDir The directory location for images to be written to.
#' @param PlotImages Plot images in the plotter.
#' @param SaveImages Save the images to in the ImageDir
#' @param FindOutliers Print potential outliers based on 3 testes failed (hclust dist, mean+3*sd, mean(variance) + 3*sd(variance))
#' @param verbose Give more information.
#' @return Multiple plots giving an overview of the data.
#' @examples
#' RRscan(data = mtcars,verbose=TRUE)
#' @export




RRscan = function(data=data,color=1,LogTransform=FALSE,ImageDir="",PlotImages=TRUE,SaveImages=FALSE,FindOutliers=TRUE,verbose=FALSE){

	
	
##---------------------------------------------------------------------------#--------------------------#
#	#							Fundametal properties of data
#	##---------------------------------------------------------------------------#--------------------------#
#	# Distribution of values
#	# Distribution of mean
#	# Distribution of variance
#
#	# Mean-variance
#
#	data=data_Transcriptomics$Counts
#	data = t(data)
#
#	coltest = round(runif(dim(data)[2],1,2))
#
#RRscan(data,LogTransform = T,color=coltest)
	#-----------------------------------------------------------------------------------------------------#
	#							Check conditions
	#-----------------------------------------------------------------------------------------------------#
	data=t(data)
	
	if(LogTransform){
		data=log(data+1,base=2)
	}
	
	ImageFolderLocation=paste0(ImageDir)
	
	



	#-----------------------------------------------------------------------------------------------------#
	#							PACKAGES
	#-----------------------------------------------------------------------------------------------------#
        RRLab::libraryR(c("ggplot2", "ggdendro", "tidyr"))

	#-----------------------------------------------------------------------------------------------------#
	#							Function 
	#-----------------------------------------------------------------------------------------------------#
	skew = function(x){
		# Galton/Bowley skewness
		iqr = IQR(x)
		mediann = median(x)
		Res = (iqr - (2*mediann)) / iqr
	}

	QQscore = function(x){
		qref = quantile(rnorm(1:1000,mean = mean(x),sd = sd(x)),c(0:100)/100)
		qobs = quantile(x,c(0:100)/100)
		
		Res = sum(sqrt((qref-qobs)^2))/length(x)
		return(Res)
	}

	#-----------------------------------------------------------------------------------------------------#
	#							OVERALL METRICS (collection per sample)
	#-----------------------------------------------------------------------------------------------------#
	if(verbose){cat("\nGetting sample-based metics\n")}
	S_Means = apply(data,2,mean)
	S_Medians = apply(data,2,median)
	S_SDs = apply(data,2,sd)
	S_Skews = apply(data,2,skew)
	S_QQs = apply(data,2,QQscore)


	#-----------------------------------------------------------------------------------------------------#
	#							OVERALL METRICS (collection per sample)
	#-----------------------------------------------------------------------------------------------------#
	if(verbose){cat("\nGetting feature-based metics\n")}
	F_Means = apply(data,1,mean)
	F_Medians = apply(data,1,median)
	F_SDs = apply(data,1,sd)
	F_Skews = apply(data,1,skew)
	F_QQs = apply(data,1,QQscore)


	#-----------------------------------------------------------------------------------------------------#
	#							OMNI-HIST 
	#-----------------------------------------------------------------------------------------------------#
	# get all S or F types
	List_metrics = ls()[grep(ls(),pattern="^F_|^S_")]
	if(verbose){cat("\nPlotting histogram\n")}
	for(i in 1:length(List_metrics)){
		temp_select = List_metrics[i]
		temp_data = get(temp_select)

		temp_data = data.frame(temp_data)%>%gather()

		g <- ggplot(temp_data, aes(value))
		g = g + geom_histogram(color="black", fill="white") + 
			labs(title=paste0("Histogram of ",temp_select))
			 #subtitle="")+
			 #caption="Source: mpg",+
			 #coord_flip()+
			 # xlab("Mean")+
			 # ylab("QQerror")
	
	
		if(PlotImages){print(g)}
		
		if(SaveImages){	 
			ggsave(paste0(ImageFolderLocation,"Histogram_",temp_select,".png"), plot = g, width = 20, height = 20, units = "cm",dpi = 500,bg = "transparent")
		}
	}




	#-----------------------------------------------------------------------------------------------------#
	#							QUANTILE DISTRIBUTION
	#-----------------------------------------------------------------------------------------------------#
	#boxplot(data)

	temp_data = as.data.frame(data) %>% gather
	if(verbose){cat("\nPlotting boxplot\n")}
	g <- ggplot(temp_data, aes(key, value))
	g = g + geom_boxplot( outlier.size = .01,color=(color)) + 
		labs(title="Sample Box plot")+ 
		 #subtitle="")+
		 #caption="Source: mpg",+
		 coord_flip()+
		  xlab("")+
		  ylab("")
	if(PlotImages){print(g)}
	if(SaveImages){				 
		ggsave(paste0(ImageFolderLocation,"Boxplot.png"), plot = g, width = 20, height = ceiling(dim(data)[2]*0.29), units = "cm",dpi = 500,bg = "transparent")
	}





	#-----------------------------------------------------------------------------------------------------#
	#							DISTRIBUTION DENSITY
	#-----------------------------------------------------------------------------------------------------#
	#DensList = apply((data),2,density)
	#plot(DensList[[1]])
	#for(i in 2:length(DensList)){
	#	lines(DensList[[i]],add=T)
	#}

	temp_data = as.data.frame(data) %>% gather
	if(verbose){cat("\nPlotting density\n")}
	g <- ggplot(temp_data, aes(value,group=key))
	g = g + geom_density() + 
		labs(title="Sample Density plot")+ 
		 #subtitle="")+
		 #caption="Source: mpg",+
		 #coord_flip()+
		  xlab("")+
		  ylab("")+
		  scale_y_continuous(name = "Density")
	
	if(PlotImages){print(g)}
	if(SaveImages){		 
		ggsave(paste0(ImageFolderLocation,"Density.png"), plot = g, width = 20, height = 20, units = "cm",dpi = 500,bg = "transparent")
	}


	#-----------------------------------------------------------------------------------------------------#
	#							DENDROGRAM (HCLUST)
	#-----------------------------------------------------------------------------------------------------#
	Hclustobj = hclust(dist(t(data)))
	#plot(Hclustobj)
	if(verbose){cat("\nPlotting dendrogram\n")}
	# Build dendrogram object from hclust results
	dend <- as.dendrogram(Hclustobj)
	# Extract the data (for rectangular lines)
	# Type can be "rectangle" or "triangle"
	dend_data <- dendro_data(dend, type = "rectangle")
	# What contains dend_data
	names(dend_data)

	# Plot line segments and add labels
	g <- ggplot(dend_data$segments) + 
	ylim(-20,max(dend_data$segments$yend))+
	xlim(min(dend_data$segments$xend),max(dend_data$segments$xend))+
	geom_segment(aes(x = x, y = y, xend = xend, yend = yend))+
	geom_text(data = dend_data$labels, aes(x, y, label = label),
	hjust = 1, angle = 0, size = 3,color=color)+
	coord_flip()+
	labs(title="Sample Dendrogram")+ 
		 #subtitle="")+
		 #caption="Source: mpg",+
		 #coord_flip()+
		  ylab("Height (Euclidean)")+
		  xlab("Samples")
		  
	if(PlotImages){print(g)}
	if(SaveImages){	 
		ggsave(paste0(ImageFolderLocation,"Dendrogram.png"), plot = g, width = 20, height = dim(data)[2]*0.3, units = "cm",dpi = 500,bg = "transparent")
	}


	#-----------------------------------------------------------------------------------------------------#
	#							MEAN-VARIANCE
	#-----------------------------------------------------------------------------------------------------#

	#plot(F_Means,F_SDs^2,
	#main="Mean-variance plot",
	#xlab="Mean",
	#ylab="Variance")

	temp_data = data.frame(Mean=F_Means,Var=F_SDs^2)
	if(verbose){cat("\nPlotting mean-variance\n")}
	g <- ggplot(temp_data, aes(Mean,Var))
	g = g + geom_point() + 
		labs(title="Feature Mean-Variance plot")+ 
		 #subtitle="")+
		 #caption="Source: mpg",+
		 #coord_flip()+
		  xlab("Mean")+
		  ylab("Variance")

	if(PlotImages){print(g)}
	if(SaveImages){		 
		ggsave(paste0(ImageFolderLocation,"Mean-Variance.png"), plot = g, width = 20, height = 20, units = "cm",dpi = 500,bg = "transparent")
	}
	#-----------------------------------------------------------------------------------------------------#
	#							MEAN-SKEW
	#-----------------------------------------------------------------------------------------------------#

	#plot(F_Means,F_Skews,
	#main="Mean-Skew plot",
	#xlab="Mean",
	#ylab="Skewness")


	temp_data = data.frame(Mean=F_Means,Skew=F_Skews)
	if(verbose){cat("\nPlotting mean-skew\n")}
	g <- ggplot(temp_data, aes(Mean,Skew))
	g = g + geom_point() + 
		labs(title="Feature Mean-Skewness plot")+ 
		 #subtitle="")+
		 #caption="Source: mpg",+
		 #coord_flip()+
		  xlab("Mean")+
		  ylab("Skewness")

	if(PlotImages){print(g)}
	if(SaveImages){
		ggsave(paste0(ImageFolderLocation,"Mean-Skewness.png"), plot = g, width = 20, height = 20, units = "cm",dpi = 500,bg = "transparent")
	}
	#-----------------------------------------------------------------------------------------------------#
	#							Variance-SKEW
	#-----------------------------------------------------------------------------------------------------#

	#plot(F_SDs^2,F_Skews,
	#main="Variance-Skew plot",
	#xlab="Variance",
	#ylab="Skewness")


	temp_data = data.frame(Var=F_SDs^2,Skew=F_Skews)
	if(verbose){cat("\nPlotting variance-skew\n")}
	g <- ggplot(temp_data, aes(Var,Skew))
	g = g + geom_point() + 
		labs(title="Feature Variance-Skewness plot")+ 
		 #subtitle="")+
		 #caption="Source: mpg",+
		 #coord_flip()+
		  xlab("Variance")+
		  ylab("Skewness")

	if(PlotImages){print(g)}
	if(SaveImages){		 
		ggsave(paste0(ImageFolderLocation,"Variance-Skewness.png"), plot = g, width = 20, height = 20, units = "cm",dpi = 500,bg = "transparent")
	}
	#-----------------------------------------------------------------------------------------------------#
	#							Mean-Median
	#-----------------------------------------------------------------------------------------------------#


	temp_data = data.frame(Means=F_Means,Medians=F_Medians)
	if(verbose){cat("\nPlotting mean-median\n")}
	g <- ggplot(temp_data, aes(Means,Medians))
	g = g + geom_point() + 
		labs(title="Feature Mean-Median plot")+ 
		 #subtitle="")+
		 #caption="Source: mpg",+
		 #coord_flip()+
		  xlab("Mean")+
		  ylab("Median")

	if(PlotImages){print(g)}
	if(SaveImages){	 
		ggsave(paste0(ImageFolderLocation,"Mean-Median.png"), plot = g, width = 20, 	height = 20, units = "cm",dpi = 500,bg = "transparent")
	}
	#-----------------------------------------------------------------------------------------------------#
	#							Mean-QQscore
	#-----------------------------------------------------------------------------------------------------#


	temp_data = data.frame(Means=F_Means,QQscore=F_QQs)
	if(verbose){cat("\nPlotting mean-QQerror\n")}
	g <- ggplot(temp_data, aes(Means,QQscore))
	g = g + geom_point() + 
		labs(title="Feature Mean-QQerror plot")+ 
		 #subtitle="")+
		 #caption="Source: mpg",+
		 #coord_flip()+
		  xlab("Mean")+
		  ylab("QQerror")

	if(PlotImages){print(g)}
	if(SaveImages){		 
		ggsave(paste0(ImageFolderLocation,"Mean-QQerror.png"), plot = g, width = 20, height = 20, units = "cm",dpi = 500,bg = "transparent")
	}




	#-----------------------------------------------------------------------------------------------------#
	#							PCA
	#-----------------------------------------------------------------------------------------------------#
	temp_data = t(data)
	PCA = prcomp(temp_data,scale=TRUE)
	if(verbose){cat("\nPlotting PC1-PC2\n")}
	#Set relevevant PC contratst
	PCAcontrastX=1
	PCAcontrastY=2

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
	g  = ggplot() + 
	theme(legend.position="bottom")+
	geom_point(data.frame(temp_data), mapping = aes(PCAx, PCAy),color=color) +
	xlim(min(PCAx)*1.1, max(PCAx)*1.1) +
	ylim(min(PCAy)*1.1, max(PCAy)*1.1) +
	labs( x = paste0("PC",PCAcontrastX," (",TotVar[PCAcontrastX],"%)"), y = paste0("PC",PCAcontrastY," (",TotVar[PCAcontrastY],"%)")) + 
	ggtitle("Optimal unsupervised contrast") 
	
	if(PlotImages){print(g)}
	if(SaveImages){
		ggsave(paste0(ImageFolderLocation,"PCA-plot.png"), plot = g, width = 20, height = 20, units = "cm",dpi = 500,bg = "transparent")
	}
	#-----------------------------------------------------------------------------------------------------#
	#							OUTLIER REMOVAL testing
	#-----------------------------------------------------------------------------------------------------#
	if(FindOutliers){
		# By usage of TREE
		# cut tree in most 2 extreme groups
		Hclust_cut = cutree(tree = Hclustobj, k = 2)

		# find the minor one and find samples
		Outliers.1 = names(which(Hclust_cut == names(which(table(Hclust_cut)==min(table(Hclust_cut))))))


		# By usage of MEANS
		S_Means = apply(data,2,mean)
		DEV = S_Means-mean(S_Means)

		Upper_bound = mean(DEV)+(sd(DEV)*3)
		Lower_bound = mean(DEV)-(sd(DEV)*3)

		Outliers.2 = names(which(DEV>Upper_bound | DEV<Lower_bound))


		# By usage of VARIANCE
		VARs = apply(data,2,sd)^2
		DEV = VARs-mean(VARs)

		Upper_bound = mean(DEV)+(sd(DEV)*3)
		Lower_bound = mean(DEV)-(sd(DEV)*3)

		Outliers.3 = names(which(DEV>Upper_bound | DEV<Lower_bound))

		# Combine outliers
		Outliers.all = c(Outliers.1,Outliers.2,Outliers.3)
		table(Outliers.all) # all which match 3 these should be removed..



		data = data[,-which(colnames(data)==names(which(table(Outliers.all)==3 )))]
		cat("Detected possible outliers:\n")
		cat(names(which(table(Outliers.all)==3)))
		cat("\n")
	}
}