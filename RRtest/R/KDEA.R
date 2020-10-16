#' KDEA
#'
#' Leave-p-Out Cross-Validation Differential Expression Analysis (KDEA or LpO CV DEA)
#' Purpose:
#' Repeated random-sampled Limma analysis is applied to get estimates of variability in DEA, then by selecting the most robust features,
#' improved follow-up analysis is possible. Please see details for required dataset structure!
#'
#' Dataset:
#' Requires a dataset with rows as samples and columns as features.
#' Rownames should contain the unique identifier of the samples.
#' It should contain a "Class" column to be able to detect classes. (OR use phenotypic data Classes)
#'
#' Phenotypic data:
#' Can be added to account for covariates using formula.
#' Requires rows as samples and columns as features.
#' Should contain a column with unique identifiers of the samples.
#' It should contain a "Class" column to be able to detect classes. (OR use dataset Classes)
#'
#' @param dataset Input the dataset object (row=sample; col=feature; 'class' column required).
#' @param f_dataset_class_column_id Index number of class column (eg. for iris dataset classcolumn ID is 5 (species)). Can be added to s_PhenoDataFrame.
#' @param s_PhenoDataFrame This is a dataframe containing the phenotypic observations, rows as samples, columns as features. Make sure one of columns has exact identifiers found in rownames(dataset)
#' @param s_omitNA Remove NAs from data.
#' @param s_partitionlength Percentage number used in resampling (0-1) (default 0.5).
#' @param s_k Amount of folds (auto generated based on sqrt(sampleNo).
#' @param s_pvalTH Significance threshold.
#' @param s_AmountSignTH Amount of times found to be significant throughout all folds threshold (integer).
#' @param s_logFCTH Minimum logFC required threshold (float).
#' @param s_CovFormula Defaults to "~Class"; can be changed, however, the column entered in f_dataset_class_column_id is called class, and in formula should be treated so. Example: in iris the function can be run using KDEA(iris,5). The fifth column ("species") will be renamed to "Class" therefore the formula should be "~Class" and not "~Species". Other columns keep their names, therefore "~Class+Sepal.Width" is possible. This function extracts the 1st term from the formula, this is set by s_CovOfImportance, which can be changed appropriately. 
#' @param s_CovOfImportance Automatically searches the correct "Class" column, usally 1; This is the column which is extracted after running limma::topTable. When s_CovFormula = NA then the first column is the LogFC column. When edited to eg "~Class+Sepal.Width", the first column is the "Class" FC and the second column is the effect of "Sepal.Width". Make sure this matches.
#' @param s_MakePlot Create plots (can take a long time when evaluating big datasets (50k+ features) set to TRUE (default)
#' @param s_Verbose Show more info if TRUE (default)
#'
#' @return A list of imporant features, a dataframe of ranked features, the plot, and indermediate results.
#' @importFrom magrittr "%>%"
#' @importFrom foreach `%dopar%`
#' @examples
#' KDEA(dataset = dataset, s_PhenoDataFrame = Phenotipic_data, s_CovFormula = "~Class + age + sex")
#' KDEA(dataset = mtcars, f_dataset_class_column_id = which(colnames(mtcars)=="am"), s_CovFormula = "~Class + gear")
#' KDEA(dataset = iris, f_dataset_class_column_id = 5, s_CovOfImportance = 1)
#' @export

KDEA = function(dataset = NA, f_dataset_class_column_id = NA, s_PhenoDataFrame = NA,s_omitNA = TRUE, s_partitionlength = 0.5, s_k = floor(sqrt(dim(dataset)[1])), s_pvalTH = 0.05, s_AmountSignTH = floor(s_k*0.3), s_logFCTH=NA, s_CovFormula = NA, s_CovOfImportance = NA, s_MakePlot=TRUE, s_Verbose=TRUE) {

#-----------------------------------------------------------------------------------------------------#
#							checks
#-----------------------------------------------------------------------------------------------------#
# it needs data
# if(is.na(dataset)){stop("Please enter dataset")}

# it cannot be more significant (number of times) than the amount of folds
s_AmountSignTH = min(s_AmountSignTH,s_k)



# check if phenotypic dataframe is added
if(!is.null(dim(s_PhenoDataFrame)[1])){

	# make sure s_PhenoDataFrame is dataframe
	s_PhenoDataFrame = as.data.frame(s_PhenoDataFrame)

	cat(paste0("Using phenotypic dataframe for Class and covariates (if needed):\nMake sure rownames(dataset) and rownames(s_PhenoDataFrame) use the same identifiers!!\n"))
	
	# test if dataframe
	if(!is.data.frame(s_PhenoDataFrame)){
		#cat(paste0("Please check if s_PhenoDataFrame is a data.frame!\n"))
		stop("Please check if s_PhenoDataFrame is a data.frame!\n")
	}
	
	if(!"Class"%in%colnames(s_PhenoDataFrame)){
		stop("No 'Class' found in colnames s_PhenoDataFrame")
	}
	
	#SampleOrder = match(rownames(dataset), rownames(s_PhenoDataFrame))[!is.na(match(rownames(dataset), rownames(s_PhenoDataFrame)))]
	#s_PhenoDataFrame = s_PhenoDataFrame[SampleOrder,]
	#
	#SampleOrder2 = match(rownames(s_PhenoDataFrame), rownames(dataset))[!is.na(match(rownames(s_PhenoDataFrame), rownames(dataset)))]
	
	
	# get the linking column between dataset and pheno
	IdNameColInPhenoDF = names(which(lapply(s_PhenoDataFrame,FUN = function(X){sum(as.character(rownames(dataset))%in%as.character(X))})>0)[1])
	
	# print found linker
	cat(paste0("Found column '",IdNameColInPhenoDF,"' as universal linker between pheno and data\n"))
	
	# Add the linker to rownames of frame, this is required for temp_design
	rownames(s_PhenoDataFrame) = as.character(s_PhenoDataFrame[[IdNameColInPhenoDF]])

	# Select class data based on phenotype PATNO
	if(sum(colnames(dataset)=="Class")==1){
		dataset$Class = s_PhenoDataFrame$Class[match(x=rownames(dataset),s_PhenoDataFrame[[IdNameColInPhenoDF]])]
		}else{
		dataset = data.frame(dataset, Class = s_PhenoDataFrame$Class[match(x=rownames(dataset),s_PhenoDataFrame[[IdNameColInPhenoDF]])])
	}
	
	# match s_PhenoDataFrame to dataset; as s_PhenoDataFrame is bigger and thus has different positions for Trainindex
	s_PhenoDataFrame = s_PhenoDataFrame[match(rownames(dataset),rownames(s_PhenoDataFrame)),]

}

## packages
#library(ggfortify)
#library(caret)
#library(limma)
#library(tidyverse)
#library(ggsci)
#library(showtext)

#-----------------------------------------------------------------------------------------------------#
#							Print settings
#-----------------------------------------------------------------------------------------------------#

if(s_Verbose){
	cat(paste0("Starting KDEA using:\n"))
	cat(paste0("Using formula: ",if(is.na(s_CovFormula)){"~Class"}else{s_CovFormula} ,"\n"))
	cat(paste0("Resamplefraction: ",s_partitionlength ,"\n"))
	cat(paste0("Folds: ",s_k ,"\n"))
	cat(paste0("Pval TH: ",s_pvalTH ,"\n"))
	#cat(paste0("LogFC TH: ",s_logFCTH ,"\n")) # automated now
	cat(paste0("Amount Sign TH: ",s_AmountSignTH ,"\n"))
}

#-----------------------------------------------------------------------------------------------------#
#							NAs + numeric
#-----------------------------------------------------------------------------------------------------#

if(s_omitNA){
	dataset = na.omit(dataset)
}

#-----------------------------------------------------------------------------------------------------#
#							INITIAL DATA CLASS check
#-----------------------------------------------------------------------------------------------------#

# class processing; find class column OR ID
if(length(which(names(dataset)=="Class"))==0){

	if(is.na(f_dataset_class_column_id)){
		stop("Please enter Class column ID")
		}else{
		names(dataset)[f_dataset_class_column_id] = "Class"
	}
}else{
	f_dataset_class_column_id = which(names(dataset)=="Class")
}
	
Class = as.factor(as.character(dataset[,f_dataset_class_column_id]))


#-----------------------------------------------------------------------------------------------------#
#							INITIAL DATA SPLITTING/RANDOMISATION
#-----------------------------------------------------------------------------------------------------#
set.seed(42)
FoldSamples = caret::createDataPartition(Class, p = s_partitionlength, list = FALSE, times = s_k)
res_super = list()
CounterOneTimeOnly = 0

if(is.na(s_CovFormula)){
# switch to intercept design, because other was not appropiate...
	s_CovFormula = "~Class"
}
		
		
# start fold loop
for( i in 1:s_k){

	#-----------------------------------------------------------------------------------------------------#
	#							Select data
	#-----------------------------------------------------------------------------------------------------#
	Trainindex = FoldSamples[,i]

	#-----------------------------------------------------------------------------------------------------#
	#							LIMMA
	#-----------------------------------------------------------------------------------------------------#

	# Make contrasts based on "~Class" or used defined
	# if a pheno DF os given:
	if(!is.null(dim(s_PhenoDataFrame)[1])){
		
		temp_design <- model.matrix(as.formula(s_CovFormula), data = s_PhenoDataFrame[Trainindex,])

		}else{
		
		temp_design <- model.matrix(as.formula(s_CovFormula), data = dataset[Trainindex,])
	}

	# Create a linear model based on the groups
	temp_fit <- limma::lmFit(t(dataset[Trainindex,-f_dataset_class_column_id]), temp_design)

	# Emperical bayes
	temp_fit <- limma::eBayes(temp_fit)
	
	# extract results
	temp_results = limma::topTable(temp_fit, adjust="BH",num=Inf)
	

	# Check if the focus unchanged, than find the "Class" column.
	if(is.na(s_CovOfImportance)){
		if(s_CovFormula=="~Class"){
			s_CovOfImportance = which(names(temp_results)=="logFC")
			}else{
			s_CovOfImportance = which(names(temp_results)=="Class")
			if(is.na(s_CovOfImportance[1])){
				s_CovOfImportance=1
				cat(paste0("s_CovOfImportance is FORCED to: ",s_CovOfImportance,"\n","This results in cov: ",names(temp_results)[s_CovOfImportance],"\n","ALERT: \nPlease consider setting the s_CovOfImportance","\n"))
			}
		}
		CounterOneTimeOnly = 1
	}else{
		# Check if the focus is shifted to "Class" or to another covariate; print these results.
		if(CounterOneTimeOnly==0){
			if(s_Verbose){
				cat(paste0("s_CovOfImportance is set to: ",s_CovOfImportance,"\n","This results in cov: ",names(temp_results)[s_CovOfImportance],"\n"))
			}
			CounterOneTimeOnly = 1
		}
	}
	
	# Store all results of fold [i] into super object using predefined structure
	res_super[[i]] = data.frame(names = rownames(temp_results),FC = temp_results[,s_CovOfImportance], Pval = temp_results$P.Value,stringsAsFactors=FALSE)
	

}

#-----------------------------------------------------------------------------------------------------#
#							process dataframe for ranks and plot
#-----------------------------------------------------------------------------------------------------#
# Get all superresult runs
df=data.frame(res_super[[1]])
for(i in 2:s_k){
	df = rbind(df,res_super[[i]])
}

# make it nicer to visualize
df$Pval = -log10(df$Pval)
df$Pval = round(df$Pval,4)

# Create secific data for result (medians of FC and Pval)
RankedOrderedData = data.frame(FeatureName = NA,MedianLogFC = NA,MedianLog10Pval = NA,MeanLogFC=NA,SDLogFC=NA)
temp = df
uniquenames = unique(df$names)

# find all unique names in temp, and give them a number
MatchedIndex <<- match(uniquenames,x=temp$names)

#create lists based on unique names
temp_lists1 = vector(mode = "list", length = length(uniquenames))

#parLapply, extract all locations in specific number (name)
	cores=parallel::detectCores()
	cl <- parallel::makeCluster(cores[1]-1) #not to overload computer

	parallel::clusterExport(cl, "MatchedIndex")

	temp_lists1 = parallel::parLapply(cl,1:length(uniquenames),function(i){
		which(MatchedIndex==i)
	})

	#stop cluster
	parallel::stopCluster(cl)

# per unique name locations, get these 
temp_lists = vector(mode = "list", length = length(uniquenames))
temp_lists = lapply(temp_lists1,function(x){
	temp[x,]
})

FeatureName = lapply(temp_lists,function(x){x[1,1]})
MedianLogFC = lapply(temp_lists,function(x){median(x[,2])})
MedianLog10Pval = lapply(temp_lists,function(x){median(x[,3])})

MeanLogFC = lapply(temp_lists,function(x){mean(x[,2])})
SDLogFC = lapply(temp_lists,function(x){sd(x[,2])})

RankedOrderedData = data.frame(FeatureName = unlist(FeatureName),MedianLogFC = unlist(MedianLogFC),MedianLog10Pval = unlist(MedianLog10Pval),MeanLogFC=unlist(MeanLogFC),SDLogFC=unlist(SDLogFC))

#for(i in 1:length(uniquenames)){
#	#tempIndex = temp$names==uniquenames[i]
#	tempdf = temp_lists[[i]]
#	 
#	RankedOrderedData[i,] = #data.frame(FeatureName=uniquenames[i],MedianLogFC=as.numeric(median(tempdf$FC)),Medi#anLog10Pval=as.numeric(median(tempdf$Pval)))
#	
#	print(i)
#
#}




#setup parallel backend to use many processors
#cores=parallel::detectCores()
#cl <- parallel::makeCluster(cores[1]-1) #not to overload your computer
#doParallel::registerDoParallel(cl)
#
#RankedOrderedData <- foreach::foreach(i=1:length(unique(df$names)), #.combine=rbind) %dopar% {
#  
#	FeatureName = unique(df$names)[i]
#  
#	temp_1 = df[df$names==FeatureName,]
#    
#	MedianLogFC = median(temp_1$FC)
#	
#	MedianLog10Pval = median(temp_1$Pval)
#  
#	tempMatrix = data.frame(FeatureName = FeatureName, MedianLogFC = #MedianLogFC,MedianLog10Pval=MedianLog10Pval)
#	
#	#tempMatrix = c(Featurename,MedianLogFC,MedianLog10Pval)
#
#   tempMatrix #Equivalent to finalMatrix = rbind(finalMatrix, tempMatrix)
#}
##stop cluster
#parallel::stopCluster(cl)
#
#
## Create secific data for result (medians of FC and Pval)
#RankedOrderedData = #data.frame(FeatureName=unique(df$names),MedianLogFC=NA,MedianLog10Pval=NA)



#-----------------------------------------------------------------------------------------------------#
#							RANKS
#-----------------------------------------------------------------------------------------------------#
# Calculate the amount of times to be observed significant, 
RankValue_Pval = rank(-abs(RankedOrderedData$MedianLog10Pval),ties.method = "first")

# this works out, median is approximation of pop; should be included
RankValue_LogFC = rank(-abs(RankedOrderedData$MedianLogFC),ties.method = "first")

# this works out, median is approximation of pop; should be included
RankValue_LogFC_mean = rank(-abs(RankedOrderedData$MeanLogFC),ties.method = "first")

# this works out, median is approximation of pop; should be included
RankValue_LogFC_SD = rank(-abs(RankedOrderedData$SDLogFC),ties.method = "first")


# combine the ranks
RankValue_Combined = (RankValue_LogFC_SD+RankValue_LogFC)

# Put in object
RankedOrderedData$RankLogFC = RankValue_LogFC
RankedOrderedData$RankLog10Pval = RankValue_Pval
RankedOrderedData$RankCombined = RankValue_Combined
RankedOrderedData$RankValue_LogFC_mean = RankValue_LogFC_mean
RankedOrderedData$RankValue_LogFC_SD = RankValue_LogFC_SD

# reorder object
RankedOrderedData = RankedOrderedData[order(RankedOrderedData$RankCombined),]



#-----------------------------------------------------------------------------------------------------#
#							Plot
#-----------------------------------------------------------------------------------------------------#

# Set data in format
	# at least (unlikly)
	df_f = df[df$Pval>-log10(s_pvalTH),]  
	
	
	if(is.na(s_logFCTH)){
		s_logFCTH = quantile(abs(df_f$FC),0.5)
	}
	
	# minimal logFC
	df_f = df_f[abs(df_f$FC)>s_logFCTH,]  
		 
	# filter on at least 7 SIGNIFICANT ocurrences
	df_f = df_f[df_f$names%in%names( table(df_f$names)[table(df_f$names)>s_AmountSignTH]),]
	#unique(df_f$names)
	
	# getthe features and get the nonsignificant as well
	df_f = df[df$names%in%unique(df_f$names),]
	
	#### CHECK ####
	if(dim(df_f)[1]==0){
	
		# gather all settings and dump in var for reproducebility
		allSettings  = ls(pattern="^s_")
		s_settings = NA
		temp = data.frame(Setting = allSettings, Value=NA)
		for(o in 1:length(allSettings)){
			if(allSettings[o]=="s_PhenoDataFrame"){next} # skip for now @RRR
			tempset = allSettings[o]
			tempval = NA
			tempval = get(tempset)
			#temp[o,1] = tempset
			temp[o,2] = tempval
		}
		s_settings = temp
		rm(temp)
	
		warning("No significant values to present, increase s_pvalTH")
		out=list(Rankobject = RankedOrderedData, Plot=NA, PlotFeatues = NA, res_super = res_super, Rawdata = df, formula = s_CovFormula, design = temp_design, settings = s_settings)
		return(out)
	}else{
		# check if plot needs to be made
		if(s_MakePlot){
			# present the results in table
			Plot = df_f  %>%
			dplyr::mutate(names = fct_reorder(names, abs(FC))) %>%
			ggplot2::ggplot(
				aes(x = names, y = FC, fill = Pval)
			)+
			ggplot2::scale_y_continuous(
				breaks = round(seq(-ceiling(max(abs(df_f$FC))),ceiling(max(abs(df_f$FC))),(ceiling(max(abs(df_f$FC)))/3)),2),
				limits = c(-ceiling(max(abs(df_f$FC))*10)/10, ceiling(max(abs(df_f$FC))*10)/10), expand =  c(0.01, 0.01)
			)+
			ggplot2::ggtitle(paste0("Robust features using ",names(temp_results)[s_CovOfImportance],"\n",s_CovFormula,"\n")
			)+
			#scale_color_manual(
			#	name="-log10(P-value)",
			#)+
			#scale_colour_discrete(
			#name = '-log10(P-value)', values = heat.colors(15),discrete = TRUE
			#)+
			ggplot2::scale_fill_gradientn(
			name="Significance\n-log10(P-value)",
				colours		= c("gray70","gray70","yellow","red"),
				#space		= "Lab",
				 #high		= "blue",
				 #low		= "yellow",
				 #limits 	= c(-log10(0.05), 5),
				 na.value	= "gray50",
				 values   	= c(0,((-log10(0.05)-0.000000001)-min(df_f$Pval))/(max(df_f$Pval)-min(df_f$Pval)),(-log10(0.05)-min(df_f$Pval))/(max(df_f$Pval)-min(df_f$Pval)),1),#,values   	= c(0,-log10(0.05)/5,5/5)#, (min(df_f$Pval)-min(df_f$Pval))/(max(df_f$Pval)-min(df_f$Pval))
				 breaks   	= round(c(-log10(0.05),2,3,4,max(df_f$Pval)),1)
			)+
			ggplot2::guides(
				fill = guide_colourbar(barwidth = 10, barheight = 1,ticks.colour = "black", ticks.linewidth = 0.8)
			)+
			ggplot2::geom_hline(
				yintercept = 0, colour="gray50",linetype = "longdash", alpha = 0.60#linetype = "shortdash",
			)+
			ggplot2::geom_jitter(
				size = 4, alpha = (df_f$Pval-min(df_f$Pval))/(max(df_f$Pval)-min(df_f$Pval))+0.2, width = 0.0, shape = 21
			)+
			ggplot2::stat_summary(
				fun = median, geom = "point", size = 7,colour = "black",alpha = 1, aes(shape="Median")
			#)+
			#	scale_shape_manual(
			#		"", values=c("Median"="I")
			)+
			ggplot2::stat_summary(
				fun = mean, geom = "point", size = 3,colour = "black",alpha = 1, aes(shape="Mean")
			)+
			ggplot2::scale_shape_manual(
					"", values=c("Mean"="bullet","Median"="I")
			)+
			#geom_hline(
			#	yintercept = c(-0.5), colour="gray50",alpha = c(0.4)#linetype = "shortdash",
			#)+
			#geom_hline(
			#	yintercept = c(0.5), colour="gray50",alpha = c(0.4)#linetype = "shortdash",
			#)+		
			ggplot2::labs(
				caption = NULL, x = "Feature labels", y = "Fold change (log2)"
			)+ggplot2::theme(
				# Font sizes
				strip.text.x = element_text(size = 12),
				text = element_text(size=14),
				legend.text=element_text(size = 12),
				legend.position="bottom"
			)+
			ggplot2::coord_flip() 
			
			res_plotfeatures = unique((df_f[,1]))
			
			
			# gather all settings and dump in var for reproducebility
			allSettings  = ls(pattern="^s_")
			s_settings = NA
			temp = data.frame(Setting = allSettings, Value=NA)
			for(o in 1:length(allSettings)){
				if(allSettings[o]=="s_PhenoDataFrame"){next} # skip for now @RRR
				tempset = allSettings[o]
				tempval = NA
				tempval = get(tempset)
				#temp[o,1] = tempset
				temp[o,2] = tempval
			}
			s_settings = temp
			rm(temp)
			
			
			print(Plot)
			out=list(Rankobject = RankedOrderedData, Plot=Plot, PlotFeatues = res_plotfeatures, res_super = res_super, Rawdata = df, formula = s_CovFormula, design = temp_design, settings = s_settings)
			
			
		}else{
			# if no plot is made then do:
			
			# gather all settings and dump in var for reproducebility
			allSettings  = ls(pattern="^s_")
			s_settings = NA
			temp = data.frame(Setting = allSettings, Value=NA)
			for(o in 1:length(allSettings)){
				if(allSettings[o]=="s_PhenoDataFrame"){next} # skip for now @RRR
				tempset = allSettings[o]
				tempval = NA
				tempval = get(tempset)
				#temp[o,1] = tempset
				temp[o,2] = tempval
			}
			s_settings = temp
			rm(temp)
			
			out=list(Rankobject = RankedOrderedData, Plot=NA, PlotFeatues = NA, res_super = res_super, Rawdata = df, formula = s_CovFormula, design = temp_design, settings = s_settings)
		}
		return(out)
	}
		
}
