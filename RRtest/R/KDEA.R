#' KDEA
#'
#' K-fold Differential Expression Analysis (KDEA)
#' This function required a dataset with rows as samples and columns as features.
#' It should contain a "class" column to be able to detect classes. repeated random-sampled Limma analysis 
#' is applied to get estimates of variability in DEA, then by selecting the most robust features,
#' improved follow-up analysis is possible.
#'
#' @param dataset Input the dataset object (row=sample; col=feature; 'class' column required).
#' @param f_dataset_class_column_id Index number of class column (eg. for iris dataset classcolumn ID is 5 (species)).
#' @param s_omitNA Remove NAs from data.
#' @param s_partitionlength Percentage number used in resampling (0-1).
#' @param s_k Amount of folds (auto generated based on sqrt(sampleNo).
#' @param s_pvalTH Significance threshold.
#' @param s_AmountSignTH Amount of times found to be significant throughout all folds threshold (integer).
#' @param s_logFCTH Minimum logFC required threshold (float).
#' @param s_CovFormula Defaults to "~Class"; can be changed, however, the column entered in f_dataset_class_column_id is called class, and in formula should be treated so. Example: in iris the function can be run using KDEA(iris,5). The fifth column ("species") will be renamed to "Class" therefore the formula should be "~Class" and not "~Species". Other columns keep their names, therefore "~Class+Sepal.Width" is possible. This function extracts the 1st term from the formula, this is set by s_CovOfImportance, which can be changed appropriately. 
#' @param s_CovOfImportance Automatically searches the correct "Class" column, usally 1; This is the column which is extracted after running limma::topTable. When s_CovFormula = NA then the first column is the LogFC column. When edited to eg "~Class+Sepal.Width", the first column is the "Class" FC and the second column is the effect of "Sepal.Width". Make sure this matches.
#' @param s_Verbose Show more info if TRUE (default)
#'
#' @return A list of imporant features, a dataframe of ranked features, the plot, and indermediate results.
#' @importFrom magrittr "%>%"
#' @examples
#' KDEA(dataset = mtcars,f_dataset_class_column_id = which(colnames(mtcars)=="am"),s_CovFormula = "~Class + gear")
#' @export

KDEA = function(dataset = NA, f_dataset_class_column_id = NA, s_omitNA = TRUE, s_partitionlength = 0.8, s_k = floor(sqrt(dim(dataset)[1])), s_pvalTH = 0.05, s_AmountSignTH = floor(s_k*0.75), s_logFCTH=1, s_CovFormula = NA, s_CovOfImportance = NA, s_Verbose=TRUE) {

#-----------------------------------------------------------------------------------------------------#
#							checks
#-----------------------------------------------------------------------------------------------------#
# it needs data
# if(is.na(dataset)){stop("Please enter dataset")}

# it cannot be more significant (number of times) than the amount of folds
s_AmountSignTH = min(s_AmountSignTH,s_k)

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
	cat(paste0("LogFC TH: ",s_logFCTH ,"\n"))
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
res_logFC = data.frame(Feature=colnames(dataset)[1:((dim(dataset)[2])-1)])
res_Pval = data.frame(Feature=colnames(dataset)[1:((dim(dataset)[2])-1)])
CounterOneTimeOnly = 0
# start fold loop
for( i in 1:s_k){

	#-----------------------------------------------------------------------------------------------------#
	#							Select data
	#-----------------------------------------------------------------------------------------------------#
	Trainindex = FoldSamples[,i]
	#Testindex = (1:length(Class))[-FoldSamples[,i]]
	
	#data_train = dataset[Trainindex,]
	#data_test = dataset[Testindex,]
	
	#-----------------------------------------------------------------------------------------------------#
	#							LIMMA
	#-----------------------------------------------------------------------------------------------------#


	# set design matrix to no intercept; only groups 1 & 2
	#Class =  as.factor(Class)

	# Set the design to find the groups
	# temp_design <- model.matrix(~0+ Group, data = relevantAnnotationOrdered)
	# colnames(temp_design) <- c("Group1","Group2")

	# Make contrasts based on "~Class" or used defined
	if(is.na(s_CovFormula)){
	# switch to intercept design, because other was not appropiate...
		temp_design <- model.matrix(~Class, data = dataset[Trainindex,])
		}else{
		temp_design <- model.matrix(as.formula(s_CovFormula), data = dataset[Trainindex,])
	}

	# Create a linear model based on the groups
	temp_fit <- limma::lmFit(t(dataset[Trainindex,-f_dataset_class_column_id]), temp_design)

	# Emperical bayes
	temp_fit <- limma::eBayes(temp_fit)
	
	# extract results
	temp_results = limma::topTable(temp_fit, adjust="BH",num=Inf)
	
	# Check if the focus is shifted to "Class" or to another covariate; print these results.
	if(!is.na(s_CovOfImportance)){
		if(CounterOneTimeOnly==0){
			if(s_Verbose){
				cat(paste0("s_CovOfImportance is set to: ",s_CovOfImportance,"\n","This results in cov: ",names(temp_results)[s_CovOfImportance],"\n"))
			}
			CounterOneTimeOnly = 1
		}
	}
	
	# Check if the focus unchanged, than find the "Class" column.
	if(is.na(s_CovOfImportance)){
		s_CovOfImportance = which(names(temp_results)=="Class")
		CounterOneTimeOnly = 1
	}
	
	# Store all results of fold [i] into super object using predefined structure
	res_super[[i]] = data.frame(names = rownames(temp_results),FC = temp_results[,s_CovOfImportance], Pval = temp_results$P.Value,stringsAsFactors=FALSE)
	
	# Store only logFC results of fold [i] into super object using predefined structure
	res_logFC[,(i+1)] = as.numeric(temp_results[match(as.character(res_logFC$Feature),x=rownames(temp_results)),s_CovOfImportance])
	
	# Store only Pvalue results of fold [i] into super object using predefined structure
	res_Pval[,(i+1)] = as.numeric(temp_results$P.Value[match(as.character(res_Pval$Feature),x=rownames(temp_results))])

}

#-----------------------------------------------------------------------------------------------------#
#							process dataframe
#-----------------------------------------------------------------------------------------------------#

# Format the predefined structure, rownames = featurenames
rownames(res_logFC) = res_logFC$Feature
res_logFC = res_logFC[,-1]

# Format the predefined structure, rownames = featurenames
rownames(res_Pval) = res_Pval$Feature
res_Pval = res_Pval[,-1]

# Calulate some metrics on LogFC
for( i in 1:(dim(res_logFC)[1])){
	res_logFC[i,"Median"] = median(as.numeric(res_logFC[i,1:s_k]))
	res_logFC[i,"SD"] = sd(as.numeric(res_logFC[i,1:s_k]))
	res_logFC[i,"mean"] = mean(as.numeric(res_logFC[i,1:s_k]))
	res_logFC[i,"min"] = min(as.numeric(res_logFC[i,1:s_k]))
	res_logFC[i,"max"] = max(as.numeric(res_logFC[i,1:s_k]))
}

# Calulate some metrics on Pvalue
for( i in 1:(dim(res_Pval)[1])){
	res_Pval[i,"Median"] = median(as.numeric(res_Pval[i,1:s_k]))
	res_Pval[i,"mean"] = mean(as.numeric(res_Pval[i,1:s_k]))
	res_Pval[i,"min"] = min(as.numeric(res_Pval[i,1:s_k]))
	res_Pval[i,"max"] = max(as.numeric(res_Pval[i,1:s_k]))
	res_Pval[i,"AmountSign"] = sum(res_Pval[i,1:s_k]<=s_pvalTH)
}


#-----------------------------------------------------------------------------------------------------#
#							RANKS
#-----------------------------------------------------------------------------------------------------#
# Calculate the amount of times to be observed significant, 
RankValue_Pval = rank(-res_Pval[,"AmountSign"] , ties.method ="max")
RankValue_Pval = RankValue_Pval/max(abs(RankValue_Pval))

RankValue_LogFC = rank(-abs(res_logFC[,"Median"] ), ties.method ="max")
RankValue_LogFC = RankValue_LogFC/max(abs(RankValue_LogFC))

RankValue_Combined = rank(RankValue_Pval+RankValue_LogFC, ties.method ="max")
RankValue_OrderCombined = order(RankValue_Combined)


RankedOrderedData = data.frame(Features=rownames(res_logFC),ValuePvalAmountSig = res_Pval[,"AmountSign"], RankPval = RankValue_Pval, ValueMedianLogFC = res_logFC[,"Median"], RankLogFC = RankValue_LogFC, ValueLogFCSD = res_logFC[,"SD"], RankCombined = RankValue_Combined)

RankedOrderedData = RankedOrderedData[RankValue_OrderCombined,]


#-----------------------------------------------------------------------------------------------------#
#							Plot
#-----------------------------------------------------------------------------------------------------#


df=data.frame(res_super[[1]])
for(i in 2:s_k){
	df = rbind(df,res_super[[i]])
}

df$Pval = -log10(df$Pval)
df$Pval = round(df$Pval,1)
#df$names = gsub(df$names,pattern = "sp_|_.*?_HUMAN$|_.*?_HUMAN",replacement = "")
# now for the image


# Set data in format
	# at least (unlikly)
	df_f = df[df$Pval>-log10(s_pvalTH),]  
	
	# minimal logFC
	df_f = df_f[abs(df_f$FC)>s_logFCTH,]  
		 
	# filter on at least 7 SIGNIFICANT ocurrences
	df_f = df_f[df_f$names%in%names( table(df_f$names)[table(df_f$names)>s_AmountSignTH]),]
	#unique(df_f$names)
	
	# getthe features and get the nonsignificant as well
	df_f = df[df$names%in%unique(df_f$names),]
	
	#### CHECK ####
	if(dim(df_f)[1]==0){
		warning("No significant values to present, increase s_pvalTH")
		out=list(Rankobject = RankedOrderedData, res_super = res_super)
		return(out)
	}else{
		
		# present the results in table
		Plot = df_f  %>%
		dplyr::mutate(names = fct_reorder(names, abs(FC))) %>%
		ggplot2::ggplot(
			aes(x = names, y = FC, fill = Pval)
		)+
		ggplot2::scale_y_continuous(
			breaks = seq(-ceiling(max(abs(df_f$FC))),ceiling(max(abs(df_f$FC))),(ceiling(max(abs(df_f$FC)))/10)),
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
		
		
		#-----------------------------------------------------------------------------------------------------#
		#							Get all metrics (settings)
		#-----------------------------------------------------------------------------------------------------#
		# gather all settings and dump in var for reproducebility
		allSettings  = ls(pattern="^s_")
		temp = data.frame(Setting = allSettings, Value=NA)
		for(o in 1:length(allSettings)){
			tempset = allSettings[o]
			tempval = get(tempset)
			#temp[o,1] = tempset
			temp[o,2] = tempval
		}
		s_settings = temp
		rm(temp)
		
		out=list(Rankobject = RankedOrderedData, Plot=Plot, PlotFeatues = res_plotfeatures, res_super = res_super, formula = s_CovFormula, settings = s_settings)
		print(Plot)
		return(out)
	}
		
}
