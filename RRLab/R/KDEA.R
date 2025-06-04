#' KDEA
#' @usage
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
#' @param s_AmountSignTH Amount of times found to be significant throughout all folds threshold (integer). Set by default to roughly 30 percent of amount of folds
#' @param s_showall Ignore set cutoff and show all results, but can get messy. Logical (Default to FALSE)
#' @param s_logFCTH Minimum logFC required threshold (float).
#' @param s_CovFormula Defaults to "~Class"; can be changed, however, the column entered in f_dataset_class_column_id is called class, and in formula should be treated so. Example: in iris the function can be run using KDEA(iris,5). The fifth column ("species") will be renamed to "Class" therefore the formula should be "~Class" and not "~Species". Other columns keep their names, therefore "~Class+Sepal.Width" is possible. This function extracts the 1st term from the formula, this is set by s_CovOfImportance, which can be changed appropriately. 
#' @param s_CovOfImportance Automatically searches the correct "Class" column, usally 1; This is the column which is extracted after running limma::topTable. When s_CovFormula = NA then the first column is the LogFC column. When edited to eg "~Class+Sepal.Width", the first column is the "Class" FC and the second column is the effect of "Sepal.Width". Make sure this matches.
#' @param s_MakePlot Create plots (can take a long time when evaluating big datasets (50k+ features) set to TRUE (default)
#' @param s_Verbose Show more info if TRUE (default)
#' @param s_block_coef Block for this variable, should be defined in s_PhenoDataFrame. 
#' @param s_n_reps Used when blocking, amount of repeats to fast estimate duplicatecorrelation(limma)
#' @param s_n_features Used when blocking, number of samples used to fast estimate duplicatecorrelation(limma)
#' 
#' @import data.table
#' @importFrom magrittr "%>%"
#' @examples
#'
#' Intended use
#'
#' KDEA(dataset = dataset, s_PhenoDataFrame = Phenotipic_data, s_CovFormula = "~Class + age + sex")
#'
#'
#' Example using IRIS
#'
#' KDEA(dataset = iris, f_dataset_class_column_id = 5, s_CovOfImportance = 1)
#'
#'
#' Example using mtcars
#'
#' KDEA(dataset = mtcars, f_dataset_class_column_id = which(colnames(mtcars)=="am"), s_CovFormula = "~Class + gear")
#' 
#'
#' Example using transcriptiomic data
#'
#' Get data from mixOmics
#' RRLab::libraryR(mixOmics)
#' data(stemcells)
#'
#' KDEA(dataset = data.frame((stemcells$gene),Class = stemcells$celltype), f_dataset_class_column_id = stemcells$celltype,s_CovOfImportance = 1 , s_CovFormula = "~ Class")
#' 
#' Example using transcriptomic data and blocking study
#' 
#' KDEA(dataset = data.frame(stemcells$gene),s_PhenoDataFrame = data.frame(row_names = rownames(stemcells$gene),Block = stemcells$study, Class = stemcells$celltype=="Fibroblast",Stratify = as.numeric(stemcells$celltype == "hiPSC")),s_block_coef = "Block", f_dataset_class_column_id = stemcells$celltype,s_CovOfImportance = 1 , s_CovFormula = "~ Class + Stratify")
#' 
#' #' Example using transcriptomic data AND blocking study AND stratified sampling using formula
#' 
#' KDEA(dataset = data.frame(stemcells$gene),s_PhenoDataFrame = data.frame(row_names = rownames(stemcells$gene),Block = stemcells$study, Class = stemcells$celltype=="Fibroblast",Stratify = as.numeric(stemcells$celltype == "hiPSC")),s_block_coef = "Block", f_dataset_class_column_id = stemcells$celltype,s_CovOfImportance = 1 , s_CovFormula = "~ Class + Stratify", s_stratifiedsampling = TRUE)
#' 
#' @export

KDEA = function(dataset = NA, f_dataset_class_column_id = NA, s_PhenoDataFrame = NULL, s_logFCTH=NA, s_CovFormula = NA, s_block_coef = NA, s_CovOfImportance = NA, 
s_k = floor(sqrt(dim(dataset)[1])), s_AmountSignTH = floor(s_k*0.3), s_showall=FALSE, s_MakePlot=TRUE, s_Verbose=TRUE, s_pvalTH = 0.05, s_partitionlength = 0.5,
s_n_reps = min(25,max(floor(sqrt(dim(dataset)[2])),10)), s_n_features = max(10,floor(length(s_partitionlength*dim(dataset)[1])*0.5)),s_omitNA = TRUE, s_stratifiedsampling = FALSE) {

	# i have no idea how data.table does its operations using syntax with "." (like a[, .(mean = mean(V1))])
	# But its absolutely essential its loaded. so seeting a check to get datatable (i know this is undesired but please show me how elsewise)
	if (!"data.table" %in% .packages(all.available = TRUE)) {
  		library("data.table")
	}
	
	# start timer
	t0 = Sys.time()

	# get ggplot because it errors???
	#require(ggplot2)
	# Require data table as it is needed later and i cant call the functions directly
	#library(data.table)
	#require(splitstackshape)
	#-----------------------------------------------------------------------------------------------------#
	#							checks
	#-----------------------------------------------------------------------------------------------------#
	# it needs data
	# if(is.na(dataset)){stop("Please enter dataset")}

	# Check if blocking variable is found, then do set s_Limma_option and block for 
	if(is.na(s_block_coef)){
		s_Limma_option = 1
		}else{
		if(!is.null(s_PhenoDataFrame )){
			s_Limma_option = 2
			}else{
			stop("\n >>> Please check if s_PhenoDataFrame is added to extract the blocking variable! <<< \n")
		}
		
	}

	# it cannot be more significant (number of times) than the amount of folds
	s_AmountSignTH = min(s_AmountSignTH,s_k)

	# check if phenotypic dataframe is added
	if(!is.null(s_PhenoDataFrame )){

		# make sure s_PhenoDataFrame is dataframe
		s_PhenoDataFrame = as.data.frame(s_PhenoDataFrame)

		message(paste0("Using phenotypic dataframe for Class and covariates (if needed):\nMake sure rownames(dataset) and rownames(s_PhenoDataFrame) use the same identifiers!!\n"))
		
		# test if dataframe
		if(!is.data.frame(s_PhenoDataFrame)){
			#cat(paste0("Please check if s_PhenoDataFrame is a data.frame!\n"))
			stop("\n >>> Please check if s_PhenoDataFrame is a data.frame! <<< \n")
		}
		
		if(!"Class"%in%colnames(s_PhenoDataFrame)){
			stop("\n >>> No 'Class' found in colnames s_PhenoDataFrame <<< \n")
		}
			
		# get the linking column between dataset and pheno
		IdNameColInPhenoDF = names(which(lapply(s_PhenoDataFrame,FUN = function(X){sum(as.character(rownames(dataset))%in%as.character(X))})>0)[1])
		
		# print found linker
		message(paste0("Found column '",IdNameColInPhenoDF,"' as universal linker between pheno and data\n"))
		
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

	#-----------------------------------------------------------------------------------------------------#
	#							Print settings
	#-----------------------------------------------------------------------------------------------------#

	if(s_Verbose){
		message(paste0("Starting KDEA using:"))
		message(paste0("    Using formula: ",if(is.na(s_CovFormula)){"~Class"}else{s_CovFormula} ,""))
		message(paste0("    Resamplefraction: ",s_partitionlength ,""))
		message(paste0("    Folds: ",s_k ,""))
		message(paste0("    Pval TH: ",s_pvalTH ,""))
		#cat(paste0("LogFC TH: ",s_logFCTH ,"\n")) # automated now
		message(paste0("    Amount Sign TH: ",s_AmountSignTH ,""))

		# if blocking show what
		if(!is.na(s_block_coef)){
			message(paste0("    Blocking: ",length(table(s_PhenoDataFrame[,s_block_coef]))," entities"))
		}

		if(s_stratifiedsampling){
			message(paste0("    Using stratified sampling coefs: ",paste0(all.vars(as.formula(s_CovFormula)),collapse = ", ") ,""))
		}
		
	}


	#-----------------------------------------------------------------------------------------------------#
	#							NAs + numeric + Data.table
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
			stop("\n >>> Please enter Class column ID <<< \n")
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
	res_super = list()
	res_super_raw = list()
	CounterOneTimeOnly = 0

	if(is.na(s_CovFormula)){
	# switch to intercept design, because other was not appropiate...
		s_CovFormula = "~Class"
	}

	set.seed(42)
	# Strietified option
	if(!s_stratifiedsampling){
		FoldSamples = caret::createDataPartition(Class, p = s_partitionlength, list = FALSE, times = s_k)
		
	}else{
		stop("\n >>> @RRR Stratified sampling needs to be fixed, please turn 's_stratifiedsampling' to FALSE <<< \n")
		FoldSamples = sapply(1:s_k,function(x){which(rownames(s_PhenoDataFrame) %in% splitstackshape::stratified(indt = s_PhenoDataFrame, 
		c(all.vars(as.formula(s_CovFormula))),size = s_partitionlength,keep.rownames = TRUE)$rn)})
		
		if(s_Verbose){
			message(paste0("    Foldsamples dimention: ",paste0(dim(FoldSamples),collapse = " x ") ,""))
		}
	}		

	#-----------------------------------------------------------------------------------------------------#
	#							duplicateCorrelation Estimation for BLOCKING
	#-----------------------------------------------------------------------------------------------------#
	if(!is.na(s_block_coef )){
		# start duplicatecorrelation progressbar
		pb <- progress:: progress_bar$new(
		format = "    Estimating duplicatecorrelation [:bar] :percent eta: :eta",
		total = s_n_reps, clear = FALSE, width= 60)

		if(s_Verbose){
			#message(paste0("    Using ",s_n_reps," repeats and ",s_n_features," samples in duplicatecorrelation"))
			message(paste0("    Duplicatecorrelation n repeats: ",s_n_reps))
			message(paste0("    Duplicatecorrelation n samples: ",s_n_features))
		}

		# BLOCKING out of loop estimator
		# This is a partial information leak, but likely not critical as only consensus correlation is used
		# fast estimate conesnsus
		# into arguments
		#s_n_reps = min(25,max(floor(sqrt(dim(dataset)[2])),10)) # min 10, max 25; compensating for low nsamples to get bigger resampling, works and tested; max 100; min related to amount of features
		#s_n_features = max(10,floor(length(Trainindex)*0.5)) # nsamples min is 10, the more the better; takes 50 percent!

		#cat("\n      Fast estimating consensus for duplicatecorrelation\n")
		#cat(paste0("\n      Using: ",s_n_reps," repeats x ",s_n_features," samples\n"))

		# This part takes most of the time in this part of the function. (testing folds)
		# due to duplicaeCorrelation being slow!
		a = na.omit(sapply(rep(s_n_features,s_n_reps),
			function(p){
				sample_set = sample(x = 1:dim(dataset)[2],p)
				loop_object = t(dataset[,sample_set])
				loop_design = model.matrix(as.formula(s_CovFormula),s_PhenoDataFrame)
				loop_block = s_PhenoDataFrame[,s_block_coef]

				
				# up the pb
				pb$tick()

				return(limma::duplicateCorrelation( object = loop_object, design = loop_design, block=loop_block)$consensus)
			}
		))
		#plot(a)
		#abline(h= median(a));median(a)
		Dup_correlation_consensus_median = median(a)

		# print stats
		if(s_Verbose){
			message(paste0("    Duplicatecorrelation estimate median (IQR): ",round(Dup_correlation_consensus_median,3) ," (",round(IQR(a),3)),")")
		}
	}


	# start fold loop progressbar
	pb <- progress:: progress_bar$new(
	format = "    Testing folds [:bar] :percent eta: :eta",
	total = s_k, clear = FALSE, width= 60)

	for( i in 1:s_k){


		#-----------------------------------------------------------------------------------------------------#
		#							Select data
		#-----------------------------------------------------------------------------------------------------#
		Trainindex = FoldSamples[,i]


		#-----------------------------------------------------------------------------------------------------#
		#							LIMMA OPTION 2
		#							BLOCKing
		#-----------------------------------------------------------------------------------------------------#
		if(s_Limma_option == 2){
			# check if i have all items needed to run the normal limma route
			# @RRR check s_block_coef is not NA

			# Make contrasts based on "~Class" or used defined
			# if a pheno DF os given:
			if(!is.null(s_PhenoDataFrame )){
				
				temp_design <- model.matrix(as.formula(s_CovFormula), data = s_PhenoDataFrame[Trainindex,])

				}else{
				
				temp_design <- model.matrix(as.formula(s_CovFormula), data = dataset[Trainindex,])
			}

			#plot(a)
			#cat(paste0("\n      Duplicatecorrelation estimated at: ",round(Dup_correlation_consensus_median,4),"\n"))

			# Create a linear model based on the groups
			temp_fit <- limma::lmFit(t(dataset[Trainindex,-f_dataset_class_column_id]), temp_design, block = s_PhenoDataFrame[Trainindex,s_block_coef], correlation = Dup_correlation_consensus_median)

			# Emperical bayes
			temp_fit <- limma::eBayes(temp_fit)
			
			# extract results
			if(colnames(temp_fit)[1]=="(Intercept)"){
				temp_results = limma::topTable(temp_fit, coef =2, adjust="BH",num=Inf)
			}else{
				temp_results = limma::topTable(temp_fit, adjust="BH",num=Inf)
			}
			
			# Check if the focus unchanged, than find the "Class" column.
			if(is.na(s_CovOfImportance)){
				if(s_CovFormula=="~Class"){
					s_CovOfImportance = which(names(temp_results)=="logFC")
					}else{
					s_CovOfImportance = which(names(temp_results)=="Class")
					if(is.na(s_CovOfImportance[1])){
						s_CovOfImportance=1
						message(paste0(" !  s_CovOfImportance is FORCED to: ",s_CovOfImportance,"\n"," !      This results in cov: ",names(temp_results)[s_CovOfImportance],"\n"," >    Please consider setting the s_CovOfImportance",""))
					}
				}
				CounterOneTimeOnly = 1
			}else{
				# Check if the focus is shifted to "Class" or to another covariate; print these results.
				if(CounterOneTimeOnly==0){
					if(s_Verbose){
						message(paste0("    s_CovOfImportance is set to: ",s_CovOfImportance,"\n","      This results in cov: ",names(temp_results)[s_CovOfImportance],""))
					}
					CounterOneTimeOnly = 1
				}
			}
			
			# Store all results of fold [i] into super object using predefined structure
			res_super[[i]] = data.frame(names = rownames(temp_results),FC = temp_results[,s_CovOfImportance], Pval = temp_results$P.Value,stringsAsFactors=FALSE)
			
			# Store all results of fold [i] into super object using predefined structure
			res_super_raw[[i]] = list(toptable = temp_results, s2post = temp_fit$s2.post)
			
			# up the pb
			pb$tick()

		} # End limma option 2 (Blocking)

		#-----------------------------------------------------------------------------------------------------#
		#							LIMMA OPTION 1
		#							Normal
		#-----------------------------------------------------------------------------------------------------#
		if(s_Limma_option == 1){
			# Make contrasts based on "~Class" or used defined
			# if a pheno DF os given:
			if(!is.null(s_PhenoDataFrame )){
				
				temp_design <- model.matrix(as.formula(s_CovFormula), data = s_PhenoDataFrame[Trainindex,])

				}else{
				
				temp_design <- model.matrix(as.formula(s_CovFormula), data = dataset[Trainindex,])
			}

			# Create a linear model based on the groups
			temp_fit <- limma::lmFit(t(dataset[Trainindex,-f_dataset_class_column_id]), temp_design)

			# Emperical bayes
			temp_fit <- limma::eBayes(temp_fit)
			
			# extract results
			if(colnames(temp_fit)[1]=="(Intercept)"){
				temp_results = limma::topTable(temp_fit, coef =2, adjust="BH",num=Inf)
			}else{
				temp_results = limma::topTable(temp_fit, adjust="BH",num=Inf)
			}
			
			# Check if the focus unchanged, than find the "Class" column.
			if(is.na(s_CovOfImportance)){
				if(s_CovFormula=="~Class"){
					s_CovOfImportance = which(names(temp_results)=="logFC")
					}else{
					s_CovOfImportance = which(names(temp_results)=="Class")
					if(is.na(s_CovOfImportance[1])){
						s_CovOfImportance=1
						message(paste0(" !  s_CovOfImportance is FORCED to: ",s_CovOfImportance,"\n"," !    This results in cov: ",names(temp_results)[s_CovOfImportance],"\n"," >    Please consider setting the s_CovOfImportance",""))
					}
				}
				CounterOneTimeOnly = 1
			}else{
				# Check if the focus is shifted to "Class" or to another covariate; print these results.
				if(CounterOneTimeOnly==0){
					if(s_Verbose){
						message(paste0("    s_CovOfImportance is set to: ",s_CovOfImportance,"\n","    This results in cov: ",names(temp_results)[s_CovOfImportance],""))
					}
					CounterOneTimeOnly = 1
				}
			}
			
			# Store all results of fold [i] into super object using predefined structure
			res_super[[i]] = data.frame(names = rownames(temp_results),FC = temp_results[,s_CovOfImportance], Pval = temp_results$P.Value, Fit_S2post = temp_fit$s2.post, stringsAsFactors=FALSE)
			
			# Store all results of fold [i] into super object using predefined structure
			res_super_raw[[i]] = temp_results
			
			# up the pb
			pb$tick()

		} # End limma option 1 (standard)

	} # End fold loop


	#-----------------------------------------------------------------------------------------------------#
	#							Prepare for results extraction
	#-----------------------------------------------------------------------------------------------------#
	# Get all superresult runs
	df=data.frame(res_super[[1]])
	for(i in 2:s_k){
		df = rbind(df,res_super[[i]])
	}


	# round pvalues (max significance to 6)
	df$Pval = round(df$Pval,6)

	# make interpretable
	vals = -log10(df$Pval)
	df$Pval = vals

	# Make any pvalue that is infinite to the highest Pvalue observed
	df$Pval[is.infinite(df$Pval)] = max(df$Pval[!is.infinite(df$Pval)])



	# Create secific data for result (medians of FC and Pval)
	RankedOrderedData = data.frame(FeatureName = NA,MedianLogFC = NA,MedianLog10Pval = NA,MeanLogFC=NA,SDLogFC=NA)
	temp = df
	uniquenames = unique(df$names)

	# find all unique names in temp, and give them a number
	MatchedIndex <<- match(uniquenames,x=temp$names)

	#-----------------------------------------------------------------------------------------------------#
	#							Extract result from all folds and calculates stats
	# 							Data.table is amazing
	#-----------------------------------------------------------------------------------------------------#
	# Convert 'temp' to a data.table
	temp2 <- as.data.table(temp)

	# make pb for Collecting results
	pb <- progress:: progress_bar$new(
	format = "    Collecting results [:bar] :percent eta: :eta",
	total = length(uniquenames), clear = FALSE, width= 60)
	pb$tick(0)

	# Group by MatchedIndex and calculate the desired statistics
	# This. is. speed. (from 8 hours to 10 minutes)
	RankedOrderedData <- temp2[, {pb$tick(); .(MedianLogFC = median(FC),
								MedianLog10Pval = median(Pval),
								MeanLogFC = mean(FC),
								SDLogFC = sd(FC),
								IQRLogFC = IQR(FC))},
							by = .(FeatureName = names, MatchedIndex),]

	# Optionally, if you want to keep unique rows based on MatchedIndex, you can use:
	RankedOrderedData <- unique(RankedOrderedData, by = "MatchedIndex")



	#-----------------------------------------------------------------------------------------------------#
	#							Calculate RANKS
	#-----------------------------------------------------------------------------------------------------#
	# Calculate the amount of times to be observed significant, 
	RankValue_Pval = rank(-abs(RankedOrderedData$MedianLog10Pval))

	# this works out, median is approximation of pop; should be included
	RankValue_LogFC = rank(-abs(RankedOrderedData$MedianLogFC))

	# this works out, median is approximation of pop; should be included
	RankValue_LogFC_mean = rank(-abs(RankedOrderedData$MeanLogFC))

	# this works out, median is approximation of pop; should be included
	RankValue_LogFC_SD = rank(-abs(RankedOrderedData$SDLogFC))

	# combine the ranks
	RankValue_Combined = (RankValue_Pval+RankValue_LogFC)

	# Best estimation; Small spread (IQR). high logFC. high sign.
	RankValue_bestRank = rank(RankedOrderedData$IQRLogFC) + rank(-abs(RankedOrderedData$MeanLogFC)) + rank(-RankedOrderedData$MedianLog10Pval)

	# Put in object
	RankedOrderedData$RankLogFC = RankValue_LogFC
	RankedOrderedData$RankLog10Pval = RankValue_Pval
	RankedOrderedData$RankCombined = RankValue_Combined
	RankedOrderedData$RankValue_LogFC_mean = RankValue_LogFC_mean
	RankedOrderedData$RankValue_LogFC_SD = RankValue_LogFC_SD
	RankedOrderedData$Best_Rank = RankValue_bestRank

	# reorder object
	RankedOrderedData = RankedOrderedData[order(RankedOrderedData$Best_Rank,rank(RankedOrderedData$IQRLogFC),rank(-abs(RankedOrderedData$MeanLogFC)),rank(-RankedOrderedData$MedianLog10Pval)),]


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
	if(s_showall != TRUE){
		df_f = df_f[abs(df_f$FC)>s_logFCTH,]  
	} 
	# filter on at least  SIGNIFICANT ocurrences
	if(s_showall != TRUE){
		df_f = df_f[df_f$names%in%names( table(df_f$names)[table(df_f$names)>s_AmountSignTH]),]
	}
	# filter on best features only! TEMP@RRR top 10% with min of 3
	if(s_showall != TRUE){
		df_f = df_f[df_f$names%in%RankedOrderedData$FeatureName[1:max(ceiling(length(RankedOrderedData$Best_Rank) * 0.1),3)],]
	}
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
		out=list(Rankobject = RankedOrderedData, Plot=NA, PlotFeatures = NA, res_super = res_super, Rawdata = df, RawLimmaRes = res_super_raw, formula = s_CovFormula, design = temp_design, settings = s_settings)
		
	}else{
		# check if plot needs to be made
		if(s_MakePlot){
			# present the results in table
			df_f = df_f[order(abs(df_f$FC),decreasing = TRUE),]
			
			message("    Plotting results")
			Plot = ggplot2::ggplot(df_f,
				aes(x = reorder(names, abs(FC)), y = FC, fill = Pval)
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
				colours		= c("gray70","gray70","yellow","yellow","red"),
				#space		= "Lab",
					#high		= "blue",
					#low		= "yellow",
					#limits 	= c(-log10(0.05), 5),
					na.value	= "gray50",
					values   	= c(0,
								((-log10(0.05)-0.000000001)-min(df_f$Pval))/(max(df_f$Pval)-min(df_f$Pval)),
								(-log10(0.05)-min(df_f$Pval))/(max(df_f$Pval)-min(df_f$Pval)),
								((-log10(0.05)+0.1)-min(df_f$Pval))/(max(df_f$Pval)-min(df_f$Pval)),
								1),#,values   	= c(0,-log10(0.05)/5,5/5)#, (min(df_f$Pval)-min(df_f$Pval))/(max(df_f$Pval)-min(df_f$Pval))
					breaks   	= round(c(-log10(0.05),3,4,5,6),1)
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
			out=list(Rankobject = RankedOrderedData, Plot=Plot, PlotFeatures = res_plotfeatures, res_super = res_super, Rawdata = df, RawLimmaRes = res_super_raw, formula = s_CovFormula, design = temp_design, settings = s_settings)
			
			
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
			
			out=list(Rankobject = RankedOrderedData, Plot=NA, PlotFeatures = NA, res_super = res_super, Rawdata = df, RawLimmaRes = res_super_raw, formula = s_CovFormula, design = temp_design, settings = s_settings)
		}
		#class(out) = c("KDEA","list")
		#return(out)
	}

	# Stop timer
	t1 = Sys.time()
	
	# show time
	if(s_Verbose){
		x = t1-t0
		message(paste0("    Total running time: ", format(round(unclass(x),2), digits = getOption("digits")), " ", attr(x, "units"), "\n", sep = ""))
	}

	class(out) = c("KDEA","list")
	return(out)
}