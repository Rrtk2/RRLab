#' RRFE
#'
#' This function required a dataset with rows as samples and columns as features.
#' It should contain a "class" column to be able to detect classes. Feature-extraction
#' will be performed using PCA and seeded-kmeans. RRFEDefaults() can be used to set default settings.
#'
#' @param dataset Input the dataset object (row=sample; col=feature; 'class' column advised).
#' @param f_dataset_class_column_id Index number of class column (eg. for iris dataset classcolumn ID is 5 (species)).
#' @param s_AmountFeatures Retrieves X amount of important features from PCA.
#' @param s_MinimalVariance Cumulative relative contribution of loading coefficients.
#' @param s_MaxComponents Maximum number of PCs to evaluate.
#' @param s_KRepeats Repeat k-means clustering s_KRepeats times.
#' @param s_KmeansRepeat Repeat k-means clustering SCORING s_KmeansRepeat times.
#' @param verbose More info during running.
#' @param ShowPlots Show the plots during running.
#' @param s_GetLoadingDistance Gets the distances between loading vectors and k-means centres (classes).
#' @param s_AngleFilter Use orthogonal loadings to filter the data on; selecting only features relevant to TWO groups.
#' @param s_MakeFilteredObject Create the filtered output, based on the important features.
#' @param s_scale Normalise the features by scaling (prcomp). Default to FALSE as some features might not be detected otherwise.
#' @param s_ZNorm Normalise the features by scaling (Z score scaling). Default to FALSE as some features might not be detected otherwise.
#'
#' @return A list of imporant features in a designed structure.
#' @examples
#' dataset = iris, f_dataset_class_column_id = which(colnames(iris)=="Species"), s_MinimalVariance = 0.5, s_MaxComponents = 50, s_KRepeats = 25, s_KmeansRepeat = 10, verbose = TRUE, ShowPlots = TRUE, s_MakeFilteredObject = TRUE
#' @export

RRFE_svm = function(dataset = NA, f_dataset_class_column_id = NA, s_AmountFeatures = NA, s_MinimalVariance = 0.5 , s_MaxComponents = 50,
s_KRepeats = 25, s_KmeansRepeat = 10, verbose = TRUE, ShowPlots = FALSE,s_GetLoadingDistance = FALSE, s_AngleFilter = FALSE, s_MakeFilteredObject = FALSE, s_scale = FALSE, s_ZNorm = FALSE) {
#-----------------------------------------------------------------------------------------------------#
#								NOTES
#-----------------------------------------------------------------------------------------------------#
# http://tinyheero.github.io/jekyll/update/2015/07/26/making-your-first-R-package.html

# devtools::load_all("D:\\Github\\PhD_Thesis\\Packages\\RRLab")
# devtools::document("D:\\Github\\PhD_Thesis\\Packages\\RRLab")
#	
RRLab::libraryR(c("caret", "data.table", "doParallel", "ggfortify"))
s_tuneLength = 1
s_partitionlength = 0.8
s_number = 2
s_repeats = 2
#-----------------------------------------------------------------------------------------------------#
#							INTERNAL FUNCTIONS
#-----------------------------------------------------------------------------------------------------#

# set MCC function:
MCC = function(OBSPRED, lev=NULL, model=NULL, showCM = FALSE){
	df = data.frame(obs=as.character(OBSPRED$obs),pred=as.character(OBSPRED$pred))
	
	CM = table(df)
	
	#OBSPRED$obs <- factor(OBSPRED$obs, levels = lev)
	if(names(labels(CM)[1]) == "obs"){CM = t(CM)}
	
	if((dim(CM)[1] == 2 & dim(CM)[2] == 2)){
		
		if(showCM){
			cat("Confusion matrix:\n")
			print(CM)
			cat("\n")
		}
		
		TP = CM[1,1]
		FP = CM[1,2]
		TN = CM[2,2]
		FN = CM[2,1]
		
		# Convert types to double for better precision
		TP <- as.double(TP)
		FP <- as.double(FP)
		TN <- as.double(TN)
		FN <- as.double(FN)
		
		# Calculate MCC
		numerator <- (TP * TN - FP * FN)
		denominator <- sqrt((TP + FP)*(TP + FN)*(TN + FP)*(TN + FN))
		if(denominator == 0) denominator <- 1
		MCCval <- numerator/denominator
		
		#names(out) <- c("MCC")
		AccuracyVal		=	((TP+TN)/((TP+TN)+(FP+FN)))
		RecallVal		=	(TP)/(TP+ FP)
		PrecisionVal	=	(TP) / (TP + FP)
		F1val			=	2* ( (PrecisionVal*RecallVal) / (PrecisionVal+RecallVal) )
		CombinedScVal 	= 	(MCCval + F1val)/2
		out = c(MCCval,AccuracyVal,RecallVal, PrecisionVal,  F1val, CombinedScVal)
	}else{
		out = c(0,0,0, 0, 0, 0)
	}	
	names(out) = c("MCC","Accuracy","Recall","Precision","F1","CombinedScore")
	if(showCM){print(out)}
	return(out)
}


	#-----------------------------------------------------------------------------------------------------#
	#							Start / Checks
	#-----------------------------------------------------------------------------------------------------#
	if(sum(is.na(dataset))==1){stop("Please enter dataset")}
	#if(is.na(f_dataset_class_column_id)){stop("Please enter column ID")}
	
	if(!is.na(s_AmountFeatures)){
		if(!(s_AmountFeatures >= 1 & s_AmountFeatures <= (dim(dataset)[2]-1))){
			stop(paste("Please set amount of to be retrieved features to number between 0-",(dim(dataset)[2]-1),sep=""))
		}
		
		if(!is.na(s_MinimalVariance)){
			warning("s_AmountFeatures overrules s_MinimalVariance; dropping s_MinimalVariance.")
			s_MinimalVariance = NA
		}
	}
	
	if(!is.na(s_MinimalVariance)){
		if(!(s_MinimalVariance >= 0 & s_MinimalVariance <= 1)){stop("Please set variance to number between 0-1")}
	}
	
	
	if(!(s_MaxComponents >= 0 & s_MaxComponents <= 1000)){stop("Please set max components to a number between 0-1000")}
	if(!(s_KRepeats >= 0 & s_KRepeats <= 10000)){stop("Please set k-repeats to a number between 0-10000")}
	if(!(s_KmeansRepeat >= 0 & s_KmeansRepeat <= 100)){stop("Please set kmeans repeated scoring to a number between 0-100")}

	if(!(ShowPlots == TRUE | ShowPlots == FALSE)){
		warning("ShowPlots needs TRUE or FALSE; setting to true...")
		ShowPlots = TRUE
	}
	if(!(verbose == TRUE | verbose == FALSE)){
		warning("Verbose needs TRUE or FALSE; setting to true...")
		verbose = TRUE
	}

	
	#-----------------------------------------------------------------------------------------------------#
	#							Result object
	#-----------------------------------------------------------------------------------------------------#
	f_result = NULL


	#-----------------------------------------------------------------------------------------------------#
	#							Dataset processing
	#-----------------------------------------------------------------------------------------------------#
	dataset_noclass = NULL
	dataset_class = NULL
	dataset_pre = NULL

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
	
	
	#-----------------------------------------------------------------------------------------------------#
	#							Data (type) checks
	#-----------------------------------------------------------------------------------------------------#
	# Transfer factor to numeric if needed @RRR might want to introduce a option to de/activate this
	for(i in 1:dim(dataset)[2]){
		if(colnames(dataset)[i]=="Class"){next}
		if("factor"%in%class(dataset[,i])){
			dataset[,i] = as.numeric(as.factor(dataset[,i]))
			warning(paste("Column ",i, "(",colnames(dataset)[i],") has been transformed from factor to numeric.",sep=""))
		}
	}
	
	# Find the class column and check if factor
	if(class(dataset$Class)!="factor"){
		warning(paste("Column ",f_dataset_class_column_id,"(Class) has been transformed from ",class(dataset$Class)," to factor.",sep=""))
		dataset$Class = as.factor(dataset$Class)
	}
	
	#if no Class levels are found then report and stop.
	if(is.null(levels(dataset[,f_dataset_class_column_id]))){stop("No classes found, check if there are any classes are within 'Class' column.")}
	

	#-----------------------------------------------------------------------------------------------------#
	#							Report on what found
	#-----------------------------------------------------------------------------------------------------#
	if(verbose == TRUE){ cat(paste0("#--------------------------------------------#\n"))}
	if(verbose == TRUE){ cat(paste0("Found Class column ID: ",f_dataset_class_column_id,"\n"))}
	if(verbose == TRUE){ cat(paste0("Using classes: ",paste0(as.character(levels(dataset[,f_dataset_class_column_id])),collapse = ", "),"\n"))}

	dataset_pre = dataset
	dataset_noclass = dataset_pre[,-f_dataset_class_column_id]
	dataset_class = dataset_pre[,f_dataset_class_column_id]


	# Transfer to factor if numeric
	if(is.numeric(dataset_class)){
		dataset_class = paste0("class_",dataset_class)
		dataset_class = as.factor(dataset_class)
		dataset_pre[,f_dataset_class_column_id] = dataset_class
		}else{
		dataset_class = as.factor(dataset_class)
	}


	#-----------------------------------------------------------------------------------------------------#
	#							Z-score normalisation
	#-----------------------------------------------------------------------------------------------------#
	if(s_ZNorm){
		Znorm = function(X){
			Z = X - mean(X)
			Y = Z / sd(X)
			Y
		}
		for(i in 1:dim(dataset_noclass)[2]){
			temp_Zvalue = Znorm(dataset_noclass[,i])
			dataset_noclass[,i] = temp_Zvalue
		}
		
	}
	#-----------------------------------------------------------------------------------------------------#
	#							PCA
	#-----------------------------------------------------------------------------------------------------#
	# Do PCA and check how the dataset loks like
	
	#split data 
	trainIndex <- createDataPartition(dataset_class, p = s_partitionlength, list = FALSE, times = 1)
	
	
	train <- dataset_noclass[ trainIndex,]
	dataset_class_train = dataset_class[ trainIndex] 
	
	test  <- dataset_noclass[-trainIndex,]
	dataset_class_test = dataset_class[ -trainIndex] 
	
	
	PCA = prcomp(train, scale = s_scale) # setting to true will wash away true effect? test dataset result is skewed then.
	f_result$PCA = PCA

	# Make PCA+loading plot. This indicates highes variance.
	NormalPCAPlot = ggplot2:::autoplot(PCA, data = train, colour = as.numeric(as.factor(dataset_class_train)), loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3, x = 1 ,y = 2, label = FALSE) + ggplot2::ggtitle("Optimal unsupervised contrast")
	NormalPCAPlot
	
	NormalPCAPlot_nolabbels = ggplot2:::autoplot(PCA, data = train, colour = as.numeric(as.factor(dataset_class_train)), loadings = FALSE, x = 1 ,y = 2, label = FALSE) + ggplot2::ggtitle("Optimal unsupervised contrast")
	NormalPCAPlot_nolabbels

	#-----------------------------------------------------------------------------------------------------#
	#							Start loop
	#-----------------------------------------------------------------------------------------------------#
	# Define the amount of PCs used in the comparisson. max 50.
	nmax = min(c(dim(dataset_noclass)[2],s_MaxComponents)) # max 50 components; its alot!

	# Max score is 100% accurate; max samples correct -> amount of samples
	maxscore = dim(dataset_noclass)[1]


	#-----------------------------------------------------------------------------------------------------#
	#							Do k-fold cross val?
	#-----------------------------------------------------------------------------------------------------#
	# Create empty scoring table;
	score_df = as.data.frame(matrix(nrow = (nmax^2-nmax)/2,ncol = 2+s_KmeansRepeat))
	#k2_centres = as.data.frame(matrix(nrow = (nmax^2-nmax)/2,ncol = 2+s_KmeansRepeat))

	for( p in 1:s_KmeansRepeat){

		counter = 0

		

		# Select starting centres; based on 1 point from each group.
		selected_centeres = NULL
		for( i in as.numeric(unique(dataset_class_train))){
			Samplelist = which(dataset_class_train==unique(dataset_class_train)[i])
			selected_centeres[i] = sample(Samplelist,1)
		}
		
		# Start scoring the PC contrasts
		for(i in 1:nmax){
			for( o in 1:nmax){

				# if redundant; skip
				if( o >= i){next}

				# set counter for scoring table
				counter = counter + 1

				# get the contrast
				df = PCA$x[,c(i,o)]
				df = data.frame(df, Class1 = as.factor(dataset_class_train))
				# SVM 2d class detection prediction
				
					
					# Kmeans check to speed up analysis
					k2 <- kmeans(df[,-3], centers = df[selected_centeres,-3], nstart = s_KRepeats)
					quickscore = abs((sum(as.factor(k2$cluster)==as.numeric(as.factor(df[,3])))/maxscore)-0.5)*2
					cat(paste0(p,"_",i,"_",o,":",quickscore,"\n"))
					if(quickscore>0.4){
					
						fitControl <- trainControl(method = "repeatedcv", number = s_number, repeats = s_repeats, search = "random", summaryFunction = MCC)#,classProb=TRUE,,
						
						# Start Multi-core processing
						#cl <- makePSOCKcluster(max(1,detectCores()-1))
						#registerDoParallel(cl)

						# start traingin
						fit <- train(Class1 ~ ., data = df,
						metric="MCC",
						method = "svmLinear",
						tuneLength = s_tuneLength,
						trControl = fitControl)
						#preProcess=c("center", "scale"))

						#stop Multi-core processing -> if fails -> registerDoSEQ()
						#stopCluster(cl)

						assign(x = 'fit',fit)
						## Loading required package: rpart
						#fit
						
						
						# fit test data into this PCA space and test using ML model
						df_test = scale(test, PCA$center, PCA$scale) %*% PCA$rotation 
						df_test = data.frame(df_test, Class1 = as.factor(dataset_class_test))

						# Predict test set using new model
						predictions_test <- predict(fit, df_test)
						RES = MCC(data.frame(pred=predictions_test, obs=df_test$Class1))["MCC"]
						cat(paste0("SVM:",RES,"\n"))
					}else{
						RES = 0
					}

				# Make scoring table
				score_df[counter,1] = i
				score_df[counter,2] = o
				#k2_centres[counter,1] = i
				#k2_centres[counter,2] = o
				#k2_centres[counter,p+2] = paste(a$centers[1:4],collapse = "_")
				
				# results are based on ((match/maxmatch)-0.5)*2
				# This is due the 2 cases it can be (match/nomatch) and is used to overcome groupswaps. (eg. all samples within A group belong to class 2; while kmeans CALLS this group 1; The prediction is perfect, but the class is not. -> abs ( (99/100) - 0.5)*2 = 0.98. Poor performing classes will have chance (50%); abs ( (49/100) - 0.5)*2 = 0.02

				score_df[counter,p+2] = RES
			}
		}
	}
	
	
	#-----------------------------------------------------------------------------------------------------#
	#							Select contrast
	#-----------------------------------------------------------------------------------------------------#
	# Mayority vote highest 95%
	TH_setting = max(score_df[,3:dim(score_df)[2]])*0.8
	Result_mayorvote = .rowSums(score_df[,3:dim(score_df)[2]]>TH_setting,dim(score_df)[1],dim(score_df)[2]-2)
	score_df$Result_mayorvote = Result_mayorvote
	Result_mayorvote_id = which(Result_mayorvote==max(Result_mayorvote))[1]

	# Select contrast with best score
	Resulting_contrast_data = score_df[Result_mayorvote_id,]

	# Store raw scoring table
	f_result$RawScoring = score_df

	# Make variable to store contrast
	Resulting_contrast = as.numeric(Resulting_contrast_data[1:2])

	Resulting_contrast = Resulting_contrast[order(Resulting_contrast)]
	f_result$ImportantContrast = Resulting_contrast


	#-----------------------------------------------------------------------------------------------------#
	#							kmeans image
	#-----------------------------------------------------------------------------------------------------#
	# Replicate kmeans results for image
	k2 <- kmeans(PCA$x[,Resulting_contrast], centers = PCA$x[selected_centeres,Resulting_contrast], nstart = s_KRepeats)

	#transform the inital data to the 


	# Make PCA+loading plot. This indicates best contrast to explain classes.
	KmeansPlot = ggplot2::autoplot(PCA, data = train, colour = as.numeric(dataset_class_train), loadings = TRUE, loadings.label = TRUE, loadings.label.size = 5, x = Resulting_contrast[1] ,y = Resulting_contrast[2],shape = k2$cluster+14, label = FALSE) + ggplot2::ggtitle("Optimal supervised contrast") #+geom_polygon(data = hull1, alpha = 0.3)

	# Make PCA+loading plot. This indicates best contrast to explain classes.
	KmeansPlot_NL = ggplot2::autoplot(PCA, data = train, colour = as.numeric(dataset_class_train), loadings = FALSE, loadings.label = FALSE, loadings.label.size = 5, x = Resulting_contrast[1] ,y = Resulting_contrast[2],shape = k2$cluster+14, label = FALSE) + ggplot2::ggtitle("Optimal supervised contrast") #+geom_polygon(data = hull1, alpha = 0.3)

	#-----------------------------------------------------------------------------------------------------#
	#							Extract the most important features for both components
	#-----------------------------------------------------------------------------------------------------#

	# Extract the loadings of found contrast
	Feature_loadings = abs(PCA$rotation[,c(Resulting_contrast)])

	# Create result variable which indicates most important feature names
	allnames = NULL

	# Order the Feature loadings PER component
	ordered1 = Feature_loadings[order(Feature_loadings[,1],decreasing = T),1]
	ordered2 = Feature_loadings[order(Feature_loadings[,2],decreasing = T),2]

	# Sum to get relative quantities
	maxvar1 = sum(ordered1)
	maxvar2 = sum(ordered2)

	# Get the minimal step (if feature classifies really well)
	minstep1 = max(ordered1/maxvar1)
	minstep2 = max(ordered2/maxvar2)


	if(!is.na(s_MinimalVariance)){
		# For future reference: the cumulative relative variance is the explained top x% of the two contrasts. If these two contrasts only explain 5% in total, it will be x% of total variance!

		# Define maximum minimal step which is possible
		Minimal_variance1 = max(minstep1,s_MinimalVariance)
		Minimal_variance2 = max(minstep2,s_MinimalVariance)

		# Get feature names by cumulative relative variance
		allnames = names(ordered1[cumsum(ordered1/maxvar1)<=Minimal_variance1])
		allnames = unique(c(allnames,names(ordered2[cumsum(ordered2/maxvar2)<=Minimal_variance2])))
	}
	
	if(!is.na(s_AmountFeatures)){
		# For future reference: selecting the top performing features with hard cutoff.

		# Define maximum minimal step which is possible
		Minimal_variance1 = max(minstep1,s_MinimalVariance)
		Minimal_variance2 = max(minstep2,s_MinimalVariance)

		# Get feature names by cumulative relative variance
		allnames = names(ordered1[1:s_AmountFeatures])
		allnames = unique(c(allnames,names(ordered2[1:s_AmountFeatures])))[1:s_AmountFeatures]
	}
	
	# Print min variance
	if(!is.na(s_MinimalVariance)){
	if(verbose == TRUE){ cat(paste0("#--------------------------------------------#\n"))}
	if(verbose == T){cat(paste0("min relative feature coefficient set to: \n"))}
	if(verbose == T){cat(paste0(min(Minimal_variance1,Minimal_variance2)))}
	if(verbose == T){cat("\n")}
	}

	# print table
	if(verbose == TRUE){ cat(paste0("#--------------------------------------------#\n"))}
	if(verbose == T){cat("Table features selected: \n")}
	if(verbose == T){cat(Feature_loadings[allnames,])}
	if(verbose == T){cat("\n")}

	# print feature names
	if(verbose == TRUE){ cat(paste0("#--------------------------------------------#\n"))}
	if(verbose == T){cat("Names features selected: \n")}
	if(verbose == T){cat(allnames)}
	if(verbose == T){cat("\n")}
	if(verbose == TRUE){ cat(paste0("#--------------------------------------------#\n"))}

	# Store FeatureLoading
	f_result$FeatureLoading = Feature_loadings

	# Store BestFeatureLoading
	f_result$BestFeatureLoading = Feature_loadings[allnames,]

	# Store Names
	f_result$BestFeatureNames = allnames


	#-----------------------------------------------------------------------------------------------------#
	#							Store Images
	#-----------------------------------------------------------------------------------------------------#
	if(ShowPlots == T){print(NormalPCAPlot)}
	if(ShowPlots == T){print(KmeansPlot)}

	combinedPlot = gridExtra::grid.arrange(NormalPCAPlot, KmeansPlot, nrow = 1, newpage = F)
	if(ShowPlots == T){plot(combinedPlot)}

	f_result$PlotUnsupervised = NormalPCAPlot
	f_result$PlotSupervised = KmeansPlot
	f_result$PlotCombined = combinedPlot


	#-----------------------------------------------------------------------------------------------------#
	#							Give fltered Object
	#-----------------------------------------------------------------------------------------------------#
	if(s_MakeFilteredObject == TRUE){
		newsubset = match(c(allnames,"Class"),colnames(dataset))
		f_result$dataset_new = dataset[,newsubset]
	}

	#-----------------------------------------------------------------------------------------------------#
	#							Include angles!
	#-----------------------------------------------------------------------------------------------------#
	if(s_AngleFilter == TRUE){
		# Check if it can run because it only takes 2 groups for now.
		if(length(unique(dataset_class))>2){
			warning("Cannot use more than 2 groups on this function")
			warning(paste0("**USING GROUP ",as.character(unique(dataset_class)[1])," AND ",as.character(unique(dataset_class)[2])," ONLY**"))
			next
		}
	
		# Convert some names to accept this script
		ClassID = f_dataset_class_column_id
		dataset = dataset_pre
		a = PCA

		# Plot best contrast
		ggplot2::autoplot(a, data = dataset_pre, colour = 'Class', loadings = TRUE, loadings.label = TRUE, loadings.label.size = 5, x = Resulting_contrast[1] ,y = Resulting_contrast[2],shape = k2$cluster+14, label = FALSE) + ggplot2::ggtitle("Optimal supervised contrast") #+geom_polygon(data = hull1, alpha = 0.3)

		#selct class1
		selectlevel1 = which(dataset[,ClassID]==levels(dataset[,ClassID])[1])

		#selct class2
		selectlevel2 = which(dataset[,ClassID]==levels(dataset[,ClassID])[2])

		#selct mean of class1
		G1_x = median(a$x[selectlevel1,Resulting_contrast[1]])
		G1_y = median(a$x[selectlevel1,Resulting_contrast[2]])

		#selct mean of class2
		G2_x = median(a$x[selectlevel2,Resulting_contrast[1]])
		G2_y = median(a$x[selectlevel2,Resulting_contrast[2]])

		# find angle between groups
		Angle_groups = atan2(abs(G2_y - G1_y), abs(G2_x - G1_x))
		Angle_groups_deg = (Angle_groups/pi)*180
		

		# Find angle based on 0 point in loadings
		Angle_Features = atan2(a$rotation[,Resulting_contrast[2]], a$rotation[,Resulting_contrast[1]])
		Angle_Features_deg = (Angle_Features/pi)*180
		
		# Compute angle difference modulo 90°
                Angle_diff_0 = abs(((Angle_Features_deg - Angle_groups_deg + 90) %% 180) - 90)

                Angle_diff_90 = abs(((Angle_Features_deg - Angle_groups_deg) %% 180) - 90)
		
		# order angles to get least up top
		Angle_ordered = Angle_diff_0[order(Angle_diff_0)]

		# get features
		maxangledev = max(min(Angle_diff_0)*1.2,5)
		f_result$AngleResults = rbind(Angle_diff_0,Angle_diff_90)
		f_result$AngleResults0 = (Angle_diff_0[which(Angle_diff_0 <= maxangledev)][order(Angle_diff_0[which(Angle_diff_0 <= maxangledev)])])
		f_result$AngleResults90 = (Angle_diff_0[which(Angle_diff_90 <= maxangledev)][order(Angle_diff_90[which(Angle_diff_90 <= maxangledev)])])
		f_result$AngleResultsNames = unique(c(names(f_result$AngleResults0),names(f_result$AngleResults90)))
	}
	
	
	#-----------------------------------------------------------------------------------------------------#
	#							TEST
	#-----------------------------------------------------------------------------------------------------#
	if(F){
		a=PCA$rotation[allnames,Resulting_contrast]
		a=asin(a)*180/pi # gives all rotations of the loadings
		
		a[which(a<=0)]= a[which(a<=0)]+180
		
		
		k2nmax = length(unique(dataset_class))	
		k2Anglestuff= as.data.frame(matrix(nrow = k2nmax,ncol = 2))
		b=as.data.frame(matrix(nrow = k2nmax,ncol = 2))
		# Start scoring the PC contrasts
		for(i in 1:k2nmax){
			# set counter for scoring table
	
			k2Anglestuff[i,1] = rownames(k2$centers)[i]
			#k2Anglestuff[counter,3] = abs(matlib::angle(k2$centers[i,],k2$centers[o,])-90)
			k2Anglestuff[i,2] = matlib::angle(k2$centers[i,],c(0.000001,0.000001))
			print(k2Anglestuff[i,2])
			b[i,1] = paste0("Class_",i)
			a-k2Anglestuff$V2[i]
			b[i,2] = names(which(a[order(a[,i],decreasing = T),i][1]==a[,i]))
		}
	}
	#-----------------------------------------------------------------------------------------------------#
	#							Get distances to k-centres; important signature features
	#-----------------------------------------------------------------------------------------------------#
	#@RRR Imporant!!! this seems to be working; but not quite right... look into it!
	if(s_GetLoadingDistance == TRUE){
		# get PCA object and info
		k2nmax = length(unique(dataset_class))	
		PCArows = length(allnames)
		
		# create distmatrix frame
		Distancematrix= as.data.frame(matrix(nrow = PCArows,ncol = 2+k2nmax))
		rownames(Distancematrix) = allnames
		colnames(Distancematrix) = c("X","Y",paste0(levels(dataset_class)))
		Distancematrix[,1:2] = PCA$rotation[allnames,Resulting_contrast]
		
		# push all into symmetrical axis using absolute
		Distancematrix_abs = (Distancematrix[,1:2])
		k2centers_abs = (k2$centers)
		Distancematrix_abs = (((Distancematrix_abs)-min(Distancematrix_abs))/(max(Distancematrix_abs)-min(Distancematrix_abs))) * max(abs(k2centers_abs))
		#k2centers_abs = ((k2centers_abs)-min(k2centers_abs))/(max(k2centers_abs)-min(k2centers_abs))
		
		# for every k find best top x loadings nearby; absolute distance
		for(i in 1:k2nmax){
			for(o in 1:PCArows){
				distanceE = sqrt(abs(sum((Distancematrix_abs[o,] - k2centers_abs[i,1]) ^ 2)))
				Distancematrix[o,2+i] = distanceE
			}
		}
		
		# Print in object
		f_result$Distancematrix = Distancematrix
	}
	
	#-----------------------------------------------------------------------------------------------------#
	#							Result object
	#-----------------------------------------------------------------------------------------------------#
	return(f_result)
	
	
	
}