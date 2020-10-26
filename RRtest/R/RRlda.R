#' RRlda
#'
#' This will extract the most contributing features from a dataset, based on the given formula. Internally computed by LDA, the IQR between classes is calculated and ordered by amplitude. Biggest amplitude holds the biggest difference between classes, thus should contibute to classification.
#'
#' @param dataset Input the dataset object.
#' @param f_dataset_class_column_id Column number of "Class" column. Detected automatically if this column is called "Class" (case sensitive).
#' @param s_formula The formula used in the LDA. Should be like "Class~." (class determined by remaining columns)
#' @param minimumfeaturesPercentagePlaceholdertext PLACEHOLDER; extracts % from ordered top IQR; 
#' @param plotPCAImage Plot a PCA image from filtered dataset.
#' @param minimumAmountOfFeatures Minimum amount of resulted featured used to in PCA plot (and dataset filtering).
#'
#' @return A list of raw LDA, filtered andordered IQR values, filtered dataset.
#' @examples
#' RRlda(iris,5)
#' @export
RRlda = function(dataset=iris,f_dataset_class_column_id = NA,s_formula="Class~.",minimumfeaturesPercentagePlaceholdertext = 0.2, plotPCAImage= TRUE,minimumAmountOfFeatures =2){

	#create resulting object
	result_object = list()

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
	
	# LDA on dataset using FORMULA
	a = lda(as.formula(s_formula), dataset)
	result_object[["LDAraw"]] = a
	
	# some ordering and IQR
	a_df = data.frame(lapply(data.frame(a$means),IQR))
	a_df = a_df[order(a_df[,],decreasing=T)]
	a_res = t(a_df)
	rownames(a_res) = colnames(a_df)
	colnames(a_res) = c("IQR")
	result_object[["Importance"]] = a_res
	
	# select % of features & filter data
	temp_featureNum = max(minimumAmountOfFeatures,ceiling(length(a_res)*minimumfeaturesPercentagePlaceholdertext))
	All_new_data_cals = c(rownames(a_res)[1:temp_featureNum],"Class")
	dataset_small = dataset[,All_new_data_cals]
	
	result_object[["DatasetReduced"]] = dataset_small
		
	# show PCA of current data
	if(plotPCAImage){
		classid = which(colnames(dataset_small)=="Class")
		print(autoplot(prcomp((dataset_small[,-classid]),scale=F),colour=as.numeric(as.factor(dataset_small$Class)),main = paste0("Filtered data PCA of all features")))
		
	}
	
	# return result
	return(result_object)
}
