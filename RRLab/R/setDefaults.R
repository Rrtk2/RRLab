#' RRFEDefaults
#'
#' This function sets defaults for RRFE
#'
#' @return Variables to set the RRFE function to default.
#' @examples
#' dataset = RRFEDefaults()
#' @export

RRFEDefaults = function() {
	dataset <<- iris
	f_dataset_class_column_id  <<-  5
	s_MinimalVariance  <<-  0.5
	s_MaxComponents  <<-  50
	s_KRepeats  <<-  25
	s_KFoldCV  <<-  10
	verbose  <<-  TRUE 
	ShowPlots  <<-  TRUE
	s_MakeFilteredObject  <<-  FALSE
	s_GetLoadingDistance <<- TRUE
	s_AmountFeatures <<- NULL
	s_KmeansRepeat <<- 10
	
}
