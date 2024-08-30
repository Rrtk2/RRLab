#' median
#'
#' This function calculates the median of a numeric vector or the median factor level of a factor vector.
#' For numeric vectors, it returns the standard median value, while for factor vectors, it calculates the median 
#' based on the level order or frequency distribution of the factor levels.
#'
#' @param x A numeric or factor vector. For numeric vectors, the function returns the median value. For factor vectors, 
#' the function returns the median level based on the level order or frequency distribution.
#' @return The median value for numeric vectors or the median level for factor vectors.
#' @examples
#' # Example usage for numeric vector:
#' numeric_vector <- c(1, 2, 3, 4, 5)
#' median2(numeric_vector)  # Should return 3
#'
#' # Example usage for factor vector with an odd number of levels:
#' factor_vector <- factor(c("low", "medium", "high"), levels = c("low", "medium", "high"))
#' median2(factor_vector)  # Should return "medium", middle number
#'
#' # Example usage for factor vector with multiple occurrences of levels:
#' factor_vector <- factor(c("low", "low", "low", "low", "medium", "high", "high", "high", "high"), 
#' levels = c("low", "medium", "high"))
#' median2(factor_vector)  # Based on middle number, should return "middle"
#'
#' # Example usage for factor vector with skewed level distribution:
#' factor_vector <- factor(c("low", "low", "low", "low", "medium", "high"), 
#' levels = c("low", "medium", "high"))
#' median2(factor_vector)  # Based on frequency, should return "low"
#' @export

median2 = function(x) {
  
   if (is.numeric(x)) {
    return(median(x, na.rm = TRUE))

  } else if (is.factor(x)) {
    #cat("Assuming level order:\n")
    #cat(levels(x))
    #cat("\n")
    return(levels(x)[median(as.numeric(x),na.rm = TRUE)]) # if case hits with median being index in between, it selects the lower index. (eg 1.5 -> 1)

 } else if (is.character(x)) {
    #cat("Assuming level order:\n")
    #cat(levels(x))
    #cat("\n")
    return(levels(x)[median(as.numeric(as.factor(x),na.rm = TRUE))]) # if case hits with median being index in between, it selects the lower index. (eg 1.5 -> 1)

  } else {
    stop("Input must be numeric or a factor")
  }
}