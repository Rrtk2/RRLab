| [HOME](https://github.com/Rrtk2/RRtest)  |  [FUNCTIONS](https://github.com/Rrtk2/RRtest/blob/master/docs/Functions/FunctionsOverview.md)  | 

To create a GitHub Markdown file describing the function, providing an example, and showing the output, you can follow the template below:

```markdown
# MCCx

This function calculates the Matthews Correlation Coefficient (MCC) for multiclass classification tasks. It requires a dataset with two columns: column 1 for observations and column 2 for predictions. It is inspired by Macro-Averaging per class.

## Function Description

```R
#' MCCx
#'
#' This function required a dataset with column 1 as observations and column 2 with predictions. Multiclass!
#' It borrows inspiration from Macro-Averaging per class.
#' 
#' @param data Input the dataset object (column 1 = OBS; column 2 = PRED).
#' @param lev NULL
#' @param model NULL
#' @param showCM Show output of confusion matrix & metrics as cat.
#'
#' @return Several metrics to indicate machine learning performance ("MCC","Accuracy","Recall","Precision","F1","CombinedScore")
#' @examples
#' MCCx(data.frame(obs=c(1,1,1,2,2,2,3,3,3),pred=c(1,1,1,1,2,2,3,3,3)))
#' @export

MCCx = function(data, lev=NULL, model=NULL, showCM = FALSE){
  # Function implementation here
}
```

## Parameters

- `data`: Input the dataset object with column 1 as observations and column 2 with predictions.
- `lev`: Unused parameter (set to NULL).
- `model`: Unused parameter (set to NULL).
- `showCM`: If set to TRUE, it will show the output of the confusion matrix and metrics.

## Return Value

The function returns a numeric vector with the Matthews Correlation Coefficient (MCC) value.

## Example

```R
# Example dataset
data <- data.frame(obs = c(1, 1, 1, 2, 2, 2, 3, 3, 3), pred = c(1, 1, 1, 1, 2, 2, 3, 3, 3))

# Calculate MCC
result <- MCCx(data)

# Print the MCC value
print(result)
```

## Output

The output will be a numeric vector containing the MCC value:

```
MCC 
0.1790287 
```
```

Make sure to replace the example dataset (`data`) with your own dataset if you want to test the function with different input data. You can also add more examples with different datasets to showcase the function's behavior with various scenarios. Additionally, you can include more information about the MCC metric and its interpretation in the Markdown file if needed.