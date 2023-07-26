| [HOME](https://github.com/Rrtk2/RRtest)  |  [FUNCTIONS](https://github.com/Rrtk2/RRtest/blob/master/docs/Functions/FunctionsOverview.md)  | 

## pMCC Function

The `pMCC` function calculates performance metrics for binary classification models based on observed and predicted values.

### Description

This function requires a dataset with two columns: `obs` (observations) and `pred` (predictions). It works for two-class classification problems only.

### Parameters

- `data`: Input the dataset object with columns `obs` and `pred`.
- `lev`: Not used (can be NULL).
- `model`: Not used (can be NULL).
- `showCM`: Boolean value to determine if the confusion matrix and metrics should be shown as output.
- `level1`: The actual label/number of the first class to be compared to. (NEEDS FIXING, unclear from the code comments).
- `s_TH`: TH for setting classes from probabilities. Probabilities greater than or equal to `s_TH` are considered as class 1, otherwise class 0.

### Output

The function returns a vector of performance metrics that include:
- `MCC`: Matthews Correlation Coefficient.
- `pMCC`: Modified Matthews Correlation Coefficient (a variant of MCC).

### Examples

```R
# Example 1
pMCC(data.frame(obs=c(1,1,1,2,2,2),pred=c(0.1,0.1,0.4999,0.5001,0.9,0.9)))

# Example 2
pMCC(data.frame(obs=c(1,1,1,2,2,2),pred=c(0.1,0.1,0.2,0.8,0.9,0.9)))
```

### Example Output

The output for the examples will be a vector containing the calculated `MCC` and `pMCC` values.

Please note that there are some commented-out code and notes in the original function. These are not part of the function description or usage, but they seem to be used for testing and plotting purposes in the code. They can be further explored and modified as needed.

To use this function, you can copy the R code and save it as an R script or an R package file (e.g., `pMCC.R`). Then, you can call the function in your R environment after sourcing or loading the file.