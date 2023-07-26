| [HOME](https://github.com/Rrtk2/RRtest)  |  [FUNCTIONS](https://github.com/Rrtk2/RRtest/blob/master/docs/Functions/FunctionsOverview.md)  | 

## RMostContibutingFeatures2 Function

The `RMostContibutingFeatures2` function is designed to identify the most important features in a dataset based on a vector of importances. It utilizes linear modeling and robust regression techniques to determine the relevant features and returns their names along with other useful information.

### Usage

```R
RMostContibutingFeatures2(Dat = NA, amountSDTH = 3, Quantile_TH_range = 0.9, plotfigure = TRUE)
```

### Parameters

- `Dat`: Input the Dat object, which should be a 1 column dataframe with rownames.
- `amountSDTH`: The number of standard deviations to be passed before considering a value as relevant. Default is 3.
- `Quantile_TH_range`: The quantile range (in terms of percentile) to determine the data selected for modeling the average increase. Default is 0.9 (i.e., 90%).
- `plotfigure`: A logical parameter to decide whether to plot the loadings in decreasing order and show the model and residuals. Default is TRUE.

### Returns

The function returns a list containing the following elements:

- `relevant_features`: The names of the most important features.
- `model`: The robust linear model obtained using the selected data.
- `sd`: The standard deviation of the residuals from the model.
- `upper_QUANT_TH`: The upper threshold value determined by the specified quantile range.
- `lower_QUANT_TH`: The lower threshold value determined by the specified quantile range.

### Example

```R
# Load the required libraries (assuming they are already installed)
# library(robustbase)

# Generate sample data
Dat = data.frame(Vals = c(rnorm(100), (1.0 + (1:10)/10)^3))
rownames(Dat) = paste0("Feature", rownames(Dat))

# Apply the RMostContibutingFeatures2 function
Results = RMostContibutingFeatures2(Dat)

# Print the relevant features
print(Results$relevant_features)

> [1] "Feature21"  "Feature59"  "Feature60"  "Feature104" "Feature105" "Feature106" "Feature107" "Feature108" "Feature109" "Feature110"
```

### Output

The function will output the names of the most important features based on the provided dataset and parameters.

Note: Ensure you have the required `robustbase` library installed before using the function. The function calculates the relevant features based on the robust linear model and the specified thresholds. It also provides an option to plot the loadings, model, and residuals if desired.