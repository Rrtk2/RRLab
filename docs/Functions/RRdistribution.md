| [HOME](https://github.com/Rrtk2/RRtest)  |  [FUNCTIONS](https://github.com/Rrtk2/RRtest/blob/master/docs/Functions/FunctionsOverview.md)  | 

# RRDistribution Function

This R function `RRDistribution` estimates the distribution of the input data and suggests the distribution that best fits the data.

## Parameters
- `Data`: Unlisted vector of numeric values.

## Returns
The function returns a list containing two elements:
- `Scoreresult`: A data frame with information about various distribution fits, their AIC scores, and ranks.
- `Fit`: The best-fit distribution model obtained using the Minimum Mean Square Error (MME) method.

## Examples
```R
# Load the required package
library(fitdistrplus)

# Example 1: Using iris dataset
data(iris)
# Applying the RRDistribution function to iris dataset, excluding the species column (column 5)
result1 <- RRDistribution(Data = iris[, -5])

# Example 2: Using a custom dataset
# Generate some example data
set.seed(123)
custom_data <- rnorm(100)

# Applying the RRDistribution function to custom_data
result2 <- RRDistribution(Data = custom_data)
```

## Output
The output of the function will be a list, providing information about various distribution fits and plots of the best-fit distribution.

Note: The function expects only numeric input; if non-numeric data is provided, it will be converted to numeric using `unlist`.

Make sure to have the required `fitdistrplus` package installed before using this function.

Ensure to properly handle the results from the function, and use the `Fit` element of the returned list to access the best-fit distribution model for further analysis and plotting.