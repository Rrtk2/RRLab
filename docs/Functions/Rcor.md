| [HOME](https://github.com/Rrtk2/RRtest)  |  [FUNCTIONS](https://github.com/Rrtk2/RRtest/blob/master/docs/Functions/FunctionsOverview.md)  | 

# Rcor function

## Description
This function, `Rcor`, takes a dataset with variables that can be transformed to numeric (not character) and computes the correlation matrix. It then applies a threshold (`th`) to shrink correlations below the cutoff to 0.

## Usage
```R
Rcor(dataset, th)
```

## Arguments
- `dataset`: Input the dataset object (`data.frame`) with variables that can be transformed to numeric.
- `th`: Threshold for correlations. Correlations with an absolute value less than `th` will be set to 0.

## Return Value
A correlation matrix containing all columns that can be transformed to numeric, with correlations below the threshold `th` set to 0.

## Examples
```R
# Load the mtcars dataset from R's base package
data(mtcars)

# Calculate the correlation matrix using Rcor function with a threshold of 0.3
result <- Rcor(mtcars, th = 0.3)
print(result)
```

## Output
The output will be a correlation matrix with rows and columns corresponding to the variables in the dataset. Correlations with an absolute value less than 0.3 will be set to 0.
```

Please note that you can copy the above Markdown content and save it with a `.md` extension to create a GitHub Markdown file. Make sure to include the function `Rcor` in an R script file, for example, `Rcor.R`, so that it can be imported into other scripts or R sessions.
