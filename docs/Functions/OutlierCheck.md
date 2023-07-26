| [HOME](https://github.com/Rrtk2/RRtest)  |  [FUNCTIONS](https://github.com/Rrtk2/RRtest/blob/master/docs/Functions/FunctionsOverview.md)  | 

# OutlierCheck function

## Description
This function checks for outliers in a dataset with rows as features and columns as samples. It performs a set of tests to identify outlier samples or features, depending on the specified type.

## Function Signature
```R
OutlierCheck(data, Type = 1)
```

## Parameters
- `data`: The input dataset object. It should be a data.frame with rows as features and columns as samples.
- `Type`: An integer indicating the type of outliers to be detected. Default is `1` (SAMPLES), and it can be set to `2` (FEATURES) for identifying feature outliers.

## Return Value
A PCA plot indicative of outlier samples based on a set of tests. Additionally, the function returns a matrix with outlier metrics for each feature.

## Examples
```R
# Example 1: Checking for outlier samples
data(mtcars)
outlier_samples <- OutlierCheck(t(mtcars), Type = 1)
```

```R
# Example 2: Checking for outlier features
data(mtcars)
outlier_features <- OutlierCheck(mtcars, Type = 2)
```

## Output
The output of the function is a matrix containing outlier metrics for each feature, and a PCA plot representing outlier samples (or features) based on the computed metrics.

Note: The output examples are not shown here, as they are graphical representations (PCA plots) and a matrix of metrics for each feature.
```

Remember to replace `Example 1` and `Example 2` with suitable descriptions that align with the dataset you are using and the results you expect. Also, you can modify the parameter descriptions and add any additional relevant details as needed.
