| [HOME](https://github.com/Rrtk2/RRLab)  |  [FUNCTIONS](https://github.com/Rrtk2/RRLab/blob/master/docs/Functions/FunctionsOverview.md)  |

# RRFE_svm Function

Support vector machine implementation of the RRFE feature selection algorithm.

## Usage
```R
RRFE_svm(dataset, f_dataset_class_column_id, s_AmountFeatures = NA, s_MinimalVariance = 0.5,
         s_MaxComponents = 50, s_KRepeats = 25, s_KmeansRepeat = 10,
         verbose = TRUE, ShowPlots = FALSE)
```

## Parameters
- `dataset`: Data frame containing features and a class column.
- `f_dataset_class_column_id`: Index of the class column.
- Other arguments configure the PCA and k-means steps.

## Returns
A list describing the selected features and intermediate results.
