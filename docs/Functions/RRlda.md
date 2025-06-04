| [HOME](https://github.com/Rrtk2/RRLab)  |  [FUNCTIONS](https://github.com/Rrtk2/RRLab/blob/master/docs/Functions/FunctionsOverview.md)  |

# RRlda Function

Performs linear discriminant analysis and ranks features by the interquartile range of class means.

## Usage
```R
RRlda(dataset, f_dataset_class_column_id, s_formula = "Class~.",
      minimumfeaturesPercentagePlaceholdertext = 0.2, plotPCAImage = TRUE,
      minimumAmountOfFeatures = 2)
```

## Parameters
- `dataset`: Data frame with a `Class` column.
- `f_dataset_class_column_id`: Column index if `Class` is unnamed.
- `s_formula`: Formula passed to `lda`.
- `minimumfeaturesPercentagePlaceholdertext`: Fraction of top features to keep.
- `plotPCAImage`: Plot PCA of the reduced data.
- `minimumAmountOfFeatures`: Minimum number of features to retain.

## Returns
A list containing the LDA model, importance ranking and reduced dataset.
