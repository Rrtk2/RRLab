| [HOME](https://github.com/Rrtk2/RRLab)  |  [FUNCTIONS](https://github.com/Rrtk2/RRLab/blob/master/docs/Functions/FunctionsOverview.md)  |

# RRRemoveCor Function

Removes highly correlated features by modelling each pair and replacing one with the fitted values while keeping residuals for the other.

## Usage
```R
RRRemoveCor(dataset, TH = 0.7, Addmean = TRUE)
```

## Parameters
- `dataset`: Data frame of numeric features.
- `TH`: Correlation threshold.
- `Addmean`: Add the mean back to residuals.

## Returns
A modified data frame with reduced correlation between features.
