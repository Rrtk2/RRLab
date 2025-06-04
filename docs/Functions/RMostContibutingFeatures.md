| [HOME](https://github.com/Rrtk2/RRLab)  |  [FUNCTIONS](https://github.com/Rrtk2/RRLab/blob/master/docs/Functions/FunctionsOverview.md)  |

# RMostContibutingFeatures Function

Identifies top contributing features from a PCA object by modelling the ordered loadings and selecting those above a 3×SD threshold.

## Usage
```R
RMostContibutingFeatures(PCA, maxPCs = 10, Quantile_TH_range = 0.9, use_ABS = FALSE, plotfigure = TRUE)
```

## Parameters
- `PCA`: A `prcomp` object.
- `maxPCs`: Number of components to evaluate.
- `Quantile_TH_range`: Quantile range for fitting the baseline model.
- `use_ABS`: Whether to use absolute loadings.
- `plotfigure`: Plot diagnostic figures.

## Returns
A list of features driving variance for each evaluated component.
