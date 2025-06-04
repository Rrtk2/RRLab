| [HOME](https://github.com/Rrtk2/RRLab)  |  [FUNCTIONS](https://github.com/Rrtk2/RRLab/blob/master/docs/Functions/FunctionsOverview.md)  |

# RRPCAplot Function

Generates a scatter plot of principal component scores with text labels for the most influential loadings.

## Usage
```R
RRPCAplot(dataset, Class, PCAcontrastX = 1, PCAcontrastY = 2)
```

## Parameters
- `dataset`: Data frame to run PCA on.
- `Class`: Optional class labels for colouring points.
- `PCAcontrastX`, `PCAcontrastY`: Indices of PCs to plot.

## Returns
A ggplot object showing samples and selected loadings.
