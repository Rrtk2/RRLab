| [HOME](https://github.com/Rrtk2/RRLab)  |  [FUNCTIONS](https://github.com/Rrtk2/RRLab/blob/master/docs/Functions/FunctionsOverview.md)  |

# ProjectTimePCA Function

Projects samples from a second time point onto a PCA derived from the first time point and reports the components with the largest change.

## Usage
```R
ProjectTimePCA(Data_t0, Data_t1, ClassColumnID)
```

## Parameters
- `Data_t0`: Data frame of baseline measurements including class column.
- `Data_t1`: Data frame of follow-up measurements.
- `ClassColumnID`: Column index of the class label.

## Returns
A list containing PCA objects, plots and distance summaries.
