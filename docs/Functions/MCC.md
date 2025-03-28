| [HOME](https://github.com/Rrtk2/RRLab)  |  [FUNCTIONS](https://github.com/Rrtk2/RRLab/blob/master/docs/Functions/FunctionsOverview.md)  | 

# MCC Function

This R function, named MCC, is used to calculate various performance metrics for a binary classification model. The function requires a dataset with two columns: the first column represents the actual observations (true labels), and the second column represents the predicted values from the model. It calculates metrics such as Matthews Correlation Coefficient (MCC), Accuracy, Recall, Precision, F1 score, and others.

## Function Signature

```R
MCC(data, lev = NULL, model = NULL, showCM = FALSE, Verbose = FALSE)
```

## Parameters

- **data**: Input the dataset object with two columns (column 1 = OBS; column 2 = PRED) representing actual observations and predicted values, respectively.

- **lev**: NULL (not used in the function).

- **model**: NULL (not used in the function).

- **showCM**: A logical value indicating whether to show the confusion matrix and computed metrics as output.

- **Verbose**: A logical value indicating whether to print additional information about zero or infinite values in the calculated metrics.

## Returns

The function returns a named numeric vector containing the following performance metrics:

- **TPR**: True Positive Rate (Sensitivity).

- **FPR**: False Positive Rate.

- **FNR**: False Negative Rate.

- **TNR**: True Negative Rate (Specificity).

- **PPV**: Positive Predictive Value (Precision).

- **FDR**: False Discovery Rate.

- **FOR**: False Omission Rate.

- **NPV**: Negative Predictive Value.

- **PLR**: Positive Likelihood Ratio.

- **NLR**: Negative Likelihood Ratio.

- **MK**: Markedness.

- **DOR**: Diagnostic Odds Ratio.

- **BA**: Balanced Accuracy.

- **F1**: F1 score.

- **FMI**: Fowlkes–Mallows index.

- **MCC**: Matthews Correlation Coefficient.

- **TS**: Threat Score.

- **Prevalence**: Prevalence.

- **Prevalence_THR**: Prevalence Threshold.

- **ACC**: Accuracy.

- **Informedness**: Informedness.

- **YoudenJ**: Youden's J statistic.

## Examples

```R
# Example 1: High accuracy
MCC(data.frame(obs=c(1,1,1,2,2,2), pred=c(1,1,1,1,2,2)))

# Example 2: Inverse accurate
MCC(data.frame(obs=c(rep(2, 1000), rep(1, 1000)), pred=c(rep(1, 1000), rep(2, 1000))))

# Example 3: Random data
MCC(data.frame(obs=c(round(runif(200000, 0, 1))), pred=c(round(runif(200000, 0, 1)))))

# Example 4: High accuracy imbalance
MCC(data.frame(obs=c(rep(1, 1000), rep(2, 10)), pred=c(rep(1, 1000), rep(2, 10))))

# Example 5: High accuracy EXTREME imbalance
MCC(data.frame(obs=c(rep(1, 1000), rep(2, 10)), pred=c(rep(1, 1010))))

# Example 6: Moderate accuracy imbalance
MCC(data.frame(obs=c(rep(1, 1000), round(runif(10, 1, 2))), pred=c(rep(1, 1000), rep(2, 10))))
```

Note: The function is capable of handling cases with extreme class imbalance and returns appropriate metrics accordingly.

Please make sure to install and load any required packages before using the MCC function in your R environment.