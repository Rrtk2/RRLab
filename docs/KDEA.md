#### KDEA (K-fold Differential Expression Analysis)
This algorithm randomly resamples the original dataset using only 80% of all samples for K folds. Each fold is processed using limma, Pvalues and LogFC is generated and stored. Median LogFC and amount of significant ocurrences are ranked and combined to a combined rank. An image is generated indicative of the variation and significance based on the collective DEA results. The Ranked features are resulted and the robust features from the image are resulted in a list.

[See the example and results](/docs/KDEA.md) 

An example:
```
RRtest::KDEA(mtcars,which(colnames(mtcars)=="am"))
```