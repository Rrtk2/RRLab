#### RRscan (RR general characteristics of data scan)
This function will evaluate all distribution properties per feature and per sample. Also detects potential outliers based on hclust, mean+sd, var+sd(var).

An example using the mtcars dataset, trying to evaluate the characteristics.
```
RRscan(data=mtcars,LogTransform = FALSE,ImageDir = "",PlotImages = TRUE,SaveImages = FALSE,FindOutliers = TRUE,verbose = TRUE)
```

Results in:
```
Getting sample-based metics

Getting feature-based metics

Plotting histogram

Plotting boxplot

Plotting density

Plotting dendrogram

Plotting mean-variance

Plotting mean-skew

Plotting variance-skew

Plotting mean-median

Plotting mean-QQerror

Plotting PC1-PC2
Detected possible outliers:


```
***Images need to be added here***


And the resulted images:
![Image](/docs/404)

