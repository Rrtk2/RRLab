| [HOME](https://github.com/Rrtk2/RRLab)  |  [FUNCTIONS](https://github.com/Rrtk2/RRLab/blob/master/docs/Functions/FunctionsOverview.md)  | 

### Leave-p-Out Cross-Validation Differential Expression Analysis (KDEA or LpO-CV-DEA)
This algorithm randomly resamples the original dataset using only 80% of all samples for K folds. Each fold is processed using limma, Pvalues and LogFC is generated and stored. Median LogFC and amount of significant ocurrences are ranked and combined to a combined rank. An image is generated indicative of the variation and significance based on the collective DEA results. The Ranked features are resulted and the robust features from the image are resulted in a list.

#### Running example
An example using the diamonds dataset, trying to estimate the best robust features for the "cut" labels (column 2, filtered on "Ideal" and "Fair" only)
```
# from tibble to dataframe
data_diamond = as.data.frame(diamonds)

# Find the fair and ideal rows
data_diamond = data_diamond[data_diamond[,2]%in%c("Ideal","Fair"),]

# remove ghost factors
data_diamond[,2] = as.factor(as.character(data_diamond[,2]))

# remove non-numeric columns
data_diamond = data_diamond[,-c(3,4)]

# perform KDEA
KDEA(dataset = data_diamond,f_dataset_class_column_id = 2,s_k = 10)
```

#### Results
This results in:

```
Starting KDEA using:
Resamplefraction: 0.8
Folds: 10
Pval TH: 0.05
LogFC TH: 1
Amount Sign TH: 7
Removing intercept from test coefficients
Removing intercept from test coefficients
Removing intercept from test coefficients
Removing intercept from test coefficients
Removing intercept from test coefficients
Removing intercept from test coefficients
Removing intercept from test coefficients
Removing intercept from test coefficients
Removing intercept from test coefficients
Removing intercept from test coefficients
$Rankobject
  Features ValuePvalAmountSig  RankPval ValueMedianLogFC RankLogFC ValueLogFCSD
6        x                 10 0.8571429     -913.5116644 0.1428571  33.47888488
4    table                 10 0.8571429       -2.3748766 0.2857143   0.33609423
7        y                 10 0.8571429       -0.7421438 0.4285714   0.01045810
5    price                 10 0.8571429       -0.6660808 0.5714286   0.01026483
1    carat                 10 0.8571429       -0.5850776 0.7142857   0.10302704
2    Class                 10 0.8571429       -0.3482493 0.8571429   0.09754024
3    depth                 NA 1.0000000               NA 1.0000000           NA
  RankCombined
6            1
4            2
7            3
5            4
1            5
2            6
3            7

$Plot

$PlotFeatues
[1] "depth" "table" "price"

$res_super
$res_super[[1]]
  names           FC          Pval
1 depth   -2.3666391  0.000000e+00
2 table   -3.0990596  0.000000e+00
3     z   -0.5765123 1.291413e-196
4 carat   -0.3416958 2.703149e-155
5     x   -0.7267382 9.743502e-123
6     y   -0.6508156  9.703495e-97
7 price -901.5582043  2.555325e-16

$res_super[[2]]
  names           FC          Pval
1 depth   -2.3688701  0.000000e+00
2 table   -3.0952121  0.000000e+00
3     z   -0.5840572 3.549111e-203
4 carat   -0.3437580 3.288801e-159
5     x   -0.7400040 6.727514e-128
6     y   -0.6635091 3.729260e-101
7 price -914.5623290  5.469696e-17

$res_super[[3]]
  names           FC          Pval
1 depth   -2.3458016  0.000000e+00
2 table   -3.1359875  0.000000e+00
3     z   -0.5912685 7.273300e-207
4 carat   -0.3499935 9.275258e-163
5     x   -0.7537895 7.767097e-132
6     y   -0.6775867 7.165085e-105
7 price -955.2428917  3.403321e-18

$res_super[[4]]
  names           FC          Pval
1 depth   -2.3808831  0.000000e+00
2 table   -3.0444535  0.000000e+00
3     z   -0.5970260 4.230006e-211
4 carat   -0.3526035 5.048792e-166
5     x   -0.7583233 1.400147e-133
6     y   -0.6804020 8.362866e-106
7 price -927.8728079  2.336613e-17

$res_super[[5]]
  names           FC          Pval
1 depth   -2.4342298  0.000000e+00
2 table   -3.0181773  0.000000e+00
3     z   -0.5874752 3.121336e-205
4 carat   -0.3439536 1.140423e-158
5     x   -0.7392634 9.921472e-128
6     y   -0.6619571 4.487708e-104
7 price -873.5461999  1.624460e-15

$res_super[[6]]
  names           FC          Pval
1 table   -3.1124940  0.000000e+00
2 depth   -2.2531348  0.000000e+00
3     z   -0.5848317 6.032566e-204
4 carat   -0.3493259 3.273092e-164
5     x   -0.7516006 6.629317e-132
6     y   -0.6744410 7.310583e-108
7 price -912.1873258  9.804056e-17

$res_super[[7]]
  names           FC          Pval
1 depth   -2.3447381  0.000000e+00
2 table   -3.1089313  0.000000e+00
3     z   -0.5860980 1.185171e-203
4 carat   -0.3454698 1.973201e-159
5     x   -0.7442836 3.723262e-129
6     y   -0.6683768 1.820670e-102
7 price -945.1764452  7.751653e-18

$res_super[[8]]
  names           FC          Pval
1 table   -3.2143900  0.000000e+00
2 depth   -2.2907196  0.000000e+00
3     z   -0.5713009 3.844162e-195
4 carat   -0.3374208 2.645846e-154
5     x   -0.7292742 1.300247e-124
6     y   -0.6520796  5.460871e-98
7 price -884.2392587  5.564942e-16

$res_super[[9]]
  names           FC          Pval
1 depth   -2.3886539  0.000000e+00
2 table   -3.0465146  0.000000e+00
3     z   -0.5934138 8.758381e-210
4 carat   -0.3521099 6.294721e-167
5     x   -0.7498735 1.677017e-131
6     y   -0.6751962 2.443659e-108
7 price -985.1121953  1.544513e-19

$res_super[[10]]
  names           FC          Pval
1 depth   -2.3385315  0.000000e+00
2 table   -3.1474432  0.000000e+00
3     z   -0.5821075 2.028030e-201
4 carat   -0.3465051 1.001632e-160
5     x   -0.7371269 1.426025e-126
6     y   -0.6637847 7.412290e-101
7 price -912.4609998  1.084312e-16
```

And the resulted images:

[ ![KDEA result](/docs/Functions/KDEA.png)](/docs/Functions/KDEA.png)


This image indicated that between the groups "Fair" and "Ideal", price is the biggest difference (as expected). Also, table and depth are different in such a way that these are deemed robust features. Take note, this does not indicate causality as price is dependent on table and depth.

[ ![Diamond](/docs/Functions/diamond.png)](/docs/Functions/diamond.png) 

When evaluating the results it can be seen in the figure above that they are critical measures in the appearance of a diamond. The size of the diamond and price are the biggest difference when comparing the catagories "Fair" and "Ideal". 

#### Roadmap
[ ![KDEA workflow](/docs/Functions/KDEA-roadmap.png)](/docs/Functions/KDEA-roadmap.png) 


