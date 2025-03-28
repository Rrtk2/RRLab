| [HOME](https://github.com/Rrtk2/RRLab)  |  [FUNCTIONS](https://github.com/Rrtk2/RRLab/blob/master/docs/Functions/FunctionsOverview.md)  | 

# R package: RRLab
[![GitHub License](https://img.shields.io/github/license/Rrtk2/RRLab)](https://github.com/Rrtk2/RRLab/blob/master/LICENSE.md) ![](https://img.shields.io/badge/Status-Setting_up-red) [![GitHub Watches](https://img.shields.io/github/watchers/Rrtk2/RRLab.svg?style=social&label=Watch&maxAge=2592000)](https://github.com/Rrtk2/RRLab/watchers) 


#### Package definition
The RRLab package was created as a personal package, filled with tools to help with (big-data) analysis and machine learning. The main interesting functions are RRFE and KDEA!

---

#### RR-Feature extraction (RRFE)
This algorithm evaluates each possible combination of contrasts (generated by PCA) and applies seeded knn per contrast to identify the best contrast which achieved the highest classification accuracy. Then the resulted contrast is resulted and the most important features are extracted (based on this contrast).

[See the example and results](/docs/Functions/RRFE.md) 


#### Leave-p-Out Cross-Validation Differential Expression Analysis (KDEA or LpO CV DEA)
This algorithm randomly resamples the original dataset using only 80% of all samples for K folds. Each fold is processed using limma, Pvalues and LogFC is generated and stored. Median LogFC and amount of significant ocurrences are ranked and combined to a combined rank. An image is generated indicative of the variation and significance based on the collective DEA results. The Ranked features are resulted and the robust features from the image are resulted in a list.

[See the example and results](/docs/Functions/KDEA.md) 

#### RR general characteristics of data scan (RRscan)
This function will evaluate all distribution properties per feature and per sample. Also detects potential outliers based on hclust, mean+sd, var+sd(var).

[See the example and results](/docs/Functions/RRscan.md) 

---

#### Installation
First, you need to install the devtools package. You can do this from CRAN.
```
install.packages("devtools")
```

To install the RRLab package
```
devtools::install_github("Rrtk2/RRLab/RRLab")
```
---

#### Contact
ra.reijnders@maastrichtuniversity.nl


#### License and contributing guidelines
[License](/LICENSE.md) 

[Contributing guidelines](/CONTRIBUTING.md) 


#### Who is involved, and what are their roles.
RRtK2 (owner and contributor)


#### Status of project
Setting up. Big changes likely; many bugs.


#### Copyright and authors
All code and documents in the RRLab folder was created by [these author(s)](/AUTHORS.md).
