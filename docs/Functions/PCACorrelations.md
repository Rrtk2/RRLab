| [HOME](https://github.com/Rrtk2/RRtest)  |  [FUNCTIONS](https://github.com/Rrtk2/RRtest/blob/master/docs/Functions/FunctionsOverview.md)  | 

# Simulating DNA Methylation Data and Analyzing Covariate Effects in R

This repository provides a set of R functions to simulate realistic DNA methylation beta values with accompanying phenotype data, visualize the distributions of the simulated data, and perform principal component analysis (PCA) to explore the relationships between methylation patterns and phenotypic covariates.

## 1. Function Discussion

### 1.1 generateRealisticBetaData

**Purpose:**  
Simulate realistic DNA methylation data along with phenotype data that include various covariates.

**Key Features:**
- **Phenotype Data Generation:**  
  Creates a data frame with covariates including significant (e.g., `Sex`, `Age`, `CellType`), mild (e.g., `Mild1`, `Mild2`, `Mild3`), and null covariates.
- **Beta Matrix Generation:**  
  Simulates a baseline beta matrix (representing methylation values) using a Beta distribution.
- **Covariate Effects Assignment:**  
  Each feature (CpG) is assigned a category ("none", "single", or "multiple") based on specified proportions. Effects from selected covariates are added to the beta values accordingly, and values are kept within the [0, 1] range.

### 1.2 generateSamplePlotsForBetas

**Purpose:**  
Generate visual representations (density plot and boxplot) of the beta values across samples.

**Key Features:**
- **Density Plot:**  
  Overlays the density curves for beta values of each sample in one plot.
- **Boxplot:**  
  Compares the distribution of beta values across all samples.
- **Output:**  
  Both plots are saved as PDF files in a user-specified directory.

### 1.3 MakePCACorrelates

**Purpose:**  
Perform PCA on the beta matrix and correlate the principal components with the phenotypic variables to uncover underlying patterns.

**Key Features:**
- **PCA Execution:**  
  Conducts PCA on the beta matrix after ensuring complete cases.
- **Scree Plot:**  
  Generates a scree plot to visualize the proportion of variance explained by each principal component.
- **Correlation Analysis:**  
  Computes correlations between significant principal components and phenotypic variables, only highlighting correlations with a p-value below a specified threshold.
- **Detailed Visualization:**  
  Creates individual PC vs. phenotype plots and, if applicable, pairwise PC plots for PCs with strong phenotype associations.
- **Output:**  
  The function returns detailed correlation data, the PCA result object, and the file paths to all generated plots.

## 2. Running Example

Below is an example script that runs the entire workflow:

```R
# Load necessary package
library(ggplot2)

# 1. Generate simulated data
simulated_data <- generateRealisticBetaData(
  n_features = 1000, 
  n_samples = 100,
  none_prop = 0.5, 
  one_prop = 0.3, 
  multiple_prop = 0.2,
  shape1 = 0.5, 
  shape2 = 0.5
)

# Extract the beta matrix and phenotype data
Beta <- simulated_data$Beta
pheno <- simulated_data$pheno

# 2. Generate sample plots for beta values (density plot and boxplot)
generateSamplePlotsForBetas(
  toy_data = simulated_data, 
  output_dir = "./plots", 
  density_filename = "density_plot.pdf", 
  boxplot_filename = "boxplot.pdf"
)

# 3. Perform PCA and correlate PCs with phenotypic variables
pca_results <- MakePCACorrelates(
  Beta = Beta, 
  pheno = pheno, 
  output_dir = "./results", 
  savename = "analysis1", 
  var_threshold = 0.9, 
  p_threshold = 0.05
)

# Display detailed correlations between PCs and phenotype variables
print(pca_results$detailed_correlations)

