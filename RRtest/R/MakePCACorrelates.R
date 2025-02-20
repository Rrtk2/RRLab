#' MakePCACorrelates
#'
#' This function performs Principal Component Analysis (PCA) on a numeric data matrix (Beta) and evaluates its relationship with phenotypic variables provided in a data frame (pheno). It generates a scree plot to visualize variance distribution, computes the correlations between significant principal components and phenotypic variables, and produces individual as well as pairwise PC plots when appropriate.
#'
#' @param Beta A numeric matrix where columns correspond to samples and rows to features. If not already a matrix, it will be coerced into one.
#' @param pheno A data frame containing phenotypic data, with rows corresponding to samples and columns to phenotypic variables.
#' @param output_dir Character. Directory to save output plots. Defaults to "./output/".
#' @param savename Character. A string appended to output filenames to distinguish results. Defaults to "01".
#' @param var_threshold Numeric. Cumulative variance threshold (e.g., 0.9 for 90%) used to determine the number of significant principal components. Defaults to 0.9.
#' @param p_threshold Numeric. p-value threshold to consider a correlation significant. Defaults to 0.05.
#'
#' @return A list containing:
#' \describe{
#'   \item{correlation_frame}{A data frame of correlation coefficients between each significant principal component and the phenotypic variables.}
#'   \item{detailed_correlations}{A data frame summarizing the strongest correlated phenotypic variable for each significant principal component, including the correlation value and the percentage of variance explained.}
#'   \item{pca_result}{The PCA result object as returned by \code{prcomp}.}
#'   \item{scree_plot_path}{Path to the saved scree plot PDF.}
#'   \item{plots_path}{Path to the saved PDF containing individual PC versus phenotype plots.}
#'   \item{pairwise_plots_path}{Path to the saved PDF containing pairwise PC plots (if generated), or NA otherwise.}
#' }
#'
#' @examples
#' \dontrun{
#'   # Assume Beta is a numeric matrix and pheno is a data frame with matching sample rows.
#'   result <- MakePCACorrelates(Beta, pheno, 
#'                               output_dir = "./results/", 
#'                               savename = "analysis1", 
#'                               var_threshold = 0.9, 
#'                               p_threshold = 0.05)
#' }
#'
#' @export


MakePCACorrelates <- function(Beta, pheno, 
                              output_dir = "./plots/", 
                              savename = "01",
                              var_threshold = 0.9,
                              p_threshold = 0.05) {
  
  # Input validation
  if (!is.matrix(Beta)) {
    Beta <- as.matrix(Beta)
  }
  if (!is.data.frame(pheno)) {
    stop("pheno must be a data frame")
  }
  if (ncol(Beta) != nrow(pheno)) {
    stop("Dimensions of Beta and pheno do not match.")
  }

  # Check for valid row names in pheno.
  # This includes checking if row names are missing, NA, empty, or simply the default numeric sequence.
  if (is.null(rownames(pheno)) || 
      any(is.na(rownames(pheno)) | rownames(pheno) == "") ||
      all(rownames(pheno) == as.character(seq_len(nrow(pheno))))) {
    rownames(pheno) <- paste0("Sample_", seq_len(nrow(pheno)))
  }

  # Check if the output directory exists; if not, create it (including subdirectories)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Perform PCA on complete cases
  completeBeta <- Beta[stats::complete.cases(Beta), ]
  pca <- stats::prcomp(t(completeBeta))
  
  # Save scree plot using base R plotting (for simplicity)
  scree_path <- file.path(output_dir, paste0("01_ScreePlot_", savename, ".pdf"))
  pdf(file = scree_path)
  plot(pca, main = "Scree Plot")
  dev.off()
  
  # Compute relative variance explained by each PC
  rel_variance <- round(pca$sdev^2 / sum(pca$sdev^2), 3)
  num_significant_PCs <- min(which(cumsum(rel_variance) > var_threshold))
  cat("PC1 explains", rel_variance[1] * 100, "% of variance in the data\n")
  
  # Convert phenodata to numeric matrix for correlation computations
  pheno_numeric <- apply(pheno, 2, function(x) as.numeric(as.factor(x)))
  pheno_numeric <- pheno_numeric[, colSums(is.na(pheno_numeric)) == 0]
  
# Determine the number of PCs to use
num_PCs <- min(num_significant_PCs, ncol(Beta))

# Convert phenotypic variables to a data frame
pheno_df <- as.data.frame(pheno_numeric)

# Initialize a matrix to store the correlation coefficients
corr_mat <- matrix(NA, nrow = ncol(pheno_df), ncol = num_PCs)
rownames(corr_mat) <- colnames(pheno_df)
colnames(corr_mat) <- paste0("PC", 1:num_PCs)

# Loop through each PC and each phenotypic variable
for (i in seq_len(num_PCs)) {
pc_scores <- pca$x[, i]
for (j in seq_along(pheno_df)) {
	pheno_var <- pheno_df[[j]]
	
	# Skip variables with zero variance to avoid errors
	if (sd(pheno_var) == 0) next
	
	# Perform the correlation test
	test <- cor.test(pc_scores, pheno_var)
	
	# Save the rounded correlation if the p-value is below the threshold
	#if (test$p.value < p_threshold) {
	corr_mat[j, i] <- round(test$estimate, 2)
	#}
}
}
  rownames(corr_mat) <- colnames(pheno_numeric)
  correlation_frame  = corr_mat
  # Initialize a matrix to store the p-values
pvalue_mat <- matrix(NA, nrow = ncol(pheno_df), ncol = num_PCs)
rownames(pvalue_mat) <- colnames(pheno_df)
colnames(pvalue_mat) <- paste0("PC", 1:num_PCs)

# Loop through each PC and each phenotypic variable
for (i in seq_len(num_PCs)) {
  pc_scores <- pca$x[, i]
  for (j in seq_along(pheno_df)) {
    pheno_var <- pheno_df[[j]]
    
    # Skip variables with zero variance to avoid errors
    if (sd(pheno_var) == 0) next
    
    # Perform the correlation test
    test <- cor.test(pc_scores, pheno_var)
    
    # Save the rounded p-value (to 4 decimal places)
    pvalue_mat[j, i] <- round(test$p.value, 4)
  }
}

# Convert the matrix to a data frame for easier handling
correlation_frame_p <- as.data.frame(pvalue_mat)
  
  ### Plot individual PC vs phenotype plots using ggplot2
  plot_path <- file.path(output_dir, paste0("01_PCA_Plots_", savename, ".pdf"))
  pdf(file = plot_path, width = 8, height = 6)
  
  detailed_correlations <- data.frame(Variable = character(),
                                      Variable_index = integer(),
                                      PC = integer(),
                                      Correlation = numeric(),
                                      RelVariance = numeric(),
                                      stringsAsFactors = FALSE)
  
  for (PC in 1:num_significant_PCs) {
    col_corrs <- correlation_frame[, PC]
    if (sum(!is.na(col_corrs)) > 0) {
      max_idx <- which.max(abs(col_corrs))
      var_name <- rownames(correlation_frame)[max_idx]
      corr_value <- col_corrs[max_idx]
      
      # Define plot title and y-axis label
      main_title <- paste0("PC ", PC, " - ", var_name, " (", corr_value, ")")
      rel_var_text <- paste0("PC ", PC, " (", round(rel_variance[PC] * 100, 1), "% variance)")
      
      # Retrieve the phenotype data for the best-correlated variable
      phenotype_data <- pheno[, var_name]
      
      # Create enhanced plots based on the variable type
      if (is.numeric(phenotype_data)) {
        # Prepare data frame for plotting
        plot_data <- data.frame(phenotype = phenotype_data, PC = pca$x[, PC])
        
        # Calculate linear model to add regression line and extract metrics
        lm_model <- stats::lm(PC ~ phenotype, data = plot_data)
        r2_val <- summary(lm_model)$r.squared
        corr_test <- stats::cor.test(plot_data$phenotype, plot_data$PC)
        
        p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = phenotype, y = PC)) +
          ggplot2::geom_point(ggplot2::aes(color = as.factor(phenotype)), size = 2) +
          ggplot2::geom_smooth(method = "lm", se = TRUE, color = "blue") +
          ggplot2::labs(title = main_title,
                        subtitle = paste0("r = ", round(corr_test$estimate, 2), 
                                          ", p = ", round(corr_test$p.value, 4),
                                          ", R² = ", round(r2_val, 2)),
                        x = var_name,
                        y = rel_var_text) +
          ggplot2::theme_minimal()
        
        print(p)
      } else {
        # For categorical data, treat the phenotype as a factor and use a boxplot with jitter
        plot_data <- data.frame(phenotype = factor(phenotype_data), PC = pca$x[, PC])
        p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = phenotype, y = PC, fill = phenotype)) +
          ggplot2::geom_boxplot(alpha = 0.7) +
          ggplot2::geom_jitter(width = 0.2, size = 2, ggplot2::aes(color = phenotype)) +
          ggplot2::labs(title = main_title,
                        x = var_name,
                        y = rel_var_text) +
          ggplot2::theme_minimal()
        
        print(p)
      }
      
      # Print console message with highlighting if correlation is strong
      if (abs(corr_value) > 0.3) {
        corr_value_text <- paste0("\033[32m", corr_value, "\033[0m")
        var_name_text <- paste0("\033[32m", var_name, "\033[0m")
        highlighter <- " - "
      } else {
        corr_value_text <- corr_value
        var_name_text <- var_name
        highlighter <- ""
      }
      
      cat("\n", highlighter, var_name_text, "(", max_idx, 
          ") is most correlated with PC", PC, "(", corr_value_text, 
          ") [", round(rel_variance[PC] * 100, 1), "%]", highlighter, "\n")
      
      detailed_correlations <- rbind(detailed_correlations, data.frame(
        Variable = var_name,
        Variable_index = max_idx,
        PC = PC,
        Correlation = corr_value,
        RelVariance = round(rel_variance[PC] * 100, 1),
        stringsAsFactors = FALSE
      ))
    } else {
      detailed_correlations <- rbind(detailed_correlations, data.frame(
        Variable = NA,
        Variable_index = NA,
        PC = PC,
        Correlation = NA,
        RelVariance = round(rel_variance[PC] * 100, 1),
        stringsAsFactors = FALSE
      ))
    }
  }
  dev.off()
  
  ### Create additional pairwise PC plots for the PCs with strong phenotype correlations
# Filter for PCs with absolute correlation > 0.3
selected_detail <- detailed_correlations[!is.na(detailed_correlations$Correlation) & 
                                           abs(detailed_correlations$Correlation) > 0.3, ]
if (nrow(selected_detail) >= 2) {
  pairwise_path <- file.path(output_dir, paste0("01_PCA_PairwisePlots_", savename, ".pdf"))
  pdf(file = pairwise_path, width = 8, height = 6)
  
  # Ensure gridExtra is available for arranging plots when needed
  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("Package 'gridExtra' is required for arranging plots when both phenotypes have many unique values.")
  }
  
  # Get the list of PC indices that meet the criteria
  pcs <- selected_detail$PC
  # Create pairwise combinations
  combs <- utils::combn(pcs, 2)
  
  for (i in 1:ncol(combs)) {
    pc1 <- combs[1, i]
    pc2 <- combs[2, i]
    # Get the best correlated phenotype for each selected PC
    var1 <- selected_detail$Variable[selected_detail$PC == pc1]
    var2 <- selected_detail$Variable[selected_detail$PC == pc2]
    
    # Prepare a data frame with the two PC scores and corresponding phenotype variables
    plot_data <- data.frame(
      PC1 = pca$x[, pc1],
      PC2 = pca$x[, pc2],
      Phenotype1 = as.factor(pheno[, var1]),
      Phenotype2 = as.factor(pheno[, var2])
    )
    
    # Compute Pearson correlation between PC1 and PC2
    pc_cor <- stats::cor(plot_data$PC1, plot_data$PC2)
    
    # Determine aesthetic mapping based on number of unique values in each phenotype
    threshold <- 6  # threshold for "low" unique counts
    n_unique1 <- length(unique(pheno[, var1]))
    n_unique2 <- length(unique(pheno[, var2]))
    
    # Case 1: Both phenotypes have many unique values
    if (n_unique1 > threshold & n_unique2 > threshold) {
      # Plot A: Use Phenotype1 for color, fixed shape (e.g., shape=16)
      p1 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = PC1, y = PC2, color = Phenotype1)) +
        ggplot2::geom_point(shape = 16, size = 2) +
        ggplot2::labs(title = paste0("PC", pc1, " vs PC", pc2, " (Plot A)"),
                      subtitle = paste0("Correlation = ", round(pc_cor, 2),
                                        " | Phenotype1 (color): ", var1),
                      x = paste0("PC", pc1, " (", round(rel_variance[pc1] * 100, 1), "% variance)"),
                      y = paste0("PC", pc2, " (", round(rel_variance[pc2] * 100, 1), "% variance)")) +
        ggplot2::theme_minimal()
      
      # Plot B: Use Phenotype2 for color, fixed shape
      p2 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = PC1, y = PC2, color = Phenotype2)) +
        ggplot2::geom_point(shape = 16, size = 2) +
        ggplot2::labs(title = paste0("PC", pc1, " vs PC", pc2, " (Plot B)"),
                      subtitle = paste0("Correlation = ", round(pc_cor, 2),
                                        " | Phenotype2 (color): ", var2),
                      x = paste0("PC", pc1, " (", round(rel_variance[pc1] * 100, 1), "% variance)"),
                      y = paste0("PC", pc2, " (", round(rel_variance[pc2] * 100, 1), "% variance)")) +
        ggplot2::theme_minimal()
      
      # Arrange the two plots vertically on one page
      gridExtra::grid.arrange(p1, p2, ncol = 1)
      
    } else if (n_unique1 > threshold) {
      # Case 2: Only Phenotype1 has many unique values; use it for color and Phenotype2 for shape
      p_pair <- ggplot2::ggplot(plot_data, ggplot2::aes(x = PC1, y = PC2, color = Phenotype1, shape = Phenotype2)) +
        ggplot2::geom_point(size = 2) +
        ggplot2::labs(title = paste0("PC", pc1, " vs PC", pc2),
                      subtitle = paste0("Correlation = ", round(pc_cor, 2),
                                        " | Phenotype1 (color): ", var1,
                                        " | Phenotype2 (shape): ", var2),
                      x = paste0("PC", pc1, " (", round(rel_variance[pc1] * 100, 1), "% variance)"),
                      y = paste0("PC", pc2, " (", round(rel_variance[pc2] * 100, 1), "% variance)")) +
        ggplot2::theme_minimal()
      
      print(p_pair)
      
    } else if (n_unique2 > threshold) {
      # Case 3: Only Phenotype2 has many unique values; use it for color and Phenotype1 for shape
      p_pair <- ggplot2::ggplot(plot_data, ggplot2::aes(x = PC1, y = PC2, color = Phenotype2, shape = Phenotype1)) +
        ggplot2::geom_point(size = 2) +
        ggplot2::labs(title = paste0("PC", pc1, " vs PC", pc2),
                      subtitle = paste0("Correlation = ", round(pc_cor, 2),
                                        " | Phenotype1 (shape): ", var1,
                                        " | Phenotype2 (color): ", var2),
                      x = paste0("PC", pc1, " (", round(rel_variance[pc1] * 100, 1), "% variance)"),
                      y = paste0("PC", pc2, " (", round(rel_variance[pc2] * 100, 1), "% variance)")) +
        ggplot2::theme_minimal()
      
      print(p_pair)
      
    } else {
      # Case 4: Both phenotypes have few unique values; default mapping
      p_pair <- ggplot2::ggplot(plot_data, ggplot2::aes(x = PC1, y = PC2, color = Phenotype1, shape = Phenotype2)) +
        ggplot2::geom_point(size = 2) +
        ggplot2::labs(title = paste0("PC", pc1, " vs PC", pc2),
                      subtitle = paste0("Correlation = ", round(pc_cor, 2),
                                        " | Phenotype1 (color): ", var1,
                                        " | Phenotype2 (shape): ", var2),
                      x = paste0("PC", pc1, " (", round(rel_variance[pc1] * 100, 1), "% variance)"),
                      y = paste0("PC", pc2, " (", round(rel_variance[pc2] * 100, 1), "% variance)")) +
        ggplot2::theme_minimal()
      
      print(p_pair)
    }
  }
  dev.off()
} else {
  pairwise_path <- NA
}
	# also make density plots because why not
	# Beta <- toy_data$Beta    # Beta is a matrix with rows as features and columns as samples
	# pheno <- toy_data$pheno  # pheno contains sample information
	generateSamplePlotsForBetas(list(Beta = Beta, pheno=pheno),output_dir)
	
  return(list(correlation_frame = correlation_frame,
              detailed_correlations = detailed_correlations,
              pca_result = pca,
              scree_plot_path = scree_path,
              plots_path = plot_path,
              pairwise_plots_path = pairwise_path))
}



