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
  
  # Compute correlations between PCs and phenotypic variables (coefficients)
  correlation_frame <- as.data.frame(
    apply(pca$x[, 1:min(num_significant_PCs, ncol(Beta))], 2, function(pc_scores) {
      sapply(as.data.frame(pheno_numeric), function(pheno_var) {
        if (stats::sd(pheno_var) == 0) return(NA)
        test <- stats::cor.test(pc_scores, pheno_var)
        if (test$p.value < p_threshold) {
          return(round(test$estimate, 2))
        } else {
          return(NA)
        }
      })
    })
  )
  rownames(correlation_frame) <- colnames(pheno_numeric)
  
  # Also compute p values
  correlation_frame_p <- as.data.frame(
    apply(pca$x[, 1:min(num_significant_PCs, ncol(Beta))], 2, function(pc_scores) {
      sapply(as.data.frame(pheno_numeric), function(pheno_var) {
        if (stats::sd(pheno_var) == 0) return(NA)
        test <- stats::cor.test(pc_scores, pheno_var)
        return(round(test$p.value, 4))
      })
    })
  )
  
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
      
      p_pair <- ggplot2::ggplot(plot_data, ggplot2::aes(x = PC1, y = PC2)) +
        ggplot2::geom_point(ggplot2::aes(color = Phenotype1, shape = Phenotype2), size = 2) +
        ggplot2::geom_smooth(method = "lm", se = TRUE, color = "blue") +
        ggplot2::labs(title = paste0("PC", pc1, " vs PC", pc2),
                      subtitle = paste0("Correlation = ", round(pc_cor, 2),
                                        " | Phenotype1 (color): ", var1,
                                        " | Phenotype2 (shape): ", var2),
                      x = paste0("PC", pc1, " (", round(rel_variance[pc1] * 100, 1), "% variance)"),
                      y = paste0("PC", pc2, " (", round(rel_variance[pc2] * 100, 1), "% variance)")) +
        ggplot2::theme_minimal()
      
      print(p_pair)
    }
    dev.off()
  } else {
    pairwise_path <- NA
  }
  
  return(list(correlation_frame = correlation_frame,
              detailed_correlations = detailed_correlations,
              pca_result = pca,
              scree_plot_path = scree_path,
              plots_path = plot_path,
              pairwise_plots_path = pairwise_path))
}



