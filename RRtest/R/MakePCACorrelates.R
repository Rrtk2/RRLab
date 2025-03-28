#' MakePCACorrelates
#'
#' @usage
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
#'   # Assume Beta is a numeric matrix and pheno is a data frame with matching sample rows.
#'   result <- MakePCACorrelates(Beta, pheno, 
#'                               output_dir = "./results/", 
#'                               savename = "analysis1", 
#'                               var_threshold = 0.9, 
#'                               p_threshold = 0.05)
#'
#' @export


MakePCACorrelates <- function(Beta, pheno, 
                              output_dir = "./plots/", 
                              savename = "01",
                              var_threshold = 0.9,
                              p_threshold = 0.05) {
  
  # Input validation
  # Convert Beta to a matrix if necessary
  if (!is.matrix(Beta)) {
    Beta_conv <- tryCatch(as.matrix(Beta), error = function(e) e)
    if (inherits(Beta_conv, "error")) {
      stop("Unable to convert Beta to a matrix: ", Beta_conv$message)
    } else {
      Beta <- Beta_conv
      message("Converted Beta to a matrix.")
    }
  }
  
  # Convert pheno to a data frame if necessary
  if (!is.data.frame(pheno)) {
    pheno_conv <- tryCatch(as.data.frame(pheno), error = function(e) e)
    if (inherits(pheno_conv, "error")) {
      stop("Unable to convert pheno to a data frame: ", pheno_conv$message)
    } else {
      pheno <- pheno_conv
      message("Converted pheno to a data frame.")
    }
  }
  
  # Check dimensions; if Beta's dimensions are off, try transposing
  if (ncol(Beta) != nrow(pheno)) {
    if (nrow(Beta) == nrow(pheno)) {
      Beta <- t(Beta)
      if (ncol(Beta) == nrow(pheno)) {
        message("Transposed Beta to match the dimensions of pheno.")
      } else {
        stop("Dimensions of Beta and pheno still do not match after transposing.")
      }
    } else {
      stop("Dimensions of Beta and pheno do not match and cannot be auto-corrected.")
    }
  }
  
  # Ensure pheno has proper row names
  if (is.null(rownames(pheno)) || 
      any(is.na(rownames(pheno)) | rownames(pheno) == "") ||
      all(rownames(pheno) == as.character(seq_len(nrow(pheno))))) {
    rownames(pheno) <- paste0("Sample_", seq_len(nrow(pheno)))
  }
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Perform PCA on complete cases (ensuring Beta has no missing rows)
  completeBeta <- Beta[stats::complete.cases(Beta), ]
  pca <- stats::prcomp(t(completeBeta))
  
  # Save scree plot using base R plotting
  scree_path <- file.path(output_dir, paste0("01_ScreePlot_", savename, ".pdf"))
  pdf(file = scree_path)
  plot(pca, main = "Scree Plot")
  dev.off()
  
  # Compute relative variance explained and determine number of significant PCs
  rel_variance <- round(pca$sdev^2 / sum(pca$sdev^2), 3)
  num_significant_PCs <- min(which(cumsum(rel_variance) > var_threshold))
  cat("PC1 explains", rel_variance[1] * 100, "% of variance in the data\n")
  
  # Convert phenotypic data to a numeric matrix for correlation computations
  pheno_numeric <- apply(pheno, 2, function(x) as.numeric(as.factor(x)))
  pheno_numeric <- pheno_numeric[, colSums(is.na(pheno_numeric)) == 0]
  pheno_df <- as.data.frame(pheno_numeric)
  
  # Determine number of PCs to use
  num_PCs <- min(num_significant_PCs, ncol(Beta))
  
  # Initialize matrices to store correlations and p-values
  # (Use colnames from pheno_df to ensure the lengths match the number of numeric variables)
  corr_mat <- matrix(NA, nrow = ncol(pheno_df), ncol = num_PCs)
  rownames(corr_mat) <- colnames(pheno_df)
  colnames(corr_mat) <- paste0("PC", 1:num_PCs)
  
  pvalue_mat <- matrix(NA, nrow = ncol(pheno_df), ncol = num_PCs)
  rownames(pvalue_mat) <- colnames(pheno_df)
  colnames(pvalue_mat) <- paste0("PC", 1:num_PCs)
  
  # Loop through PCs and phenotypic variables to compute correlations and p-values
  for (i in seq_len(num_PCs)) {
    pc_scores <- pca$x[, i]
    for (j in seq_along(pheno_df)) {
      pheno_var <- pheno_df[[j]]
      if (sd(pheno_var) == 0) next  # skip zero variance variables
      
      test <- cor.test(pc_scores, pheno_var)
      corr_mat[j, i] <- round(test$estimate, 2)
      pvalue_mat[j, i] <- round(test$p.value, 4)
    }
  }
  
  correlation_frame <- corr_mat
  correlation_frame_p <- as.data.frame(pvalue_mat)
  
  ### Plot individual PC vs. phenotype plots using ggplot2
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
      
      main_title <- paste0("PC ", PC, " - ", var_name, " (", corr_value, ")")
      rel_var_text <- paste0("PC ", PC, " (", round(rel_variance[PC] * 100, 1), "% variance)")
      
      phenotype_data <- pheno[, var_name]
      
      if (is.numeric(phenotype_data)) {
        plot_data <- data.frame(phenotype = phenotype_data, PC = pca$x[, PC])
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
  
  ### Create pairwise PC plots for PCs with strong phenotype correlations
  selected_detail <- detailed_correlations[!is.na(detailed_correlations$Correlation) & 
                                             abs(detailed_correlations$Correlation) > 0.3, ]
  if (nrow(selected_detail) >= 2) {
    pairwise_path <- file.path(output_dir, paste0("01_PCA_PairwisePlots_", savename, ".pdf"))
    pdf(file = pairwise_path, width = 8, height = 6)
    
    if (!requireNamespace("gridExtra", quietly = TRUE)) {
      stop("Package 'gridExtra' is required for arranging plots when both phenotypes have many unique values.")
    }
    
    pcs <- selected_detail$PC
    combs <- utils::combn(pcs, 2)
    
    for (i in 1:ncol(combs)) {
      pc1 <- combs[1, i]
      pc2 <- combs[2, i]
      var1 <- selected_detail$Variable[selected_detail$PC == pc1]
      var2 <- selected_detail$Variable[selected_detail$PC == pc2]
      
      plot_data <- data.frame(
        PC1 = pca$x[, pc1],
        PC2 = pca$x[, pc2],
        Phenotype1 = as.factor(pheno[, var1]),
        Phenotype2 = as.factor(pheno[, var2])
      )
      
      pc_cor <- stats::cor(plot_data$PC1, plot_data$PC2)
      threshold <- 6
      n_unique1 <- length(unique(pheno[, var1]))
      n_unique2 <- length(unique(pheno[, var2]))
      
      if (n_unique1 > threshold & n_unique2 > threshold) {
        p1 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = PC1, y = PC2, color = Phenotype1)) +
          ggplot2::geom_point(shape = 16, size = 2) +
          ggplot2::labs(title = paste0("PC", pc1, " vs PC", pc2, " (Plot A)"),
                        subtitle = paste0("Correlation = ", round(pc_cor, 2),
                                          " | Phenotype1 (color): ", var1),
                        x = paste0("PC", pc1, " (", round(rel_variance[pc1] * 100, 1), "% variance)"),
                        y = paste0("PC", pc2, " (", round(rel_variance[pc2] * 100, 1), "% variance)")) +
          ggplot2::theme_minimal()
        
        p2 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = PC1, y = PC2, color = Phenotype2)) +
          ggplot2::geom_point(shape = 16, size = 2) +
          ggplot2::labs(title = paste0("PC", pc1, " vs PC", pc2, " (Plot B)"),
                        subtitle = paste0("Correlation = ", round(pc_cor, 2),
                                          " | Phenotype2 (color): ", var2),
                        x = paste0("PC", pc1, " (", round(rel_variance[pc1] * 100, 1), "% variance)"),
                        y = paste0("PC", pc2, " (", round(rel_variance[pc2] * 100, 1), "% variance)")) +
          ggplot2::theme_minimal()
        
        gridExtra::grid.arrange(p1, p2, ncol = 1)
      } else if (n_unique1 > threshold) {
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
  
  # Show the user where the files were saved
  abs_path <- normalizePath(file.path(getwd(), output_dir), mustWork = FALSE)
  message("Plots saved to: ", abs_path)
  
  return(list(correlation_frame = correlation_frame,
              detailed_correlations = detailed_correlations,
              pca_result = pca,
              scree_plot_path = scree_path,
              plots_path = plot_path,
              pairwise_plots_path = pairwise_path))
}



