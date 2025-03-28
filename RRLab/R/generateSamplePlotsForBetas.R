#' generateSamplePlotsForBetas
#'
#' This function generates sample density and boxplots for beta values using a provided toy dataset.
#' It creates a density plot overlaying the beta value distributions for each sample and a boxplot to compare
#' the distribution of beta values across samples. The plots are saved as PDF files in the specified output directory.
#'
#' @param toy_data A list containing:
#'   \describe{
#'     \item{Beta}{A numeric matrix of beta values with rows as features and columns as samples.}
#'     \item{pheno}{A data frame with sample information including a 'SampleID' column.}
#'   }
#' @param output_dir Character. Directory where the plots will be saved. Defaults to "./plots".
#' @param density_filename Character. Filename for the density plot PDF. Defaults to "density_plot.pdf".
#' @param boxplot_filename Character. Filename for the boxplot PDF. Defaults to "boxplot.pdf".
#'
#' @return No return value. The function saves the density plot and boxplot as PDF files and outputs their paths via console messages.
#'
#' @examples
#' \dontrun{
#'   # Assuming toy_data is a list with Beta and pheno components.
#'   generateSamplePlots(toy_data, output_dir = "./plots", 
#'                       density_filename = "density_plot.pdf", 
#'                       boxplot_filename = "boxplot.pdf")
#' }
#'
#' @export
#' 
generateSamplePlotsForBetas <- function(toy_data, 
                                output_dir = "./plots", 
                                density_filename = "density_plot.pdf", 
                                boxplot_filename = "boxplot.pdf") {
  # Create output directory if it does not exist
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  Beta <- toy_data$Beta    # Beta is a matrix with rows as features and columns as samples
  pheno <- toy_data$pheno  # pheno contains sample information
  n_samples <- ncol(Beta)
  
  # Define a color palette for the samples
  colors <- rainbow(n_samples)
  
  ## Density Plot: Overlay density curves for each sample
  density_path <- file.path(output_dir, density_filename)
  pdf(file = density_path)
  
  # Pre-calculate density estimates for each sample and determine the overall y-axis max
  y_max <- 0
  densities <- vector("list", n_samples)
  for (i in 1:n_samples) {
    # Calculate density of Beta values for the sample, forcing the range [0, 1]
    dens <- density(Beta[, i], from = 0, to = 1)
    densities[[i]] <- dens
    y_max <- max(y_max, max(dens$y))
  }
  
  # Set up an empty plot
  plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, y_max),
       xlab = "Beta Value", ylab = "Density", 
       main = "Density Plot of Beta Values for Each Sample")
  
  # Overlay density lines for each sample
  for (i in 1:n_samples) {
    lines(densities[[i]]$x, densities[[i]]$y, col = colors[i])
  }
  
  # Add legend with sample IDs (adjust cex if too many samples)
  legend("topright", legend = pheno$SampleID, col = colors, lty = 1, cex = 0.6)
  dev.off()
  
  ## Boxplot: Compare the distribution of Beta values across samples
  boxplot_path <- file.path(output_dir, boxplot_filename)
  pdf(file = boxplot_path)
  
  # Convert the Beta matrix to a data frame so that each column is a sample
  boxplot(as.data.frame(Beta), 
          col = colors,
          main = "Boxplot of Beta Values by Sample",
          xlab = "Samples",
          ylab = "Beta Values",
          las = 2,      # Rotate x-axis labels for better readability
          cex.axis = 0.8)
  dev.off()
  
  message("Density plot saved to: ", density_path)
  message("Boxplot saved to: ", boxplot_path)
}
