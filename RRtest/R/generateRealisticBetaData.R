#' generateRealisticBetaData
#'
#' This function simulates realistic DNA methylation beta values along with accompanying phenotype data.
#' It generates a phenotype data frame with various covariates including significant effects (e.g., Sex, Age, CellType),
#' mild effects (e.g., Mild1, Mild2, Mild3), and null covariates (e.g., Null1, Null2, Null3). A baseline beta matrix is 
#' created with values drawn from a Beta distribution, after which covariate effects are added to each feature (CpG) 
#' based on predefined proportions for no effect, a single effect, or multiple effects.
#'
#' @param n_features Integer. Number of features (CpGs) to simulate. Default is 1000.
#' @param n_samples Integer. Number of samples to simulate. Default is 100.
#' @param none_prop Numeric. Proportion of features with no covariate effect. Must be between 0 and 1. Default is 0.5.
#' @param one_prop Numeric. Proportion of features driven by a single covariate effect. Must be between 0 and 1. Default is 0.3.
#' @param multiple_prop Numeric. Proportion of features driven by multiple covariate effects. Must be between 0 and 1. Default is 0.2.
#' @param shape1 Numeric. First shape parameter for the Beta distribution used to generate baseline beta values. Default is 0.5.
#' @param shape2 Numeric. Second shape parameter for the Beta distribution used to generate baseline beta values. Default is 0.5.
#'
#' @return A list containing:
#' \describe{
#'   \item{Beta}{A matrix of simulated methylation beta values (n_features x n_samples) adjusted for covariate effects.}
#'   \item{pheno}{A data frame of simulated phenotype data including covariates such as SampleID, Sex, Age, CellType, Mild1, Mild2, Mild3, and additional null covariates.}
#'   \item{categories}{A character vector indicating the effect category ("none", "single", or "multiple") assigned to each feature (CpG).}
#' }
#'
#' @examples
#' \dontrun{
#'   simulated_data <- generateRealisticBetaData(n_features = 1000, n_samples = 100,
#'                                               none_prop = 0.5, one_prop = 0.3, multiple_prop = 0.2,
#'                                               shape1 = 0.5, shape2 = 0.5)
#'   Beta <- simulated_data$Beta
#'   pheno <- simulated_data$pheno
#'   categories <- simulated_data$categories
#' }
#'
#' @export

generateRealisticBetaData <- function(n_features = 1000, 
                                     n_samples = 100, 
                                     none_prop = 0.5,    # proportion of CpGs with no effect
                                     one_prop = 0.3,     # proportion driven by a single effect
                                     multiple_prop = 0.2, # proportion driven by multiple effects
                                     shape1 = 0.5, 
                                     shape2 = 0.5) {
  # Check that the proportions sum to 1
  if (abs(none_prop + one_prop + multiple_prop - 1) > 1e-6) {
    stop("The proportions none_prop, one_prop, and multiple_prop must sum to 1.")
  }
  
  #---------------------------------------------------------------
  # 1. Generate the Phenotype Data (pheno)
  #    - Significant covariates:
  #         Sex, Age, CellType
  #    - Mild covariates:
  #         Mild1 (e.g., BMI), Mild2 (Smoking), Mild3 (Alcohol)
  #    - Null covariates (no effect on methylation):
  #         Null1 (StudySite), Null2 (Batch), Null3 (Random Factor)
  #---------------------------------------------------------------
  pheno <- data.frame(
    SampleID   = paste0("Sample_", 1:n_samples),
    Sex        = sample(c("Male", "Female"), n_samples, replace = TRUE),
    Age        = round(rnorm(n_samples, mean = 50, sd = 10)),
    CellType   = sample(c("TypeA", "TypeB", "TypeC"), n_samples, replace = TRUE),
    Mild1      = round(rnorm(n_samples, mean = 25, sd = 3), 1),  # e.g., BMI
    Mild2      = sample(c("Non-Smoker", "Smoker"), n_samples, replace = TRUE, prob = c(0.8, 0.2)),
    Mild3      = sample(c("Low", "Medium", "High"), n_samples, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
    Null1      = sample(c("Site1", "Site2", "Site3"), n_samples, replace = TRUE),
    Null2      = sample(c("Batch1", "Batch2", "Batch3"), n_samples, replace = TRUE),
    Null3      = sample(c("FactorA", "FactorB", "FactorC"), n_samples, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  #---------------------------------------------------------------
  # 2. Create the baseline Beta Matrix (n_features x n_samples)
  #    Simulate methylation values drawn from a Beta distribution
  #    that typically shows a bimodal (0 and 1 enriched) pattern.
  #---------------------------------------------------------------
  Beta <- matrix(rbeta(n_features * n_samples, shape1, shape2), 
                 nrow = n_features, ncol = n_samples)
  
  # Pre-calculate medians for continuous covariates (for centering)
  med_age   <- median(pheno$Age)
  med_mild1 <- median(pheno$Mild1)
  
  # List of potential effect names from the pheno data (only those with influence)
  effect_names <- c("Sex", "Age", "CellType", "Mild1", "Mild2", "Mild3")
  
  #---------------------------------------------------------------
  # 3. Assign each CpG a category: "none", "single", or "multiple"
  #    using realistic proportions.
  #---------------------------------------------------------------
  set.seed(123)  # for reproducibility of category assignment
  categories <- sample(c("none", "single", "multiple"), size = n_features, 
                       replace = TRUE, prob = c(none_prop, one_prop, multiple_prop))
  
  #---------------------------------------------------------------
  # 4. Loop through each CpG and add covariate effects based on category
  #---------------------------------------------------------------
  for (i in 1:n_features) {
    # Start with no effect for this CpG (a vector of zeros for each sample)
    effect_vector <- rep(0, n_samples)
    
    if (categories[i] == "single") {
      # Select one effect randomly from the list
      selected_effect <- sample(effect_names, 1)
      effect_vector <- switch(selected_effect,
                              "Sex" = ifelse(pheno$Sex == "Female", 0.2, 0),
                              "Age" = (pheno$Age - med_age) * 0.005,
                              "CellType" = ifelse(pheno$CellType == "TypeA", -0.2, 
                                                  ifelse(pheno$CellType == "TypeB", 0, 0.2)),
                              "Mild1" = ifelse(pheno$Mild1 > med_mild1, 0.05, -0.05),
                              "Mild2" = ifelse(pheno$Mild2 == "Smoker", 0.05, 0),
                              "Mild3" = ifelse(pheno$Mild3 == "High", 0.05, 
                                               ifelse(pheno$Mild3 == "Medium", 0.025, 0))
      )
      
    } else if (categories[i] == "multiple") {
      # Randomly choose 2 or 3 unique effects from the list
      n_effects <- sample(2:3, 1)
      selected_effects <- sample(effect_names, n_effects, replace = FALSE)
      
      # Sum the effects from each selected covariate
      for (eff in selected_effects) {
        effect_vector <- effect_vector + switch(eff,
                                                "Sex" = ifelse(pheno$Sex == "Female", 0.2, 0),
                                                "Age" = (pheno$Age - med_age) * 0.005,
                                                "CellType" = ifelse(pheno$CellType == "TypeA", -0.2, 
                                                                    ifelse(pheno$CellType == "TypeB", 0, 0.2)),
                                                "Mild1" = ifelse(pheno$Mild1 > med_mild1, 0.05, -0.05),
                                                "Mild2" = ifelse(pheno$Mild2 == "Smoker", 0.05, 0),
                                                "Mild3" = ifelse(pheno$Mild3 == "High", 0.05, 
                                                                 ifelse(pheno$Mild3 == "Medium", 0.025, 0))
        )
      }
    }
    # If category is "none", effect_vector remains 0.
    
    # Add the effect vector to the baseline Beta values for CpG i
    Beta[i, ] <- Beta[i, ] + effect_vector
    
    # Ensure Beta values remain within [0, 1]
    Beta[i, ] <- pmax(0, pmin(1, Beta[i, ]))
  }
  
  # Return the simulated Beta matrix, phenotype data, and the category assignment
  return(list(Beta = Beta, pheno = pheno, categories = categories))
}
