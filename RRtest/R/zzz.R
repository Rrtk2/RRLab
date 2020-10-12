.onAttach <- function(libname, pkgname) {

	# Welcome message
	packageStartupMessage(cat("Welcome to the RRtest package! \nPlease help me improve at GitHub(RRtk2/RRtest)!\nPlease run devtools::install_github('RRtk2/RRtest/RRtest') to get the latest version.\n"))
	
	# Set packages
	s_requiredpackages = c("ggplot2","ggfortify","matlib","fitdistrplus","caret","limma","tidyverse","ggsci","showtext","foreach","doParallel","parallel") #,"parallel"

	# Install packages if needed and load, or just load packages
	if (!requireNamespace("BiocManager", quietly = TRUE))
	  install.packages("BiocManager", ask = F)

	for (i in s_requiredpackages) {
		if (!requireNamespace(i, quietly = TRUE))
			BiocManager::install(i, ask = F)  # dependencies = c("Depends", "Imports")
		require(as.character(i), character.only = TRUE)
		#print(i)
	}


}