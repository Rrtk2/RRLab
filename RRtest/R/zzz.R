.onAttach <- function(libname, pkgname) {

	# Welcome message
	packageStartupMessage("Welcome to the RRtest package!")
	
	# Set packages
	s_requiredpackages = c("ggplot2","ggfortify","matlib","fitdistrplus","caret","limma","tidyverse","ggsci","showtext") #,"parallel"

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