.onAttach <- function(libname, pkgname) {

	if (!isTRUE(getOption("RRtest.suppressStartup", FALSE))) {
		# Welcome message
		#packageStartupMessage(cat("Welcome to the RRtest package! \nPlease help me improve at GitHub(RRtk2/RRtest)!\nPlease run devtools::install_github('RRtk2/RRtest/RRtest') to get the latest version.\n"))
		
		message <- "Welcome to the RRtest package!"
		color_set <- c(0,1,0)           # Define a range of ANSI color codes
		n <- nchar(message)        # Number of characters in the message

		for (iter in 1:length(color_set)) {
		cat("\r")               # Return to the start of the line
		for (i in 1:n) {
			# Calculate the color for this letter based on its position and current iteration
			#color_index <- ((i + iter - 1) %% length(colors)) + 1
			#color <- color
			letter <- substr(message, i, i)
			cat(paste0("\033[", color_set[iter], "m", letter, "\033[0m"))
			Sys.sleep(0.005)          # Pause before the next frame (adjust for desired speed)
		}
		#color = color + 1
		flush.console()         # Ensure immediate display of output
		
		}
		cat("\n")
		#cat("Please run devtools::install_github('RRtk2/RRtest/RRtest') to get the latest version.\n")
	}

	s_requiredpackages <- c("ggplot2", "ggfortify", "matlib", "fitdistrplus",
                          "caret", "limma", "tidyverse", "ggsci", "showtext",
                          "foreach", "doParallel", "parallel", "progress",
                          "data.table", "splitstackshape")
  
  if (!requireNamespace("BiocManager", quietly = TRUE)){
    	install.packages("BiocManager", ask = FALSE)
  }

	for (pkg in s_requiredpackages) {
		if (!requireNamespace(pkg, quietly = TRUE))
		BiocManager::install(pkg, ask = FALSE)

		# Load the package quietly and without warnings for masked objects
		suppressPackageStartupMessages(
		library(pkg, character.only = TRUE, warn.conflicts = FALSE)
		)
	}


}