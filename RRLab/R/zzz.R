
check_rrlab_update <- function() {
  local_sha <- utils::packageDescription("RRLab")$GithubSHA1

  if (is.null(local_sha) || is.na(local_sha))
    return(invisible(FALSE))

  remote_sha <- tryCatch({
    jsonlite::fromJSON(
      "https://api.github.com/repos/Rrtk2/RRLab/commits/master"
    )$sha
  }, error = function(e) NA_character_)

  if (!is.na(remote_sha) && !identical(remote_sha, local_sha)) {
    packageStartupMessage(
      "A newer version of RRLab is available. Run devtools::install_github('Rrtk2/RRLab/RRLab') to update."
    )
  }

  invisible(TRUE)
}

.onAttach <- function(libname, pkgname) {

	if (!isTRUE(getOption("RRLab.suppressStartup", FALSE))) {
		# Welcome message
		#packageStartupMessage(cat("Welcome to the RRLab package! \nPlease help me improve at GitHub(RRtk2/RRLab)!\nPlease run devtools::install_github('RRtk2/RRLab/RRLab') to get the latest version.\n"))
		
		message <- "Welcome to the RRLab package!"
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
		#cat("Please run devtools::install_github('RRtk2/RRLab/RRLab') to get the latest version.\n")
	}

  s_requiredpackages <- c("ggplot2", "ggfortify", "matlib", "fitdistrplus",
                        "caret", "limma", "tidyverse", "ggsci", "showtext",
                        "foreach", "doParallel", "parallel", "progress",
                        "data.table", "splitstackshape")

  RRLab::libraryR(c(s_requiredpackages))

  check_rrlab_update()


}
