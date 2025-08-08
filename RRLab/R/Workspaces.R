# all of the files needed to manage the RRLab projects and saving/loading of objects

# INTERNAL: Get config file path
.get_projects_config_file = function() {
  tools::R_user_dir("RRLab", "config") |> fs::path("projects.yaml")
}

.test_projects_config_file_available = function() {
  config_file = .get_projects_config_file()

  config = yaml::read_yaml(config_file)
  Project_folder_dest = config$projects_folder

  if (is.null(Project_folder_dest)) {
    return(FALSE) # Project_folder_dest is missing or invalid 
    } else {
    return(TRUE) # Project_folder_dest exists 
  }
}

#' Set or update the base path for RRLab projects
#'
#' @param path The new projects folder path (absolute or relative)
#' @param create If TRUE (default), the folder is created if it doesn't exist
#' @return Invisible TRUE
#' @export
set_projects_path = function(path, create = TRUE) {
  path_abs = fs::path_abs(path)
  
  if (!fs::dir_exists(path_abs)) {
    if (create) {
      fs::dir_create(path_abs)
    } else {
      stop("Folder does not exist: ", path_abs)
    }
  }
  
  config_file = .get_projects_config_file()
  fs::dir_create(fs::path_dir(config_file))
  yaml::write_yaml(list(projects_folder = path_abs), config_file)
  
  message("RRLab projects path set to: ", path_abs)
  invisible(TRUE)
}

#' Get the current RRLab projects base path
#' @export
.get_projects_path = function() {
  config_file = .get_projects_config_file()
  
  if (!.test_projects_config_file_available()) {
    stop("Projects path not set. Use RRLab::set_projects_path() first.")
  }
  
  config = yaml::read_yaml(config_file)
  path = config$projects_folder
  
  if (is.null(path) || !fs::dir_exists(path)) {
    stop("Stored projects path is missing or invalid: ", path)
  }
  
  fs::path_abs(path)
}

get_config = function() {
  config_file = .get_projects_config_file()
  
  if (!fs::file_exists(config_file)) {
    stop("Projects path not set. Use RRLab::set_projects_path() first.")
  }
  
  config = yaml::read_yaml(config_file)
  return(config)
}

#' Set up a project folder structure inside the RRLab projects path
.setup_project_structure = function(project_name) {
  base = .get_projects_path()
  project_path = fs::path(base, project_name)
  
  fs::dir_create(project_path)
  fs::dir_create(fs::path(project_path, "input"))
  #fs::dir_create(fs::path(project_path, "input" , "data"))
  #fs::dir_create(fs::path(project_path, "input" , "metadata"))
  fs::dir_create(fs::path(project_path, "output"))
  fs::dir_create(fs::path(project_path, "output", "main"))
  fs::dir_create(fs::path(project_path, "scripts"))
  fs::dir_create(fs::path(project_path, "scripts", "main"))
  fs::dir_create(fs::path(project_path, "documents"))
  fs::dir_create(fs::path(project_path, "documents" , "presentations"))
  fs::dir_create(fs::path(project_path, "documents" , "manuscript"))
  fs::dir_create(fs::path(project_path, "documents" , "workflows"))
  fs::dir_create(fs::path(project_path, "documents" , "protocols"))
      
  .generate_settings_file(project_name)

  message("Project created at: ", project_path)
  invisible(project_path)
}

.generate_settings_file = function(project_name) {
  project_path = fs::path(.get_projects_path(), project_name, "scripts", "main", "00_settings.R")
  
  if (!fs::file_exists(project_path)) {
    fs::file_create(project_path)
    writeLines(c("# Project settings file",
                 "# Edit this file to configure your project settings, it will be sourced every time the project is loaded",
                 paste0("project_name <<- '", project_name, "'"),
                 "s_seed = 42",
                 "s_sign_th = 0.05"),
               con = project_path)
    message("Settings file created at: ", project_path)
  } else {
    message("Settings file already exists at: ", project_path)
  }
}


#' Set up a project folder structure inside the RRLab projects path
.load_project_structure = function(project_name, branch = "main") {
  base = .get_projects_path()
  
  # these are needed for saving and loading objects
  s_base_dir <<- fs::path(base, project_name)
  s_folder_raw <<-  fs::path(s_base_dir, "input")  #paste0(s_base_dir,"/Data_RAW/")
  s_folder_qc <<-   fs::path(s_base_dir, "output") #paste0(s_base_dir,"/Data_QC/")
  s_saveloc_qc <<-  fs::path(s_base_dir, "output" , branch) #paste0(s_base_dir,"/Data_QC/")
  s_branch <<- branch

  # source settings file
  settings_file = fs::path(s_base_dir, "scripts", "main", "00_settings.R")
  fs::file_exists(settings_file)

  if(fs::file_exists(settings_file)){
    message("Sourcing settings file: ", settings_file)
    source(settings_file)
  } else {
    message("Settings file does not exist: ", settings_file)
    .generate_settings_file(project_name)
    source(settings_file)
  }
  

}

#' @export
.get_workspaces = function(){
# check if the set_projects_path exists
  if (!.test_projects_config_file_available()) {
    stop("Projects path not set. Use RRLab::set_projects_path() first.")
  }

# ok the config folder exists with the project destination
# next step to if the asked project name exists, if not then list the projects that exsit (these are the folders within the projects path)
  config = get_config()

    existing_projects = fs::dir_ls(config$projects_folder, type = "directory")
    message("Available projects: ", paste(basename(existing_projects), collapse = ", "))

  


}


#' Rsave
#'
#' Convenience wrapper around `qs::qsavem` that saves an object using its
#' name in the calling environment. A prefix and postfix can be supplied to
#' modify the file name. If `file_location` is not supplied, `s_saveloc_qc`
#' will be used when available.
#'
#' @param object Object to save or the name of the object.
#' @param prefix Optional prefix for the file name.
#' @param postfix Optional postfix for the file name.
#' @param file_location Directory where the file will be saved. If `NULL`,
#'   `s_saveloc_qc` will be used when available.
#' @param ... Additional arguments passed to `qs::qsavem`.
#'
#' @return Invisibly returns the file path used.
#' @examples
#' \dontrun{
#' a <- 1:5
#' Rsave(a)
#' Rsave(a, prefix = "08A", postfix = "cohort")
#' }
#' @export
Rsave <- function(object_name, prefix = "", postfix = "", file_location = NULL, ...) {
  obj_expr <- substitute(object_name)
  obj_name <- deparse(obj_expr)

  if (is.null(file_location)) {
    if (exists("s_saveloc_qc", inherits = TRUE)) {
      file_location <- get("s_saveloc_qc", inherits = TRUE)
    } else {
      stop("file_location must be provided or 's_saveloc_qc' must exist")
    }
  }

  # If a character string is provided, treat it as a variable name
  if (is.character(object_name) && length(object_name) == 1 && !exists(obj_name, envir = parent.frame())) {
    obj_name <- object_name
    object_name <- get(obj_name, envir = parent.frame())
  }

  pre <- if (nzchar(prefix)) paste0(prefix, "_") else ""
  post <- if (nzchar(postfix)) paste0("_", postfix) else ""

  if (!grepl("/$", file_location)) file_location = paste0(file_location, "/")

  file_path <- paste0(file_location, paste0(pre, obj_name, post, ".qs"))

  #args <- c(setNames(list(object), obj_name), list(file = file_path), list(...))
  #do.call(qs::qsavem, args, ...)
  qs::qsave(setNames(object = list(object_name), nm = as.character(obj_expr)), file = file_path, ...)

  invisible(file_path)
}


#' Rload
#'
#' Wrapper around `qs::qload` to read objects saved with `Rsave`.
#' If `name` is an object, its name will be used to construct the
#' file name using the supplied prefix and postfix. Alternatively a
#' character string can be provided to directly specify the base file
#' name. If `file_location` is not provided, the function attempts to use
#' `s_saveloc_qc` from the calling environment. If neither is available an
#' error is thrown.
#'
#' @param name Object or character string identifying the file to load.
#' @param prefix Optional prefix used during saving.
#' @param postfix Optional postfix used during saving.
#' @param file_location Directory of the saved file. If `NULL`, `s_saveloc_qc`
#'   is used when available.
#' @param envir Environment to load the object into. Defaults to the caller's
#'   environment.
#' @param ... Additional arguments passed to `qs::qload`.
#'
#' @return Invisibly returns the result of `qs::qload`.
#' @examples
#' \dontrun{
#' Rload(a)
#' Rload("08A_a_cohort")
#' }
#' @export
Rload <- function(name, prefix = "", postfix = "", file_location = NULL,
                  envir = parent.frame(), ...) {

  if(!exists(substitute(name), envir = envir )) {
      name <- as.character(substitute(name))
  }

  if (is.null(file_location)) {
    if (exists("s_saveloc_qc", inherits = TRUE)) {
      file_location <- get("s_saveloc_qc", inherits = TRUE)
    } else {
      stop("file_location must be provided or 's_saveloc_qc' must exist")
    }
  }



  base <- as.character(substitute(name))
  pre <- if (nzchar(prefix)) paste0(prefix, "_") else ""
  post <- if (nzchar(postfix)) paste0("_", postfix) else ""
  base <- paste0(pre, base, post)
  
  if (!grepl("/$", file_location)) file_location = paste0(file_location, "/")

  file_path <- paste0(file_location, paste0(base, ".qs"))
  qs::qload(file_path, env = envir, ...)
}

#' @export
Workspace = function(project_name, branch = "main") {
# check if the set_projects_path exists
  if (!.test_projects_config_file_available()) {
    stop("Projects path not set. Use RRLab::set_projects_path() first.")
  }

# ok the config folder exists with the project destination
# next step to if the asked project name exists, if not then list the projects that exsit (these are the folders within the projects path)
  config = get_config()
  project_path = fs::path(config$projects_folder, project_name)

  if (!fs::dir_exists(project_path)) {
    message("Project does not exist: ", project_name)
    # print existing projects
    existing_projects = fs::dir_ls(config$projects_folder, type = "directory")
    message("Available projects: ", paste(basename(existing_projects), collapse = ", "))

    # now given the assumption that the project needs to be generated, we can create the folder structure
    .setup_project_structure(project_name)
  }

  .load_project_structure(project_name, branch)
  message("Project workspace loaded: ", project_name)


}

#' @export
help_workspaces = function() {
  cli::cli_h1("RRLab Projects")
  cli::cli_text("{cli::col_cyan('First time')}")
  cli::cli_code("RRLab::set_projects_path('D:/Projects')")

  cli::cli_text("{cli::col_cyan('Create or open')}")
  cli::cli_code("RRLab::Workspace(project_name = 'Test')")

  cli::cli_text("{cli::col_cyan('List, info')}")
  cli::cli_code("RRLab::.get_projects_path()")
  cli::cli_code("RRLab::.get_workspaces()")

  cli::cli_text("{cli::col_cyan('Switch branch')}")
  cli::cli_code("RRLab::Workspace(project_name = 'Test', branch = 'main')")
  cli::cli_code("RRLab::Workspace(project_name = 'Test', branch = 'branch2')")

  cli::cli_text("{cli::col_cyan('Save and load')}")
  cli::cli_code("RRLab::Rsave(result_df, prefix = '01', postfix = 'Processed')")
  cli::cli_code("RRLab::Rload(result_df, prefix = '01', postfix = 'Processed')")
  invisible(TRUE)
}

#set_projects_path("D:/Projects")        # Set or change path

#Workspace("Test")
#help_workspaces()