# Function to install ambrosia once per session (subsequent calls to this function
# will not re-install), unless force_install is TRUE, useful for when edits
# have been made to ambrosia code that need testing

# first a helper to identify the path to ambrosia based on the machine, using 
# paths defined in common_definitions.R
get_ambrosia_path <- function() {
  node <- Sys.info()[["nodename"]]
  
  if (node == "WF10681") {
    return(ambrosia_path_windows)
  } else {
    # default to PIC path for any non-WF10681 node
    return(ambrosia_path_pic)
  }
}

# main function
install_ambrosia_once <- function(path = NULL, force_install = FALSE) {
  
  # If no path supplied, pick one based on machine
  if (is.null(path)) {
    if (!exists("get_ambrosia_path")) {
      stop("get_ambrosia_path() not found. Did you source common_definitions.R?")
    }
    path <- get_ambrosia_path()
  }
  
  already_loaded <- "package:ambrosia" %in% search()
  
  if (already_loaded && !force_install) {
    message("'ambrosia' already loaded — skipping install.")
    return(invisible(TRUE))
  }
  
  # Detach old version if force installing
  if (already_loaded && force_install) {
    message("Detaching previously loaded 'ambrosia' package...")
    detach("package:ambrosia", unload = TRUE, character.only = TRUE)
  }
  
  message("Installing 'ambrosia' from source at: ", path)
  tryCatch({
    devtools::load_all(path)
    # devtools::install(path, upgrade = "never", quiet = TRUE)
    # library(ambrosia)
    message("Successfully installed and loaded 'ambrosia'.")
    invisible(TRUE)
  }, error = function(e) {
    message("Failed to install or load 'ambrosia': ", e$message)
    invisible(FALSE)
  })
}

