# function to install ambrosia once per session (subsequent calls to this function
# will not re-install), unless force_install is TRUE, useful for when edits
# have been made to ambrosia code that need testing
install_ambrosia_once <- function(path, force_install = FALSE) {
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
    devtools::install(path, upgrade = "never", quiet = TRUE)
    library(ambrosia)
    message("Successfully installed and loaded 'ambrosia'.")
  }, error = function(e) {
    message("Failed to install or load 'ambrosia': ", e$message)
  })
}
