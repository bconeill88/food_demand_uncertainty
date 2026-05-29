# Helper function to guarantee that a required R package is available and loaded,
# in a way that allows scripts to run automatically on different machines (local, 
# cluster, etc.) without manual package installation or library path management.
#
# 1. Takes the name of a package (optionally with a user-specified local library path).
# 2. Ensures a valid CRAN mirror is set (defaults to cloud.r-project.org if unset);
#    This is important because in some environments (e.g., HPC clusters) R often starts
#    with no default repository, in which case this function will fail.
# 3. Checks whether the package is already installed.
#       • If not installed:
#             - Creates the local library directory (if provided and missing).
#             - Installs the package (in the local library if specified, otherwise default).
# 4. Loads the package, using the local library if supplied.
#
ensure_package <- function(pkg, local_lib = NULL) {
  pkg <- as.character(substitute(pkg))
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  
  # Helper: choose a writable user library
  choose_userlib <- function() {
    userlib <- Sys.getenv("R_LIBS_USER")
    if (nzchar(userlib)) return(userlib)
    file.path(path.expand("~"), "R", "library", paste0("R-", getRversion()))
  }
  
  # If already installed somewhere on current .libPaths(), just attach
  if (requireNamespace(pkg, quietly = TRUE)) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
    return(invisible(TRUE))
  }
  
  # Decide where to install:
  # 1) If caller provided local_lib, use it.
  # 2) Else, if first lib path is writable, use default behavior.
  # 3) Else, install into a user library and prepend it.
  if (is.null(local_lib)) {
    first_lib <- .libPaths()[1]
    if (dir.exists(first_lib) && file.access(first_lib, 2) == 0) {
      install.packages(pkg, dependencies = TRUE)
      suppressPackageStartupMessages(library(pkg, character.only = TRUE))
      return(invisible(TRUE))
    } else {
      local_lib <- choose_userlib()
    }
  }
  
  # Ensure local_lib exists and is on .libPaths() first
  if (!dir.exists(local_lib)) dir.create(local_lib, recursive = TRUE, showWarnings = FALSE)
  .libPaths(c(local_lib, .libPaths()))
  
  # Install into local_lib
  install.packages(pkg, lib = local_lib, dependencies = TRUE)
  suppressPackageStartupMessages(library(pkg, character.only = TRUE, lib.loc = local_lib))
  
  invisible(TRUE)
}
