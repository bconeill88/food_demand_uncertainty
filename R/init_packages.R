# Helper function to guarantee that a required R package is available and loaded,
# in a way that allows scripts to run automatically on different machines (local, 
# cluster, etc.) without manual package installation or library path management.
#
# What this function does:
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
  
  # Set default CRAN mirror if not already set
  if (is.null(getOption("repos")) || getOption("repos")["CRAN"] == "@CRAN@") {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  }
  
  if (!requireNamespace(pkg, quietly = TRUE)) {
    if (!is.null(local_lib)) {
      if (!dir.exists(local_lib)) {
        dir.create(local_lib, recursive = TRUE, showWarnings = FALSE)
      }
      install.packages(pkg, lib = local_lib)
      library(pkg, character.only = TRUE, lib.loc = local_lib)
    } else {
      install.packages(pkg)
      library(pkg, character.only = TRUE)
    }
  } else {
    if (!is.null(local_lib)) {
      library(pkg, character.only = TRUE, lib.loc = local_lib)
    } else {
      library(pkg, character.only = TRUE)
    }
  }
}

