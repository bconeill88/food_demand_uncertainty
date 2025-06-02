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

