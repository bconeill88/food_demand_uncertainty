

# subdirectories of "data/raw/" to use
#rawdata_dir <- "update9_cnstrlam_agg32FE_24jan25"
rawdata_dir <- "unweighted"

# names of raw parameter data files to use
# param_data_global_file <- "ambrosia_9_params_24Jan25.dat"
# param_data_FE_file <- "FE_paramsJan2425.dat"
param_data_global_file <- "ambrosia_9_params_16Nov25.dat"
param_data_FE_file <- "FE_params_16Nov25.dat"

# name of observational data file to use
obs_data_file <- "Processed_group_data_13Jan25.csv"

# for these raw data files, the initial and final iteration number to use,
# accounting for the burn in period
# 24 Jan 2025: 193001 - Inf
# 16 Nov 2025: 1 - Inf
iter_start <- 1
iter_end <- Inf

# size of sub-sample to take to do the analysis on
subsample <- 10000

# subdirectories of "data/processed/" to use
procdata_dir <- "mcmc_16nov25"
procdata_subdir <- "Ref_ML_gcam"
# procdata_dir <- "update9_cnstrlam_agg32FE_24jan25"
# procdata_subdir <- "Ref_ML_gcam/ens_bc_20251009_221027"
gcam_results_dir <- "results_gcam"

# create directories that don't exist yet
directories_to_check <- c(
  file.path("data", "processed", procdata_dir, procdata_subdir),
  file.path("data", "processed", procdata_dir, "inputs")
  )
lapply(directories_to_check, function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE) # recursive = TRUE creates parent directories if needed
    message(paste("Created directory:", dir_path))
  }
})

# paths to ambrosia source on different machines
ambrosia_path_windows <- "H:/My Drive/R projects/food_demand/ambrosia"
ambrosia_path_pic     <- "/qfs/people/onei736/food_demand/ambrosia"

# confidence interval to use for the parameter uncertainty ranges in 
# 02_parameter_uncertainty_intervals.R
confinterval <- 90
