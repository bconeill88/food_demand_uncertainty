

# subdirectories of "data/raw/" where MCMC parameter results can be found
#rawdata_dir <- "update9_cnstrlam_agg32FE_24jan25"
rawdata_dir <- "unweighted"

# names of raw MCMC parameter results files to use
# param_data_global_file <- "ambrosia_9_params_24Jan25.dat"
# param_data_FE_file <- "FE_paramsJan2425.dat"
# param_data_global_file <- "ambrosia_9_params_16Nov25.dat"
# param_data_FE_file <- "FE_params_16Nov25.dat"
param_data_global_file <- "ambrosia_9_params_23Nov2025.dat"
param_data_FE_file <- "FE_params_23Nov25.dat"

# name of observational data file to use
obs_data_file <- "Processed_group_data_13Jan25.csv"

# for these raw data files, the initial and final iteration number to use,
# accounting for the burn in period
# 24 Jan 2025: 193001 - Inf
# 16 Nov 2025: 1 - Inf
# 23 Nov 2025: 69500 - Inf
iter_start <- 69500
iter_end <- Inf

# size of sub-sample of the raw MCMC results to take to do the analysis on
subsample <- 10000

# subdirectories of "data/processed/" to use for data analysis results
procdata_dir <- "mcmc_23nov25"
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

# scenarios to run in 03a_calculate_demand_ens_only.R
# scen_list_demand_ens_only

# scenarios to run in 03b_calculate_demand_ML.R
# scen_list_demand_ML

# scenarios to run in 03c_calculate_demand_GCAM_ML.R
scen_list_demand_GCAM_ML <- 
  #  list("Ref_ML_gcam_MLparams_bc")
  #  list("Ref_ML_gcam_LDparams_bc")
  #  list("Ref_ML_gcam_HDparams_bc")
  #  list("Ref_LD_gcam_LDparams_bc")
  #  list("Ref_HD_gcam_HDparams_bc")
  #  list("Ref_ML_gcam_MLparams_bc", "Ref_ML_gcam_LDparams_bc", "Ref_ML_gcam_HDparams_bc")
  #  list("Ref_ML_gcam_MLparams_bc", "Ref_ML_gcam_LDparams_bc", "Ref_ML_gcam_HDparams_bc",
  #       "Ref_HD_gcam_HDparams_bc", "Ref_LD_gcam_LDparams_bc")
  list("Ref_ML_gcam_ens_bc")

# scenarios to run in 08_calculate_demand_GCAM_HDLD.R
scen_list_demand_GCAM_HDLD <- 
  #  list("Ref_ML_gcam_MLparams_bc")
  #  list("Ref_ML_gcam_LDparams_bc")
  #  list("Ref_ML_gcam_HDparams_bc")
  #  list("Ref_LD_gcam_LDparams_bc")
  #  list("Ref_HD_gcam_HDparams_bc")
  #  list("Ref_ML_gcam_MLparams_bc", "Ref_ML_gcam_LDparams_bc", "Ref_ML_gcam_HDparams_bc")
  #  list("Ref_ML_gcam_MLparams_bc", "Ref_ML_gcam_LDparams_bc", "Ref_ML_gcam_HDparams_bc",
  #       "Ref_HD_gcam_HDparams_bc", "Ref_LD_gcam_LDparams_bc")
  list("Ref_ML_gcam_MLparams_bc", "Ref_ML_gcam_ens_bc")

# minimum food demand thresholds for demand function; not currently used (hard wired
# in ambrosia) but retained for possible future use
Qs_floor <- NULL
Qn_floor <- NULL