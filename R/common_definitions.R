
# Directories and file names for Raw MCMC and observational data ---------------

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

# Directories for GCAM raw output ------------------------------------------------

# define sub-directories of data/raw where GCAM output can be found
gcam_rawdata_dir <- "final_rgcam_outputs"
gcam_rawdata_subdir <- "tables_1dec25"

# Directories for analysis results ---------------------------------------------

# subdirectory of "data/processed/" to use for data analysis results
procdata_dir <- "mcmc_23nov25"

# subdirectory of "procdata_dir" to extract results from
procdata_subdir_RefMLgcam <- "Ref_ML_gcam/ens_bc_combined_20251203_070624"
procdata_subdir_RefMLHPgcam <- "Ref_ML_HP_gcam/ens_bc_combined_20251211_024942"
demand_diffs_subdir <- "diffs_Ref_ML_HP_gcam"

# subdirectory of "data/processed/" to use for processed gcam results
gcam_results_dir <- "results_gcam"

# Create directories that don't exist yet --------------------------------------

directories_to_check <- c(
  file.path("data", "processed", procdata_dir, "inputs"),
  file.path("data", "processed", procdata_dir, gcam_results_dir)
)
lapply(directories_to_check, function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE) # recursive = TRUE creates parent directories if needed
    message(paste("Created directory:", dir_path))
  }
})

# Paths to ambrosia ------------------------------------------------------------

# paths to ambrosia source on different machines
ambrosia_path_windows <- "H:/My Drive/R projects/food_demand/ambrosia"
ambrosia_path_pic     <- "/qfs/people/onei736/food_demand/ambrosia"

# Parameters for data cleaning -------------------------------------------------

# for raw MCMC data files, the initial and final iteration number to use,
# accounting for the burn in period
# 24 Jan 2025: 193001 - Inf
# 16 Nov 2025: 1 - Inf
# 23 Nov 2025: 69500 - Inf
iter_start <- 69500
iter_end <- Inf

# size of sub-sample of the raw MCMC results to take to do the analysis on
subsample <- 10000

# Parameters for parameter-based uncertainty intervals -------------------------
# SHOULD THE R SCRIPT HERE BE "DEMAND FOR PARAMETER INTERVALS", WHILE BELOW
# IT WOULD BE "PARAMETERS FOR DEMAND INTERVALS"?

# confidence interval to use for the parameter uncertainty ranges in 
# 02_parameter_uncertainty_intervals.R
confinterval <- 90

# Parameters for demand calculations -------------------------------------------

# minimum food demand thresholds for demand function; not currently used (hard wired
# in ambrosia) but retained for possible future use
Qs_floor <- NULL
Qn_floor <- NULL

# Parameters for demand-based uncertainty intervals ----------------------------

# years to use for identifying iterations in 07_parameters_for_intervals.R
year_iter <- c(seq(2020,2100, by=5))
# quantile intervals for defining high and low demand
hi_min <- 95
hi_max <- 100
lo_min <- 0
lo_max <- 5

# Scenario lists to run demand calculations for --------------------------------

# scenarios to run in 03a_calculate_demand_ens_only.R
# scen_list_demand_ens_only

# scenarios to run in 03b_calculate_demand_ML.R
# scen_list_demand_ML

# scenarios to run in 03c_calculate_demand_GCAM_ML.R
scen_list_demand_GCAM_ML <- 
  # list("Ref_ML_gcam_MLparams_bc")
  # list("Ref_ML_gcam_ens_bc")
  list("Ref_ML_HP_gcam_ens_bc")
  # list("Ref_ML_gcam_MLparams_bc", "Ref_ML_gcam_ens_bc")

# scenarios to run in 08_calculate_demand_GCAM_HDLD.R
scen_list_demand_GCAM_HDLD <-
  #  list("Ref_ML_gcam_LDparams_bc")
  #  list("Ref_ML_gcam_HDparams_bc")
  #  list("Ref_LD_gcam_LDparams_bc")
  #  list("Ref_HD_gcam_HDparams_bc")
  #  list("Ref_ML_gcam_LDparams_bc", "Ref_ML_gcam_HDparams_bc")
  #  list("Ref_ML_gcam_LDparams_bc", "Ref_ML_gcam_HDparams_bc",
  #       "Ref_HD_gcam_HDparams_bc", "Ref_LD_gcam_LDparams_bc")
  # list("Ref_HD_gcam_ens_bc")
  list("Ref_LD_gcam_ens_bc")

