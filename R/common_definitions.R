

# subdirectories of "data/raw/" to use
#rawdata_dir <- "update9_cnstrlam_agg32FE_24jan25"
rawdata_dir <- "unweighted"

# names of raw parameter and observational data files to use
param_data_global_file <- "ambrosia_9_params_24Jan25.dat"
param_data_FE_file <- "FE_paramsJan2425.dat"
obs_data_file <- "Processed_group_data_13Jan25.csv"

# for these raw data files, the initial and final iteration number to use,
# accounting for the burn in period
iter_start <- 193001  # for 24 Jan 2025 files
iter_end <- Inf       # goes up to 240k

# size of sub-sample to take to do the analysis on
subsample <- 10000

# subdirectories of "data/processed/" to use
procdata_dir <- "update9_cnstrlam_agg32FE_24jan25"
procdata_subdir <- "Ref_ML_gcam/ens_bc_20251009_221027"
gcam_results_dir <- "results_gcam"

# paths to ambrosia source on different machines
ambrosia_path_windows <- "H:/My Drive/R projects/food_demand/ambrosia"
ambrosia_path_pic     <- "/qfs/people/onei736/food_demand/ambrosia"