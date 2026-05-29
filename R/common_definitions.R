# Definitions common across multiple scripts or to set up specific runs

# Directories and file names for Raw MCMC and observational data ---------------

# subdirectories of "data/raw/" where MCMC parameter results and observational
# data can be found
mcmc_dir <- "mcmc_params"
obs_dir <- "obs_data"

# names of raw MCMC parameter results files to use
param_data_global_file <- "ambrosia_9_params_23Nov2025.dat"
param_data_FE_file <- "FE_params_23Nov25.dat"

# name of raw observational data file to use
obs_data_file <- "Processed_group_data_13Jan25.csv"

# Directories, file names, and region mapping for GCAM raw output --------------

# define sub-directories of data/raw where GCAM output can be found
gcam_rawdata_dir <- "final_rgcam_outputs"
gcam_rawdata_subdir <- "tables_7mar26"
income_dist_baseyr_file <- "income_dist_by2021.csv"
GCAM_region_ID_mapping <- readRDS(file.path("data", "raw", "GCAM_region_ID_mapping.RDS"))

# Directories for analysis results ---------------------------------------------

# subdirectory of "data/processed/" to use for data analysis results
procdata_dir <- "mcmc_23nov25"

# subdirectory of "procdata_dir" for processed gcam results
gcam_results_dir <- "results_gcam_by2021"

# subdirectory of "procdata_dir" for MCMC parameter uncertainty intervals
# (ML parameters and intervals of parameter uncertainty, not demand uncertainty)
param_intervals_dir <- "param_intervals"

# subdirectory of "procdata_dir" for cleaned MCMC parameter ensembles and
# observational data
clean_data_dir <- "clean_data"

# subdirectory of "procdata_dir" to extract results from
# for base year 2021
procdata_subdir_RefMLgcam <- "Ref_ML_gcam/by2021/ens_bc_combined_20260216_030130"
procdata_subdir_RefMLgcam_price <- 
  "Ref_ML_gcam/by2021/ens_bc_combined_MLprice_20260218_121355"
procdata_subdir_RefMLgcam_income <- 
  "Ref_ML_gcam/by2021/ens_bc_combined_MLincome_20260218_122105"
procdata_subdir_RefMLgcam_scale <- 
  "Ref_ML_gcam/by2021/ens_bc_combined_MLscale_20260218_122355"
procdata_subdir_RefMLHPgcam <- "Ref_ML_HP_gcam/by2021/ens_bc_combined_20260216_113450"
procdata_subdir_RefMLHPgcam_price <- 
  "Ref_ML_HP_gcam/by2021/ens_bc_combined_MLprice_20260218_123119"
procdata_subdir_RefMLHPgcam_income <- 
  "Ref_ML_HP_gcam/by2021/ens_bc_combined_MLincome_20260218_123338"
procdata_subdir_RefMLHPgcam_scale <- 
  "Ref_ML_HP_gcam/by2021/ens_bc_combined_MLscale_20260218_123621"

# sub-subdirectories for specific types of results
demand_abs_subdir <- "abs"
demand_diffs_subdir <- "diffs_Ref_ML_HP_gcam"
demand_both_subdir <- "both"

# Create directories that don't exist yet --------------------------------------

directories_to_check <- c(
  file.path("data", "processed", procdata_dir, clean_data_dir),
  file.path("data", "processed", procdata_dir, gcam_results_dir)
)
lapply(directories_to_check, function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    message(paste("Created directory:", dir_path))
  }
})

# Paths to ambrosia ------------------------------------------------------------

# paths to ambrosia source on different machines
ambrosia_path_windows <- "H:/My Drive/R projects/food_demand/ambrosia"
ambrosia_path_pic     <- "/qfs/people/onei736/food_demand/ambrosia"

# Parameters for data cleaning -------------------------------------------------

# for raw MCMC data files, the initial and final iteration number to use,
# accounting for the burn in period; for results from 23 November 2025
iter_start <- 69500
iter_end <- Inf

# size of sub-sample of the raw MCMC results to take to do the analysis on
subsample <- 10000

# Parameters for parameter-based uncertainty intervals -------------------------

# confidence interval to use for the parameter uncertainty ranges in 
# 02_parameter_stats.R
confinterval <- 90

# Parameters for demand calculations -------------------------------------------

# base year for GCAM-derived input assumptions to ambrosia
base_year <- 2021

# minimum food demand thresholds for demand function; not currently used (hard wired
# in ambrosia) but retained for possible future use
Qs_floor <- 0.6
Qn_floor <- 0

# Parameters for demand-based uncertainty intervals ----------------------------

# years to use for identifying iterations in 05_param_scenario_discovery.R
# start with first year divisible by five that is greater than base year
seq_start <- base_year + (5 - base_year %% 5)
year_iter <- c(seq(seq_start,2100, by=5))

# quantile intervals for defining high and low demand
hi_min <- 95
hi_max <- 100
lo_min <- 0
lo_max <- 5

# Scenario lists to run demand calculations for --------------------------------

# scenarios to run in 03_calculate_demand.R
scen_list_demand_GCAM_ML <- 
  # list("Ref_ML_gcam_MLparams_bc")
  # list("Ref_ML_gcam_ens_bc")
  # list("Ref_ML_HP_gcam_ens_bc")
  # list("Ref_ML_gcam_ens_bc", "Ref_ML_HP_gcam_ens_bc")
  # list("Ref_ML_gcam_MLparams_bc", "Ref_ML_gcam_ens_bc")
  list("Ref_ML_gcam_MLprice_ens_bc", "Ref_ML_gcam_MLincome_ens_bc", "Ref_ML_gcam_MLscale_ens_bc")
  # list("Ref_ML_HP_gcam_MLprice_ens_bc", "Ref_ML_HP_gcam_MLincome_ens_bc", "Ref_ML_HP_gcam_MLscale_ens_bc")
