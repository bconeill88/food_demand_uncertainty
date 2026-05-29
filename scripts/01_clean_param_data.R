# Read in raw parameter ensemble data and observational data from /data/raw in
# the subdirectories given by mcmc_dir and obs_dir (in common_definitions.R). Clean and
# save data in /data/processed in the subdirectory given by procdata_dir

# load functions and common definitions
source("R/common_definitions.R")
source("R/init_packages.R")

# install or load packages
ensure_package(tidyverse)

# read in data files
# ensemble of global parameters
param_data_global_raw <- 
  read.table(file.path("data", "raw", mcmc_dir, param_data_global_file), header = TRUE)
# ensemble of regional fixed effect parameters
param_data_FE_raw <-
  read.table(file.path("data", "raw", mcmc_dir, param_data_FE_file), header = TRUE) 
# observational data for 32 GCAM regions
obs_data <- 
  read.csv(file.path("data", "raw", obs_dir, obs_data_file), header = TRUE)

# Make sure only iterations in common between datasets are retained

# Count distinct iterations *before* filtering
n_global_before <- dplyr::n_distinct(param_data_global_raw$iteration_number)
n_FE_before     <- dplyr::n_distinct(param_data_FE_raw$iteration_number)
# Identify common iterations
common_iterations <- intersect(
  param_data_global_raw$iteration_number,
  param_data_FE_raw$iteration_number
)
# Filter both datasets to the common iterations
param_data_global_raw <- param_data_global_raw %>% 
  filter(iteration_number %in% common_iterations)
param_data_FE_raw <- param_data_FE_raw %>% 
  filter(iteration_number %in% common_iterations)
# Count after filtering
n_global_after <- dplyr::n_distinct(param_data_global_raw$iteration_number)
n_FE_after     <- dplyr::n_distinct(param_data_FE_raw$iteration_number)
# log how many were removed
cat("Global params: removed", n_global_before - n_global_after, "iterations\n")
cat("FE params:     removed", n_FE_before     - n_FE_after,     "iterations\n")

# manually check for highest value of log likelihood to see which iteration is ML result
# param_data_global_raw[which.max(param_data_global_raw$LL),]

# clean global and FE parameter data, and take subset of data for analysis

# one unique initial step for raw data case: Jan 25 MCMC results
if(grepl("ambrosia_9_params_24Jan25", param_data_global_file)) {
  
  # remove unnecessary column
  param_data_global_raw <- param_data_global_raw %>% select(-chain)

  # check to make sure that data file name exists
} else if(!grepl("ambrosia_9_params_16Nov25", param_data_global_file) &
          !grepl("ambrosia_9_params_23Nov2025", param_data_global_file)) {
  
  stop("Raw parameter data file names not correctly specified.")
}

# common steps for all raw data cases
param_data_global_clean <- param_data_global_raw %>%
  
  # rename for consistency
  rename(iteration = iteration_number) %>%
  # convert all columns to numeric
  mutate_all(as.numeric) %>%
  # keep samples only after burn in
  filter(iteration >= iter_start, iteration <= iter_end) %>%
  # order by iteration for clarity
  arrange(iteration)

param_data_global_clean_sub <- param_data_global_clean %>%
  slice_sample(n = subsample) %>%
  arrange(iteration)

param_data_FE_clean <- param_data_FE_raw %>%
  
  # rename for consistency
  rename(iteration = iteration_number) %>%
  # select same iterations as in global parameter data
  subset(iteration %in% param_data_global_clean$iteration) %>%
  arrange(iteration)

param_data_FE_clean_sub <- param_data_FE_clean %>%
  
  # select same iterations as in sub-sample of global parameter data
  subset(iteration %in% param_data_global_clean_sub$iteration) %>%
  arrange(iteration)

# save cleaned and sampled files
saveRDS(param_data_global_clean,
        file = file.path("data", "processed", procdata_dir, clean_data_dir, 
                         "param_data_global_clean.RDS"))
saveRDS(param_data_global_clean_sub,
        file = file.path("data", "processed", procdata_dir, clean_data_dir, 
                         "param_data_global_clean_sub.RDS"))
saveRDS(param_data_FE_clean,
        file = file.path("data", "processed", procdata_dir, clean_data_dir, 
                         "param_data_FE_clean.RDS"))
saveRDS(param_data_FE_clean_sub,
        file = file.path("data", "processed", procdata_dir, clean_data_dir, 
                         "param_data_FE_clean_sub.RDS"))
saveRDS(obs_data,
        file = file.path("data", "processed", procdata_dir, clean_data_dir, 
                         "obs_data.RDS"))
