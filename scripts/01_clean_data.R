# Read in raw parameter ensemble data and observational data from /data/raw in
# the subdirectory given by rawdata_dir (in common_definitions.R). Clean and
# save data in /data/processed in the subdirectory given by procdata_dir

# load functions and common definitions
source("R/common_definitions.R")
source("R/init_packages.R")
source("R/install_ambrosia_function.R")

# install or load packages
ensure_package(tidyverse)

# read in data files
# ensemble of global parameters
param_data_global_raw <- 
  read.table(file.path("data", "raw", rawdata_dir, param_data_global_file), header = TRUE)
# ensemble of regional fixed effect parameters
param_data_FE_raw <-
  read.table(file.path("data", "raw", rawdata_dir, param_data_FE_file), header = TRUE) 
# observational data for 32 GCAM regions
obs_data <- 
  read.csv(file.path("data", "raw", rawdata_dir, obs_data_file), header = TRUE)

# clean global and FE parameter data, and take subset of data for analysis; details 
# depend on the specific files that were generated, so there is separate code for each set
if(grepl("ambrosia_9_params_24Jan25", param_data_global_file)) {
  
  param_data_global_clean <- param_data_global_raw %>%
    
    # remove unnecessary column
    select(-chain) %>%
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
  
} else {
  
  stop("Raw parameter data file names not correctly specified.")
}
  
# save cleaned and sampled files
saveRDS(param_data_global_clean,
        file = file.path("data", "processed", procdata_dir, "inputs", 
                         "param_data_global_clean.RDS"))
saveRDS(param_data_global_clean_sub,
        file = file.path("data", "processed", procdata_dir, "inputs", 
                         "param_data_global_clean_sub.RDS"))
saveRDS(param_data_FE_clean,
        file = file.path("data", "processed", procdata_dir, "inputs", 
                         "param_data_FE_clean.RDS"))
saveRDS(param_data_FE_clean_sub,
        file = file.path("data", "processed", procdata_dir, "inputs", 
                         "param_data_FE_clean_sub.RDS"))
saveRDS(obs_data,
        file = file.path("data", "processed", procdata_dir, "inputs", 
                         "obs_data.RDS"))

# clean up
rm(param_data_global_raw, param_data_FE_raw)
rm(param_data_global_clean, param_data_global_clean_sub, param_data_FE_clean,
   param_data_FE_clean_sub, obs_data)

print("Saved cleaned data")



