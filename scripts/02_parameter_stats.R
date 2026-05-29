# Calculate and save parameter ML and independent uncertainty intervals. Note these are 
# parameter sets identified based on the distribution of parameters themselves, not on 
# the distribution of demand outcomes, which is addressed in 07_parameters_for_intervals.R.
#
# Parameter sets identified here are:
# ML - maximum likelihood
# CI - independent confidence intervals for each parameter separately, with the 
#      variable confinterval specifying which quantiles are identified
# 
# These parameter sets are identified based on the full parameter ensemble. They are
# then added to the sub-sample of parameters if they are not present already, so that
# further analyses will include them.

# load functions and common definitions
source("R/common_definitions.R")
source("R/init_packages.R")
source("R/parameter_uncertainty_functions.R")

# load packages
ensure_package(tidyverse)

# load parameter ensembles and sub-samples
param_data_global_clean <- 
  readRDS(file.path("data", "processed", procdata_dir, clean_data_dir, 
                    "param_data_global_clean.RDS")) 
param_data_FE_clean <-
  readRDS(file.path("data", "processed", procdata_dir, clean_data_dir, 
                    "param_data_FE_clean.RDS")) 
param_data_global_clean_sub <- 
  readRDS(file.path("data", "processed", procdata_dir, clean_data_dir, 
                    "param_data_global_clean_sub.RDS")) 
param_data_FE_clean_sub <- 
  readRDS(file.path("data", "processed", procdata_dir, clean_data_dir, 
                    "param_data_FE_clean_sub.RDS")) 

# ML and independent uncertainty interval parameters
params_MLci_global <- make_param_MLintervals(param_data_global_clean, "global", confinterval)
params_MLci_FE <- split(param_data_FE_clean, f=param_data_FE_clean$region) %>%
  lapply(function(x) make_param_MLintervals(x, "FE", confinterval))

# combine all uncertainty interval results
params_ML_intervals_global <- params_MLci_global
params_ML_intervals_FE <- bind_rows(params_MLci_FE) 

# add uncertainty interval samples to subsample files if they are not there already, to
# make sure demand gets calculated for these samples, and re-save subsample files
new_samples <- params_ML_intervals_global[
  !(params_ML_intervals_global$iteration %in% param_data_global_clean_sub$iteration),] %>%
  select(-measure)
param_data_global_clean_sub <- rbind(param_data_global_clean_sub, new_samples)
new_samples <- params_ML_intervals_FE[
  !(params_ML_intervals_FE$iteration %in% param_data_FE_clean_sub$iteration),] %>%
  select(-measure)
param_data_FE_clean_sub <- rbind(param_data_FE_clean_sub, new_samples)

# Save results
# ML and uncertainty interval parameters, including csv versions for Kanishka
saveRDS(params_ML_intervals_global,
        file = file.path("data", "processed", procdata_dir, param_intervals_dir,
                         "params_ML_intervals_global.RDS"))
saveRDS(params_ML_intervals_FE,
        file = file.path("data", "processed", procdata_dir, param_intervals_dir,
                         "params_ML_intervals_FE.RDS"))
write.csv(params_ML_intervals_global,
        file = file.path("data", "processed", procdata_dir, param_intervals_dir,
                         "params_ML_intervals_global.csv"))
write.csv(params_ML_intervals_FE,
        file = file.path("data", "processed", procdata_dir, param_intervals_dir,
                         "params_ML_intervals_FE.csv"))
# potentially modifed parameter sub-samples
saveRDS(param_data_global_clean_sub,
        file = file.path("data", "processed", procdata_dir, clean_data_dir, 
                         "param_data_global_clean_sub.RDS"))
saveRDS(param_data_FE_clean_sub,
        file = file.path("data", "processed", procdata_dir, clean_data_dir, 
                         "param_data_FE_clean_sub.RDS"))
