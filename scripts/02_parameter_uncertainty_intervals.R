# Calculate and save parameter ML and uncertainty intervals. Note these are parameter
# sets identified based on the distribution of parameters themselves, not on the 
# distribution of demand outcomes, which is addressed in 07_parameters_for_intervals.R.
#
# Parameter sets identified here are:
# ML - maximum likelihood
# CI - independent confidence intervals for each parameter separately, with the 
#      variable confinterval specifying which quantiles are identified
# HPR/LPR - high and low price response parameter sets, which are joint uncertainty
#           intervals across parameters specified in advance to be important to 
#           the response of demand to prices
# HIR/LIR - high and low income response parameter sets; similar definition to HPR/LPR
# HSR/LSR - high and low scale response paramet sets; similar definition to HPR/LPR
# 
# These parameter sets are identified based on the full parameter ensemble. They are
# then added to the sub-sample of parameters if they are not present already, so that
# futher analyses will include them.

# load functions and common definitions
source("R/common_definitions.R")
source("R/init_packages.R")
source("R/parameter_uncertainty_functions.R")

# load packages
ensure_package(tidyverse)

# load parameter ensembles and sub-samples
param_data_global_clean <- 
  readRDS(file.path("data", "processed", procdata_dir, "inputs", 
                    "param_data_global_clean.RDS")) 
param_data_FE_clean <-
  readRDS(file.path("data", "processed", procdata_dir, "inputs", 
                    "param_data_FE_clean.RDS")) 
param_data_global_clean_sub <- 
  readRDS(file.path("data", "processed", procdata_dir, "inputs", 
                    "param_data_global_clean_sub.RDS")) 
param_data_FE_clean_sub <- 
  readRDS(file.path("data", "processed", procdata_dir, "inputs", 
                    "param_data_FE_clean_sub.RDS")) 

# ML and independent uncertainty interval parameters
params_MLci_global <- make_param_MLintervals(param_data_global_clean, "global", confinterval)
params_MLci_FE <- split(param_data_FE_clean, f=param_data_FE_clean$region) %>%
  lapply(function(x) make_param_MLintervals(x, "FE", confinterval))

# global parameters for hi/lo price sensitivity cases
params_joint_price_global <- get_sens_params_price(param_data_global_clean, confinterval)
# regional FE parameters associated with the global hi/lo parameter sets
FEvals_lo <- param_data_FE_clean[
  param_data_FE_clean$iteration ==
    params_joint_price_global[params_joint_price_global$measure == "LPR", 'iteration'],] %>%
  mutate(measure = "LPR") %>% relocate(measure)
FEvals_hi <- param_data_FE_clean[
  param_data_FE_clean$iteration ==
    params_joint_price_global[params_joint_price_global$measure == "HPR", 'iteration'],] %>%
  mutate(measure = "HPR") %>% relocate(measure)
params_joint_price_FE <- rbind(FEvals_lo, FEvals_hi)

# global parameters for hi/lo income sensitivity cases
params_joint_income_global <- get_sens_params_income(param_data_global_clean, confinterval)
# regional FE parameters associated with the global hi/lo parameter sets
FEvals_lo <- param_data_FE_clean[
  param_data_FE_clean$iteration ==
    params_joint_income_global[params_joint_income_global$measure == "LIR", 'iteration'],] %>%
  mutate(measure = "LIR") %>% relocate(measure)
FEvals_hi <- param_data_FE_clean[
  param_data_FE_clean$iteration ==
    params_joint_income_global[params_joint_income_global$measure == "HIR", 'iteration'],] %>%
  mutate(measure = "HIR") %>% relocate(measure)
params_joint_income_FE <- rbind(FEvals_lo, FEvals_hi)

# global parameters for hi/lo scale sensitivity cases
params_joint_scale_global <- get_sens_params_scale(param_data_global_clean, confinterval)
# regional FE parameters associated with the global hi/lo parameter sets
FEvals_lo <- param_data_FE_clean[
  param_data_FE_clean$iteration ==
    params_joint_scale_global[params_joint_scale_global$measure == "LSR", 'iteration'],] %>%
  mutate(measure = "LSR") %>% relocate(measure)
FEvals_hi <- param_data_FE_clean[
  param_data_FE_clean$iteration ==
    params_joint_scale_global[params_joint_scale_global$measure == "HSR", 'iteration'],] %>%
  mutate(measure = "HSR") %>% relocate(measure)
params_joint_scale_FE <- rbind(FEvals_lo, FEvals_hi)

# combine all uncertainty interval results
params_ML_intervals_global <- 
  bind_rows(list(params_MLci_global, params_joint_price_global, 
                 params_joint_income_global, params_joint_scale_global))
params_ML_intervals_FE <- 
  bind_rows(params_MLci_FE) %>%
  list(params_joint_price_FE,params_joint_income_FE,params_joint_scale_FE) %>%
  bind_rows()

# add uncertainty interval samples to subsample files if they are not there already, to
# make sure demand gets calculated for these samples, and re-save subsample files
new_samples <- params_ML_intervals_global[
  !(params_ML_intervals_global$iteration %in% param_data_global_clean_sub$iteration),] %>%
  select(-measure, -quant)
param_data_global_clean_sub <- rbind(param_data_global_clean_sub, new_samples)
new_samples <- params_ML_intervals_FE[
  !(params_ML_intervals_FE$iteration %in% param_data_FE_clean_sub$iteration),] %>%
  select(-measure)
param_data_FE_clean_sub <- rbind(param_data_FE_clean_sub, new_samples)

# Save results
# ML and uncertainty interval parameters, including csv versions for Kanishka
saveRDS(params_ML_intervals_global,
        file = file.path("data", "processed", procdata_dir,
                         "params_ML_intervals_global.RDS"))
saveRDS(params_ML_intervals_FE,
        file = file.path("data", "processed", procdata_dir,
                         "params_ML_intervals_FE.RDS"))
write.csv(params_ML_intervals_global,
        file = file.path("data", "processed", procdata_dir,
                         "params_ML_intervals_global.csv"))
write.csv(params_ML_intervals_FE,
        file = file.path("data", "processed", procdata_dir,
                         "params_ML_intervals_FE.csv"))
# potentially modifed parameter sub-samples
saveRDS(param_data_global_clean_sub,
        file = file.path("data", "processed", procdata_dir, "inputs", 
                         "param_data_global_clean_sub.RDS"))
saveRDS(param_data_FE_clean_sub,
        file = file.path("data", "processed", procdata_dir, "inputs", 
                         "param_data_FE_clean_sub.RDS"))
