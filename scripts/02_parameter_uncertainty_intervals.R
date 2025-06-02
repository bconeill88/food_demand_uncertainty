# calculate and save parameter ML and uncertainty intervals --------------------

# TO DO:
# load required packages
# source functions
# redo path names to be consistent with project structure
# change file format to rds for saved files
  


# load needed files
load(paste(analysis_dir,input_path,"param_data_global_clean.RData",sep="/"))
load(paste(analysis_dir,input_path,"param_data_FE_clean.RData",sep="/"))
load(paste(analysis_dir,input_path,"param_data_global_clean_sub.RData",sep="/"))
load(paste(analysis_dir,input_path,"param_data_FE_clean_sub.RData",sep="/"))

# ML and independent uncertainty interval parameters
params_MLci_global <- make_param_MLintervals(param_data_global_clean,"global",confinterval)
params_MLci_FE <- split(param_data_FE_clean,f=param_data_FE_clean$region) %>%
  lapply(function(x) make_param_MLintervals(x,"FE",confinterval))

# global parameters for hi/lo price sensitivity cases
params_joint_price_global <- get_sens_params_price(param_data_global_clean,confinterval)
# regional FE parameters associated with the global hi/lo parameter sets
FEvals_lo <- param_data_FE_clean[
  param_data_FE_clean$iteration ==
    params_joint_price_global[params_joint_price_global$measure == "LPR",'iteration'],] %>%
  mutate(measure = "LPR") %>% relocate(measure)
FEvals_hi <- param_data_FE_clean[
  param_data_FE_clean$iteration ==
    params_joint_price_global[params_joint_price_global$measure == "HPR",'iteration'],] %>%
  mutate(measure = "HPR") %>% relocate(measure)
params_joint_price_FE <- rbind(FEvals_lo,FEvals_hi)

# global parameters for hi/lo income sensitivity cases
params_joint_income_global <- get_sens_params_income(param_data_global_clean,confinterval)
# regional FE parameters associated with the global hi/lo parameter sets
FEvals_lo <- param_data_FE_clean[
  param_data_FE_clean$iteration ==
    params_joint_income_global[params_joint_income_global$measure == "LIR",'iteration'],] %>%
  mutate(measure = "LIR") %>% relocate(measure)
FEvals_hi <- param_data_FE_clean[
  param_data_FE_clean$iteration ==
    params_joint_income_global[params_joint_income_global$measure == "HIR",'iteration'],] %>%
  mutate(measure = "HIR") %>% relocate(measure)
params_joint_income_FE <- rbind(FEvals_lo,FEvals_hi)

# global parameters for hi/lo scale sensitivity cases
params_joint_scale_global <- get_sens_params_scale(param_data_global_clean,confinterval)
# regional FE parameters associated with the global hi/lo parameter sets
FEvals_lo <- param_data_FE_clean[
  param_data_FE_clean$iteration ==
    params_joint_scale_global[params_joint_scale_global$measure == "LSR",'iteration'],] %>%
  mutate(measure = "LSR") %>% relocate(measure)
FEvals_hi <- param_data_FE_clean[
  param_data_FE_clean$iteration ==
    params_joint_scale_global[params_joint_scale_global$measure == "HSR",'iteration'],] %>%
  mutate(measure = "HSR") %>% relocate(measure)
params_joint_scale_FE <- rbind(FEvals_lo,FEvals_hi)

# combine all uncertainty interval results and save
params_ML_intervals_global <-
  bind_rows(list(params_MLci_global,params_joint_price_global,
                 params_joint_income_global,params_joint_scale_global))
params_ML_intervals_FE <- bind_rows(params_MLci_FE) %>%
  list(params_joint_price_FE,params_joint_income_FE,params_joint_scale_FE) %>%
  bind_rows()
save(params_ML_intervals_global,
     file=paste(analysis_dir,results_path,"params_ML_intervals_global.RData",sep="/"))
save(params_ML_intervals_FE,
     file=paste(analysis_dir,results_path,"params_ML_intervals_FE.RData",sep="/"))

print("saved parameter uncertainty interval results")

# add uncertainty interval samples to subsample files if they are not there already, to
# make sure demand gets calculated for these samples, and re-save subsample files
new_samples <- params_ML_intervals_global[
  !(params_ML_intervals_global$iteration %in% param_data_global_clean_sub$iteration),] %>%
  select(-measure,-quant)
param_data_global_clean_sub <- rbind(param_data_global_clean_sub,new_samples)
new_samples <- params_ML_intervals_FE[
  !(params_ML_intervals_FE$iteration %in% param_data_FE_clean_sub$iteration),] %>%
  select(-measure)
param_data_FE_clean_sub <- rbind(param_data_FE_clean_sub,new_samples)
save(param_data_global_clean_sub,
     file=paste(analysis_dir,input_path,"param_data_global_clean_sub.RData",sep="/"))
save(param_data_FE_clean_sub,
     file=paste(analysis_dir,input_path,"param_data_FE_clean_sub.RData",sep="/"))

# clean up
rm(param_data_global_clean,param_data_FE_clean,param_data_global_clean_sub,
   param_data_FE_clean_sub)

