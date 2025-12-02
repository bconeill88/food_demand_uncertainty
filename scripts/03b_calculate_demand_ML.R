# Calculate and save demand and elasticities for cases that require the MCMC
# ensemble of parameters and ML parameter values, but nothing else (i.e., don't 
# require any GCAM results). These consist of stylized prices and income ranges
# with parameters partially based on ML values in order to decompose uncertainty
# in the Pdef scenario.

# To Do:
# Code for all scenarios needs to be updated to use new food demand functions.
# common_definitions.R needs to be updated to add desired scenarios to run for 
# this script.
# If/else statements below may be able to be simplified by mapping single generic
# food demand function call to the list of scenarios to be run.

# load functions
source("R/demand_functions.R")
source("R/install_ambrosia_function.R")
source("R/init_packages.R")

# install or load packages as needed
ensure_package(tidyverse)
ensure_package(progressr)
install_ambrosia(force_install = FALSE)

# define regions to run over
# get command line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  reg_list <- c(as.numeric(args[1]))
} else {
  reg_list <- c(1:32)
}

# load parameter files

# ensembles of parameters from MCMC; files read in are produced by running 01_clean_data.R
load(file.path("data", "processed", procdata_dir, "inputs", "param_data_global_clean_sub.RData"))
load(file.path("data", "processed", procdata_dir, "inputs", "param_data_FE_clean_sub.RData"))

# ML parameter values; files read in are produced by running 02_parameter_uncertainty_intervals.R
load(file.path("data", "processed", procdata_dir, "params_ML_intervals_global.RData"))
load(file.path("data", "processed", procdata_dir, "params_ML_intervals_FE.RData"))

# calculate demand and elasticities for the subsample parameter iterations, for all
# regions given a dataframe of income and prices, saves regional results, for each
# scenario in scen_list; note can't use case_when() here because it evaluates code
# for all cases, so need to use extensive if statements
lapply(scen_list_demand_ML, function(x) {
  
  # calculate demand and elasticities for decomposing uncertainty in outcomes for
  # default prices
  
  if(x %in% c("MLprice","MLincome","MLscale","MLpm")) {
    # in any of these cases FE parameters are set to ML values so do that first; should be
    # able to use apply function but couldn't get it to work
    param_data_FE_ML <- params_ML_intervals_FE %>% filter(measure == "ML")
    param_data_FE_clean_sub_ML <- param_data_FE_clean_sub
    for(i in 1:nrow(param_data_FE_clean_sub_ML)){
      target_reg <- param_data_FE_clean_sub_ML[i,]['GCAM_region_ID'][[1]]
      param_data_FE_clean_sub_ML[i,'staples_FE'] <-
        param_data_FE_ML[param_data_FE_ML$GCAM_region_ID == target_reg,'staples_FE']
    }}
  
  if(x == "MLprice") {
    # calculate food demand with all but price elasticity parameters fixed at ML values
    param_data_global_ML <- params_ML_intervals_global %>% filter(measure == "ML")
    param_data_global_uncty <- param_data_global_clean_sub %>%
      mutate(lambda = param_data_global_ML['lambda'],
             ks = param_data_global_ML['ks'],
             eps1n = param_data_global_ML['eps1n'],
             An = param_data_global_ML['An'],
             As = param_data_global_ML['As'],
             Pm = param_data_global_ML['Pm'])
    food.dmnd.plus.FE.regions(param_data_global_uncty,param_data_FE_clean_sub_ML,
                              incprice_def,"MLprice",reg_list)
    
  } else if(x == "MLincome") {
    # calculate food demand with all but income elasticity parameters fixed at ML values
    param_data_global_ML <- params_ML_intervals_global %>% filter(measure == "ML")
    param_data_global_uncty <- param_data_global_clean_sub %>%
      mutate(xi.ss = param_data_global_ML['xi.ss'],
             xi.nn = param_data_global_ML['xi.nn'],
             xi.cross = param_data_global_ML['xi.cross'],
             An = param_data_global_ML['An'],
             As = param_data_global_ML['As'],
             Pm = param_data_global_ML['Pm'])
    food.dmnd.plus.FE.regions(param_data_global_uncty,param_data_FE_clean_sub_ML,
                              incprice_def,"MLincome",reg_list)
    
  } else if(x == "MLscale") {
    # calculate food demand with all but scale parameters fixed at ML values
    param_data_global_ML <- params_ML_intervals_global %>% filter(measure == "ML")
    param_data_global_uncty <- param_data_global_clean_sub %>%
      mutate(lambda = param_data_global_ML['lambda'],
             ks = param_data_global_ML['ks'],
             eps1n = param_data_global_ML['eps1n'],
             xi.ss = param_data_global_ML['xi.ss'],
             xi.nn = param_data_global_ML['xi.nn'],
             xi.cross = param_data_global_ML['xi.cross'],
             Pm = param_data_global_ML['Pm'])
    food.dmnd.plus.FE.regions(param_data_global_uncty,param_data_FE_clean_sub_ML,
                              incprice_def,"MLscale",reg_list)
    
  } else if(x == "MLpm") {
    # calculate food demand with all but scale parameters fixed at ML values
    param_data_global_ML <- params_ML_intervals_global %>% filter(measure == "ML")
    param_data_global_uncty <- param_data_global_clean_sub %>%
      mutate(lambda = param_data_global_ML['lambda'],
             ks = param_data_global_ML['ks'],
             eps1n = param_data_global_ML['eps1n'],
             xi.ss = param_data_global_ML['xi.ss'],
             xi.nn = param_data_global_ML['xi.nn'],
             xi.cross = param_data_global_ML['xi.cross'],
             An = param_data_global_ML['An'],
             As = param_data_global_ML['As'])
    food.dmnd.plus.FE.regions(param_data_global_uncty,param_data_FE_clean_sub_ML,
                              incprice_def,"MLpm",reg_list)
  }
} # end function(x)

) %>% invisible # end lapply()

