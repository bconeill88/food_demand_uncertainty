# Calculate and save demand and elasticities for cases that only require the MCMC
# ensemble of parameters to run (i.e., don't require GCAM results to drive the
# simulations). These consist of stylized prices and income ranges.

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
# get command line arguments if they exist
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  reg_list <- c(as.numeric(args[1]))
} else {
  reg_list <- c(1:32)
}

# load parameter files

# ensembles of parameters from MCMC; files read in are produced by running 01_clean_data.R
load(file.path("data", "processed", procdata_dir, clean_data_dir, "param_data_global_clean_sub.RData"))
load(file.path("data", "processed", procdata_dir, clean_data_dir, "param_data_FE_clean_sub.RData"))

# Calculate demand and elasticities for the subsample of parameter iterations, for all
# regions, given a dataframe of income and prices, and save regional results, for each
# scenario in scen_list; note can't use case_when() here because it evaluates code
# for all cases, so need to use extensive if statements

lapply(scen_list_demand_ens_only, function(x) {
  
  if(x == "Pdef") {
    # default prices
    food.dmnd.plus.FE.regions(param_data_global_clean_sub,param_data_FE_clean_sub,
                              incprice_def,"Pdef",reg_list)
    
  } else if(x == "2xPsPn") {
    # 2xPsPn
    food.dmnd.plus.FE.regions(param_data_global_clean_sub,param_data_FE_clean_sub,
                              incprice_2xPsPn,"2xPsPn",reg_list)
    
  } else if(x == "2xPs") {
    # 2xPs
    food.dmnd.plus.FE.regions(param_data_global_clean_sub,param_data_FE_clean_sub,
                              incprice_2xPs,"2xPs",reg_list)
    
  } else if(x == "2xPn") {
    # 2xPn
    food.dmnd.plus.FE.regions(param_data_global_clean_sub,param_data_FE_clean_sub,
                              incprice_2xPn,"2xPn",reg_list)
    
  } else if(x == "PsPnHi") {
    # PsPnHi
    food.dmnd.plus.FE.regions(param_data_global_clean_sub,param_data_FE_clean_sub,
                              incprice_PsPnHi,"PsPnHi",reg_list)
  } 
  
})

