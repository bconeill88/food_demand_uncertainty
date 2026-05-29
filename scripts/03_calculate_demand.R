# Use ambrosia to calculate and save demand and elasticities using the MCMC ensemble
# of parameters and GCAM income and prices when GCAM employs the ML parameter values.
# Runs are:
# - Stylized income/price simulations
# - Single ambrosia scenario driven by GCAM Reference/ML scenario
# - Ensembles driven by GCAM Reference/ML scenarios with standard or high price conditions
# - Ensembles used for decomposing results into the effects of price, income, and scale 
#   parameters.
#
# The scenarios/ensembles to be run are set in common_definitions.R in the 
# scen_list_demand_GCAM_ML variable.

# load functions
source("R/common_definitions.R")
source("R/demand_functions.R")
source("R/install_ambrosia_function.R")
source("R/init_packages.R")

# install or load packages as needed
ensure_package(tidyverse)
ensure_package(progressr)
install_ambrosia_once(force_install = FALSE)

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
param_data_global_clean_sub <- 
  readRDS(file.path("data", "processed", procdata_dir, clean_data_dir, 
                    "param_data_global_clean_sub.RDS"))
param_data_FE_clean_sub <- 
  readRDS(file.path("data", "processed", procdata_dir, clean_data_dir, 
                    "param_data_FE_clean_sub.RDS")) %>%
  mutate(region = if_else(region == "Europe_Eastern", "Ukraine", region))

# ML parameter values; files read in are produced by running 02_parameter_uncertainty_intervals.R
params_ML_intervals_global <- 
  readRDS(file.path("data", "processed", procdata_dir, param_intervals_dir,
                    "params_ML_intervals_global.RDS"))
params_ML_intervals_FE <- 
  readRDS(file.path("data", "processed", procdata_dir, param_intervals_dir,
                    "params_ML_intervals_FE.RDS"))

# load GCAM results, as input assumptions for ambrosia (prices, income); file read
# in is produced by running 101_process_GCAM_results.R
gcamoutput_Ref_ML <-
  readRDS(file.path("data", "processed", procdata_dir, gcam_results_dir,
                    "gcamoutput_Ref_ML.RDS"))
gcamoutput_Ref_ML_HP <- 
  readRDS(file.path("data", "processed", procdata_dir, gcam_results_dir, 
                    "gcamoutput_HP_ML.RDS"))

# Calculate demand and elasticities for the subsample parameter iterations, for all
# regions given a dataframe of income and prices, saves regional results, for each
# scenario in scen_list_demand_GCAM_ML
lapply(scen_list_demand_GCAM_ML, function(x) {
  
  # stylized ensemble with constant mean price and specified income range
  if(x == "RefML") {
    # RefML
    food.dmnd.ens_bc(
      globalparams = param_data_global_clean_sub,
      regparams = param_data_FE_clean_sub,
      inputdata = incprice_RefML,
      regionIDs = reg_list,
      output_dir = procdata_dir, gcam_dir = gcam_results_dir,
      scen = "Ref_ML",
      case = "ens_bc",       # file ext: bias corrected ensemble
      baseyr = base_year,
      Qs_min = Qs_floor,
      Qn_min = Qn_floor,
      save_result = TRUE
    )
    
  # single scenario driven by GCAM Reference scenario income and prices with ML parameters
  } else if(x == "Ref_ML_gcam_MLparams_bc") {
    
    food.dmnd.ens_bc(
      globalparams = params_ML_intervals_global %>% filter(measure == "ML"),
      regparams = params_ML_intervals_FE %>% filter(measure == "ML"),
      inputdata = gcamoutput_Ref_ML,
      regionIDs = reg_list,
      output_dir = procdata_dir, gcam_dir = gcam_results_dir,
      scen = "Ref_ML_gcam",
      case = "MLparams_bc",
      baseyr = base_year,
      Qs_min = Qs_floor,
      Qn_min = Qn_floor,
      save_result = TRUE)
    
  # GCAM scenario ensembles
    
    # ensemble driven by GCAM Reference scenario income and prices with ML parameters
  } else if(x == "Ref_ML_gcam_ens_bc") {
    
    food.dmnd.ens_bc(
      globalparams = param_data_global_clean_sub,
      regparams = param_data_FE_clean_sub,
      inputdata = gcamoutput_Ref_ML,
      regionIDs = reg_list,
      output_dir = procdata_dir, gcam_dir = gcam_results_dir,
      scen = "Ref_ML_gcam",
      case = "ens_bc",
      baseyr = base_year,
      Qs_min = Qs_floor,
      Qn_min = Qn_floor,
      save_result = TRUE
    )
       
    # ensemble driven by GCAM Reference scenario income and high prices with ML parameters
  } else if(x == "Ref_ML_HP_gcam_ens_bc") {
    
    food.dmnd.ens_bc(
      globalparams = param_data_global_clean_sub,
      regparams = param_data_FE_clean_sub,
      inputdata = gcamoutput_Ref_ML_HP,
      regionIDs = reg_list,
      output_dir = procdata_dir, gcam_dir = gcam_results_dir,
      scen = "Ref_ML_HP_gcam",
      case = "ens_bc",
      baseyr = base_year,
      Qs_min = Qs_floor,
      Qn_min = Qn_floor,
      save_result = TRUE
    )

    # ensemble driven by GCAM Reference scenario income and prices with ML parameters,
    # varying only price-related parameters
  } else if(x == "Ref_ML_gcam_MLprice_ens_bc") {
    
    # get ML parameter values
    param_data_global_ML <- params_ML_intervals_global %>% filter(measure == "ML")
    param_data_FE_ML <- params_ML_intervals_FE %>% filter(measure == "ML")
    
    # FE ML values as a lookup table
    fe_ml_lookup <- param_data_FE_ML %>%
      select(GCAM_region_ID, staples_FE) %>%
      mutate(GCAM_region_ID = as.integer(GCAM_region_ID))
    
    # set FE parameters to ML values
    param_data_FE_clean_sub_ML <- param_data_FE_clean_sub %>%
      mutate(GCAM_region_ID = as.integer(GCAM_region_ID)) %>%
      select(-staples_FE) %>%
      left_join(fe_ml_lookup, by = "GCAM_region_ID")
    
    # hard check: should not have missing FE after join
    stopifnot(!anyNA(param_data_FE_clean_sub_ML$staples_FE))
    
    # set global non-price parameters to ML values
    # extract ML scalars safely (assumes exactly one ML row)
    stopifnot(nrow(param_data_global_ML) == 1)
    ml <- param_data_global_ML %>% slice(1)
    param_data_global_clean_sub_MLprice <- param_data_global_clean_sub %>%
      mutate(
        lambda = ml$lambda[[1]],
        ks     = ml$ks[[1]],
        eps1n  = ml$eps1n[[1]],
        An     = ml$An[[1]],
        As     = ml$As[[1]],
        Pm     = ml$Pm[[1]]
      )

    food.dmnd.ens_bc(
      globalparams = param_data_global_clean_sub_MLprice,
      regparams = param_data_FE_clean_sub_ML,
      inputdata = gcamoutput_Ref_ML,
      regionIDs = reg_list,
      output_dir = procdata_dir, gcam_dir = gcam_results_dir,
      scen = "Ref_ML_gcam",
      case = "MLprice_ens_bc",
      baseyr = base_year,
      Qs_min = Qs_floor,
      Qn_min = Qn_floor,
      save_result = TRUE
    )
    
    # ensemble driven by GCAM Reference scenario income and prices with ML parameters,
    # varying only income-related parameters
  } else if(x == "Ref_ML_gcam_MLincome_ens_bc") {
    
    # get ML parameter values
    param_data_global_ML <- params_ML_intervals_global %>% filter(measure == "ML")
    param_data_FE_ML <- params_ML_intervals_FE %>% filter(measure == "ML")
    
    # FE ML values as a lookup table
    fe_ml_lookup <- param_data_FE_ML %>%
      select(GCAM_region_ID, staples_FE) %>%
      mutate(GCAM_region_ID = as.integer(GCAM_region_ID))
    
    # set FE parameters to ML values
    param_data_FE_clean_sub_ML <- param_data_FE_clean_sub %>%
      mutate(GCAM_region_ID = as.integer(GCAM_region_ID)) %>%
      select(-staples_FE) %>%
      left_join(fe_ml_lookup, by = "GCAM_region_ID")
    
    # hard check: should not have missing FE after join
    stopifnot(!anyNA(param_data_FE_clean_sub_ML$staples_FE))
    
    # set global non-income parameters to ML values
    # extract ML scalars safely (assumes exactly one ML row)
    stopifnot(nrow(param_data_global_ML) == 1)
    ml <- param_data_global_ML %>% slice(1)
    param_data_global_clean_sub_MLincome <- param_data_global_clean_sub %>%
      mutate(
        xi.ss    = ml$xi.ss[[1]],
        xi.nn    = ml$xi.nn[[1]],
        xi.cross = ml$xi.cross[[1]],
        An       = ml$An[[1]],
        As       = ml$As[[1]],
        Pm       = ml$Pm[[1]]
      )
    
    food.dmnd.ens_bc(
      globalparams = param_data_global_clean_sub_MLincome,
      regparams = param_data_FE_clean_sub_ML,
      inputdata = gcamoutput_Ref_ML,
      regionIDs = reg_list,
      output_dir = procdata_dir, gcam_dir = gcam_results_dir,
      scen = "Ref_ML_gcam",
      case = "MLincome_ens_bc",       # file ext: bias corrected ensemble
      baseyr = base_year,
      Qs_min = Qs_floor,
      Qn_min = Qn_floor,
      save_result = TRUE
    )
    
    # ensemble driven by GCAM Reference scenario income and prices with ML parameters,
    # varying only scale-related parameters
  } else if(x == "Ref_ML_gcam_MLscale_ens_bc") {
    
    # get ML parameter values
    param_data_global_ML <- params_ML_intervals_global %>% filter(measure == "ML")
    param_data_FE_ML <- params_ML_intervals_FE %>% filter(measure == "ML")
    
    # FE ML values as a lookup table
    fe_ml_lookup <- param_data_FE_ML %>%
      select(GCAM_region_ID, staples_FE) %>%
      mutate(GCAM_region_ID = as.integer(GCAM_region_ID))
    
    # set FE parameters to ML values
    # param_data_FE_clean_sub_ML <- param_data_FE_clean_sub %>%
    #   mutate(GCAM_region_ID = as.integer(GCAM_region_ID)) %>%
    #   select(-staples_FE) %>%
    #   left_join(fe_ml_lookup, by = "GCAM_region_ID")
    
    # hard check: should not have missing FE after join
    # stopifnot(!anyNA(param_data_FE_clean_sub_ML$staples_FE))
    
    # set global non-scale parameters to ML values
    # extract ML scalars safely (assumes exactly one ML row)
    stopifnot(nrow(param_data_global_ML) == 1)
    ml <- param_data_global_ML %>% slice(1)
    param_data_global_clean_sub_MLscale <- param_data_global_clean_sub %>%
      mutate(
        lambda = ml$lambda[[1]],
        ks     = ml$ks[[1]],
        eps1n  = ml$eps1n[[1]],
        xi.ss    = ml$xi.ss[[1]],
        xi.nn    = ml$xi.nn[[1]],
        xi.cross = ml$xi.cross[[1]],
        Pm       = ml$Pm[[1]]
      )
    
    food.dmnd.ens_bc(
      globalparams = param_data_global_clean_sub_MLscale,
      regparams = param_data_FE_clean_sub,
      inputdata = gcamoutput_Ref_ML,
      regionIDs = reg_list,
      output_dir = procdata_dir, gcam_dir = gcam_results_dir,
      scen = "Ref_ML_gcam",
      case = "MLscale_ens_bc",       # file ext: bias corrected ensemble
      baseyr = base_year,
      Qs_min = Qs_floor,
      Qn_min = Qn_floor,
      save_result = TRUE
    )
    
    # ensemble driven by GCAM Reference scenario income and high prices with ML parameters,
    # varying only price-related parameters
  } else if(x == "Ref_ML_HP_gcam_MLprice_ens_bc") {
    
    # get ML parameter values
    param_data_global_ML <- params_ML_intervals_global %>% filter(measure == "ML")
    param_data_FE_ML <- params_ML_intervals_FE %>% filter(measure == "ML")
    
    # FE ML values as a lookup table
    fe_ml_lookup <- param_data_FE_ML %>%
      select(GCAM_region_ID, staples_FE) %>%
      mutate(GCAM_region_ID = as.integer(GCAM_region_ID))
    
    # set FE parameters to ML values
    param_data_FE_clean_sub_ML <- param_data_FE_clean_sub %>%
      mutate(GCAM_region_ID = as.integer(GCAM_region_ID)) %>%
      select(-staples_FE) %>%
      left_join(fe_ml_lookup, by = "GCAM_region_ID")
    
    # hard check: should not have missing FE after join
    stopifnot(!anyNA(param_data_FE_clean_sub_ML$staples_FE))
    
    # set global non-price parameters to ML values
    # extract ML scalars safely (assumes exactly one ML row)
    stopifnot(nrow(param_data_global_ML) == 1)
    ml <- param_data_global_ML %>% slice(1)
    param_data_global_clean_sub_MLprice <- param_data_global_clean_sub %>%
      mutate(
        lambda = ml$lambda[[1]],
        ks     = ml$ks[[1]],
        eps1n  = ml$eps1n[[1]],
        An     = ml$An[[1]],
        As     = ml$As[[1]],
        Pm     = ml$Pm[[1]]
      )

    food.dmnd.ens_bc(
      globalparams = param_data_global_clean_sub_MLprice,
      regparams = param_data_FE_clean_sub_ML,
      inputdata = gcamoutput_Ref_ML_HP,
      regionIDs = reg_list,
      output_dir = procdata_dir, gcam_dir = gcam_results_dir,
      scen = "Ref_ML_HP_gcam",
      case = "MLprice_ens_bc",       # file ext: bias corrected ensemble
      baseyr = base_year,
      Qs_min = Qs_floor,
      Qn_min = Qn_floor,
      save_result = TRUE
    )

    # ensemble driven by GCAM Reference scenario income and high prices with ML parameters,
    # varying only income-related parameters
  } else if(x == "Ref_ML_HP_gcam_MLincome_ens_bc") {
    
    # get ML parameter values
    param_data_global_ML <- params_ML_intervals_global %>% filter(measure == "ML")
    param_data_FE_ML <- params_ML_intervals_FE %>% filter(measure == "ML")
    
    # FE ML values as a lookup table
    fe_ml_lookup <- param_data_FE_ML %>%
      select(GCAM_region_ID, staples_FE) %>%
      mutate(GCAM_region_ID = as.integer(GCAM_region_ID))
    
    # set FE parameters to ML values
    param_data_FE_clean_sub_ML <- param_data_FE_clean_sub %>%
      mutate(GCAM_region_ID = as.integer(GCAM_region_ID)) %>%
      select(-staples_FE) %>%
      left_join(fe_ml_lookup, by = "GCAM_region_ID")
    
    # hard check: should not have missing FE after join
    stopifnot(!anyNA(param_data_FE_clean_sub_ML$staples_FE))
    
    # set global non-income parameters to ML values
    # extract ML scalars safely (assumes exactly one ML row)
    stopifnot(nrow(param_data_global_ML) == 1)
    ml <- param_data_global_ML %>% slice(1)
    param_data_global_clean_sub_MLincome <- param_data_global_clean_sub %>%
      mutate(
        xi.ss    = ml$xi.ss[[1]],
        xi.nn    = ml$xi.nn[[1]],
        xi.cross = ml$xi.cross[[1]],
        An       = ml$An[[1]],
        As       = ml$As[[1]],
        Pm       = ml$Pm[[1]]
      )
    
    food.dmnd.ens_bc(
      globalparams = param_data_global_clean_sub_MLincome,
      regparams = param_data_FE_clean_sub_ML,
      inputdata = gcamoutput_Ref_ML_HP,
      regionIDs = reg_list,
      output_dir = procdata_dir, gcam_dir = gcam_results_dir,
      scen = "Ref_ML_HP_gcam",
      case = "MLincome_ens_bc",       # file ext: bias corrected ensemble
      baseyr = base_year,
      Qs_min = Qs_floor,
      Qn_min = Qn_floor,
      save_result = TRUE
    )
    
    # ensemble driven by GCAM Reference scenario income and high prices with ML parameters,
    # varying only scale-related parameters
  } else if(x == "Ref_ML_HP_gcam_MLscale_ens_bc") {
    
    # get ML parameter values
    param_data_global_ML <- params_ML_intervals_global %>% filter(measure == "ML")
    param_data_FE_ML <- params_ML_intervals_FE %>% filter(measure == "ML")
    
    # FE ML values as a lookup table
    fe_ml_lookup <- param_data_FE_ML %>%
      select(GCAM_region_ID, staples_FE) %>%
      mutate(GCAM_region_ID = as.integer(GCAM_region_ID))
    
    # set FE parameters to ML values
    # param_data_FE_clean_sub_ML <- param_data_FE_clean_sub %>%
    #   mutate(GCAM_region_ID = as.integer(GCAM_region_ID)) %>%
    #   select(-staples_FE) %>%
    #   left_join(fe_ml_lookup, by = "GCAM_region_ID")
    
    # hard check: should not have missing FE after join
    # stopifnot(!anyNA(param_data_FE_clean_sub_ML$staples_FE))
    
    # set global non-scale parameters to ML values
    # extract ML scalars safely (assumes exactly one ML row)
    stopifnot(nrow(param_data_global_ML) == 1)
    ml <- param_data_global_ML %>% slice(1)
    param_data_global_clean_sub_MLscale <- param_data_global_clean_sub %>%
      mutate(
        lambda = ml$lambda[[1]],
        ks     = ml$ks[[1]],
        eps1n  = ml$eps1n[[1]],
        xi.ss    = ml$xi.ss[[1]],
        xi.nn    = ml$xi.nn[[1]],
        xi.cross = ml$xi.cross[[1]],
        Pm       = ml$Pm[[1]]
      )
    
    food.dmnd.ens_bc(
      globalparams = param_data_global_clean_sub_MLscale,
      regparams = param_data_FE_clean_sub,
      inputdata = gcamoutput_Ref_ML_HP,
      regionIDs = reg_list,
      output_dir = procdata_dir, gcam_dir = gcam_results_dir,
      scen = "Ref_ML_HP_gcam",
      case = "MLscale_ens_bc",       # file ext: bias corrected ensemble
      baseyr = base_year,
      Qs_min = Qs_floor,
      Qn_min = Qn_floor,
      save_result = TRUE
    )
    
  }

} # end function(x)

) %>% invisible # end lapply()

