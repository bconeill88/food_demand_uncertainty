# calculate and save demand and elasticities for subsample and intervals 

# To Do:
# Code for all scenarios needs to be updated to use new food.dmnd.wrapper function
# except Ref_ML_gcam, Ref_ML_gcam_ens, and Ref_ML_gcam_ens_bc, which are up to
# date
# Now need to create these same versions but for GCAM-based scenarios with higher
# prices. I believe Kanishka created such a scenario but it did not lead to very 
# big price changes.

# load functions
source("R/demand_functions.R")
source("R/install_ambrosia_function.R")
source("R/init_packages.R")

# install or load packages as needed
ensure_package(tidyverse)

# install ambrosia if necessary based on machine (WF10681 or PIC)
force_install <- FALSE   # set to TRUE for testing new ambrosia version
if (Sys.info()["nodename"] == "WF10681") {
  install_ambrosia_once("H:/My Drive/R projects/food_demand/ambrosia/", force_install)
} else {
  setwd("/qfs/people/onei736/food_demand/uncertainty")
  install_ambrosia_once("/qfs/people/onei736/food_demand/ambrosia/", force_install)
}

# subdirectories of data/processed to use
procdata_dir <- "update9_cnstrlam_agg32FE_24jan25"

# define list of scenarios to run
scen_list_demand <- list("Ref_ML_gcam_ens_bc")

# define regions to run over
# get command line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  reg_list <- c(as.numeric(args[1]))
} else {
  reg_list <- c(1:32)
}

# load input assumptions for ambrosia (prices, income, etc.)
gcamoutput_Ref_ML <- readRDS(paste("data/processed",procdata_dir,
                                   "results_gcam/gcamoutput_Ref_ML.RDS",sep="/"))

# load parameter files
load(paste("data/processed",procdata_dir,"inputs/param_data_global_clean_sub.RData",sep="/"))
load(paste("data/processed",procdata_dir,"inputs/param_data_FE_clean_sub.RData",sep="/"))
load(paste("data/processed",procdata_dir,"params_ML_intervals_global.RData",sep="/"))
load(paste("data/processed",procdata_dir,"params_ML_intervals_FE.RData",sep="/"))

# calculate demand and elasticities for the subsample parameter iterations, for all
# regions given a dataframe of income and prices, saves regional results, for each
# scenario in scen_list; note can't use case_when() here because it evaluates code
# for all cases, so need to use extensive if statements

lapply(scen_list_demand,function(x) {
  
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
  } else if(x == "RefML") {
    # RefML
    food.dmnd.plus.FE.regions(param_data_global_clean_sub,param_data_FE_clean_sub,
                              incprice_RefML,"RefML",reg_list)

  } else if(x == "Ref_ML_gcam") {

    # Ref_ML_gcam
    food.dmnd.wrapper(
      params_ML_intervals_global %>% filter(measure == "ML"),
      params_ML_intervals_FE %>% filter(measure == "ML"),
      gcamoutput_Ref_ML,
      reg_list,
      procdata_dir,
      "Ref_ML_gcam",
      "MLparams",
      TRUE)            # save results to files
    
  } else if(x == "Ref_ML_gcam_ens") {

    # Ref_ML_gcam
    food.dmnd.wrapper(
      param_data_global_clean_sub,
      param_data_FE_clean_sub,
      gcamoutput_Ref_ML,
      reg_list,
      procdata_dir,
      "Ref_ML_gcam",
      "MLparams_ens",
      TRUE)            # save results to files
    
  } else if(x == "Ref_ML_gcam_ens_bc") {
    
    # modifications for small test runs
    param_data_global_clean_sub <- param_data_global_clean_sub[1:10,]
    param_data_FE_clean_sub <- 
      subset(param_data_FE_clean_sub,
             iteration %in% param_data_global_clean_sub$iteration)
    reg_list <- c(1,2)
    
    food.dmnd.ens_bc(
      globalparams = param_data_global_clean_sub,
      regparams = param_data_FE_clean_sub,
      inputdata = gcamoutput_Ref_ML,
      regions = reg_list,
      output_dir = procdata_dir,
      scen = "Ref_ML_gcam",
      case = "ens_bc_10jun",  # file ext: bias corrected ensemble
      baseyr = 2015
    )
  }

  # check results: shows that newly calculated demand is correctly bias corrected
  # to the 2015 regional results in the reference scenario (demand_ref below)
  # tmp <- readRDS(paste("data/processed",procdata_dir,"Ref_ML_gcam/demand_R1_ens_bc_10jun.RDS",
  #               sep = "/"))
  # tmp1 <- readRDS(paste("data/processed",procdata_dir,"Ref_ML_gcam/demand_R1_ens_bc_rerun.RDS",
  #                      sep = "/"))
  # tmp2 <- readRDS(paste("data/processed",procdata_dir,"Ref_ML_gcam/demand_R1_ens_bc.RDS",
  #                      sep = "/"))
  # demand_ref <- readRDS("data/processed/update9_cnstrlam_agg32FE_24jan25/Ref_ML_gcam/demand_R1_MLparams.RDS")
  
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

# clean up
rm(param_data_global_clean_sub,param_data_FE_clean_sub,params_ML_intervals_global,
   params_ML_intervals_FE)

print("saved demand and elasticities for subsample")
