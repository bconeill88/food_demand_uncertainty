# identify and save uncertainty intervals for demand and elasticities ----------
# ----

# load functions
source("R/demand_interval_functions.R")

# load packages
library(dplyr)
library(purrr)

# on pic, set working directory
setwd("/qfs/people/onei736/food_demand/uncertainty")

# subdirectories of data/processed to use
procdata_dir <- "update9_cnstrlam_agg32FE_24jan25"

# define run
confinterval <- 90
scen_list_demand <- list("Ref_ML_gcam")
case <- "_ens_bc"
joint_intervals <- c("LPR", "HPR", "LIR", "HIR", "LSR", "HSR")

# define regions to run over
# get command line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  reg_list <- c(as.numeric(args[1]))
} else {
  reg_list <- c(1:29,31,32) # skip Taiwan, no post-2015 output from GCAM
}

# load needed file; stored in params_ML_intervals_global
load(paste0("data/processed/",procdata_dir,"/params_ML_intervals_global.RData"))

# identify ML, calculate independent and identify joint uncertainty intervals for
# regions for a given independent interval level and scenario, and save results;
# second scenario is "None" to get intervals for demand rather than demand differences;
# only the ML and independent interval results are meaningful for the uncertainty
# decomposition scenarios (MLprice, MLincome, etc.)
walk(
  scen_list_demand,
  ~ make_demand_intervals(
    ci = confinterval,
    scen1 = .x,
    scen2 = "None",
    case = case,
    regions = reg_list,
    paramintervals = params_ML_intervals_global,
    measures = joint_intervals,
    data_dir = procdata_dir
  )
)

# check results
#tmp <- readRDS(paste0("data/processed/",procdata_dir,"/Ref_ML_gcam/demand_intervals_R2_ens_bc.RDS"))

# clean up
rm(params_ML_intervals_global)

print("saved intervals for demand")

