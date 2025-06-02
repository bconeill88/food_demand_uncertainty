
# initializations --------------------------------------------------------------
# ----

print("testing parallel job implementation")
cat("\n")

# load packages
# note: was unable to install ambrosia on pic, instead cloned it to indicated directory
#devtools::load_all("/qfs/people/onei736/food_demand/ambrosia/")
devtools::load_all("H:/My Drive/R projects/food_demand/ambrosia/")
library(tidyverse)

# define the raw data to use
input_raw_path <- "H:/My Drive/R projects/food_demand/uncertainty/inputs/raw/unweighted"
#input_raw_path <- "/qfs/people/onei736/food_demand/uncertainty/inputs/raw/unweighted"
param_data_global_file <- "ambrosia_9_params_24Jan25.dat"
param_data_FE_file <- "FE_paramsJan2425.dat"
obs_data_file <- "Processed_group_data_13Jan25.csv"

# define the working directory and basic sub-directory paths
work_dir <- "H:/My Drive/R projects/food_demand/uncertainty"
#work_dir <- "/qfs/people/onei736/food_demand/uncertainty"
setwd(work_dir)
input_path <-  "inputs"
results_path <- "results"
fig_path <- "figures"
gcam_results_path <- "results_gcam/analysis_results"

# get command line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  reg_list <- c(as.numeric(args[1]))
} else {
  reg_list <- c(1:32)
}

# define broad features of the analysis to run
analysis_dir <- "update9_cnstrlam_agg32FE_24jan25"
iter_start <- 239850 # 193001 # 122900 for 13jan25 files
iter_end <- Inf # goes up to 240k
subsample <- 25 # 10000
scen_list_demand <- c("Ref_ML_gcam")    # scenarios to calculate demand uncertainty for
scen_list_diffs <- c("2xPsPn")           # scenarios to calculate differences for
scen_base_diffs <- c("Pdef")             # scenario against which diffs are taken
#scen_list_demand <- c("Pdef","2xPsPn","2xPs","2xPn","PsPnHi","MLprice","MLincome",
#               "MLscale","MLpm")
#scen_list_diffs <- c("2xPsPn","2xPs","2xPn","PsPnHi","MLprice","MLincome","MLscale","MLpm")
food_budget_limit <- 1 # max food budget share

# define the sub-parts of the analysis to run when running on PIC
CLEAN_DATA <- FALSE
PARAM_UNCTY <- FALSE
DEMAND_PARAMS <- TRUE
DEMAND_OBS <- FALSE
DEMAND_UNCTY <- TRUE
DEMAND_GCAM <- FALSE
DEMAND_DIFFS <- FALSE
DEMAND_DIFFS_UNCTY <- FALSE
PARAM_DEMAND_UNCTY <- FALSE
CALCS_2017 <- FALSE
CALCS_2021 <- FALSE

# define different income and price scenarios; sequences need to be rounded to
# avoid inexact values, a problem if later trying to select values eg with '=='
incprice_def <-
  data.frame(Y=c(seq(0.1,5, by=0.1),seq(5.5,30,by=0.5),seq(35,100,by=5))) %>%
  round(.,dig=1) %>% mutate(Ps=0.1,Pn=0.2)
incprice_2xPsPn <-
  data.frame(Y=c(seq(0.1,5, by=0.1),seq(5.5,30,by=0.5),seq(35,100,by=5))) %>%
  round(.,dig=1) %>% mutate(Ps=2*0.1,Pn=2*0.2)
incprice_2xPs <-
  data.frame(Y=c(seq(0.1,5, by=0.1),seq(5.5,30,by=0.5),seq(35,100,by=5))) %>%
  round(.,dig=1) %>% mutate(Ps=2*0.1,Pn=0.2)
incprice_2xPn <-
  data.frame(Y=c(seq(0.1,5, by=0.1),seq(5.5,30,by=0.5),seq(35,100,by=5))) %>%
  round(.,dig=1) %>% mutate(Ps=0.1,Pn=2*0.2)
incprice_PsPnHi <-
  data.frame(Y=c(seq(0.1,5, by=0.1),seq(5.5,30,by=0.5),seq(35,100,by=5))) %>%
  round(.,dig=1) %>% mutate(Ps=2.0,Pn=2.0)
# get regionally specific income, price, and demand scenarios from GCAM results
load(paste(analysis_dir,gcam_results_path,"incpricedem_Ref_ML_gcam.RData",sep="/"))
incpricedem_Ref_ML_gcam <- incpricedem
load(paste(analysis_dir,gcam_results_path,"incpricedem_Ref_HD_gcam.RData",sep="/"))
incpricedem_Ref_HD_gcam <- incpricedem
# construct regionally specific income and price scenario with prices as mean of
# GCAM ML scenario and income over wide range
prices <- incpricedem_Ref_ML_gcam %>%
  filter(year >= 2015) %>%   # only include base year and projections in the means
  group_by(region) %>%
  summarize(Ps = mean(Ps),Pn = mean(Pn)) %>%
  mutate(Y = NULL)
inc <- data.frame(Y=c(seq(0.1,5, by=0.1),seq(5.5,30,by=0.5),seq(35,100,by=5))) %>%
  round(.,dig=1)
incprice_RefML <- full_join(prices,inc,by=character()) # cross_join(prices,inc) # depends on R version

rm(incpricedem,prices,inc)

# define confidence interval to use for uncertainty analyses
confinterval <- 90

# create analysis directory and all subdirectories if necessary
if(!dir.exists(analysis_dir)) {
  dir.create(analysis_dir)
  dir.create(paste(analysis_dir,"inputs",sep="/"))
  dir.create(paste(analysis_dir,"results",sep="/"))
  dir.create(paste(analysis_dir,"figures",sep="/"))
  # results sub-directories for specific scenarios of prices, parameters, etc.
  subdirs <- c("Pdef","2xPsPn","2xPs","2xPn","PsPnHi","Obs","MLprice","MLincome",
               "MLscale","MLpm")
  lapply(subdirs,function(x) dir.create(paste(analysis_dir,"results",x,sep="/"))) %>%
    invisible()
}

print("finished initializations")


source("scripts/08_demand_for_GCAM_scenario.R")

print("saved ambrosia results, including bias-corrected, based on GCAM income and price scenario")



# Load parameters for intervals for examination --------------------------------
# ----

# load(paste(analysis_dir,results_path,"params_ML_intervals_global.RData",sep="/"))
# load(paste(analysis_dir,results_path,"params_ML_intervals_FE.RData",sep="/"))
# load(paste0(analysis_dir,"/",results_path,"/RefML/","params_HDLD_freq_global.RData"))
# load(paste0(analysis_dir,"/",results_path,"/RefML/","params_HDLD_freq_FE.RData"))
# load(paste0(analysis_dir,"/",results_path,"/RefML/","params_HDLD_freq_table.RData"))


print("done")
