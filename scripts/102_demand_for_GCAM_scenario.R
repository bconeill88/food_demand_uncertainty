# calculate ambrosia demand given income, prices, and bias adders from GCAM scenarios:
# Ref_ML_gcam, Ref_HD_gcam, Ref_LD_gcam, Ref_ML_gcam_HDparams

# load packages
library(tidyverse)
devtools::load_all("H:/My Drive/R projects/food_demand/ambrosia/")

# define sub-directories of data/processed to use
procdata_dir <- "update9_cnstrlam_agg32FE_24jan25"
gcam_results_dir <- "results_gcam"

# define regions to run over
reg_list <- c(1:32)

# get functions
source("R/demand_functions.R")

# load files containing the ML parameters
load(paste("data/processed",procdata_dir,"params_ML_intervals_global.RData",sep="/"))
load(paste("data/processed",procdata_dir,"params_ML_intervals_FE.RData",sep="/"))

# load files containing the HD/LD parameters (Kanishka's runs used the Pdef version)
load(paste("data/processed",procdata_dir,"Pdef/params_HDLD_freq_global.RData",sep="/"))
load(paste("data/processed",procdata_dir,"Pdef/params_HDLD_freq_FE.RData",sep="/"))

# load gcam scenario output to use as input to ambrosia runs
# Ref ML
gcamoutput_Ref_ML <- readRDS(paste("data/processed",procdata_dir,gcam_results_dir,
                                         "gcamoutput_Ref_ML.RDS",sep="/"))
# Ref HD
gcamoutput_Ref_HD <- readRDS(paste("data/processed",procdata_dir,gcam_results_dir,
                                         "gcamoutput_Ref_HD.RDS",sep="/"))
# # Ref LD
# gcamoutput_Ref_LD <- readRDS(paste("data/processed",procdata_dir,gcam_results_dir,
#            "gcamoutput_Ref_LD.RDS",sep="/"))

# calculate and save bias corrected demand

# Ref_ML scenario with ML parameters
food.dmnd.wrapper(
  params_ML_intervals_global[params_ML_intervals_global$measure == 'ML',],
  params_ML_intervals_FE[params_ML_intervals_FE$measure == 'ML',],
  gcamoutput_Ref_ML,
  "Ref_ML_gcam",
  "MLparams",
  reg_list,
  procdata_dir)

# Ref_HD scenario with HD parameters
food.dmnd.wrapper(
  params_global[params_global$measure == 'HD_Qtot',],
  params_FE[params_FE$measure == 'HD_Qtot',],
  gcamoutput_Ref_HD,
  "Ref_HD_gcam",
  "HDparams",
  reg_list,
  procdata_dir)

# # Ref_LD scenario with LD parameters
# food.dmnd.wrapper(
#   params_global[params_global$measure == 'LD_Qtot',],
#   params_FE[params_FE$measure == 'LD_Qtot',],
#   gcamoutput_Ref_LD,
#   "Ref_LD_gcam",
#   "LDparams",
#   reg_list,
#   procdata_dir)

# Ref_ML scenario with HD parameters
# should not use any information from GCAM HD run

# get ambrosia demand for ML scenario with HD parameters and zero bias terms
food.dmnd.wrapper(
  params_global[params_global$measure == 'HD_Qtot',],
  params_FE[params_FE$measure == 'HD_Qtot',],
  gcamoutput_Ref_ML %>% mutate(RBs = 0, RBn = 0),
  "Ref_ML_gcam",
  "HDparams_noBias",
  reg_list,
  procdata_dir)

# calculate bias terms from difference between the zero bias scenario and the
# reference scenario
# get zero bias scenario results
amboutput_Ref_ML_HDparams_noBias <- map_dfr(reg_list, function(r) {
  readRDS(paste0("data/processed/",procdata_dir,"/Ref_ML_gcam/demand_R",r,"_HDparams_noBias.RDS"))
})
# get reference scenario results
amboutput_Ref_ML_MLparams <- map_dfr(reg_list, function(r) {
  readRDS(paste0("data/processed/",procdata_dir,"/Ref_ML_gcam/demand_R",r,"_MLparams.RDS"))
})
# replace bias terms in the reference scenario with a calculated term
ambinput_Ref_ML_HDparams <- 
  inner_join(amboutput_Ref_ML_MLparams, amboutput_Ref_ML_HDparams_noBias,
             join_by(region, `gcam-consumer`, year),
             suffix = c("", ".HD")) %>%
  # calculate new bias term for all historical years
  mutate(RBs = ifelse(year <= 2015, Qs.region - Qs.region.HD, NA_real_),
         RBn = ifelse(year <= 2015, Qn.region - Qn.region.HD, NA_real_)) %>%
  group_by(region, `gcam-consumer`) %>%
  # assign all future years the bias term of the base year
  mutate(RBs = ifelse(year > 2015, first(na.omit(RBs[year == 2015])), RBs),
         RBn = ifelse(year > 2015, first(na.omit(RBn[year == 2015])), RBn)) %>%
  ungroup() %>%
  # keep only meaningful columns (outputs like demand not meaningful)
  select(c(GCAM_region_ID,region,year,`gcam-consumer`,Y,Ps,Pn,RBs,RBn))

# calculate ambrosia demand with derived bias terms
food.dmnd.wrapper(
  params_global[params_global$measure == 'HD_Qtot',],
  params_FE[params_FE$measure == 'HD_Qtot',],
  ambinput_Ref_ML_HDparams,
  "Ref_ML_gcam",
  "HDparams",
  reg_list,
  procdata_dir)

# clean up
rm(params_ML_intervals_global,params_ML_intervals_FE,params_global,params_FE)
FILES <- list.files(pattern = "_noBias.RDS$")
file.remove(FILES)

message("saved ambrosia results based on GCAM income and price scenario")


