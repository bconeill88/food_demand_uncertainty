# Produce GCAM scenario outcomes suitable for use as input to ambrosia and for
# comparison with ambrosia results.
# Reads GCAM results from rgcam output (in data/raw/final_rgcam_outputs) provided 
# by Kanishka, extracts needed variables, calculates per capita income by decile 
# (since direct GCAM output for this variable is incorrect) from separate data on 
# regional decile income shares, calculates consumption shares and regional demand, 
# and saves result as a dataframe (in data/processed/<procdata_dir>/results_gcam)

# reminder this is what works to install rgcam, not the github instructions
# need Rtools installed first
# install.packages("pak")
# library(pak)
# pkg_install("JGCRI/rgcam")

# Load libraries
library(rgcam)
library(tidyverse)

# get functions
source("R/common_definitions.R")
source("R/gcam_results_function.R")
source("R/demand_difference_functions.R")

# load GCAM scenario results: Kanishka's rgcam query results using rgcam function; 
# assign to names indicating GCAM scenario first, then parameters used in the GCAM run
gcam_tables_dir <- file.path("data/raw", gcam_rawdata_dir, gcam_rawdata_subdir)

prj_Ref_ML <- loadProject(file.path(gcam_tables_dir, "tables_ML.proj"))
prj_Ref_HD_HPR <- loadProject(file.path(gcam_tables_dir, "tables_HD_HPR_Qtot.proj"))
prj_Ref_HD_LPR <- loadProject(file.path(gcam_tables_dir, "tables_HD_LPR_Qtot.proj"))
prj_Ref_LD_HPR <- loadProject(file.path(gcam_tables_dir, "tables_LD_HPR_Qtot.proj"))
prj_Ref_LD_LPR <- loadProject(file.path(gcam_tables_dir, "tables_LD_LPR_Qtot.proj"))

prj_HP_ML <- loadProject(file.path(gcam_tables_dir, "tables_MLHiprice.proj"))
prj_HP_HD_HPR <- loadProject(file.path(gcam_tables_dir, "tables_HD_HPR_QtotHiPrice.proj"))
prj_HP_HD_LPR <- loadProject(file.path(gcam_tables_dir, "tables_HD_LPR_QtotHiPrice.proj"))
prj_HP_LD_HPR <- loadProject(file.path(gcam_tables_dir, "tables_LD_HPR_QtotHiPrice.proj"))
prj_HP_LD_LPR <- loadProject(file.path(gcam_tables_dir, "tables_LD_LPR_QtotHiPrice.proj"))

# get and clean income share data
inc_share_data <- 
  read.csv(file.path("data", "raw", gcam_rawdata_dir, income_dist_baseyr_file)) %>%
  # drop unused columns
  select(-L106.income_distributions.subregional.population.share) %>%
  # clean up column names
  rename_with(~ str_replace(.x, ".*distributions.", "")) %>%
  # change names and contents to match those used in GCAM output
  rename(`gcam-consumer` = gcam.consumer,income.share = subregional.income.share) %>%
  mutate(`gcam-consumer` = str_replace(`gcam-consumer`, "d*", "FoodDemand_Group"))

# get and save GCAM data for income, prices, and demand for all regions and 
# consumer groups; use named list for scenarios, where the name is the extension
# to be used in the file name that results are saved to; names indicate GCAM scenario
# first (Ref or High Price (HP)), then the parameters used (ML, HD, LD, etc.), then
# _gcam to indicate that they are gcam results, not ambrosia
GCAM_output_list <- 
  # list("HP_ML"=prj_HP_ML)
  # list("Ref_ML" = prj_Ref_ML, "HP_ML" = prj_HP_ML)
  list("Ref_HD_HPR" = prj_Ref_HD_HPR, "Ref_HD_LPR" = prj_Ref_HD_LPR,
       "Ref_LD_HPR" = prj_Ref_LD_HPR, "Ref_LD_LPR" = prj_Ref_LD_LPR,
       "HP_HD_HPR" = prj_HP_HD_HPR, "HP_HD_LPR" = prj_HP_HD_LPR,
       "HP_LD_HPR" = prj_HP_LD_HPR) # , "HP_LD_LPR" = prj_HP_LD_LPR)
  # list("Ref_HD"=prj_Ref_HD, "Ref_LD"=prj_Ref_LD)
  # list("Ref_ML"=prj_Ref_ML, "Ref_HD"=prj_Ref_HD, "Ref_LD"=prj_Ref_LD)
  # list("Ref_HPR"=prj_Ref_HPR,"HP_HPR"=prj_HP_HPR,
  #      "Ref_LPR"=prj_Ref_LPR,"HP_LPR"=prj_HP_LPR)
  # list("Ref_HD"=prj_Ref_HD,"Ref_HPR"=prj_Ref_HPR,"HP_HPR"=prj_HP_HPR,
  #      "Ref_LD"=prj_Ref_LD,"Ref_LPR"=prj_Ref_LPR,"HP_LPR"=prj_HP_LPR,
  #      "Ref_ML"=prj_Ref_ML,"HP_ML"=prj_HP_ML,"Ref_GCAM7"=prj_Ref_GCAM7,
  #      "HP_GCAM7"=prj_HP_GCAM7)

# produce processed GCAM output data
iwalk(GCAM_output_list, ~ get_GCAM_results(.x, inc_share_data, .y, procdata_dir,
                                           gcam_results_dir))

# calculate and save differences in GCAM results between high price and reference
# scenarios

# define directory for saving results
out_dir <- file.path("data", "processed", procdata_dir, gcam_results_dir)

# get processed GCAM results
gcam_Ref_ML <- readRDS(file.path("data", "processed", procdata_dir, gcam_results_dir,
                                     "gcamoutput_Ref_ML.RDS"))
gcam_HP_ML <- readRDS(file.path("data", "processed", procdata_dir, gcam_results_dir,
                                    "gcamoutput_HP_ML.RDS"))
gcam_Ref_HD_HPR <- readRDS(file.path("data", "processed", procdata_dir, gcam_results_dir,
                             "gcamoutput_Ref_HD_HPR.RDS"))
gcam_HP_HD_HPR <- readRDS(file.path("data", "processed", procdata_dir, gcam_results_dir,
                                        "gcamoutput_HP_HD_HPR.RDS"))
gcam_Ref_HD_LPR <- readRDS(file.path("data", "processed", procdata_dir, gcam_results_dir,
                                     "gcamoutput_Ref_HD_LPR.RDS"))
gcam_HP_HD_LPR <- readRDS(file.path("data", "processed", procdata_dir, gcam_results_dir,
                                    "gcamoutput_HP_HD_LPR.RDS"))
gcam_Ref_LD_HPR <- readRDS(file.path("data", "processed", procdata_dir, gcam_results_dir,
                                     "gcamoutput_Ref_LD_HPR.RDS"))
gcam_HP_LD_HPR <- readRDS(file.path("data", "processed", procdata_dir, gcam_results_dir,
                                    "gcamoutput_HP_LD_HPR.RDS"))
gcam_Ref_LD_LPR <- readRDS(file.path("data", "processed", procdata_dir, gcam_results_dir,
                                     "gcamoutput_Ref_LD_LPR.RDS"))
gcam_HP_LD_LPR <- readRDS(file.path("data", "processed", procdata_dir, gcam_results_dir,
                                    "gcamoutput_HP_LD_LPR.RDS"))

# define lists of the scenario variants and the base scenarios
scen_string_list <- list("ML", "HD_HPR", "HD_LPR", "LD_HPR", "LD_LPR")

# calculate scenario differences and save results
walk(scen_string_list, ~ subtract_gcam_demand_dfs(
  get(paste0("gcam_HP_", .x)), 
  "HP", 
  get(paste0("gcam_Ref_", .x)), 
  paste0("Ref_", .x),
  out_dir = out_dir)
)

# read a results file to check
# tmp <- readRDS(file.path(out_dir, "gcamoutput_diffs_Ref_LD_HPR_HP.RDS"))
