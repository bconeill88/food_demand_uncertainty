# produces GCAM scenario outcomes suitable for use as input to ambrosia and for
# comparison with ambrosia results;
# reads GCAM results from rgcam output provided by Kanishka, extracts needed
# variables, calculates per capita income by decile (since direct GCAM output
# for this variable is incorrect) from separate data on regional decile
# income shares, calculates consumption shares and regional demand, and saves 
# result as a dataframe

# reminder this is what works to install rgcam, not the github instructions
# need Rtools installed first
# install.packages("pak")
# library(pak)
# pkg_install("JGCRI/rgcam")

# Load libraries
library(rgcam)
library(tidyverse)

# get functions
source("R/gcam_results_function.R")

# define sub-directories of data/processed or data/raw to use
gcam_rawdata_dir <- "final_rgcam_outputs"
procdata_dir <- "update9_cnstrlam_agg32FE_24jan25"
gcam_results_dir <- "results_gcam"

# load GCAM scenario results: Kanishka's rgcam query results using rgcam function; 
# assign to names indicating GCAM scenario first, then parameters used in the GCAM run
prj_Ref_ML <- loadProject(paste("data/raw",gcam_rawdata_dir,"tables_ML.proj",sep="/"))
# prj_HP_ML <- loadProject(paste("data/raw",gcam_rawdata_dir,"tables_MLHipricecond.proj",sep="/"))
prj_Ref_HD <- loadProject(paste("data/raw",gcam_rawdata_dir,"tables_HiDemand.proj",sep="/"))
# prj_Ref_HPR <- loadProject(paste("data/raw",gcam_rawdata_dir,"tables_Hi_price.proj",sep="/"))
# prj_HP_HPR <- loadProject(paste("data/raw",gcam_rawdata_dir,"tables_HipriceHipricecond.proj",sep="/"))
prj_Ref_LD <- loadProject(paste("data/raw",gcam_rawdata_dir,"tables_LoDemand.proj",sep="/"))
# prj_Ref_LPR <- loadProject(paste("data/raw",gcam_rawdata_dir,"tables_Lo_price.proj",sep="/"))
# prj_HP_LPR <- loadProject(paste("data/raw",gcam_rawdata_dir,"tables_LopriceHipricecond.proj",sep="/"))
# prj_Ref_GCAM7 <- loadProject(paste("data/raw",gcam_rawdata_dir,"tables_Ref.proj",sep="/"))
# prj_HP_GCAM7 <- loadProject(paste("data/raw",gcam_rawdata_dir,"tables_RefHipricecond.proj",sep="/"))

# get and clean income share data
inc_share_data <- read.csv(file.path("data", "raw", gcam_rawdata_dir, "incomes.csv")) %>%
  # drop unused columns
  select(-X, -L106.income_distributions.subregional.population.share) %>%
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
#  list("Ref_ML"=prj_Ref_ML)
  list("Ref_ML"=prj_Ref_ML, "Ref_HD"=prj_Ref_HD, "Ref_LD"=prj_Ref_LD)
  # list("Ref_HD"=prj_Ref_HD,"Ref_HPR"=prj_Ref_HPR,"HP_HPR"=prj_HP_HPR,
  #      "Ref_LD"=prj_Ref_LD,"Ref_LPR"=prj_Ref_LPR,"HP_LPR"=prj_HP_LPR,
  #      "Ref_ML"=prj_Ref_ML,"HP_ML"=prj_HP_ML,"Ref_GCAM7"=prj_Ref_GCAM7,
  #      "HP_GCAM7"=prj_HP_GCAM7)

iwalk(GCAM_output_list, ~ get_GCAM_results(.x, inc_share_data, .y, procdata_dir,
                                           gcam_results_dir))

# debug by checking results
# results <- readRDS(paste("data/processed", gcam_procdata_dir, "gcamoutput_Ref_HD.RDS",sep="/"))
# tmp_new <- readRDS(paste("data/processed",gcam_procdata_dir,"gcamoutput_Ref_ML_test.RDS",sep="/"))
# tmp_old <- readRDS(paste("data/processed",gcam_procdata_dir,"gcamoutput_Ref_ML_11apr25.RDS",sep="/"))

# check results for income by comparing average per cap income across deciles
# to the GCAM regional income values
# incpricedem <- readRDS(paste0(
#    "data/processed/",gcam_procdata_dir,"/incpricedem_Ref_ML_gcam.RDS"))
# raw_reg_income_pc <- prj_Ref_ML[[1]][['GDP per capita PPP by region']]
# calc_reg_income_pc <- incpricedem %>% group_by(region,year) %>% 
#   summarize(mean_Y = mean(Y))
# left_join(raw_reg_income_pc,calc_reg_income_pc) %>% ggplot() +
#   geom_point(aes(x = value, y = mean_Y))

