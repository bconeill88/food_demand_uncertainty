# Calculate and save demand and elasticities for cases that require the MCMC
# ensemble of parameters and GCAM results for the HD and LD parameter values. These 
# consist of single scenarios and ensemble runs driven by GCAM price and income 
# paths.

# To Do:
# Code for all scenarios needs to be updated to use new food.dmnd.wrapper function
# except Ref_ML_gcam, Ref_ML_gcam_ens, and Ref_ML_gcam_ens_bc, which are up to
# date

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

# HD/LD parameter values; files read in are produced by running 07_params_for_intervals.R
params_HDLD_global <- read.csv(file.path("data", "processed", procdata_dir, procdata_subdir,
                                         "params_global_HDLD_Qtot.csv"))
params_HDLD_FE <- read.csv(file.path("data", "processed", procdata_dir, procdata_subdir,
                                         "params_FE_HDLD_Qtot.csv"))
iter_table_HDLD <- read.csv(file.path("data", "processed", procdata_dir, procdata_subdir,
                                      "iter_table_HDLD_Qtot.csv"))

# identify iterations associated with global HD and LD parameters
iter_HD <- iter_table_HDLD[iter_table_HDLD$ID == 33, "HD_Qtot"]
iter_LD <- iter_table_HDLD[iter_table_HDLD$ID == 33, "LD_Qtot"]

# load GCAM results, as input assumptions for ambrosia (prices, income)
gcamoutput_Ref_ML <- readRDS(file.path("data", "processed", procdata_dir, 
                                       gcam_results_dir, "gcamoutput_Ref_ML.RDS"))
gcamoutput_Ref_HD <- readRDS(file.path("data", "processed", procdata_dir, 
                                       gcam_results_dir, "gcamoutput_Ref_HD.RDS"))
gcamoutput_Ref_LD <- readRDS(file.path("data", "processed", procdata_dir, 
                                       gcam_results_dir, "gcamoutput_Ref_LD.RDS"))

# calculate demand and elasticities for the subsample parameter iterations, for all
# regions given a dataframe of income and prices, saves regional results, for each
# scenario in scen_list; note can't use case_when() here because it evaluates code
# for all cases, so need to use extensive if statements

lapply(scen_list_demand_GCAM_HDLD, function(x) {
  
  if(x == "Ref_ML_gcam_HDparams_bc") {
    
    food.dmnd.ens_bc(
      globalparams = params_HDLD_global %>% filter(iteration == iter_HD),
      regparams = params_HDLD_FE %>% filter(iteration == iter_HD),
      inputdata = gcamoutput_Ref_ML,
      regionIDs = reg_list,
      output_dir = procdata_dir,
      scen = "Ref_ML_gcam",
      case = "HDparams_bc",
      baseyr = 2015,
      Qs_min = Qs_floor,
      Qn_min = Qn_floor,
      alloc_thresh = alloc_ratio,
      bias_method = "solve",       # either "subtract" or "solve"
      save_result = TRUE)

  } else if(x == "Ref_ML_gcam_LDparams_bc") {
    
    food.dmnd.ens_bc(
      globalparams = params_HDLD_global %>% filter(iteration == iter_LD),
      regparams = params_HDLD_FE %>% filter(iteration == iter_LD),
      inputdata = gcamoutput_Ref_ML,
      regionIDs = reg_list,
      output_dir = procdata_dir,
      scen = "Ref_ML_gcam",
      case = "LDparams_bc",
      baseyr = 2015,
      Qs_min = Qs_floor,
      Qn_min = Qn_floor,
      alloc_thresh = alloc_ratio,
      bias_method = "solve",       # either "subtract" or "solve"
      save_result = TRUE)
    
  } else if(x == "Ref_HD_gcam_HDparams_bc") {
    
    food.dmnd.ens_bc(
      globalparams = params_HDLD_global %>% filter(iteration == iter_HD),
      regparams = params_HDLD_FE %>% filter(iteration == iter_HD),
      inputdata = gcamoutput_Ref_HD,
      regionIDs = reg_list,
      output_dir = procdata_dir,
      scen = "Ref_HD_gcam",
      case = "HDparams_bc",
      baseyr = 2015,
      Qs_min = Qs_floor,
      Qn_min = Qn_floor,
      alloc_thresh = alloc_ratio,
      bias_method = "solve",       # either "subtract" or "solve"
      save_result = TRUE)
    
    # plot comparison of variants of HD scenario
    if(FALSE) {
      
      HD_base <- readRDS(file.path("data", "processed", procdata_dir, "Ref_HD_gcam",
                                   "HDparams_bc_20251021_093840", 
                                   "demand_R2_HDparams_bc.RDS"))
      HD_shares <- readRDS(file.path("data", "processed", procdata_dir, "Ref_HD_gcam",
                                   "HDparams_bc_shares_20251022_093208", 
                                   "demand_R2_HDparams_bc_shares.RDS"))
      HD_scale <- readRDS(file.path("data", "processed", procdata_dir, "Ref_HD_gcam",
                                     "HDparams_bc_shares_scale_20251022_093658", 
                                    "demand_R2_HDparams_bc_shares_scale.RDS"))
      
      library(ggplot2)

      # Combine into one long data frame with an identifier
      df_all <- bind_rows(
        HD_base  %>% filter(`gcam-consumer` == "FoodDemand_Group1") %>% mutate(source = "Base"),
        HD_shares %>% filter(`gcam-consumer` == "FoodDemand_Group1") %>% mutate(source = "Shares"),
        HD_scale %>% filter(`gcam-consumer` == "FoodDemand_Group1") %>% mutate(source = "Scale")
      )
      
      # Reshape to long format for faceting by variable
      df_long <- df_all %>%
        select(Y, Qs, Qn, source) %>%
        pivot_longer(cols = c(Qs, Qn), names_to = "food_type", values_to = "demand")
      
      # Single plot with two panels (Qs and Qn)
      p <- ggplot(df_long, aes(x = Y, y = demand, color = source)) +
        geom_point(size = 2) +
        facet_wrap(~ food_type, scales = "free_y") +
        labs(x = "Income (Y)", y = "Demand", color = "Dataset",
             title = "Staples and Non-Staples Demand vs Income") +
        theme_minimal()
      
      p
    }
    
  } else if(x == "Ref_LD_gcam_LDparams_bc") {
    
    food.dmnd.ens_bc(
      globalparams = params_HDLD_global %>% filter(iteration == iter_LD),
      regparams = params_HDLD_FE %>% filter(iteration == iter_LD),
      inputdata = gcamoutput_Ref_LD,
      regionIDs = reg_list,
      output_dir = procdata_dir,
      scen = "Ref_LD_gcam",
      case = "LDparams_bc",
      baseyr = 2015,
      Qs_min = Qs_floor,
      Qn_min = Qn_floor,
      alloc_thresh = alloc_ratio,
      bias_method = "solve",       # either "subtract" or "solve"
      save_result = TRUE)

  # GCAM scenario ensembles

  } else if(x == "Ref_HD_gcam_ens_bc") {
    
    food.dmnd.ens_bc(
      globalparams = param_data_global_clean_sub,
      regparams = param_data_FE_clean_sub,
      inputdata = gcamoutput_Ref_HD,
      regions = reg_list,
      output_dir = procdata_dir,
      scen = "Ref_HD_gcam",
      case = "HDparams_ens_bc",        # file ext: bias corrected ensemble
      baseyr = 2015,
      Qs_min = Qs_floor,
      Qn_min = Qn_floor,
      alloc_thresh = alloc_ratio,
      impose_min = TRUE,
      save_result = TRUE
    )
    
  } else if(x == "Ref_LD_gcam_ens_bc") {
    
    food.dmnd.ens_bc(
      globalparams = param_data_global_clean_sub,
      regparams = param_data_FE_clean_sub,
      inputdata = gcamoutput_Ref_LD,
      regions = reg_list,
      output_dir = procdata_dir,
      scen = "Ref_LD_gcam",
      case = "LDparams_ens_bc",          # file ext: bias corrected ensemble
      baseyr = 2015,
      Qs_min = Qs_floor,
      Qn_min = Qn_floor,
      alloc_thresh = alloc_ratio,
      impose_min = TRUE,
      save_result = TRUE
    )
  }

} # end function(x)

) %>% invisible # end lapply()

