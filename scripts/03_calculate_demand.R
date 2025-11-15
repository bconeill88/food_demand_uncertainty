# calculate and save demand and elasticities either for a single scenario or for 
# a parameter ensemble 

# To Do:
# Code for all scenarios needs to be updated to use new food.dmnd.wrapper function
# except Ref_ML_gcam, Ref_ML_gcam_ens, and Ref_ML_gcam_ens_bc, which are up to
# date

# load functions
source("R/demand_functions.R")
#source("R/min_calorie_adjustment.R")
source("R/install_ambrosia_function.R")
source("R/init_packages.R")

# install or load packages as needed
ensure_package(tidyverse)
ensure_package(progressr)

# install ambrosia if necessary based on machine (WF10681 or PIC)
force_install <- TRUE   # set to TRUE for testing new ambrosia version
if (Sys.info()["nodename"] == "WF10681") {
  install_ambrosia_once("H:/My Drive/R projects/food_demand/ambrosia", force_install)
} else {
  # setwd("/qfs/people/onei736/food_demand/uncertainty")
  # install_ambrosia_once(force_install)
  install_ambrosia_once("/qfs/people/onei736/food_demand/ambrosia", force_install)
}

# subdirectories of data/processed to use
procdata_dir <- "update9_cnstrlam_agg32FE_24jan25"
#procdata_subdir <- "Ref_ML_gcam/ens_bc_20251009_221027"
gcam_results_dir <- "results_gcam"

# define list of scenarios to run
scen_list_demand <- 
#  list("Ref_ML_gcam_MLparams_bc")
#  list("Ref_ML_gcam_LDparams_bc")
#  list("Ref_ML_gcam_HDparams_bc")
#  list("Ref_LD_gcam_LDparams_bc")
#  list("Ref_HD_gcam_HDparams_bc")
#  list("Ref_ML_gcam_MLparams_bc", "Ref_ML_gcam_LDparams_bc", "Ref_ML_gcam_HDparams_bc")
#  list("Ref_ML_gcam_MLparams_bc", "Ref_ML_gcam_LDparams_bc", "Ref_ML_gcam_HDparams_bc",
#       "Ref_HD_gcam_HDparams_bc", "Ref_LD_gcam_LDparams_bc")
   list("Ref_ML_gcam_ens_bc")

# define regions to run over
# get command line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  reg_list <- c(as.numeric(args[1]))
} else {
  reg_list <- c(1:32)
}

# define minimum food demand level and allocation threshold
Qs_floor <- 0.6
Qn_floor <- 0.01
alloc_ratio <- 1.1

# load input assumptions for ambrosia (prices, income)
gcamoutput_Ref_ML <- readRDS(file.path("data", "processed", procdata_dir, 
                                       gcam_results_dir, "gcamoutput_Ref_ML.RDS"))
gcamoutput_Ref_HD <- readRDS(file.path("data", "processed", procdata_dir, 
                                       gcam_results_dir, "gcamoutput_Ref_HD.RDS"))
gcamoutput_Ref_LD <- readRDS(file.path("data", "processed", procdata_dir, 
                                       gcam_results_dir, "gcamoutput_Ref_LD.RDS"))

# load parameter files

# ensembles of parameters from MCMC; files read in are produced by running 01_clean_data.R
load(file.path("data", "processed", procdata_dir, "inputs", "param_data_global_clean_sub.RData"))
load(file.path("data", "processed", procdata_dir, "inputs", "param_data_FE_clean_sub.RData"))

# ML parameter values; files read in are produced by running 02_parameter_uncertainty_intervals.R
load(file.path("data", "processed", procdata_dir, "params_ML_intervals_global.RData"))
load(file.path("data", "processed", procdata_dir, "params_ML_intervals_FE.RData"))

# HD/LD parameter values; files read in are produced by running 07_params_for_intervals.R
# params_HDLD_global <- read.csv(file.path("data", "processed", procdata_dir, procdata_subdir,
#                                          "params_global_HDLD_Qtot.csv"))
# params_HDLD_FE <- read.csv(file.path("data", "processed", procdata_dir, procdata_subdir,
#                                          "params_FE_HDLD_Qtot.csv"))
# iter_table_HDLD <- read.csv(file.path("data", "processed", procdata_dir, procdata_subdir,
#                                       "iter_table_HDLD_Qtot.csv"))

# identify iterations associated with global HD and LD parameters
# iter_HD <- iter_table_HDLD[iter_table_HDLD$ID == 33, "HD_Qtot"]
# iter_LD <- iter_table_HDLD[iter_table_HDLD$ID == 33, "LD_Qtot"]

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

  # single GCAM scenarios

  } else if(x == "Ref_ML_gcam_MLparams_bc") {

    food.dmnd.ens_bc(
      globalparams = params_ML_intervals_global %>% filter(measure == "ML"),
      regparams = params_ML_intervals_FE %>% filter(measure == "ML"),
      inputdata = gcamoutput_Ref_ML,
      regionIDs = reg_list,
      output_dir = procdata_dir,
      scen = "Ref_ML_gcam",
      case = "MLparams_bc",
      baseyr = 2015,
      Qs_min = Qs_floor,
      Qn_min = Qn_floor,
      alloc_thresh = alloc_ratio,
      bias_method = "solve",       # either "subtract" or "solve"
      save_result = TRUE)
    
    # check against GCAM base year results
    # tmp_gcam <- gcamoutput_Ref_ML %>% 
    #   filter(year == 2015,
    #          `gcam-consumer` == "FoodDemand_Group1") %>%
    #   select(region, year, Qs.region, Qn.region) 
    # tmp_amb <- map_dfr(reg_list, function(r) {
    #   readRDS(paste0("data/processed/", procdata_dir, "/Ref_ML_gcam/demand_R", r, "_ens_bc_10jun.RDS")) %>%
    #             filter(year == 2015,
    #                    `gcam-consumer` == "FoodDemand_Group1") %>%
    #             select(region, year, Qs.region, Qn.region)
    #           })

  } else if(x == "Ref_ML_gcam_HDparams_bc") {
    
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

  } else if(x == "Ref_ML_gcam_ens_bc") {
    
    # modifications for small test runs
    param_data_global_clean_sub <- param_data_global_clean_sub[sample(nrow(param_data_global_clean_sub), 200),]
    param_data_FE_clean_sub <-
      subset(param_data_FE_clean_sub,
             iteration %in% param_data_global_clean_sub$iteration)
    # reg_list <- c(1,2)

    food.dmnd.ens_bc(
      globalparams = param_data_global_clean_sub,
      regparams = param_data_FE_clean_sub,
      inputdata = gcamoutput_Ref_ML,
      regionIDs = reg_list,
      output_dir = procdata_dir,
      scen = "Ref_ML_gcam",
      case = "MLparams_ens_bc",       # file ext: bias corrected ensemble
      baseyr = 2015,
      Qs_min = Qs_floor,
      Qn_min = Qn_floor,
      alloc_thresh = alloc_ratio,  # only needed for "subtract" method
      bias_method = "solve",       # either "subtract" or "solve"
      save_result = TRUE
    )
    
    # checking results in various ways
    
    # load("data/raw/GCAM_region_ID_mapping.Rdata")
    # 
    # iterationML <- params_ML_intervals_global %>% 
    #   filter(measure == "ML") %>% 
    #   pull(iteration)
    # 
    # tmpR1 <- readRDS(paste("data/processed",procdata_dir,
    #                        "Ref_ML_gcam/ens_bc_20251009_173040/demand_R1_ens_bc.RDS",
    #                        sep = "/")) %>%
    #   filter(year == 2015)
    # tmpR2 <- readRDS(paste("data/processed",procdata_dir,
    #                        "Ref_ML_gcam/ens_bc_20251009_173040/demand_R2_ens_bc.RDS",
    #                        sep = "/")) %>%
    #   filter(year == 2015)
    # tmpR3 <- readRDS(paste("data/processed",procdata_dir,
    #                        "Ref_ML_gcam/demand_ens_bc_12jul25/demand_R3_ens_bc.RDS",
    #                        sep = "/")) %>%
    #   filter(year == 2015, iteration == iterationML)
    # tmpR28 <- readRDS(paste("data/processed",procdata_dir,
    #                         "Ref_ML_gcam/demand_ens_bc_12jul25/demand_R28_ens_bc.RDS",
    #                         sep = "/")) %>%
    #   filter(year == 2015, iteration == iterationML)
    # write.csv(bind_rows(tmpR2, tmpR3, tmpR28), file = "baseyr_bias_corrected_toKanishka.csv")
    # #
    # tmpR2_old <- readRDS(paste("data/processed",procdata_dir,
    #                            "Ref_ML_gcam/ens_bc_20251008_103435/demand_R2_ens_bc.RDS",
    #                            sep = "/")) # %>%
    # #      filter(year == 2015, iteration == iterationML)
    # tmpR2_new <- readRDS(paste("data/processed",procdata_dir,
    #                            "Ref_ML_gcam/ens_bc_20251008_103517/demand_R2_ens_bc.RDS",
    #                            sep = "/")) # %>%
    # #     filter(year == 2015, iteration == iterationML)
    # 
    # # check results: shows that newly calculated demand is correctly bias corrected
    # # to the 2015 regional results in the reference scenario (demand_ref below)
    # selected_cols <- c("Qs.region", "Qn.region")
    # procdata_subdir <- "ens_bc_20250804_105557"
    # filename <- "demand_R11_ens_bc.RDS"
    # 
    # tmp <- readRDS(paste("data/processed", procdata_dir, "Ref_ML_gcam", procdata_subdir, 
    #                      filename, sep = "/")) %>%
    #   filter(year == 2015, `gcam-consumer` == "FoodDemand_Group1") %>%
    #   summarise(across(all_of(selected_cols), list(median = median, range = ~ list(range(.)))))
    # tmp1 <- readRDS(paste("data/processed",procdata_dir,"Ref_ML_gcam/demand_R2_ens_bc_10jun.RDS",
    #                      sep = "/")) %>% 
    #   filter(year == 2015, `gcam-consumer` == "FoodDemand_Group1") %>%
    #   summarise(across(all_of(selected_cols), list(median = median, range = ~ list(range(.)))))
    # tmp2 <- readRDS(paste("data/processed",procdata_dir,"Ref_ML_gcam/demand_R2_ens_bc.RDS",
    #                      sep = "/")) %>% 
    #   filter(year == 2015, `gcam-consumer` == "FoodDemand_Group1") %>%
    #   summarise(across(all_of(selected_cols), list(median = median, range = ~ list(range(.)))))
    # demand_ref <-
    #   readRDS(paste0("data/processed/", procdata_dir, "/results_gcam/gcamoutput_Ref_ML.RDS")) %>%
    #   filter(year == 2015, `gcam-consumer` == "FoodDemand_Group1")
    
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

print("saved demand and elasticities for subsample")
