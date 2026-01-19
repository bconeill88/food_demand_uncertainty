# Calculate and save demand and elasticities for cases that require the MCMC
# ensemble of parameters and GCAM results for the ML parameter values, but nothing
# else (i.e., don't require GCAM results for other parameters). These consist of 
# stylized simulations as well as single and ensemble runs driven by GCAM ML price
# and income paths.

# To Do:
# Code for all scenarios needs to be updated to use new food demand functions.
# common_definitions.R needs to be updated to add desired scenarios to run for 
# this script.
# If/else statements below may be able to be simplified by mapping single generic
# food demand function call to the list of scenarios to be run.

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
  readRDS(file.path("data", "processed", procdata_dir, "inputs", 
                    "param_data_global_clean_sub.RDS"))
param_data_FE_clean_sub <- 
  readRDS(file.path("data", "processed", procdata_dir, "inputs", 
                    "param_data_FE_clean_sub.RDS"))

# ML parameter values; files read in are produced by running 02_parameter_uncertainty_intervals.R
params_ML_intervals_global <- 
  readRDS(file.path("data", "processed", procdata_dir, 
                    "params_ML_intervals_global.RDS"))
params_ML_intervals_FE <- 
  readRDS(file.path("data", "processed", procdata_dir, 
                    "params_ML_intervals_FE.RDS"))

# load GCAM results, as input assumptions for ambrosia (prices, income); file read
# in is produced by running 101_process_GCAM_results.R
gcamoutput_Ref_ML <-
  readRDS(file.path("data", "processed", procdata_dir, gcam_results_dir,
                    "gcamoutput_Ref_ML.RDS"))
# gcamoutput_Ref_ML_HP <- 
#   readRDS(file.path("data", "processed", procdata_dir, gcam_results_dir, 
#                     "gcamoutput_HP_ML.RDS"))

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
      output_dir = procdata_dir,
      scen = "Ref_ML",
      case = "ens_bc",       # file ext: bias corrected ensemble
      baseyr = 2015,
      Qs_min = Qs_floor,
      Qn_min = Qn_floor,
      save_result = TRUE
    )
    
  # single scenario driven by GCAM ML prices, income, parameters
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
    
    
  # GCAM scenario ensembles
    
  } else if(x == "Ref_ML_gcam_ens_bc") {
    
    # modifications for small test runs
    # param_data_global_clean_sub <- param_data_global_clean_sub[sample(nrow(param_data_global_clean_sub), 20),]
    # param_data_FE_clean_sub <-
    #   subset(param_data_FE_clean_sub,
    #          iteration %in% param_data_global_clean_sub$iteration)
    
    food.dmnd.ens_bc(
      globalparams = param_data_global_clean_sub,
      regparams = param_data_FE_clean_sub,
      inputdata = gcamoutput_Ref_ML,
      regionIDs = reg_list,
      output_dir = procdata_dir,
      scen = "Ref_ML_gcam",
      case = "ens_bc",       # file ext: bias corrected ensemble
      baseyr = 2015,
      Qs_min = Qs_floor,
      Qn_min = Qn_floor,
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
    #                        "Ref_ML_gcam/demand_ens_bc_20251202_170408/demand_R1_MLparams_ens_bc.RDS",
    #                        sep = "/")) %>%
    #   filter(year == 2015)
    # tmpR2 <- readRDS(paste("data/processed",procdata_dir,
    #                        "Ref_ML_gcam/demand_ens_bc_20251202_170408/demand_R2_ens_bc.RDS",
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
    
  } else if(x == "Ref_ML_HP_gcam_ens_bc") {
    
    food.dmnd.ens_bc(
      globalparams = param_data_global_clean_sub,
      regparams = param_data_FE_clean_sub,
      inputdata = gcamoutput_Ref_ML_HP,
      regionIDs = reg_list,
      output_dir = procdata_dir,
      scen = "Ref_ML_HP_gcam",
      case = "ens_bc",       # file ext: bias corrected ensemble
      baseyr = 2015,
      Qs_min = Qs_floor,
      Qn_min = Qn_floor,
      save_result = TRUE
    )
    
  # 
  } else if(x == "Ref_ML_gcam_MLprice_ens_bc") {
    
    # modifications for small test runs
#    param_data_global_clean_sub <- param_data_global_clean_sub[sample(nrow(param_data_global_clean_sub), 30),]
    param_data_global_clean_sub <-
      param_data_global_clean_sub[param_data_global_clean_sub$iteration == 69503 |
                                    param_data_global_clean_sub$iteration == 69506,]
    param_data_FE_clean_sub <-
      subset(param_data_FE_clean_sub,
             iteration %in% param_data_global_clean_sub$iteration)
    
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
      output_dir = procdata_dir,
      scen = "Ref_ML_gcam",
      case = "MLprice_ens_bc",       # file ext: bias corrected ensemble
      baseyr = 2015,
      Qs_min = Qs_floor,
      Qn_min = Qn_floor,
      save_result = TRUE
    )
    
    # tmpR1 <- 
    #   readRDS(file.path("data", "processed", procdata_dir,
    #                     "Ref_ML_gcam/MLprice_ens_bc_20260111_171553/demand_R1_MLprice_ens_bc.RDS"))

  } else if(x == "Ref_ML_gcam_MLincome_ens_bc") {
    
    # modifications for small test runs
    #    param_data_global_clean_sub <- param_data_global_clean_sub[sample(nrow(param_data_global_clean_sub), 30),]
    param_data_global_clean_sub <-
      param_data_global_clean_sub[param_data_global_clean_sub$iteration == 69503 |
                                    param_data_global_clean_sub$iteration == 69506,]
    param_data_FE_clean_sub <-
      subset(param_data_FE_clean_sub,
             iteration %in% param_data_global_clean_sub$iteration)
    
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
      output_dir = procdata_dir,
      scen = "Ref_ML_gcam",
      case = "MLincome_ens_bc",       # file ext: bias corrected ensemble
      baseyr = 2015,
      Qs_min = Qs_floor,
      Qn_min = Qn_floor,
      save_result = TRUE
    )
    
    # tmpR1 <- 
    #   readRDS(file.path("data", "processed", procdata_dir,
    #                     "Ref_ML_gcam/MLprice_ens_bc_20260111_171553/demand_R1_MLprice_ens_bc.RDS"))

  } else if(x == "Ref_ML_gcam_MLscale_ens_bc") {
    
    # modifications for small test runs
    #    param_data_global_clean_sub <- param_data_global_clean_sub[sample(nrow(param_data_global_clean_sub), 30),]
    param_data_global_clean_sub <-
      param_data_global_clean_sub[param_data_global_clean_sub$iteration == 69503 |
                                    param_data_global_clean_sub$iteration == 69506,]
    param_data_FE_clean_sub <-
      subset(param_data_FE_clean_sub,
             iteration %in% param_data_global_clean_sub$iteration)
    
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
      output_dir = procdata_dir,
      scen = "Ref_ML_gcam",
      case = "MLscale_ens_bc",       # file ext: bias corrected ensemble
      baseyr = 2015,
      Qs_min = Qs_floor,
      Qn_min = Qn_floor,
      save_result = TRUE
    )
    
    # tmpR1 <- 
    #   readRDS(file.path("data", "processed", procdata_dir,
    #                     "Ref_ML_gcam/MLprice_ens_bc_20260111_171553/demand_R1_MLprice_ens_bc.RDS"))
  }
  
} # end function(x)

) %>% invisible # end lapply()

