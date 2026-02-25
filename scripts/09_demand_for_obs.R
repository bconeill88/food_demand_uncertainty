# calculate and save demand and elasticities from observations -----------------
# ADD CALC OF OBS FROM HD/LD PARAMS --------------------------------------------
# ----


# TO DO:
# source functions
# redo path names to be consistent with project structure
# change file format to rds for saved files
# define region list

# load functions
source("R/common_definitions.R")
source("R/demand_functions.R")
source("R/plot_functions_model_vs_obs.R")
source("R/install_ambrosia_function.R")
source("R/init_packages.R")

# install or load packages as needed
ensure_package(tidyverse)
ensure_package(patchwork)
ensure_package(grid)
ensure_package(ggplot2)
install_ambrosia_once(force_install = FALSE)

# choose whether to recalculate model vs obs demand
RECALC <- FALSE
SAVE_CALC <- TRUE

# define directory for pdf report
report_dir <- file.path("output", "reports", procdata_dir, procdata_subdir_RefMLgcam)
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

# define output directory
output_dir <- file.path("data", "processed", procdata_dir, 
                        procdata_subdir_RefMLgcam, demand_both_subdir, 
                        "model_vs_obs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!RECALC) {
  
  demand_model_vs_obs <-  readRDS(file.path(output_dir, "demand_model_vs_obs.RDS"))
  
} else {
  
  # get observational data
  obs_data <- readRDS(file.path("data/processed", procdata_dir, clean_data_dir,
                                "obs_data.RDS")) %>%
    rename(Y.region = Y)
  
  # get global parameter sets
  params_global_ML <- readRDS(file.path("data", "processed", procdata_dir, param_intervals_dir,
                                        "params_ML_intervals_global.RDS")) %>%
    filter(measure == "ML") %>% 
    rename(case = measure)
  params_global_other <-  read.csv(file.path("data", "processed", procdata_dir, 
                                             procdata_subdir_RefMLgcam, demand_both_subdir,
                                             "params_global_BOTH_Qtot_world.csv"))
  params_global <- bind_rows(params_global_ML, params_global_other)
  
  # get regional parameter sets
  params_FE_ML <- readRDS(file.path("data", "processed", procdata_dir, param_intervals_dir,
                                    "params_ML_intervals_FE.RDS")) %>%
    filter(measure == "ML") %>% 
    rename(case = measure)
  params_FE_other <-  read.csv(file.path("data", "processed", procdata_dir, 
                                         procdata_subdir_RefMLgcam, demand_both_subdir,
                                         "params_FE_BOTH_Qtot_world.csv"))
  params_FE <- bind_rows(params_FE_ML, params_FE_other)
  
  # define regions to run over
  reg_list <- obs_data$GCAM_region_ID %>% unique() %>% sort()
  
  # calculate historical demand for each parameter set
  
  # zero bias terms to use in demand calculations
  bias_terms <- params_FE %>% select(GCAM_region_ID, iteration) %>%
    mutate(RBs = 0, RBn = 0)
  
  # calculate demand
  demand <- food.dmnd.wrapper(
    globalparams = params_global,
    regparams = params_FE,
    biasdata = bias_terms,
    Qs_min = 0,
    Qn_min = 0,
    inputdata = obs_data %>% select(GCAM_region_ID, year, Y.region, Ps, Pn),
    regions = reg_list,
    output_dir = output_dir,
    scen = "obs",
    case = "", 
    progress = FALSE,
    save_result = FALSE
  )
  
  # create model vs obs data
  demand_obs <- obs_data %>% 
    select(GCAM_region_ID, year, Qs, Qn) %>%
    rename(Qs.obs = Qs, Qn.obs = Qn) %>%
    mutate(Qtot.obs = Qs.obs + Qn.obs)
  demand_model <- demand %>%
    select(-c(Y, Qs, Qn, Qm, Qtot, RBs, RBn, rgn))
  demand_model_vs_obs <- left_join(demand_obs, demand_model, 
                                   by = c("GCAM_region_ID", "year")) %>%
    left_join(select(params_global, iteration, case), by = "iteration", )
  
  # save if desired
  if(SAVE_CALC) {
    saveRDS(demand_model_vs_obs, file.path(output_dir, "demand_model_vs_obs.RDS"))
  }
}

# create PDF of scatterplots of model vs obs demand for each case a food type
plot_model_vs_obs_scatter_rows_pdf(
  demand_model_vs_obs,
  output_dir = report_dir,
  filename = "model_vs_obs_scatter_demand.pdf",
  case_col = "case"
)

# create PDF of scatterplots of model vs obs demand for each case a food type
plot_income_vs_demand_rows_pdf(
  demand_model_vs_obs,
  output_dir = report_dir,
  filename   = "model_vs_obs_income_vs_demand.pdf",
  rows_per_page = 3,
  income_col = "Y.region",
  # if you already have a set-1 orange constant in your plotting code, pass it here:
  model_color = "#E69F00"
)
