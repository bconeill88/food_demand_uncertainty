# calculate and save demand and elasticities from observations, and plot model
# vs observations

# load functions
source("R/common_definitions.R")
source("R/demand_functions.R")
source("R/plot_functions_model_vs_obs.R")
source("R/model_fit_functions.R")
source("R/install_ambrosia_function.R")
source("R/init_packages.R")

# install or load packages as needed
ensure_package(tidyverse)
ensure_package(patchwork)
ensure_package(grid)
ensure_package(ggplot2)
install_ambrosia_once(force_install = FALSE)

# choose whether to recalculate model vs obs demand
RECALC <- TRUE
SAVE_CALC <- TRUE

# choose whether to create and save plots and tables
PLOTS <- FALSE
TABLES <- TRUE

# define directory for pdf report
report_dir <- file.path("output", "reports", procdata_dir, procdata_subdir_RefMLgcam)
output_dir_tables <- file.path("output", "tables")
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir_tables, recursive = TRUE, showWarnings = FALSE)

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
 
  # 2017 (Edmonds et al)
  load(file.path("data", "processed", procdata_dir, param_intervals_dir,
                 "params_2017.Rdata")) 
  params_global_ML_2017 <- params_2017 %>%
    filter(row.names(.) == "ML") %>%
    mutate(case = "ML 2017", iteration = 2017)
  
  # 2021 (Narayan and Walfhoff)
  load(file.path("data", "processed", procdata_dir, param_intervals_dir,
                 "params_2021.Rdata")) 
  params_global_ML_2021 <- params_2021 %>%
    filter(row.names(.) == "ML") %>%
    mutate(case = "ML 2021", iteration = 2021)
  
  params_global <- bind_rows(params_global_ML, params_global_other, 
                             params_global_ML_2017, params_global_ML_2021)
  
  # get regional parameter sets
  params_FE_ML <- readRDS(file.path("data", "processed", procdata_dir, param_intervals_dir,
                                    "params_ML_intervals_FE.RDS")) %>%
    filter(measure == "ML") %>% 
    rename(case = measure)
  params_FE_other <-  read.csv(file.path("data", "processed", procdata_dir, 
                                         procdata_subdir_RefMLgcam, demand_both_subdir,
                                         "params_FE_BOTH_Qtot_world.csv"))
  params_FE_2017 <- params_FE_ML %>% 
    mutate(staples_FE = 0, iteration = 2017)
  params_FE_2021 <- params_FE_ML %>% 
    mutate(staples_FE = 0, iteration = 2021)
  params_FE <- bind_rows(params_FE_ML, params_FE_other,
                         params_FE_2017, params_FE_2021)
  
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

if(PLOTS) {
  
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
    model_color = "#e6550d"
  )
}

if (TABLES) {
  
  # Check data quality first (all three)
  check_model_vs_obs_inputs(demand_model_vs_obs)
  
  # All three demand types
  metrics_obs <- calc_model_vs_obs_metrics(
    demand_model_vs_obs = demand_model_vs_obs,
    group_cols = c("case"),
    output_dir_tables = output_dir_tables
  )
}

head(metrics_obs)
