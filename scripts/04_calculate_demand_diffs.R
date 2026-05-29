# Calculate and save differences in demand and elasticities between two sets of
# demand results, for either ensembles or single scenarios.

# load functions and common definitions
source("R/common_definitions.R")
source("R/init_packages.R")
source("R/demand_difference_functions.R")

# install or load packages
ensure_package(tidyverse)

# define scenarios (this could go in common_definitions.R)
scen_base <- "Ref_ML_gcam"      # scenario to subtract (base scenario)
scen_base_case <- "_ens_bc"
scen_base_path <- file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam_income, demand_abs_subdir)
scen_var <- "Ref_ML_HP_gcam"    # scenario to subtract from (variant scenario)
scen_var_case <- "_ens_bc"
scen_var_path <- file.path("data", "processed", procdata_dir, procdata_subdir_RefMLHPgcam_income, demand_abs_subdir)

# where to save results
output_path <- file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam_income, demand_diffs_subdir)

# create directory for difference results if necessary
dir.create(file.path(scen_base_path, paste0("diffs_", scen_var)), 
           showWarnings = FALSE, recursive = TRUE)

# regions to take differences over
reg_list <- seq(1:32)

# calculate difference in demand and elasticities between two different demand
# scenarios, for regions defined by number in reg_list
subtract_demand_dfs(scen_var, scen_var_case, scen_var_path,
                    scen_base, scen_base_case, scen_base_path, 
                    output_path, reg_list)
