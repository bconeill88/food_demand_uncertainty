# Plot comparison of demand for the HD/LD scenarios (regional and global parameters)
# to a sample of the demand ensemble, both for regional demand and demand by decile,
# and optionally impose minimum demand constraint

# Load functions
source("R/init_packages.R")
source("R/demand_with_intervals_plot_functions.R")
source("R/min_calorie_adjustment.R")

# Make sure packages are installed/loaded
ensure_package(tidyverse)
ensure_package(ggforce)

# Define scenario for ambrosia results
scen_amb <- "Ref_ML_gcam"

# Define data directories
procdata_dir <- "update9_cnstrlam_agg32FE_24jan25"
procdata_subdir_ens_bc <- "demand_ens_bc_12jul25"
procdata_subdir_MLscen <- "MLparams_bc_20250711_163345"
procdata_subdir_HDscen <- "HDparams_bc_20250711_163827"
procdata_subdir_LDscen <- "LDparams_bc_20250711_164000"
output_dir <- file.path("output", "reports", procdata_dir, scen_amb, procdata_subdir_ens_bc)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Regions and deciles to plot
region_list <- c(1:29, 31, 32)  # Update to full list later
target_deciles <- c("FoodDemand_Group1", "FoodDemand_Group2", "FoodDemand_Group3",
                    "FoodDemand_Group6", "FoodDemand_Group10")

# Load region mapping and max iteration frequency tables
load("./data/raw/GCAM_region_ID_mapping.Rdata")
# max_iters_all <- read.csv(file.path("data", "processed", procdata_dir, scen_amb, "iter_table_HDLD_Qtot.csv")) %>%
#   mutate(GCAM_region_ID = as.integer(ID))
# global_iter_row <- max_iters_all %>% filter(GCAM_region_ID == 33)

# Load single scenario demand for all regions, for GCAM and ambrosia
# Ref_ML_gcam and Ref_ML_MLparams ambrosia
demand_MLscen <- readRDS(file.path("data", "processed", procdata_dir, "Ref_ML_gcam",
                                   procdata_subdir_MLscen, "gcam_amb_output_Ref_ML_MLparams.RDS"))
# Ref_HD_gcam and Ref_ML_HDparams ambrosia
demand_HDscen <- readRDS(file.path("data", "processed", procdata_dir, "Ref_ML_gcam",
                                   procdata_subdir_HDscen, "gcam_amb_output_Ref_ML_HDparams.RDS"))
# Ref_LD_gcam and Ref_ML_LDparams ambrosia
demand_LDscen <- readRDS(file.path("data", "processed", procdata_dir, "Ref_ML_gcam",
                                   procdata_subdir_LDscen, "gcam_amb_output_Ref_ML_LDparams.RDS"))

if(FALSE) {
  
# Plot single region example, regional total results
example_region <- 3
demand_path_ens_bc <- file.path("data", "processed", procdata_dir, scen_amb, procdata_subdir_ens_bc, 
                         paste0("demand_R", example_region, "_ens_bc.RDS"))
demand_reg_ens_bc <- readRDS(demand_path_ens_bc)
# freq_table <- max_iters_all %>% filter(GCAM_region_ID == example_region)

p_single <- plot_regional_demand_comparison(
  demand_reg = demand_reg_ens_bc,
  reg_num = example_region,
  sample_n = 100,
  ensemble_name = "Emulator",
  include_comparison_scenarios = FALSE
)
ggsave(file.path(output_dir, paste0("demand_R", example_region, "_ens_bc.png")),
       plot = p_single, width = 11, height = 4, units = "in", bg = "white")

p_single_GCAMcompare <- plot_regional_demand_comparison(
  demand_reg = demand_reg_ens_bc,
  reg_num = example_region,
  sample_n = 100,
  ensemble_name = "Emulator",
  scen_solid = demand_MLscen, scen_solid_name = "GCAM ML",
  scen_dashed = demand_HDscen, scen_dashed_name = "GCAM HD",
  scen_dotted = demand_LDscen, scen_dotted_name = "GCAM LD",
  scen_model = "gcam",               # options: "gcam", "amb"
  include_comparison_scenarios = TRUE
)
ggsave(file.path(output_dir, paste0("demand_R", example_region, "_ens_bc_GCAMcompare.png")),
       plot = p_single_GCAMcompare, width = 11, height = 4, units = "in", bg = "white")

}

# Plot all regions, regional total results
p_all_regional <- map_dfr(region_list, function(region_id) {
  demand_path_ens_bc <- file.path("data", "processed", procdata_dir, scen_amb, procdata_subdir_ens_bc,
                                  paste0("demand_R", region_id, "_ens_bc.RDS"))
  demand_reg_ens_bc <- readRDS(demand_path_ens_bc)
#  freq_table <- max_iters_all %>% filter(GCAM_region_ID == region_id)

  plot_regional_demand_comparison(
    demand_reg = demand_reg_ens_bc,
    reg_num = region_id,
    sample_n = 100,
    ensemble_name = "Emulator",
    scen_solid = demand_MLscen, scen_solid_name = "GCAM ML",
    scen_dashed = demand_HDscen, scen_dashed_name = "GCAM HD",
    scen_dotted = demand_LDscen, scen_dotted_name = "GCAM LD",
    scen_model = "gcam",               # options: "gcam", "amb"
    return_data = TRUE
  )
})
plot_regional_demand_comparison_pdf(p_all_regional, region_mapping = GCAM_region_ID_mapping, output_dir = output_dir)

if(FALSE) {
  
# Plot all regions, decile results
p_all_decile <- map_dfr(region_list, function(region_id) {
  demand_path <- file.path("data", "processed", procdata_dir, scen_amb, paste0("demand_R", region_id, "_ens_bc.RDS"))
  demand_reg <- readRDS(demand_path)

  freq_table <- max_iters_all %>% filter(GCAM_region_ID == region_id)

  map_dfr(target_deciles, function(decile_group) {
    plot_decile_demand_comparison(
      freq_table = freq_table,
      global_iter_row = global_iter_row,
      demand_reg = demand_reg,
      region = region_id,
      consumer_group = decile_group,
      sample_n = 100,
      return_data = TRUE
    ) %>%
      mutate(consumer_group = decile_group)
  }) %>%
    mutate(GCAM_region_ID = region_id)
})
plot_decile_demand_comparison_pdf(p_all_decile, 
                                  region_mapping = GCAM_region_ID_mapping, 
                                  output_dir = output_dir,
                                  filename = "demand_by_decile_adjusted.pdf")

}
