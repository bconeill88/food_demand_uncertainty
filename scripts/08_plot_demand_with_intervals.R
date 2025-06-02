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

# Define scenario
scen <- "Ref_ML_gcam"

# Define directories
procdata_dir <- "update9_cnstrlam_agg32FE_24jan25"
output_dir <- file.path("output/figures", procdata_dir, scen)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Regions and deciles to plot
region_list <- c(1:29, 31, 32)  # Update to full list later
target_deciles <- c("FoodDemand_Group1", "FoodDemand_Group2", "FoodDemand_Group3",
                    "FoodDemand_Group6", "FoodDemand_Group10")

# Set flag for applying minimum demand thresholds
apply_demand_threhold <- TRUE

# Load region mapping and max iteration frequency tables
load("./data/raw/GCAM_region_ID_mapping.Rdata")
max_iters_all <- read.csv(file.path("data", "processed", procdata_dir, scen, "iter_table_HDLD_Qtot.csv")) %>%
  mutate(GCAM_region_ID = as.integer(ID))
global_iter_row <- max_iters_all %>% filter(GCAM_region_ID == 33)

# Plot single region example, regional total results
example_region <- region_list[1]
demand_path <- file.path("data", "processed", procdata_dir, scen, paste0("demand_R", example_region, "_ens_bc.RDS"))
demand_reg <- readRDS(demand_path)
freq_table <- max_iters_all %>% filter(GCAM_region_ID == example_region)

p_single <- plot_regional_demand_comparison(
  freq_table = freq_table,
  global_iter_row = global_iter_row,
  demand_reg = demand_reg,
  region = example_region,
  sample_n = 100
)
ggsave(file.path(output_dir, paste0("demand_R", example_region, "_ens_bc.png")),
       plot = p_single, width = 11, height = 4, units = "in", bg = "white")

# Plot all regions, regional total results
p_all_regional <- map_dfr(region_list, function(region_id) {
  demand_path <- file.path("data", "processed", procdata_dir, scen, paste0("demand_R", region_id, "_ens_bc.RDS"))
  demand_reg <- readRDS(demand_path)
  freq_table <- max_iters_all %>% filter(GCAM_region_ID == region_id)

  plot_regional_demand_comparison(
    freq_table = freq_table,
    global_iter_row = global_iter_row,
    demand_reg = demand_reg,
    region = region_id,
    sample_n = 100,
    return_data = TRUE
  )
})
plot_regional_demand_comparison_pdf(p_all_regional, region_mapping = GCAM_region_ID_mapping, output_dir = output_dir)

# Plot all regions, decile results
p_all_decile <- map_dfr(region_list, function(region_id) {
  demand_path <- file.path("data", "processed", procdata_dir, scen, paste0("demand_R", region_id, "_ens_bc.RDS"))
  demand_reg <- readRDS(demand_path)
  if(apply_demand_threhold) demand_reg <- apply_min_demand(demand_reg, 0.6, 0.01, 1.1)
  
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
