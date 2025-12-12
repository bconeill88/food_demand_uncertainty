# Identify and save parameter sets that correspond to intervals in demand outcomes
# representing uncertainty space (e.g., representing the top 5% of demand outcomes,
# or bottom 5%, etc.)

# load functions
source("R/common_definitions.R")
source("R/parameters_for_intervals_functions.R")
source("R/init_packages.R")

# install or load packages as needed
ensure_package(tidyverse)

# define regions to run over
# get command line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  reg_list <- c(as.numeric(args[1]))
} else {
  reg_list <- c(1:29,31,32) # skip Taiwan (30), no post-2015 output from GCAM
}

# load region name/number mapping
load(file.path("data", "raw", "GCAM_region_ID_mapping.Rdata"))

# path to location for saving results
results_path <- file.path("data", "processed", procdata_dir, 
                          procdata_subdir_RefMLgcam, demand_diffs_subdir)

message("Identifying parameters for HD/LD parameters")

# calculate parameters by taking iteration that falls most frequently in high/low intervals
# lapply(scen_list_demand,function(x)
make_HD_LD_params_freq(
  year_vals = year_iter, 
  data_dir = procdata_dir, 
  out_dir = results_path, 
  scen = "Ref_ML_gcam",
  scen_case = "_diffs_ens_bc",       # include "diffs" if it's intervals for demand differences
  regions = reg_list, 
  hi_interval_min = hi_min,
  hi_interval_max = hi_max,
  lo_interval_min = lo_min,
  lo_interval_max = lo_max,
  save_outputs = TRUE
)

message("Finished identifying parameters for HD/LD intervals")

# create and save tables of frequencies and iteration numbers, as results

# load iteration frequency results
max_freqs <- readRDS(file.path(results_path, 
                               paste0("max_iter_frequencies", scen_case, ".RDS")))

# create table of frequencies of max frequency iterations for each case
max_freq_table <- max_freqs %>%
  select(GCAM_region_ID, case, freq) %>%
  pivot_wider(names_from = case, values_from = freq) %>%
  left_join(GCAM_region_ID_mapping, by = "GCAM_region_ID") %>% # add region names
  relocate(region, .before = GCAM_region_ID) %>%
  arrange(GCAM_region_ID) %>%
  rename(ID = GCAM_region_ID)

# create table of iteration numbers of max frequency iterations for each case
iter_table <- max_freqs %>%
  select(GCAM_region_ID, case, max_iter) %>%
  pivot_wider(names_from = case, values_from = max_iter) %>%
  left_join(GCAM_region_ID_mapping, by = "GCAM_region_ID") %>%
  relocate(region, .before = GCAM_region_ID) %>%
  arrange(GCAM_region_ID) %>%
  rename(ID = GCAM_region_ID)

# save images of tables if in interactive setting, RDS files if on PIC
if (interactive()) {
  ensure_package(gt)
  ensure_package(webshot2)
  
  gtsave(gt(max_freq_table) %>% tab_header(title = "Maximum Frequencies"), 
         file.path(results_path, "table_max_frequencies_all_regions.png"))
  
  gtsave(gt(iter_table) %>% tab_header(title = "Iteration Numbers of Maximum Frequency"), 
         file.path(results_path, "table_max_iterations_all_regions.png"))
} else {
  saveRDS(max_freq_table, file.path(results_path, 
                                    "table_max_frequencies_all_regions.RDS"))
  saveRDS(iter_table, file.path(results_path, 
                                "table_max_iterations_all_regions.RDS"))
}

# create and save parameters corresponding to max frequency iterations for use in GCAM

# save iteration table
write.csv(
  iter_table, 
  file.path(results_path, "iter_table_HDLD_Qtot.csv"), row.names = FALSE)

# load parameter data
param_data_global_clean_sub <- readRDS(file.path("data", "processed", procdata_dir, "inputs", 
               "param_data_global_clean_sub.RDS"))
param_data_FE_clean_sub <- readRDS(file.path("data", "processed", procdata_dir, "inputs", 
               "param_data_FE_clean_sub.RDS"))

# get and save global parameters for max frequency iterations
params_global_HD_Qtot <- param_data_global_clean_sub %>%
  filter(iteration %in% unique(iter_table$HD_Qtot)) %>%
  mutate(case = "HD_Qtot")
params_global_LD_Qtot <- param_data_global_clean_sub %>%
  filter(iteration %in% unique(iter_table$LD_Qtot)) %>%
  mutate(case = "LD_Qtot")
params_global_Qtot <- bind_rows(params_global_HD_Qtot, params_global_LD_Qtot)
write.csv(
  params_global_Qtot, 
  file.path(results_path, "params_global_HDLD_Qtot.csv"), row.names = FALSE)

# get and save FE parameters for max frequency iterations
params_FE_HD_Qtot <- param_data_FE_clean_sub %>%
  filter(iteration %in% unique(iter_table$HD_Qtot)) %>%
  mutate(case = "HD_Qtot")
params_FE_LD_Qtot <- param_data_FE_clean_sub %>%
  filter(iteration %in% unique(iter_table$LD_Qtot)) %>%
  mutate(case = "LD_Qtot")
params_FE_Qtot <- bind_rows(params_FE_HD_Qtot, params_FE_LD_Qtot)
write.csv(
  params_FE_Qtot, 
  file.path(results_path, "params_FE_HDLD_Qtot.csv"), row.names = FALSE)

# load files for manual inspection and writing results for Kanishka to use in GCAM

# manually inspect this file to confirm that Qtot iterations are the best ones to 
# use for HD, LD parameters
max_freq_table <- readRDS(file.path(results_path, "table_max_frequencies_all_regions.RDS"))
max_freq_table_jan25 <- 
  readRDS("H:/My Drive/R projects/food_demand/food_demand_uncertainty/data/processed/update9_cnstrlam_agg32FE_24jan25/Ref_ML_gcam/ens_bc_20251009_221027/table_max_frequencies_all_regions.RDS")

# identify iteration numbers that correspond to desired HD and LD parameter sets; 
# region ID 33 represents iteration that does the best across all regions rather 
# than for a single region
iter_table <- read_csv(file.path(results_path,"iter_table_HDLD_Qtot.csv"),
                           show_col_types = FALSE)
iter_HDLD_Qtot_world <- iter_table %>% filter(ID == 33) %>% select(HD_Qtot, LD_Qtot)

# load global and FE parameters, then extract HD and LD parameter sets
params_global_Qtot <- read_csv(file.path(results_path,"params_global_HDLD_Qtot.csv"),
                               show_col_types = FALSE)
params_FE_Qtot <- read_csv(file.path(results_path,"params_FE_HDLD_Qtot.csv"),
                           show_col_types = FALSE)
params_global_Qtot_world <- params_global_Qtot %>% 
  filter(iteration %in% iter_HDLD_Qtot_world[1,])
params_FE_Qtot_world <- params_FE_Qtot %>% 
  filter(iteration %in% iter_HDLD_Qtot_world[1,])

# write these parameter sets to files for Kanishka
write.csv(
  params_global_Qtot_world, 
  file.path(results_path, "params_global_HDLD_Qtot_world.csv"), row.names = FALSE)
write.csv(
  params_FE_Qtot_world, 
  file.path(results_path, "params_FE_HDLD_Qtot_world.csv"), row.names = FALSE)