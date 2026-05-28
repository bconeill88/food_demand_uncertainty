library("tidyverse")
source("R/common_definitions.R")

param_table <- readRDS("H:/My Drive/R projects/food_demand/food_demand_uncertainty/data/processed/mcmc_23nov25/Ref_ML_gcam/by2021/ens_bc_combined_20260131_085035/both/max_iter_frequencies_BOTH.RDS")

table_HDHPR <- param_table %>% filter(case == "HD_HPR_Qtot")
table_HDLPR <- param_table %>% filter(case == "HD_LPR_Qtot")
table_LDHPR <- param_table %>% filter(case == "LD_HPR_Qtot")
table_LDLPR <- param_table %>% filter(case == "LD_LPR_Qtot")

df_list <- list(table_HDHPR, table_HDLPR, table_LDHPR, table_LDLPR)

# 2. Use reduce with a join function
# 'full_join' keeps all rows; 'inner_join' keeps only matching rows
final_df <- df_list %>% 
  reduce(full_join, by = "GCAM_region_ID")

write.csv(final_df, file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam,
                              demand_both_subdir, "max_iter_frequencies_BOTH.csv"))
