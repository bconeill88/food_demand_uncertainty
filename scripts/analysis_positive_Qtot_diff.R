

# load functions and common definitions
source("R/common_definitions.R")
source("R/init_packages.R")

# install or load packages
ensure_package(tidyverse)

# get food demand ensemble results for one example region
abs_result <- readRDS(file.path("data", "processed", procdata_dir,
                                 procdata_subdir_RefMLgcam,
                                 "abs",
                                 "demand_R5_ens_bc.RDS"))
diff_result <- readRDS(file.path("data", "processed", procdata_dir,
                                 procdata_subdir_RefMLgcam,
                                 "diffs_Ref_ML_HP_gcam",
                                 "demand_R5_diffs_ens_bc.RDS"))

# select those with an increase in Qtot in response to price increases, join with
# absolute projection results, and keep elasticities
positive_Qtot_diff <- diff_result %>%
  filter(`gcam-consumer` == "FoodDemand_Group1",
         year == 2100,
         Qtot > 0) %>%
  left_join(abs_result, 
            by = c("GCAM_region_ID", "region", "year", "gcam-consumer", "iteration"),
            suffix = c(".diff", ".abs")) %>%
  select("GCAM_region_ID", "region", "year", "gcam-consumer", "iteration", 
         "Qtot.diff", "elast.ss.abs", "elast.nn.abs", "elast.sn.abs", "elast.ns.abs")

# number of simulations - 2421, so relatively common
n_positive_Qtot_diff <- positive_Qtot_diff %>%
  pull("iteration") %>%
  unique() %>%
  length()

# select those with an increase in both Qs and Qn in response to price increases, 
# join with absolute projection results, and keep elasticities
positive_Qs_Qn_diff <- diff_result %>%
  filter(`gcam-consumer` == "FoodDemand_Group1",
         year == 2100,
         Qn > 0,
         Qs > 0) %>%
  left_join(abs_result, 
            by = c("GCAM_region_ID", "region", "year", "gcam-consumer", "iteration"),
            suffix = c(".diff", ".abs")) %>%
  select("GCAM_region_ID", "region", "year", "gcam-consumer", "iteration", 
         "Qtot.diff", "elast.ss.abs", "elast.nn.abs", "elast.sn.abs", "elast.ns.abs")

# number of simulations - 8, so very uncommon!
n_positive_Qs_Qn_diff <- positive_Qs_Qn_diff %>%
  pull("iteration") %>%
  unique() %>%
  length()

df_long <- positive_Qtot_diff %>%
  select(elast.ss.abs, elast.nn.abs, elast.sn.abs, elast.ns.abs) %>%
  
  # reshape to long format
  pivot_longer(
    cols = everything(),
    names_to = "elasticity_type",
    values_to = "value"
  )

ggplot(df_long, aes(x = value, color = elasticity_type)) +
  geom_density(linewidth = 1) +
  theme_minimal() +
  labs(
    x = "Elasticity",
    y = "Density",
    color = "Elasticity"
  )