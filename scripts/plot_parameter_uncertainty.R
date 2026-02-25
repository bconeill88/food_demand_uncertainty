# load functions and common definitions
source("R/common_definitions.R")
source("R/plot_functions_parameter_uncertainty.R")
source("R/init_packages.R")

# install or load packages
ensure_package(tidyverse)

# load parameter sub-samples
param_data_global_clean_sub <- 
  readRDS(file.path("data", "processed", procdata_dir, clean_data_dir, 
                    "param_data_global_clean.RDS")) 

param_data_FE_clean_sub <- 
  readRDS(file.path("data", "processed", procdata_dir, clean_data_dir, 
                    "param_data_FE_clean.RDS")) 

# load specific parameter sets
params_ML_intervals_global <- 
  readRDS(file.path("data", "processed", procdata_dir, param_intervals_dir, 
                    "params_ML_intervals_global.RDS"))

params_ML_intervals_FE <- 
  readRDS(file.path("data", "processed", procdata_dir, param_intervals_dir, 
                    "params_ML_intervals_FE.RDS"))

#------------------------------------------------------------------------------
# 1. Create trace plots
#------------------------------------------------------------------------------

p_trace_global <- 
  make_trace_plots_global(param_data_global_clean_sub,
                          title = "Global paramter trace plots",
                          facet_ncol = 3)
# create a list of two plots for FEs, 16 regions each
p_trace_FE <- 
  make_trace_plots_FE(param_data_FE_clean_sub,
                      title = "Regional FE parameter trace plots",
                      regions_per_page = 16,
                      facet_ncol = 4)

#------------------------------------------------------------------------------
# 2. Create marginal density plots
#------------------------------------------------------------------------------

# Global parameters
param_cols_global <- c("As","ks","eps1n","xi.ss","xi.nn",
                       "xi.cross","lambda","An","Pm")

ml_vals_global <- params_ML_intervals_global %>%
  filter(measure == "ML") %>%
  select(all_of(param_cols_global))

p_dens_global <- make_density_plots_global(
  paramdata    = param_data_global_clean_sub,
  param_cols   = param_cols_global,
  vline_values = list(ml_vals_global),
  vline_labels = list("ML"),
  vline_colors   = c("ML" = "red"),
  title        = "Global parameter posteriors",
  subtitle     = "",
  line_width   = 0.7,
  font_size    = 8
)

plot(p_dens_global)

# Regional fixed-effect parameters (staples_FE); create a list of two plots, 16
# regions each
ml_vals_fe <- params_ML_intervals_FE %>%
  filter(measure == "ML") %>%
  select(region, staples_FE)

param_data_FE_clean_sub <- param_data_FE_clean_sub %>%
  mutate(region = recode(region, 
                         "Central America and Caribbean" = "C. Am. & Carib.", 
                         "European Free Trade Association" = "EFTA",
                         "South America_Northern" = "S. America_Northern",
                         "South America_Southern" = "S. America_Southern"))

p_dens_FE <- make_density_plots_FE(
  paramdata      = param_data_FE_clean_sub,
  value_col      = "staples_FE",
  vline_values   = list(ml_vals_fe),
  vline_labels   = list("ML"),
  vline_colors   = c("ML" = "red"),
  title          = "FE parameter posteriors",
  subtitle       = "",
  line_width   = 0.7,
  font_size    = 8,
  regions_per_page = 16,
  facet_ncol     = 4
)

plot(p_dens_FE[[1]])

#------------------------------------------------------------------------------
# 3. Create regional FE value plots (across uncertainty cases)
#------------------------------------------------------------------------------

p_FE_vals <- make_FE_values_plot(
  paramdata  = params_ML_intervals_FE,
  title     = "Regional FE parameter values",
  subtitle  = "FE values across independent or joint parameter uncertainty cases",
  facet_ncol = 3
)

#------------------------------------------------------------------------------
# 4. Write everything to a single multi-page PDF
#------------------------------------------------------------------------------

# Ensure output directory exists
out_dir <- file.path("output", "reports", procdata_dir, procdata_subdir_RefMLgcam)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

pdf_file <- file.path(out_dir, "parameter_uncertainty_plots.pdf")

pdf(pdf_file, width = 8, height = 6)

## Page 1 - global parameter traces
print(p_trace_global)

## Pages 2, 3 - FE parameter traces
if (length(p_trace_FE) >= 1) print(p_trace_FE[[1]])
if (length(p_trace_FE) >= 2) print(p_trace_FE[[2]])

## Page 4 - global parameter densities
print(p_dens_global)

## Pages 5, 6 - FE parameter densities
if (length(p_dens_FE) >= 1) print(p_dens_FE[[1]])
if (length(p_dens_FE) >= 2) print(p_dens_FE[[2]])

## Page 7 - FE parameter values
print(p_FE_vals)

dev.off()
