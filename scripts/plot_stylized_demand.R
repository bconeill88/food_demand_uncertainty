# Calculate demand and elasticities for ML parameters from this study and from
# Edmonds et al (2017) and Narayan and Waldhoff (2021) and produce stylized comparison
# plots over a range of income and for typical (fixed) prices.

# load functions
source("R/common_definitions.R")
source("R/demand_functions.R")
source("R/plot_functions_stylized_demand.R")
source("R/install_ambrosia_function.R")
source("R/init_packages.R")

# install or load packages as needed
ensure_package(tidyverse)
ensure_package(ggplot2)
ensure_package(geomtextpath)
ensure_package(gridExtra)
install_ambrosia_once(force_install = FALSE)

# output directory for plots
out_dir <- file.path("output", "reports", procdata_dir, procdata_subdir_RefMLgcam)

# define global ML parameter values for this analysis and for 2017, 2021

# this analysis (2026)
params_ML_global_2026 <- 
  readRDS(file.path("data", "processed", procdata_dir, param_intervals_dir,
                    "params_ML_intervals_global.RDS")) %>% 
  filter(measure == "ML") %>%
  select(As:pnscl)
param_structure_2026 <- vec2param(as.vector(t(params_ML_global_2026)))

# 2017 (Edmonds et al)
load(file.path("data", "processed", procdata_dir, param_intervals_dir,
               "params_2017.Rdata")) 
params_2017 <- params_2017 %>%
  filter(row.names(.) == "ML") %>%
  select(As:pnscl)
param_structure_2017 <- vec2param(as.vector(t(params_2017)))

# 2021 (Narayan and Walfhoff)
load(file.path("data", "processed", procdata_dir, param_intervals_dir,
               "params_2021.Rdata")) 
params_2021 <- params_2021 %>%
  filter(row.names(.) == "ML") %>%
  select(As:pnscl)
param_structure_2021 <- vec2param(as.vector(t(params_2021)))

# define stylized income and price scenario

# typical base year prices for GCAM scenarios
Ps_typ <- 0.05
Pn_typ <- 0.25

# create stylized input data
inputdata <- data.frame(
  Y = c(seq(0.5, 10, 0.5), seq(12, 40, 2), seq(50, 100, 10)),
  Ps = Ps_typ,
  Pn = Pn_typ)

# calculate demand and elasticities

demand_2026 <- 
  food.dmnd(inputdata$Ps, inputdata$Pn, inputdata$Y, params = param_structure_2026) %>%
  # add elasticities
  bind_cols(calc_income_elast(inputdata$Y, param_structure_2026)) %>%
  # this must be done separately so that income elasticities are available
  bind_cols(calc_price_elast(., param_structure_2026)) %>%
  # add input data
  bind_cols(inputdata)
demand_2017 <- 
  food.dmnd(inputdata$Ps, inputdata$Pn, inputdata$Y, params = param_structure_2017) %>%
  # add elasticities
  bind_cols(calc_income_elast(inputdata$Y, param_structure_2017)) %>%
  # this must be done separately so that income elasticities are available
  bind_cols(calc_price_elast(., param_structure_2017)) %>%
  # add input data
  bind_cols(inputdata)
demand_2021 <- 
  food.dmnd(inputdata$Ps, inputdata$Pn, inputdata$Y, params = param_structure_2021) %>%
  # add elasticities
  bind_cols(calc_income_elast(inputdata$Y, param_structure_2021)) %>%
  # this must be done separately so that income elasticities are available
  bind_cols(calc_price_elast(., param_structure_2021)) %>%
  # add input data
  bind_cols(inputdata)

# create plots

make_demand_vs_income_pdf(
  demand_2026,
  demand_2017,
  demand_2021,
  file.path(out_dir, "demand_stylized_vs_income.pdf"),
  x_min = 0.5,
  x_max = 50
)

make_elasticities_vs_income_pdf(
  demand_2026,
  demand_2017,
  demand_2021,
  file.path(out_dir, "elasticities_stylized_vs_income.pdf"),
  x_min = 0.5,
  x_max = 20
)

