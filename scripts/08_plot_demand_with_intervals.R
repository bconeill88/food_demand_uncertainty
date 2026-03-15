# Plot comparison of demand for the HD/LD scenarios (regional and global parameters)
# to a sample of the demand ensemble, both for regional demand and demand by decile,
# and optionally impose minimum demand constraint

# Load functions
source("R/common_definitions.R")
source("R/init_packages.R")
source("R/demand_with_intervals_plot_functions.R")

# Make sure packages are installed/loaded
ensure_package(tidyverse)
ensure_package(ggforce)
ensure_package(ggh4x)
ensure_package(patchwork)

# Indicate whether to create regional or decile demand plots over time, bar plots
# in a given target year
DEMAND_REG <- TRUE
DEMAND_DEC <- TRUE
DEMAND_REG_DEC <- TRUE
BAR <- TRUE
target_year1 = 2050
target_year2 = 2100
ELAST <- TRUE
PRICE <- TRUE # and income
DECOMP <- TRUE
LAND_WATER <- TRUE

# Indicate whether to create absolute or difference plots
ABS_ML <- TRUE
ABS_HP <- TRUE
DIFF <- TRUE

# Define paths for regional ensemble results
reg_ens_path <- generate_ens_path_names()

# Define file name extensions for ensemble results files
scen_case_abs <- "_ens_bc"
scen_case_diff <- "_diffs_ens_bc"

# Directory for report result
output_dir <- file.path("output", "reports", procdata_dir, procdata_subdir_RefMLgcam)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Define report file names
rpt_names <- generate_report_names()
# ad hoc addition
rpt_names["rpt_name_landwater1_regional_abs_ML"] <- "landwater1_regional_abs_ML.pdf"
rpt_names["rpt_name_landwater2_regional_abs_ML"] <- "landwater2_regional_abs_ML.pdf"
rpt_names <- rpt_names[order(names(rpt_names))]

# Regions and deciles to plot
region_list <- c(1:32)
target_deciles <- c("FoodDemand_Group1", "FoodDemand_Group2", "FoodDemand_Group3",
                    "FoodDemand_Group6", "FoodDemand_Group10")

# Load demand projections for individual scenarios

# helper for file reading
read_rds_map <- function(base_dir, files_named) {
  # Read a named character vector of filenames from base_dir into a named list
  lapply(files_named, function(f) readRDS(file.path(base_dir, f)))
}
gcamoutput <- list()
amboutput <- list()

if(ABS_ML) {
  
  # GCAM results, from files produced by the 101 script
  gcam_base <- file.path("data", "processed", procdata_dir, gcam_results_dir)
  gcam_files <- c(
    Ref_ML     = "gcamoutput_Ref_ML.RDS",
    Ref_HD_HPR = "gcamoutput_Ref_HD_HPR.RDS",
    Ref_HD_LPR = "gcamoutput_Ref_HD_LPR.RDS",
    Ref_LD_HPR = "gcamoutput_Ref_LD_HPR.RDS",
    Ref_LD_LPR = "gcamoutput_Ref_LD_LPR.RDS")
  gcamoutput <- bind_rows(
    lapply(gcam_files, function(f) readRDS(file.path(gcam_base, f))),
    gcamoutput)

  # ambrosia results, from files produced by the 07 parameter for intervals script
  amb_base <- file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam,
                        demand_both_subdir, "scenarios_abs")
  amb_files <- c(
    Ref_ML_MLparams        = "demand_allregions_Ref_ML_gcam_MLparams.RDS",
    Ref_ML_HDHPRparams     = "demand_allregions_Ref_ML_gcam_HD_HPR_Qtot.RDS",
    Ref_ML_HDLPRparams     = "demand_allregions_Ref_ML_gcam_HD_LPR_Qtot.RDS",
    Ref_ML_LDHPRparams     = "demand_allregions_Ref_ML_gcam_LD_HPR_Qtot.RDS",
    Ref_ML_LDLPRparams     = "demand_allregions_Ref_ML_gcam_LD_LPR_Qtot.RDS")
  amboutput <- bind_rows(
    lapply(amb_files, function(f) readRDS(file.path(amb_base, f))),
    amboutput)
} 

if(ABS_HP) {
  
  # GCAM results, from files produced by the 101 script
  gcam_base <- file.path("data", "processed", procdata_dir, gcam_results_dir)
  gcam_files <- c(
    HP_ML      = "gcamoutput_HP_ML.RDS",
    HP_HD_HPR  = "gcamoutput_HP_HD_HPR.RDS",
    HP_HD_LPR  = "gcamoutput_HP_HD_LPR.RDS",
    HP_LD_HPR  = "gcamoutput_HP_LD_HPR.RDS",
    HP_LD_LPR  = "gcamoutput_HP_LD_LPR.RDS")
  gcamoutput <- bind_rows(
    lapply(gcam_files, function(f) readRDS(file.path(gcam_base, f))),
    gcamoutput)
  
  # ambrosia results, from files produced by the 07 parameter for intervals script
  amb_base <- file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam,
                           demand_both_subdir, "scenarios_abs")
  amb_files <- c(
    Ref_ML_HP_MLparams        = "demand_allregions_Ref_ML_HP_gcam_MLparams.RDS",
    Ref_ML_HP_HDHPRparams     = "demand_allregions_Ref_ML_HP_gcam_HD_HPR_Qtot.RDS",
    Ref_ML_HP_HDLPRparams     = "demand_allregions_Ref_ML_HP_gcam_HD_LPR_Qtot.RDS",
    Ref_ML_HP_LDHPRparams     = "demand_allregions_Ref_ML_HP_gcam_LD_HPR_Qtot.RDS",
    Ref_ML_HP_LDLPRparams     = "demand_allregions_Ref_ML_HP_gcam_LD_LPR_Qtot.RDS")
  amboutput <- bind_rows(
    lapply(amb_files, function(f) readRDS(file.path(amb_base, f))),
    amboutput)
}

if(DIFF) {
    
  # GCAM results, from files produced by the 101 script
  gcam_base <- file.path("data", "processed", procdata_dir, gcam_results_dir)
  gcam_diff_files <- c(
    Ref_ML_HP       = "gcamoutput_diffs_Ref_ML_HP.RDS",
    Ref_HD_HPR_HP   = "gcamoutput_diffs_Ref_HD_HPR_HP.RDS",
    Ref_HD_LPR_HP   = "gcamoutput_diffs_Ref_HD_LPR_HP.RDS",
    Ref_LD_HPR_HP   = "gcamoutput_diffs_Ref_LD_HPR_HP.RDS",
    Ref_LD_LPR_HP   = "gcamoutput_diffs_Ref_LD_LPR_HP.RDS")
  gcamoutput_diffs <- read_rds_map(gcam_base, gcam_diff_files)
  
  # ambrosia results, from files produced by the 07 parameter for intervals script
  amb_diff_base <- file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam,
                             demand_both_subdir, "scenarios_diff")
  amb_diff_files <- c(
    Ref_ML_MLparams    = "demand_diffs_allregions_MLparams.RDS",
    Ref_ML_HDHPRparams = "demand_diffs_allregions_HD_HPR_Qtot.RDS",
    Ref_ML_HDLPRparams = "demand_diffs_allregions_HD_LPR_Qtot.RDS",
    Ref_ML_LDHPRparams = "demand_diffs_allregions_LD_HPR_Qtot.RDS",
    Ref_ML_LDLPRparams = "demand_diffs_allregions_LD_LPR_Qtot.RDS")
  amboutput_diffs <- read_rds_map(amb_diff_base, amb_diff_files)
}

# Initialize a list of plot-data dfs for every report
p_all <- setNames(
  replicate(length(rpt_names), data.frame(), simplify = FALSE),
  sub("^rpt_name_", "", names(rpt_names))
)

# Initialize lists for accumulating results for bar plots
reg_ens_bc_ML_targetyr1 <- reg_ens_bc_ML_targetyr2 <-
  reg_diffs_ens_bc_targetyr1 <- reg_diffs_ens_bc_targetyr2 <- list()

# Create plots by region
for(region_id in region_list) {
  
  message("Region ", region_id)
  
  # ----------------------------------------------------------------------------
  # Absolute outcome plots
  # ----------------------------------------------------------------------------
  
  if(ABS_HP) {
    
    message("HP plots")
    
    # Load regional ensemble data
    reg_ens_bc_HP <- readRDS(
      file.path(reg_ens_path$HP$abs,  
                paste0("demand_R", region_id, scen_case_abs, ".RDS")))
    
    if(DEMAND_REG) {
      
      message(" Creating regional demand plot")
      
      # HP: with four scenarios each from ambrosia and GCAM
      p_reg <- plot_regional_comparison(
        demand_reg = reg_ens_bc_HP,
        reg_num    = region_id,
        ci_level   = 0.90,
        
        # GCAM (set 1, orange)
        scen_solid_1              = gcamoutput[["HP_ML"]],
        scen_solid_name_1         = "GCAM HP ML",
        scen_dashed_dark_1        = gcamoutput[["HP_HD_HPR"]],
        scen_dashed_dark_name_1   = "GCAM HP HD-HPR",
        scen_dashed_light_1       = gcamoutput[["HP_HD_LPR"]],
        scen_dashed_light_name_1  = "GCAM HP HD-LPR",
        scen_dotted_dark_1        = gcamoutput[["HP_LD_HPR"]],
        scen_dotted_dark_name_1   = "GCAM HP LD-HPR",
        scen_dotted_light_1       = gcamoutput[["HP_LD_LPR"]],
        scen_dotted_light_name_1  = "GCAM HP LD-LPR",
        
        # ambrosia (set 2, blue)
        scen_solid_2              = amboutput[["Ref_ML_HP_MLparams"]],
        scen_solid_name_2         = "ambrosia HP ML",
        scen_dashed_dark_2        = amboutput[["Ref_ML_HP_HDHPRparams"]],
        scen_dashed_dark_name_2   = "ambrosia HP HD-HPR",
        scen_dashed_light_2       = amboutput[["Ref_ML_HP_HDLPRparams"]],
        scen_dashed_light_name_2  = "ambrosia HP HD-LPR",
        scen_dotted_dark_2        = amboutput[["Ref_ML_HP_LDHPRparams"]],
        scen_dotted_dark_name_2   = "ambrosia HP LD-HPR",
        scen_dotted_light_2       = amboutput[["Ref_ML_HP_LDLPRparams"]],
        scen_dotted_light_name_2  = "ambrosia HP LD-LPR",
        
        return_data = TRUE   # or FALSE to get a ggplot
      )
      
      # Accumulate results
      p_all$demand_regional_abs_HP <-
        bind_rows(p_all$demand_regional_abs_HP, p_reg)
    }
    
    if(PRICE) {
      
      message(" Creating regional price plot")
      
      # HP: with four scenarios each from ambrosia and GCAM
      p_reg <- plot_regional_comparison(
        demand_reg = reg_ens_bc_HP,
        reg_num    = region_id,
        ci_level   = 0.90,
        
        value_cols  = c("Y.region", "Ps", "Pn"),
        value_names = c("Income pc (10^3 1990$)", "Staples price (2005$/Mcal)", 
                        "Non-staples price (2005$/Mcal)"),
        
        # GCAM (set 1, orange)
        scen_solid_1              = gcamoutput[["HP_ML"]],
        scen_solid_name_1         = "GCAM HP ML",
        scen_dashed_dark_1        = gcamoutput[["HP_HD_HPR"]],
        scen_dashed_dark_name_1   = "GCAM HP HD-HPR",
        scen_dashed_light_1       = gcamoutput[["HP_HD_LPR"]],
        scen_dashed_light_name_1  = "GCAM HP HD-LPR",
        scen_dotted_dark_1        = gcamoutput[["HP_LD_HPR"]],
        scen_dotted_dark_name_1   = "GCAM HP LD-HPR",
        scen_dotted_light_1       = gcamoutput[["HP_LD_LPR"]],
        scen_dotted_light_name_1  = "GCAM HP LD-LPR",
        
        # ambrosia (set 2, blue)
        scen_solid_2              = amboutput[["Ref_ML_HP_MLparams"]],
        scen_solid_name_2         = "ambrosia HP ML",
        scen_dashed_dark_2        = amboutput[["Ref_ML_HP_HDHPRparams"]],
        scen_dashed_dark_name_2   = "ambrosia HP HD-HPR",
        scen_dashed_light_2       = amboutput[["Ref_ML_HP_HDLPRparams"]],
        scen_dashed_light_name_2  = "ambrosia HP HD-LPR",
        scen_dotted_dark_2        = amboutput[["Ref_ML_HP_LDHPRparams"]],
        scen_dotted_dark_name_2   = "ambrosia HP LD-HPR",
        scen_dotted_light_2       = amboutput[["Ref_ML_HP_LDLPRparams"]],
        scen_dotted_light_name_2  = "ambrosia HP LD-LPR",
        
        return_data = TRUE,   # or FALSE to get a ggplot
        y_label = "Value"
      )
      
      # Accumulate results
      p_all$price_regional_abs_HP <- 
        bind_rows(p_all$price_regional_abs_HP, p_reg)
    }
    
    if(DEMAND_DEC) {
      
      message(" Creating decile demand plot")
      
      # HP: with four scenarios each from ambrosia and GCAM
      p_dec <- map_dfr(target_deciles, function(cg) {
        plot_decile_demand_comparison(
          demand_reg = reg_ens_bc_HP,
          reg_num = region_id,
          consumer_group = cg,
          
          # GCAM (set 1, orange)
          scen_solid_1              = gcamoutput[["HP_ML"]],
          scen_solid_name_1         = "GCAM ML",
          scen_dashed_dark_1        = gcamoutput[["HP_HD_HPR"]],
          scen_dashed_dark_name_1   = "GCAM HD-HPR",
          scen_dashed_light_1       = gcamoutput[["HP_HD_LPR"]],
          scen_dashed_light_name_1  = "GCAM HD-LPR",
          scen_dotted_dark_1        = gcamoutput[["HP_LD_HPR"]],
          scen_dotted_dark_name_1   = "GCAM LD-HPR",
          scen_dotted_light_1       = gcamoutput[["HP_LD_LPR"]],
          scen_dotted_light_name_1  = "GCAM LD-LPR",
          
          # ambrosia (set 2, blue)
          scen_solid_2              = amboutput[["Ref_ML_HP_MLparams"]],
          scen_solid_name_2         = "ambrosia ML",
          scen_dashed_dark_2        = amboutput[["Ref_ML_HP_HDHPRparams"]],
          scen_dashed_dark_name_2   = "ambrosia HD-HPR",
          scen_dashed_light_2       = amboutput[["Ref_ML_HP_HDLPRparams"]],
          scen_dashed_light_name_2  = "ambrosia HD-LPR",
          scen_dotted_dark_2        = amboutput[["Ref_ML_HP_LDHPRparams"]],
          scen_dotted_dark_name_2   = "ambrosia LD-HPR",
          scen_dotted_light_2       = amboutput[["Ref_ML_HP_LDLPRparams"]],
          scen_dotted_light_name_2  = "ambrosia LD-LPR",
          
          ci_level = 0.90,
          return_data = TRUE
        ) %>%
          mutate(region_label = paste0("Region ", region_id))
      })
      
      # Accumulate results
      p_all$demand_decile_abs_HP <- 
        bind_rows(p_all$demand_decile_abs_HP, p_dec)  
    }
    
    if(DEMAND_REG_DEC) {
      
      message(" Creating region/decile demand plot")
      
      # HP: with four scenarios each from ambrosia and GCAM
      p_reg_dec <- plot_region_and_decile_demand_comparison(
        demand_reg = reg_ens_bc_HP,
        reg_num    = region_id,
        
        # GCAM (set 1, orange)
        scen_solid_1              = gcamoutput[["HP_ML"]],
        scen_solid_name_1         = "GCAM ML",
        scen_dashed_dark_1        = gcamoutput[["HP_HD_HPR"]],
        scen_dashed_dark_name_1   = "GCAM HD-HPR",
        scen_dashed_light_1       = gcamoutput[["HP_HD_LPR"]],
        scen_dashed_light_name_1  = "GCAM HD-LPR",
        scen_dotted_dark_1        = gcamoutput[["HP_LD_HPR"]],
        scen_dotted_dark_name_1   = "GCAM LD-HPR",
        scen_dotted_light_1       = gcamoutput[["HP_LD_LPR"]],
        scen_dotted_light_name_1  = "GCAM LD-LPR",
        
        # ambrosia (set 2, blue)
        scen_solid_2              = amboutput[["Ref_ML_HP_MLparams"]],
        scen_solid_name_2         = "ambrosia ML",
        scen_dashed_dark_2        = amboutput[["Ref_ML_HP_HDHPRparams"]],
        scen_dashed_dark_name_2   = "ambrosia HD-HPR",
        scen_dashed_light_2       = amboutput[["Ref_ML_HP_HDLPRparams"]],
        scen_dashed_light_name_2  = "ambrosia HD-LPR",
        scen_dotted_dark_2        = amboutput[["Ref_ML_HP_LDHPRparams"]],
        scen_dotted_dark_name_2   = "ambrosia LD-HPR",
        scen_dotted_light_2       = amboutput[["Ref_ML_HP_LDLPRparams"]],
        scen_dotted_light_name_2  = "ambrosia LD-LPR",
        
        ci_level    = 0.90,
        return_data = TRUE
      ) %>%
        mutate(region_label = paste0("Region ", region_id))
      
      # Accumulate results
      p_all$demand_regional_decile_abs_HP <- 
        bind_rows( p_all$demand_regional_decile_abs_HP, p_reg_dec)
    }
  }
  
  if(ABS_ML) {
    
    message("ML plots")
    
    # Load regional ensemble data
    reg_ens_bc_ML <- readRDS(
      file.path(reg_ens_path$ML$abs,  
                paste0("demand_R", region_id, scen_case_abs, ".RDS")))
    
    if(DEMAND_REG) {
      
      message(" Creating regional demand plot")

      # ML: with four scenarios each from ambrosia and GCAM
      p_reg <- plot_regional_comparison(
        demand_reg = reg_ens_bc_ML,
        reg_num    = region_id,
        ci_level   = 0.90,

        # GCAM (set 1, orange)
        scen_solid_1              = gcamoutput[["Ref_ML"]],
        scen_solid_name_1         = "GCAM ML",
        scen_dashed_dark_1        = gcamoutput[["Ref_HD_HPR"]],
        scen_dashed_dark_name_1   = "GCAM HD-HPR",
        scen_dashed_light_1       = gcamoutput[["Ref_HD_LPR"]],
        scen_dashed_light_name_1  = "GCAM HD-LPR",
        scen_dotted_dark_1        = gcamoutput[["Ref_LD_HPR"]],
        scen_dotted_dark_name_1   = "GCAM LD-HPR",
        scen_dotted_light_1       = gcamoutput[["Ref_LD_LPR"]],
        scen_dotted_light_name_1  = "GCAM LD-LPR",

        # ambrosia (set 2, blue)
        scen_solid_2              = amboutput[["Ref_ML_MLparams"]],
        scen_solid_name_2         = "ambrosia ML",
        scen_dashed_dark_2        = amboutput[["Ref_ML_HDHPRparams"]],
        scen_dashed_dark_name_2   = "ambrosia HD-HPR",
        scen_dashed_light_2       = amboutput[["Ref_ML_HDLPRparams"]],
        scen_dashed_light_name_2  = "ambrosia HD-LPR",
        scen_dotted_dark_2        = amboutput[["Ref_ML_LDHPRparams"]],
        scen_dotted_dark_name_2   = "ambrosia LD-HPR",
        scen_dotted_light_2       = amboutput[["Ref_ML_LDLPRparams"]],
        scen_dotted_light_name_2  = "ambrosia LD-LPR",

        return_data = TRUE   # or FALSE to get a ggplot
      )

      # Accumulate results
      p_all$demand_regional_abs_ML <- 
        bind_rows(p_all$demand_regional_abs_ML, p_reg)
           
      # plain version: ambrosia ML scenario only
      p_reg <- plot_regional_comparison(
        demand_reg = reg_ens_bc_ML,
        reg_num    = region_id,
        ci_level   = 0.90,

        # ambrosia (set 2)
        scen_solid_2              = amboutput[["Ref_ML_MLparams"]],
        scen_solid_name_2         = "ambrosia ML",

        return_data = TRUE   # or FALSE to get a ggplot
      )

      # Accumulate results
      p_all$demand_regional_abs_ML_plain <-
        bind_rows( p_all$demand_regional_abs_ML_plain, p_reg)

      # plain version with comparison: ambrosia and GCAM ML scenarios only
      p_reg <- plot_regional_comparison(
        demand_reg = reg_ens_bc_ML,
        reg_num    = region_id,
        ci_level   = 0.90,

        # GCAM (set 1, orange)
        scen_solid_1              = gcamoutput[["Ref_ML"]],
        scen_solid_name_1         = "GCAM ML",

        # ambrosia (set 2)
        scen_solid_2              = amboutput[["Ref_ML_MLparams"]],
        scen_solid_name_2         = "ambrosia ML",

        return_data = TRUE   # or FALSE to get a ggplot
      )

      # Accumulate results
      p_all$demand_regional_abs_ML_plain_compare <-
        bind_rows( p_all$demand_regional_abs_ML_plain_compare, p_reg)

    }
    
    if (ELAST) {
      
      message(" Creating regional elasticity plot")
      
      # with four scenarios from ambrosia
      p_el <- plot_regional_elasticity_comparison(
        elast_reg = reg_ens_bc_ML,
        reg_num   = region_id,
        ci_level  = 0.90,
        
        # ambrosia (set 2, blue)
        scen_solid_2              = amboutput[["Ref_ML_MLparams"]],
        scen_solid_name_2         = "ambrosia ML",
        scen_dashed_dark_2        = amboutput[["Ref_ML_HDHPRparams"]],
        scen_dashed_dark_name_2   = "ambrosia HD-HPR",
        scen_dashed_light_2       = amboutput[["Ref_ML_HDLPRparams"]],
        scen_dashed_light_name_2  = "ambrosia HD-LPR",
        scen_dotted_dark_2        = amboutput[["Ref_ML_LDHPRparams"]],
        scen_dotted_dark_name_2   = "ambrosia LD-HPR",
        scen_dotted_light_2       = amboutput[["Ref_ML_LDLPRparams"]],
        scen_dotted_light_name_2  = "ambrosia LD-LPR",
        
        return_data = TRUE
      )
      
      # Accumulate results
      p_all$elasticity_regional_abs_ML <- 
        bind_rows(p_all$elasticity_regional_abs_ML, p_el)
      
      # plain version: ambrosia ML scenario only
      p_el <- plot_regional_elasticity_comparison(
        elast_reg = reg_ens_bc_ML,
        reg_num   = region_id,
        ci_level  = 0.90,
        
        # ambrosia (set 2, blue)
        scen_solid_2              = amboutput[["Ref_ML_MLparams"]],
        scen_solid_name_2         = "ambrosia ML",

        return_data = TRUE
      )
      
      # Accumulate results
      p_all$elasticity_regional_abs_ML_plain <- 
        bind_rows( p_all$elasticity_regional_abs_ML_plain, p_el)
    }
    
    if(PRICE) {
      
      message(" Creating regional price plot")

      # ML: with four scenarios each from ambrosia and GCAM
      p_reg <- plot_regional_comparison(
        demand_reg = reg_ens_bc_ML,
        reg_num    = region_id,
        ci_level   = 0.90,
        
        value_cols  = c("Y.region", "Ps", "Pn"),
        value_names = c("Income pc (10^3 1990$)", "Staples price (2005$/Mcal)", 
                        "Non-staples price (2005$/Mcal)"),
        
        # GCAM (set 1, orange)
        scen_solid_1              = gcamoutput[["Ref_ML"]],
        scen_solid_name_1         = "GCAM ML",
        scen_dashed_dark_1        = gcamoutput[["Ref_HD_HPR"]],
        scen_dashed_dark_name_1   = "GCAM HD-HPR",
        scen_dashed_light_1       = gcamoutput[["Ref_HD_LPR"]],
        scen_dashed_light_name_1  = "GCAM HD-LPR",
        scen_dotted_dark_1        = gcamoutput[["Ref_LD_HPR"]],
        scen_dotted_dark_name_1   = "GCAM LD-HPR",
        scen_dotted_light_1       = gcamoutput[["Ref_LD_LPR"]],
        scen_dotted_light_name_1  = "GCAM LD-LPR",
        
        # ambrosia (set 2, blue)
        scen_solid_2              = amboutput[["Ref_ML_MLparams"]],
        scen_solid_name_2         = "ambrosia ML",
        scen_dashed_dark_2        = amboutput[["Ref_ML_HDHPRparams"]],
        scen_dashed_dark_name_2   = "ambrosia HD-HPR",
        scen_dashed_light_2       = amboutput[["Ref_ML_HDLPRparams"]],
        scen_dashed_light_name_2  = "ambrosia HD-LPR",
        scen_dotted_dark_2        = amboutput[["Ref_ML_LDHPRparams"]],
        scen_dotted_dark_name_2   = "ambrosia LD-HPR",
        scen_dotted_light_2       = amboutput[["Ref_ML_LDLPRparams"]],
        scen_dotted_light_name_2  = "ambrosia LD-LPR",
        
        return_data = TRUE,   # or FALSE to get a ggplot
        y_label = "Value"
      )
      
      # Accumulate results
      p_all$price_regional_abs_ML <- 
        bind_rows(p_all$price_regional_abs_ML, p_reg)
      
      # plain version with comparison: ambrosia and GCAM ML scenarios only
      p_reg <- plot_regional_comparison(
        demand_reg = reg_ens_bc_ML,
        reg_num    = region_id,
        ci_level   = 0.90,
        
        value_cols  = c("Y.region", "Ps", "Pn"),
        value_names = c("Income pc (10^3 1990$)", "Staples price (2005$/Mcal)", 
                        "Non-staples price (2005$/Mcal)"),
        
        # GCAM (set 1, orange)
        scen_solid_1              = gcamoutput[["Ref_ML"]],
        scen_solid_name_1         = "GCAM ML",
        
        # ambrosia (set 2)
        scen_solid_2              = amboutput[["Ref_ML_MLparams"]],
        scen_solid_name_2         = "ambrosia ML",
        
        return_data = TRUE,   # or FALSE to get a ggplot
        y_label = "Value"
      )
      
      # Accumulate results
      p_all$price_regional_abs_ML_plain_compare <- 
        bind_rows(p_all$price_regional_abs_ML_plain_compare, p_reg)
      
    }
    
    if(DEMAND_DEC) {
      
      message(" Creating decile demand plot")
      
      # ML: with four scenarios each from ambrosia and GCAM
      p_dec <- map_dfr(target_deciles, function(cg) {
        plot_decile_demand_comparison(
          demand_reg = reg_ens_bc_ML,
          reg_num = region_id,
          consumer_group = cg,

          # GCAM (set 1, orange)
          scen_solid_1              = gcamoutput[["Ref_ML"]],
          scen_solid_name_1         = "GCAM ML",
          scen_dashed_dark_1        = gcamoutput[["Ref_HD_HPR"]],
          scen_dashed_dark_name_1   = "GCAM HD-HPR",
          scen_dashed_light_1       = gcamoutput[["Ref_HD_LPR"]],
          scen_dashed_light_name_1  = "GCAM HD-LPR",
          scen_dotted_dark_1        = gcamoutput[["Ref_LD_HPR"]],
          scen_dotted_dark_name_1   = "GCAM LD-HPR",
          scen_dotted_light_1       = gcamoutput[["Ref_LD_LPR"]],
          scen_dotted_light_name_1  = "GCAM LD-LPR",

          # ambrosia (set 2, blue)
          scen_solid_2              = amboutput[["Ref_ML_MLparams"]],
          scen_solid_name_2         = "ambrosia ML",
          scen_dashed_dark_2        = amboutput[["Ref_ML_HDHPRparams"]],
          scen_dashed_dark_name_2   = "ambrosia HD-HPR",
          scen_dashed_light_2       = amboutput[["Ref_ML_HDLPRparams"]],
          scen_dashed_light_name_2  = "ambrosia HD-LPR",
          scen_dotted_dark_2        = amboutput[["Ref_ML_LDHPRparams"]],
          scen_dotted_dark_name_2   = "ambrosia LD-HPR",
          scen_dotted_light_2       = amboutput[["Ref_ML_LDLPRparams"]],
          scen_dotted_light_name_2  = "ambrosia LD-LPR",

          ci_level = 0.90,
          return_data = TRUE
        ) %>%
          mutate(region_label = paste0("Region ", region_id))
      })

      # Accumulate results
      p_all$demand_decile_abs_ML <- 
        bind_rows(p_all$demand_decile_abs_ML, p_dec)

      # plain version: ambrosia ML scenario only
      p_dec <- map_dfr(target_deciles, function(cg) {
        plot_decile_demand_comparison(
          demand_reg = reg_ens_bc_ML,
          reg_num = region_id,
          consumer_group = cg,

          # ambrosia (set 2, blue)
          scen_solid_2              = amboutput[["Ref_ML_MLparams"]],
          scen_solid_name_2         = "ambrosia ML",

          ci_level = 0.90,
          return_data = TRUE
        ) %>%
          mutate(region_label = paste0("Region ", region_id))
      })

      # Accumulate results
      p_all$demand_decile_abs_ML_plain <- 
        bind_rows(p_all$demand_decile_abs_ML_plain, p_dec)

      # plain version with compare: ambrosia and GCAM ML scenarios only
      p_dec <- map_dfr(target_deciles, function(cg) {
        plot_decile_demand_comparison(
          demand_reg = reg_ens_bc_ML,
          reg_num = region_id,
          consumer_group = cg,
          
          # GCAM (set 1, orange)
          scen_solid_1              = gcamoutput[["Ref_ML"]],
          scen_solid_name_1         = "GCAM ML",
          
          # ambrosia (set 2, blue)
          scen_solid_2              = amboutput[["Ref_ML_MLparams"]],
          scen_solid_name_2         = "ambrosia ML",
          
          ci_level = 0.90,
          return_data = TRUE
        ) %>%
          mutate(region_label = paste0("Region ", region_id))
      })
      
      # Accumulate results
      p_all$demand_decile_abs_ML_plain_compare <- 
        bind_rows(p_all$demand_decile_abs_ML_plain_compare, p_dec)
    }
    
    if(DEMAND_REG_DEC) {
      
      message(" Creating region/decile demand plot")
      
      # ML: with four scenarios each from ambrosia and GCAM
      p_reg_dec <- plot_region_and_decile_demand_comparison(
        demand_reg = reg_ens_bc_ML,
        reg_num    = region_id,
        
        # GCAM (set 1, orange)
        scen_solid_1              = gcamoutput[["Ref_ML"]],
        scen_solid_name_1         = "GCAM ML",
        scen_dashed_dark_1        = gcamoutput[["Ref_HD_HPR"]],
        scen_dashed_dark_name_1   = "GCAM HD-HPR",
        scen_dashed_light_1       = gcamoutput[["Ref_HD_LPR"]],
        scen_dashed_light_name_1  = "GCAM HD-LPR",
        scen_dotted_dark_1        = gcamoutput[["Ref_LD_HPR"]],
        scen_dotted_dark_name_1   = "GCAM LD-HPR",
        scen_dotted_light_1       = gcamoutput[["Ref_LD_LPR"]],
        scen_dotted_light_name_1  = "GCAM LD-LPR",
        
        # ambrosia (set 2, blue)
        scen_solid_2              = amboutput[["Ref_ML_MLparams"]],
        scen_solid_name_2         = "ambrosia ML",
        scen_dashed_dark_2        = amboutput[["Ref_ML_HDHPRparams"]],
        scen_dashed_dark_name_2   = "ambrosia HD-HPR",
        scen_dashed_light_2       = amboutput[["Ref_ML_HDLPRparams"]],
        scen_dashed_light_name_2  = "ambrosia HD-LPR",
        scen_dotted_dark_2        = amboutput[["Ref_ML_LDHPRparams"]],
        scen_dotted_dark_name_2   = "ambrosia LD-HPR",
        scen_dotted_light_2       = amboutput[["Ref_ML_LDLPRparams"]],
        scen_dotted_light_name_2  = "ambrosia LD-LPR",
        
        ci_level    = 0.90,
        return_data = TRUE
      ) %>%
        mutate(region_label = paste0("Region ", region_id))
      
      # Accumulate results
      p_all$demand_regional_decile_abs_ML <- 
        bind_rows( p_all$demand_regional_decile_abs_ML, p_reg_dec)
      
      # plain version: ambrosia ML scenario only
      p_reg_dec <- plot_region_and_decile_demand_comparison(
        demand_reg = reg_ens_bc_ML,
        reg_num    = region_id,
        
        # ambrosia (set 2, blue)
        scen_solid_2              = amboutput[["Ref_ML_MLparams"]],
        scen_solid_name_2         = "ambrosia ML",

        ci_level    = 0.90,
        return_data = TRUE
      ) %>%
        mutate(region_label = paste0("Region ", region_id))
      
      # Accumulate results
      p_all$demand_regional_decile_abs_ML_plain <- 
        bind_rows(p_all$demand_regional_decile_abs_ML_plain, p_reg_dec)
      
      # plain version with compare: ambrosia and GCAM ML scenarios only
      p_reg_dec <- plot_region_and_decile_demand_comparison(
        demand_reg = reg_ens_bc_ML,
        reg_num    = region_id,
        
        # GCAM (set 1, orange)
        scen_solid_1              = gcamoutput[["Ref_ML"]],
        scen_solid_name_1         = "GCAM ML",

        # ambrosia (set 2, blue)
        scen_solid_2              = amboutput[["Ref_ML_MLparams"]],
        scen_solid_name_2         = "ambrosia ML",

        ci_level    = 0.90,
        return_data = TRUE
      ) %>%
        mutate(region_label = paste0("Region ", region_id))
      
      # Accumulate results
      p_all$demand_regional_decile_abs_ML_plain_compare <- 
        bind_rows( p_all$demand_regional_decile_abs_ML_plain_compare, p_reg_dec)
    }
    
    if(BAR) {
      
      message(" Collecting regional demand for bar plot")
      
      # Accumulate demand in target years
      reg_ens_bc_ML_targetyr1 <- 
        bind_rows(reg_ens_bc_ML_targetyr1,
                  reg_ens_bc_ML %>% 
                    filter(year == target_year1,
                           `gcam-consumer` == "FoodDemand_Group1"))
      reg_ens_bc_ML_targetyr2 <- 
        bind_rows(reg_ens_bc_ML_targetyr2,
                  reg_ens_bc_ML %>% 
                    filter(year == target_year2,
                           `gcam-consumer` == "FoodDemand_Group1"))
    }
    
    if(DECOMP) {
      
      message(" Creating regional demand decomposition plot")

      # Load regional decomposition ensemble data
      reg_ens_price <- readRDS(
        file.path(reg_ens_path$MLprice$abs,  
                  paste0("demand_R", region_id, scen_case_abs, ".RDS")))
      reg_ens_income <- readRDS(
        file.path(reg_ens_path$MLincome$abs,  
                  paste0("demand_R", region_id, scen_case_abs, ".RDS")))
      reg_ens_scale <- readRDS(
        file.path(reg_ens_path$MLscale$abs,  
                  paste0("demand_R", region_id, scen_case_abs, ".RDS")))
      
      # Inside the region loop, after loading demand_full and having the 3 sub-ensembles available:
      p_decomp <- plot_regional_uncertainty_decomposition(
        demand_full = reg_ens_bc_ML,
        reg_num     = region_id,
        scen_ml     = amboutput[["Ref_ML_MLparams"]],
        scen_ml_name = "Ambrosia ML",
        ens_price   = reg_ens_price,
        ens_income  = reg_ens_income,
        ens_scale   = reg_ens_scale,
        ci_level    = 0.90,
        return_data = TRUE
      )
      
      p_all$demand_regional_abs_decomp_ML <- 
        bind_rows(p_all$demand_regional_abs_decomp_ML, p_decomp)
      
    }
    
    if(LAND_WATER) {
      
      message(" Creating land and water plots")
      
        # ML: with four scenarios from GCAM, first three land-water variables
        p_reg <- plot_regional_comparison(
          reg_num    = region_id,
          ci_level   = 0.90,
          
          value_cols  = c("cropland", "pasture", "bio_production"),
          value_names = c("Cropland (10^3 km^2)", "Pasture (10^3 km^2)", 
                          "Biomass production (EJ)"),
          
          # GCAM (set 1, orange)
          scen_solid_1              = gcamoutput[["Ref_ML"]],
          scen_solid_name_1         = "GCAM ML",
          scen_dashed_dark_1        = gcamoutput[["Ref_HD_HPR"]],
          scen_dashed_dark_name_1   = "GCAM HD-HPR",
          scen_dashed_light_1       = gcamoutput[["Ref_HD_LPR"]],
          scen_dashed_light_name_1  = "GCAM HD-LPR",
          scen_dotted_dark_1        = gcamoutput[["Ref_LD_HPR"]],
          scen_dotted_dark_name_1   = "GCAM LD-HPR",
          scen_dotted_light_1       = gcamoutput[["Ref_LD_LPR"]],
          scen_dotted_light_name_1  = "GCAM LD-LPR",
          
          show_ribbons = FALSE,
          start_year = 2020,
          return_data = TRUE,   # or FALSE to get a ggplot
          y_label = "Value"
        )
        
        # Accumulate results
        p_all$landwater1_regional_abs_ML <- 
          bind_rows(p_all$landwater1_regional_abs_ML, p_reg)
        
        # ML: with four scenarios from GCAM, second three land-water variables
        p_reg <- plot_regional_comparison(
          reg_num    = region_id,
          ci_level   = 0.90,
          
          value_cols  = c("withdrawals", "forest", "emissions"),
          value_names = c("Water withdrawals (km^3)", "Forest (10^3 km^2)", 
                          "LUC emissions (MtC/yr)"),
          
          # GCAM (set 1, orange)
          scen_solid_1              = gcamoutput[["Ref_ML"]],
          scen_solid_name_1         = "GCAM ML",
          scen_dashed_dark_1        = gcamoutput[["Ref_HD_HPR"]],
          scen_dashed_dark_name_1   = "GCAM HD-HPR",
          scen_dashed_light_1       = gcamoutput[["Ref_HD_LPR"]],
          scen_dashed_light_name_1  = "GCAM HD-LPR",
          scen_dotted_dark_1        = gcamoutput[["Ref_LD_HPR"]],
          scen_dotted_dark_name_1   = "GCAM LD-HPR",
          scen_dotted_light_1       = gcamoutput[["Ref_LD_LPR"]],
          scen_dotted_light_name_1  = "GCAM LD-LPR",
          
          show_ribbons = FALSE,
          start_year = 2020,
          return_data = TRUE,   # or FALSE to get a ggplot
          y_label = "Value"
        )
        
        # Accumulate results
        p_all$landwater2_regional_abs_ML <- 
          bind_rows(p_all$landwater2_regional_abs_ML, p_reg)
    }
        
  }
  
  # ----------------------------------------------------------------------------
  # Difference plots
  # ----------------------------------------------------------------------------
  
  if(DIFF) {
    
    # Load regional ensemble data
    reg_ens_bc_ML <- readRDS(
      file.path(reg_ens_path$ML$diff,  
                paste0("demand_R", region_id, scen_case_diff, ".RDS")))
    
    if(DEMAND_REG) {
      
      message(" Creating regional demand difference plot")
      
      # with four scenarios each from ambrosia and GCAM
      p_reg <- plot_regional_comparison(
        demand_reg = reg_ens_bc_ML,
        reg_num    = region_id,
        ci_level   = 0.90,
        
        # GCAM (set 1, orange)
        scen_solid_1              = gcamoutput_diffs[["Ref_ML_HP"]],
        scen_solid_name_1         = "GCAM ML",
        scen_dashed_dark_1        = gcamoutput_diffs[["Ref_HD_HPR_HP"]],
        scen_dashed_dark_name_1   = "GCAM HD-HPR",
        scen_dashed_light_1       = gcamoutput_diffs[["Ref_HD_LPR_HP"]],
        scen_dashed_light_name_1  = "GCAM HD-LPR",
        scen_dotted_dark_1        = gcamoutput_diffs[["Ref_LD_HPR_HP"]],
        scen_dotted_dark_name_1   = "GCAM LD-HPR",
        scen_dotted_light_1       = gcamoutput_diffs[["Ref_LD_LPR_HP"]],
        scen_dotted_light_name_1  = "GCAM LD-LPR",
        
        # ambrosia (set 2, blue)
        scen_solid_2              = amboutput_diffs[["Ref_ML_MLparams"]],
        scen_solid_name_2         = "ambrosia ML",
        scen_dashed_dark_2        = amboutput_diffs[["Ref_ML_HDHPRparams"]],
        scen_dashed_dark_name_2   = "ambrosia HD-HPR",
        scen_dashed_light_2       = amboutput_diffs[["Ref_ML_HDLPRparams"]],
        scen_dashed_light_name_2  = "ambrosia HD-LPR",
        scen_dotted_dark_2        = amboutput_diffs[["Ref_ML_LDHPRparams"]],
        scen_dotted_dark_name_2   = "ambrosia LD-HPR",
        scen_dotted_light_2       = amboutput_diffs[["Ref_ML_LDLPRparams"]],
        scen_dotted_light_name_2  = "ambrosia LD-LPR",
        
        return_data = TRUE   # or FALSE to get a ggplot
      )
      
      # Accumulate results
      p_all$demand_regional_diff_MLHP <- 
        bind_rows(p_all$demand_regional_diff_MLHP, p_reg)
      
      # plain version: with ambrosia ML scenario only
      p_reg <- plot_regional_comparison(
        demand_reg = reg_ens_bc_ML,
        reg_num    = region_id,
        ci_level   = 0.90,
        
        # ambrosia (set 2, blue)
        scen_solid_2              = amboutput_diffs[["Ref_ML_MLparams"]],
        scen_solid_name_2         = "ambrosia ML",

        return_data = TRUE   # or FALSE to get a ggplot
      )
      
      # Accumulate results
      p_all$demand_regional_diff_MLHP_plain <- 
        bind_rows(p_all$demand_regional_diff_MLHP_plain, p_reg)
      
      # plain version with compare: ambrosia and GCAM ML scenarios only
      p_reg <- plot_regional_comparison(
        demand_reg = reg_ens_bc_ML,
        reg_num    = region_id,
        ci_level   = 0.90,
        
        # GCAM (set 1, orange)
        scen_solid_1              = gcamoutput_diffs[["Ref_ML_HP"]],
        scen_solid_name_1         = "GCAM ML",

        # ambrosia (set 2, blue)
        scen_solid_2              = amboutput_diffs[["Ref_ML_MLparams"]],
        scen_solid_name_2         = "ambrosia ML",
        
        return_data = TRUE   # or FALSE to get a ggplot
      )
      
      # Accumulate results
      p_all$demand_regional_diff_MLHP_plain_compare <- 
        bind_rows(p_all$demand_regional_diff_MLHP_plain_compare, p_reg)
      
    }
    
    if (ELAST) {
      
      message(" Creating regional elasticity difference plot")
      
      # with four scenarios from ambrosia
      p_el <- plot_regional_elasticity_comparison(
        elast_reg = reg_ens_bc_ML,
        reg_num   = region_id,
        ci_level  = 0.90,
        
        # ambrosia (set 2, blue)
        scen_solid_2              = amboutput_diffs[["Ref_ML_MLparams"]],
        scen_solid_name_2         = "ambrosia ML",
        scen_dashed_dark_2        = amboutput_diffs[["Ref_ML_HDHPRparams"]],
        scen_dashed_dark_name_2   = "ambrosia HD-HPR",
        scen_dashed_light_2       = amboutput_diffs[["Ref_ML_HDLPRparams"]],
        scen_dashed_light_name_2  = "ambrosia HD-LPR",
        scen_dotted_dark_2        = amboutput_diffs[["Ref_ML_LDHPRparams"]],
        scen_dotted_dark_name_2   = "ambrosia LD-HPR",
        scen_dotted_light_2       = amboutput_diffs[["Ref_ML_LDLPRparams"]],
        scen_dotted_light_name_2  = "ambrosia LD-LPR",
        
        return_data = TRUE
      )
      
      # Accumulate results
      p_all$elasticity_regional_diff_MLHP <- 
        bind_rows(p_all$elasticity_regional_diff_MLHP, p_el)
      
      # plain version: with ambrosia ML scenario only
      p_el <- plot_regional_elasticity_comparison(
        elast_reg = reg_ens_bc_ML,
        reg_num   = region_id,
        ci_level  = 0.90,
        
        # ambrosia (set 2, blue)
        scen_solid_2              = amboutput_diffs[["Ref_ML_MLparams"]],
        scen_solid_name_2         = "ambrosia ML",

        return_data = TRUE
      )
      
      # Accumulate results
      p_all$elasticity_regional_diff_MLHP_plain <- 
        bind_rows(p_all$elasticity_regional_diff_MLHP_plain, p_el)
    }
    
    if(PRICE) {
      
      message(" Creating regional price difference plot")

      # with four scenarios each from ambrosia and GCAM
      p_reg <- plot_regional_comparison(
        demand_reg = reg_ens_bc_ML,
        reg_num    = region_id,
        ci_level   = 0.90,
        
        value_cols  = c("Y.region", "Ps", "Pn"),
        value_names = c("Income pc (10^3 1990$)", "Staples price (2005$/Mcal)", 
                        "Non-staples price (2005$/Mcal)"),
        
        # GCAM (set 1, orange)
        scen_solid_1              = gcamoutput_diffs[["Ref_ML_HP"]],
        scen_solid_name_1         = "GCAM ML",
        scen_dashed_dark_1        = gcamoutput_diffs[["Ref_HD_HPR_HP"]],
        scen_dashed_dark_name_1   = "GCAM HD-HPR",
        scen_dashed_light_1       = gcamoutput_diffs[["Ref_HD_LPR_HP"]],
        scen_dashed_light_name_1  = "GCAM HD-LPR",
        scen_dotted_dark_1        = gcamoutput_diffs[["Ref_LD_HPR_HP"]],
        scen_dotted_dark_name_1   = "GCAM LD-HPR",
        scen_dotted_light_1       = gcamoutput_diffs[["Ref_LD_LPR_HP"]],
        scen_dotted_light_name_1  = "GCAM LD-LPR",
        
        # ambrosia (set 2, blue)
        scen_solid_2              = amboutput_diffs[["Ref_ML_MLparams"]],
        scen_solid_name_2         = "ambrosia ML",
        scen_dashed_dark_2        = amboutput_diffs[["Ref_ML_HDHPRparams"]],
        scen_dashed_dark_name_2   = "ambrosia HD-HPR",
        scen_dashed_light_2       = amboutput_diffs[["Ref_ML_HDLPRparams"]],
        scen_dashed_light_name_2  = "ambrosia HD-LPR",
        scen_dotted_dark_2        = amboutput_diffs[["Ref_ML_LDHPRparams"]],
        scen_dotted_dark_name_2   = "ambrosia LD-HPR",
        scen_dotted_light_2       = amboutput_diffs[["Ref_ML_LDLPRparams"]],
        scen_dotted_light_name_2  = "ambrosia LD-LPR",
        
        return_data = TRUE,   # or FALSE to get a ggplot
        y_label = "Difference"
      )
      
      # Accumulate results
      p_all$price_regional_diff_MLHP <- 
        bind_rows(p_all$price_regional_diff_MLHP, p_reg)
      
      # plain version with comparison: ambrosia and GCAM ML scenarios only
      p_reg <- plot_regional_comparison(
        demand_reg = reg_ens_bc_ML,
        reg_num    = region_id,
        ci_level   = 0.90,
        
        value_cols  = c("Y.region", "Ps", "Pn"),
        value_names = c("Income pc (10^3 1990$)", "Staples price (2005$/Mcal)", 
                        "Non-staples price (2005$/Mcal)"),
        
        # GCAM (set 1, orange)
        scen_solid_1              = gcamoutput_diffs[["Ref_ML_HP"]],
        scen_solid_name_1         = "GCAM ML",

        # ambrosia (set 2, blue)
        scen_solid_2              = amboutput_diffs[["Ref_ML_MLparams"]],
        scen_solid_name_2         = "ambrosia ML",

        return_data = TRUE,   # or FALSE to get a ggplot
        y_label = "Difference"
      )
      
      # Accumulate results
      p_all$price_regional_diff_MLHP_plain_compare <- 
        bind_rows(p_all$price_regional_diff_MLHP_plain_compare, p_reg)
      
    }
    
    if(DEMAND_DEC) {
      
      message(" Creating decile demand difference plot")
      
      # with four scenarios each from ambrosia and GCAM
      p_dec <- map_dfr(target_deciles, function(cg) {
        plot_decile_demand_comparison(
          demand_reg = reg_ens_bc_ML,
          reg_num = region_id,
          consumer_group = cg,

          # GCAM (set 1, orange)
          scen_solid_1              = gcamoutput_diffs[["Ref_ML_HP"]],
          scen_solid_name_1         = "GCAM ML",
          scen_dashed_dark_1        = gcamoutput_diffs[["Ref_HD_HPR_HP"]],
          scen_dashed_dark_name_1   = "GCAM HD-HPR",
          scen_dashed_light_1       = gcamoutput_diffs[["Ref_HD_LPR_HP"]],
          scen_dashed_light_name_1  = "GCAM HD-LPR",
          scen_dotted_dark_1        = gcamoutput_diffs[["Ref_LD_HPR_HP"]],
          scen_dotted_dark_name_1   = "GCAM LD-HPR",
          scen_dotted_light_1       = gcamoutput_diffs[["Ref_LD_LPR_HP"]],
          scen_dotted_light_name_1  = "GCAM LD-LPR",
          
          # ambrosia (set 2, blue)
          scen_solid_2              = amboutput_diffs[["Ref_ML_MLparams"]],
          scen_solid_name_2         = "ambrosia ML",
          scen_dashed_dark_2        = amboutput_diffs[["Ref_ML_HDHPRparams"]],
          scen_dashed_dark_name_2   = "ambrosia HD-HPR",
          scen_dashed_light_2       = amboutput_diffs[["Ref_ML_HDLPRparams"]],
          scen_dashed_light_name_2  = "ambrosia HD-LPR",
          scen_dotted_dark_2        = amboutput_diffs[["Ref_ML_LDHPRparams"]],
          scen_dotted_dark_name_2   = "ambrosia LD-HPR",
          scen_dotted_light_2       = amboutput_diffs[["Ref_ML_LDLPRparams"]],
          scen_dotted_light_name_2  = "ambrosia LD-LPR",
          
          ci_level = 0.90,
          return_data = TRUE
        ) %>%
          mutate(region_label = paste0("Region ", region_id))
      })
      
      # Accumulate results
      p_all$demand_decile_diff_MLHP <- 
        bind_rows(p_all$demand_decile_diff_MLHP, p_dec)
      
      # plain version: with ambrosia ML scenario only
      p_dec <- map_dfr(target_deciles, function(cg) {
        plot_decile_demand_comparison(
          demand_reg = reg_ens_bc_ML,
          reg_num = region_id,
          consumer_group = cg,
          
          # ambrosia (set 2, blue)
          scen_solid_2              = amboutput_diffs[["Ref_ML_MLparams"]],
          scen_solid_name_2         = "ambrosia ML",

          ci_level = 0.90,
          return_data = TRUE
        ) %>%
          mutate(region_label = paste0("Region ", region_id))
      })
      
      # Accumulate results
      p_all$demand_decile_diff_MLHP_plain <- 
        bind_rows(p_all$demand_decile_diff_MLHP_plain, p_dec)
    }
    
    if(DEMAND_REG_DEC) {
      
      message(" Creating region/decile demand difference plot")
      
      # with four scenarios each from ambrosia and GCAM
      p_reg_dec <- plot_region_and_decile_demand_comparison(
        demand_reg = reg_ens_bc_ML,
        reg_num = region_id,
        
        # GCAM (set 1, orange)
        scen_solid_1              = gcamoutput_diffs[["Ref_ML_HP"]],
        scen_solid_name_1         = "GCAM ML",
        scen_dashed_dark_1        = gcamoutput_diffs[["Ref_HD_HPR_HP"]],
        scen_dashed_dark_name_1   = "GCAM HD-HPR",
        scen_dashed_light_1       = gcamoutput_diffs[["Ref_HD_LPR_HP"]],
        scen_dashed_light_name_1  = "GCAM HD-LPR",
        scen_dotted_dark_1        = gcamoutput_diffs[["Ref_LD_HPR_HP"]],
        scen_dotted_dark_name_1   = "GCAM LD-HPR",
        scen_dotted_light_1       = gcamoutput_diffs[["Ref_LD_LPR_HP"]],
        scen_dotted_light_name_1  = "GCAM LD-LPR",
        
        # ambrosia (set 2, blue)
        scen_solid_2              = amboutput_diffs[["Ref_ML_MLparams"]],
        scen_solid_name_2         = "ambrosia ML",
        scen_dashed_dark_2        = amboutput_diffs[["Ref_ML_HDHPRparams"]],
        scen_dashed_dark_name_2   = "ambrosia HD-HPR",
        scen_dashed_light_2       = amboutput_diffs[["Ref_ML_HDLPRparams"]],
        scen_dashed_light_name_2  = "ambrosia HD-LPR",
        scen_dotted_dark_2        = amboutput_diffs[["Ref_ML_LDHPRparams"]],
        scen_dotted_dark_name_2   = "ambrosia LD-HPR",
        scen_dotted_light_2       = amboutput_diffs[["Ref_ML_LDLPRparams"]],
        scen_dotted_light_name_2  = "ambrosia LD-LPR",
        
        ci_level = 0.90,
        return_data = TRUE
      ) %>%
        mutate(region_label = paste0("Region ", region_id))
      
      # Accumulate results
      p_all$demand_regional_decile_diff_MLHP <- 
        bind_rows(p_all$demand_regional_decile_diff_MLHP, p_reg_dec)
      
      # plain version: with ambrosia ML scenario only
      p_reg_dec <- plot_region_and_decile_demand_comparison(
        demand_reg = reg_ens_bc_ML,
        reg_num = region_id,
        
        # ambrosia (set 2, blue)
        scen_solid_2              = amboutput_diffs[["Ref_ML_MLparams"]],
        scen_solid_name_2         = "ambrosia ML",

        ci_level = 0.90,
        return_data = TRUE
      ) %>%
        mutate(region_label = paste0("Region ", region_id))
      
      # Accumulate results
      p_all$demand_regional_decile_diff_MLHP_plain <- 
        bind_rows(p_all$demand_regional_decile_diff_MLHP_plain, p_reg_dec)
    }
    
    if(BAR) {
      
      message(" Collecting regional demand differences for bar plot")
      
      # Accumulate demand in target year
      reg_diffs_ens_bc_targetyr1 <- 
        bind_rows(reg_diffs_ens_bc_targetyr1,
                  reg_ens_bc_ML %>% 
                    filter(year == target_year1,
                           `gcam-consumer` == "FoodDemand_Group1"))
      reg_diffs_ens_bc_targetyr2 <- 
        bind_rows(reg_diffs_ens_bc_targetyr2,
                  reg_ens_bc_ML %>% 
                    filter(year == target_year2,
                           `gcam-consumer` == "FoodDemand_Group1"))
    }
    
    if(DECOMP) {
      
      message(" Creating regional demand difference decomposition plot")

      # Load regional decomposition ensemble data
      reg_ens_price <- readRDS(
        file.path(reg_ens_path$MLprice$diff,  
                  paste0("demand_R", region_id, scen_case_diff, ".RDS")))
      reg_ens_income <- readRDS(
        file.path(reg_ens_path$MLincome$diff,  
                  paste0("demand_R", region_id, scen_case_diff, ".RDS")))
      reg_ens_scale <- readRDS(
        file.path(reg_ens_path$MLscale$diff,  
                  paste0("demand_R", region_id, scen_case_diff, ".RDS")))
      
      # Inside the region loop, after loading demand_full and having the 3 sub-ensembles available:
      p_decomp <- plot_regional_uncertainty_decomposition(
        demand_full = reg_ens_bc_ML,
        reg_num     = region_id,
        scen_ml     =  amboutput_diffs[["Ref_ML_MLparams"]],
        scen_ml_name = "Ambrosia ML",
        ens_price   = reg_ens_price,
        ens_income  = reg_ens_income,
        ens_scale   = reg_ens_scale,
        ci_level    = 0.90,
        return_data = TRUE
      )
      
      p_all$demand_regional_diff_decomp_MLHP <- 
        bind_rows(p_all$demand_regional_diff_decomp_MLHP, p_decomp)
      
    }
    
  }
}

# ------------------------------------------------------------------------------
# Produce and save pdfs
# ------------------------------------------------------------------------------

if(ABS_HP) {
  
  message("HP pdfs")
  
  if(DEMAND_REG) {
    
    message("Creating regional demand pdf")
    
    plot_regional_comparison_pdf(
      p_all = p_all[["demand_regional_abs_HP"]],
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = rpt_names[["rpt_name_demand_regional_abs_HP"]]
    )  
  }
  
  if(PRICE) {
    
    message("Creating regional price pdf")
    
    plot_regional_comparison_pdf(
      p_all = p_all[["price_regional_abs_HP"]],
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = rpt_names[["rpt_name_price_regional_abs_HP"]],
      value_cols  = c("Y.region", "Ps", "Pn"),
      value_names = c("Income pc (10^3 1990$)", "Staples price (2005$/Mcal)", 
                      "Non-staples price (2005$/Mcal)"),
      y_label = "Value"
    )
  }
  
  if(DEMAND_DEC) {
    
    message("Creating decile demand pdf")
    
    plot_decile_demand_comparison_pdf(
      p_all = p_all[["demand_decile_abs_HP"]],
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = rpt_names[["rpt_name_demand_decile_abs_HP"]]
    )
  }
  
  if(DEMAND_REG_DEC) {
    
    message("Creating region/decile demand pdf")
    
    plot_region_and_decile_demand_comparison_pdf(
      p_all = p_all[["demand_regional_decile_abs_HP"]],
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = rpt_names[["rpt_name_demand_regional_decile_abs_HP"]]
    )
  }
}

if(ABS_ML) {
  
  message("ML pdfs")
  
  if(DEMAND_REG) {
    
    message("Creating regional demand pdf")
    
    plot_regional_comparison_pdf(
      p_all = p_all[["demand_regional_abs_ML"]],
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = rpt_names[["rpt_name_demand_regional_abs_ML"]]
    )
    
    plot_regional_comparison_pdf(
      p_all = p_all[["demand_regional_abs_ML_plain"]],
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = rpt_names[["rpt_name_demand_regional_abs_ML_plain"]]
    )

    plot_regional_comparison_pdf(
      p_all = p_all[["demand_regional_abs_ML_plain_compare"]],
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = rpt_names[["rpt_name_demand_regional_abs_ML_plain_compare"]]
    )

  }
  
  if (ELAST) {
    
    message("Creating regional elasticity pdf")
    
    plot_regional_elasticity_comparison_pdf(
      p_all = p_all[["elasticity_regional_abs_ML"]],
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = rpt_names[["rpt_name_elasticity_regional_abs_ML"]],
      y_step = 0.1,
      y_label = "Elasticity"
    )
    
    plot_regional_elasticity_comparison_pdf(
      p_all = p_all[["elasticity_regional_abs_ML_plain"]],
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = rpt_names[["rpt_name_elasticity_regional_abs_ML_plain"]],
      y_step = 0.1,
      y_label = "Elasticity"
    )
  }
  
  if(PRICE) {
    
    message("Creating regional price pdf")
    
    plot_regional_comparison_pdf(
      p_all = p_all[["price_regional_abs_ML"]],
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = rpt_names[["rpt_name_price_regional_abs_ML"]],
      value_cols  = c("Y.region", "Ps", "Pn"),
      value_names = c("Income pc (10^3 1990$)", "Staples price (2005$/Mcal)", 
                      "Non-staples price (2005$/Mcal)"),
      y_label = "Value"
    )
    
    plot_regional_comparison_pdf(
      p_all = p_all[["price_regional_abs_ML_plain_compare"]],
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = rpt_names[["rpt_name_price_regional_abs_ML_plain_compare"]],
      value_cols  = c("Y.region", "Ps", "Pn"),
      value_names = c("Income pc (10^3 1990$)", "Staples price (2005$/Mcal)", 
                      "Non-staples price (2005$/Mcal)"),
      y_label = "Value"
    )

  }
  
  if(DEMAND_DEC) {
    
    message("Creating decile demand pdf")
    
    plot_decile_demand_comparison_pdf(
      p_all = p_all[["demand_decile_abs_ML"]],
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = rpt_names[["rpt_name_demand_decile_abs_ML"]]
    )
    
    plot_decile_demand_comparison_pdf(
      p_all = p_all[["demand_decile_abs_ML_plain"]],
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = rpt_names[["rpt_name_demand_decile_abs_ML_plain"]]
    )
    
    plot_decile_demand_comparison_pdf(
      p_all = p_all[["demand_decile_abs_ML_plain_compare"]],
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = rpt_names[["rpt_name_demand_decile_abs_ML_plain_compare"]]
    )
  }
  
  if(DEMAND_REG_DEC) {
    
    message("Creating region/decile demand pdf")
    
    plot_region_and_decile_demand_comparison_pdf(
      p_all = p_all[["demand_regional_decile_abs_ML"]],
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = rpt_names[["rpt_name_demand_regional_decile_abs_ML"]]
    )
    
    plot_region_and_decile_demand_comparison_pdf(
      p_all = p_all[["demand_regional_decile_abs_ML_plain"]],
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = rpt_names[["rpt_name_demand_regional_decile_abs_ML_plain"]]
    )
    
    plot_region_and_decile_demand_comparison_pdf(
      p_all = p_all[["demand_regional_decile_abs_ML_plain_compare"]],
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = rpt_names[["rpt_name_demand_regional_decile_abs_ML_plain_compare"]]
    )
  }
  
  if(BAR) {
    
    message("Creating regional demand bar pdf")
    
    plot_region_ci_bars_pdf(
      df_list = list(
        reg_ens_bc_ML_targetyr1,
        reg_ens_bc_ML_targetyr2),
      value_col = "Qtot.region",
      ci_level = 0.90,
      include_range = FALSE,
      show_median = TRUE,
      sort_by = "range_desc",
      output_dir = output_dir,
      filename = rpt_names[["rpt_name_demand_regional_abs_bar_ML"]],
      y_label = "Total demand (10^3 cal/person/day)"
    )
  }
  
  if(DECOMP) {
    
    message(" Creating regional demand decomposition pdf")

    teal <- "#2A9D8F"
    
    color_override <- c(
      "Price-only CI"  = teal,
      "Income-only CI" = teal,
      "Scale-only CI"  = teal
    )
    
    linetype_override <- c(
      "Price-only CI"  = "dashed",
      "Income-only CI" = "dotted",
      "Scale-only CI"  = "dotdash"
    )
    
    plot_regional_comparison_pdf(
      p_all = p_all[["demand_regional_abs_decomp_ML"]],
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = rpt_names[["rpt_name_demand_regional_abs_decomp_ML"]],
      value_cols  = c("Qs.region", "Qn.region", "Qtot.region"),
      value_names = c("Staples", "Non-staples", "Total"),
      color_override = color_override,
      linetype_override = linetype_override,
      ylimit_mode = "by_region",
      y_label = "Demand rel. to ML (10^3 cal/day)"
    )
  }
  
  if(LAND_WATER) {
    
    message("Creating regional land-water pdfs")
    
    plot_regional_comparison_pdf(
      p_all = p_all[["landwater1_regional_abs_ML"]],
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = rpt_names[["rpt_name_landwater1_regional_abs_ML"]],
      value_cols  = c("cropland", "pasture", "bio_production"),
      value_names = c("Cropland (10^3 km^2)", "Pasture (10^3 km^2)", 
                      "Biomass production (EJ)"),
      y_label = "Value"
    )
    
    plot_regional_comparison_pdf(
      p_all = p_all[["landwater2_regional_abs_ML"]],
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = rpt_names[["rpt_name_landwater2_regional_abs_ML"]],
      value_cols  = c("withdrawals", "forest", "emissions"),
      value_names = c("Water withdrawals (km^3)", "Forest (10^3 km^2)", 
                      "LUC emissions (MtC/yr)"),
      y_label = "Value"
    )
  }
}

if(DIFF) {
  
  if(DEMAND_REG) {
    
    message("Creating regional demand difference pdf")
    
    plot_regional_comparison_pdf(
      p_all = p_all[["demand_regional_diff_MLHP"]],
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = rpt_names[["rpt_name_demand_regional_diff_MLHP"]],
      y_label = "Demand difference (10^3 cal/day)"
    )
    
    plot_regional_comparison_pdf(
      p_all = p_all[["demand_regional_diff_MLHP_plain"]],
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = rpt_names[["rpt_name_demand_regional_diff_MLHP_plain"]],
      y_label = "Demand difference (10^3 cal/day)"
    )
    
    plot_regional_comparison_pdf(
      p_all = p_all[["demand_regional_diff_MLHP_plain_compare"]],
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = rpt_names[["rpt_name_demand_regional_diff_MLHP_plain_compare"]],
      y_label = "Demand difference (10^3 cal/day)"
    )
  }
  
  if (ELAST) {
    
    message("Creating regional elasticity difference pdf")
    
    plot_regional_elasticity_comparison_pdf(
      p_all = p_all[["elasticity_regional_diff_MLHP"]],
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = rpt_names[["rpt_name_elasticity_regional_diff_MLHP"]],
      y_step = 0.1,
      y_label = "Elasticity difference"
    )
    
    plot_regional_elasticity_comparison_pdf(
      p_all = p_all[["elasticity_regional_diff_MLHP_plain"]],
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = rpt_names[["rpt_name_elasticity_regional_diff_MLHP_plain"]],
      y_step = 0.1,
      y_label = "Elasticity difference"
    )
  }
  
  if(PRICE) {
    
    message("Creating regional price difference pdf")
    
    plot_regional_comparison_pdf(
      p_all = p_all[["price_regional_diff_MLHP"]],
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = rpt_names[["rpt_name_price_regional_diff_MLHP"]],
      value_cols  = c("Y.region", "Ps", "Pn"),
      value_names = c("Income pc (10^3 1990$)", "Staples price (2005$/Mcal)", 
                      "Non-staples price (2005$/Mcal)"),
      y_label = "Value"
    )
    
    plot_regional_comparison_pdf(
      p_all = p_all[["price_regional_diff_MLHP_plain_compare"]],
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = rpt_names[["rpt_name_price_regional_diff_MLHP_plain_compare"]],
      value_cols  = c("Y.region", "Ps", "Pn"),
      value_names = c("Income pc (10^3 1990$)", "Staples price (2005$/Mcal)", 
                      "Non-staples price (2005$/Mcal)"),
      y_label = "Value"
    )
  }
  
  if(DEMAND_DEC) {
    
    message("Creating decile demand difference pdf")
    
    plot_decile_demand_comparison_pdf(
      p_all = p_all[["demand_decile_diff_MLHP"]],
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = rpt_names[["rpt_name_demand_decile_diff_MLHP"]],
      y_label = "Demand difference (10^3 cal/day)"
    )
    
    plot_decile_demand_comparison_pdf(
      p_all = p_all[["demand_decile_diff_MLHP_plain"]],
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = rpt_names[["rpt_name_demand_decile_diff_MLHP_plain"]],
      y_label = "Demand difference (10^3 cal/day)"
    )
  }
  
  if(DEMAND_REG_DEC) {
    
    message("Creating region/decile demand difference pdf")
    
    plot_region_and_decile_demand_comparison_pdf(
      p_all = p_all[["demand_regional_decile_diff_MLHP"]],
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = rpt_names[["rpt_name_demand_regional_decile_diff_MLHP"]],
      y_label = "Demand difference (10^3 cal/day)"
    )
    
    plot_region_and_decile_demand_comparison_pdf(
      p_all = p_all[["demand_regional_decile_diff_MLHP_plain"]],
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = rpt_names[["rpt_name_demand_regional_decile_diff_MLHP_plain"]],
      y_label = "Demand difference (10^3 cal/day)"
    )
  }
  
  if(BAR) {
    
    message("Creating regional demand difference bar pdf")
    
    # Demand differences in target years, central 90% CI, sorted by median
    plot_region_ci_bars_pdf(
      df_list = list(
        reg_diffs_ens_bc_targetyr1,
        reg_diffs_ens_bc_targetyr2),
      value_col = "Qtot.region",
      ci_level = 0.90,
      include_range = FALSE,
      show_median = TRUE,
      sort_by = "range_desc",
      output_dir = output_dir,
      filename = rpt_names[["rpt_name_demand_regional_diff_bar_MLHP"]],
      y_label = "Total demand difference (10^3 cal/person/day)"
    )
  }
  
  if(DECOMP) {
    
    message("Creating regional demand difference decomposition pdf")

    teal <- "#2A9D8F"
    
    color_override <- c(
      "Price-only CI"  = teal,
      "Income-only CI" = teal,
      "Scale-only CI"  = teal
    )
    
    linetype_override <- c(
      "Price-only CI"  = "dashed",
      "Income-only CI" = "dotted",
      "Scale-only CI"  = "dotdash"
    )
    
    plot_regional_comparison_pdf(
      p_all = p_all[["demand_regional_diff_decomp_MLHP"]],
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = rpt_names[["rpt_name_demand_regional_diff_decomp_MLHP"]],
      value_cols  = c("Qs.region", "Qn.region", "Qtot.region"),
      value_names = c("Staples", "Non-staples", "Total"),
      color_override = color_override,
      linetype_override = linetype_override,
      ylimit_mode = "by_region",
      y_label = "Demand difference rel. to ML (10^3 cal/day)"
    )
  }
}
