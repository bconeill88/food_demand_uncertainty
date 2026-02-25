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
DEMAND_REG <- FALSE
DEMAND_DEC <- FALSE
DEMAND_REG_DEC <- TRUE
BAR <- FALSE
target_year = 2050
ELAST <- FALSE
PRICE <- FALSE  # and income
DECOMP <- FALSE

# Indicate whether to create absolute or difference plots
ABS <- TRUE
DIFF <- TRUE

# Define directories and file names

# Directories containing regional ensemble results
reg_results_path_abs <- 
  file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam, 
            demand_abs_subdir)
reg_results_path_diff <- 
  file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam, 
            demand_diffs_subdir)
reg_results_path_price_abs <-
  file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam_price,
            demand_abs_subdir)
reg_results_path_price_diff <-
  file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam_price,
            demand_diffs_subdir)
reg_results_path_income_abs <-
  file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam_income,
            demand_abs_subdir)
reg_results_path_income_diff <-
  file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam_income,
            demand_diffs_subdir)
reg_results_path_scale_abs <-
  file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam_scale,
            demand_abs_subdir)
reg_results_path_scale_diff <-
  file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam_scale,
            demand_diffs_subdir)

# Directory for report result
output_dir <- file.path("output", "reports", procdata_dir, procdata_subdir_RefMLgcam)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Report file names; "_plain" indicates only range, CI, and ambrosia ML scenario 
# are included, otherwise additional ambrosia and GCAM scenarios are included.
report_name_regional_abs <- "demand_regional_ens_bc_ML.pdf"
report_name_regional_abs_plain <- "demand_regional_ens_bc_ML_plain.pdf"
report_name_regional_abs_plain_compare <- "demand_regional_ens_bc_ML_plain_compare.pdf"
report_name_regional_abs_bar <- "demand_regional_bar_ens_bc_ML.pdf"
report_name_regional_elast_abs <- "elasticities_regional_ens_bc_ML.pdf"
report_name_regional_elast_abs_plain <- "elasticities_regional_ens_bc_ML_plain.pdf"
report_name_regional_price_abs_plain_compare <- "prices_regional_ens_bc_ML_plain_compare.pdf"
report_name_regional_diff <- "demand_diffs_regional_ens_bc.pdf"
report_name_regional_diff_plain <- "demand_diffs_regional_ens_bc_plain.pdf"
report_name_regional_diff_plain_compare <- "demand_diffs_regional_ens_bc_plain_compare.pdf"
report_name_regional_diff_bar <- "demand_diffs_regional_bar_ens_bc.pdf"
report_name_regional_elast_diff <- "elasticities_diffs_regional_ens_bc.pdf"
report_name_regional_elast_diff_plain <- "elasticities_diffs_regional_ens_bc_plain.pdf"
report_name_regional_price_diff_plain_compare <- "prices_diffs_regional_ens_bc_ML_plain_compare.pdf"
report_name_decile_abs <- "demand_decile_ens_bc_ML.pdf"
report_name_decile_abs_plain <- "demand_decile_ens_bc_ML_plain.pdf"
report_name_decile_abs_plain_compare <- "demand_decile_ens_bc_ML_plain_compare.pdf"
report_name_decile_price_abs_plain_compare <- "prices_decile_ens_bc_plain_compare.pdf"
report_name_decile_diff <- "demand_diffs_decile_ens_bc.pdf"
report_name_decile_diff_plain <- "demand_diffs_decile_ens_bc_plain.pdf"
report_name_decile_price_diff_plain_compare <- "prices_diffs_decile_ens_bc_plain_compare.pdf"
report_name_regional_decile_abs <- "demand_regional_decile_ens_bc_ML.pdf"
report_name_regional_decile_abs_plain <- "demand_regional_decile_ens_bc_ML_plain.pdf"
report_name_regional_decile_abs_plain_compare <- "demand_regional_decile_ens_bc_ML_plain_compare.pdf"
report_name_regional_decile_diff <- "demand_diffs_regional_decile_ens_bc.pdf"
report_name_regional_decile_diff_plain <- "demand_diffs_regional_decile_ens_bc_plain.pdf"
report_name_regional_decomp_abs <- "demand_decomp_ens_bc_ML.pdf"
report_name_regional_decomp_diff <- "demand_diffs_decomp_ens_bc.pdf"

# Scenario case to use in file names
scen_case_abs <- "_ens_bc"
scen_case_diff <- "_diffs_ens_bc"

# Regions and deciles to plot
region_list <- c(1:32)
target_deciles <- c("FoodDemand_Group1", "FoodDemand_Group2", "FoodDemand_Group3",
                    "FoodDemand_Group6", "FoodDemand_Group10")

# Load demand projections for individual scenarios
if(ABS) {
  
  # GCAM results, from files produced by the 101 script
  gcamoutput_Ref_ML <-
    readRDS(file.path("data", "processed", procdata_dir, gcam_results_dir,
                      "gcamoutput_Ref_ML.RDS"))
  gcamoutput_Ref_HD_HPR <-
    readRDS(file.path("data", "processed", procdata_dir, gcam_results_dir,
                      "gcamoutput_Ref_HD_HPR.RDS"))
  gcamoutput_Ref_HD_LPR <-
    readRDS(file.path("data", "processed", procdata_dir, gcam_results_dir,
                      "gcamoutput_Ref_HD_LPR.RDS"))
  gcamoutput_Ref_LD_HPR <-
    readRDS(file.path("data", "processed", procdata_dir, gcam_results_dir,
                      "gcamoutput_Ref_LD_HPR.RDS"))
  gcamoutput_Ref_LD_LPR <-
    readRDS(file.path("data", "processed", procdata_dir, gcam_results_dir,
                      "gcamoutput_Ref_LD_LPR.RDS"))
  # gcamoutput_Ref_HD <-
  #   readRDS(file.path("data", "processed", procdata_dir, gcam_results_dir,
  #                     "gcamoutput_Ref_HD.RDS"))
  # gcamoutput_Ref_LD <-
  #   readRDS(file.path("data", "processed", procdata_dir, gcam_results_dir,
  #                     "gcamoutput_Ref_LD.RDS"))
  
  # ambrosia results, from files produced by the 07 parameter for intervals script
  amboutput_Ref_ML_MLparams <-
    readRDS(file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam,
                      demand_both_subdir, "scenarios_abs", "demand_allregions_MLparams.RDS"))
  amboutput_Ref_ML_HDHPRparams <-
    readRDS(file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam,
                      demand_both_subdir, "scenarios_abs", "demand_allregions_HD_HPR_Qtot.RDS"))
  amboutput_Ref_ML_HDLPRparams <-
    readRDS(file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam,
                      demand_both_subdir, "scenarios_abs", "demand_allregions_HD_LPR_Qtot.RDS"))
  amboutput_Ref_ML_LDHPRparams <-
    readRDS(file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam,
                      demand_both_subdir, "scenarios_abs", "demand_allregions_LD_HPR_Qtot.RDS"))
  amboutput_Ref_ML_LDLPRparams <-
    readRDS(file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam,
                      demand_both_subdir, "scenarios_abs", "demand_allregions_LD_LPR_Qtot.RDS"))
  # amboutput_Ref_ML_MLparams <-
  #   readRDS(file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam,
  #                     "demand_allregions_MLparams.RDS"))
  # amboutput_Ref_ML_HDparams <-
  #   readRDS(file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam,
  #                     "demand_allregions_HDparams.RDS"))
  # amboutput_Ref_ML_LDparams <-
  #   readRDS(file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam,
  #                     "demand_allregions_LDparams.RDS"))
  # amboutput_Ref_ML_HPRparams <-
  #   readRDS(file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam,
  #                     "demand_allregions_HPRparams.RDS"))
  # amboutput_Ref_ML_LPRparams <-
  #   readRDS(file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam,
  #                     "demand_allregions_LPRparams.RDS"))
} 

if(DIFF) {
    
  # GCAM results, from files produced by the 101 script
  gcamoutput_diffs_Ref_ML_HP <-
    readRDS(file.path("data", "processed", procdata_dir, gcam_results_dir,
                      "gcamoutput_diffs_Ref_ML_HP.RDS"))
  gcamoutput_diffs_Ref_HD_HPR_HP <-
    readRDS(file.path("data", "processed", procdata_dir, gcam_results_dir,
                      "gcamoutput_diffs_Ref_HD_HPR_HP.RDS"))
  gcamoutput_diffs_Ref_HD_LPR_HP <-
    readRDS(file.path("data", "processed", procdata_dir, gcam_results_dir,
                      "gcamoutput_diffs_Ref_HD_LPR_HP.RDS"))
  gcamoutput_diffs_Ref_LD_HPR_HP <-
    readRDS(file.path("data", "processed", procdata_dir, gcam_results_dir,
                      "gcamoutput_diffs_Ref_LD_HPR_HP.RDS"))
  gcamoutput_diffs_Ref_LD_LPR_HP <-
    readRDS(file.path("data", "processed", procdata_dir, gcam_results_dir,
                      "gcamoutput_diffs_Ref_LD_LPR_HP.RDS"))
  
  # ambrosia results, from files produced by the 07 parameter for intervals script
  # amboutput_diffs_Ref_ML_MLparams <- 
  #   readRDS(file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam,
  #                     demand_diffs_subdir, "demand_diffs_allregions_MLparams.RDS"))
  # amboutput_diffs_Ref_ML_HPRparams <- 
  #   readRDS(file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam,
  #                     demand_diffs_subdir, "demand_diffs_allregions_HPRparams.RDS"))
  # amboutput_diffs_Ref_ML_LPRparams <- 
  #   readRDS(file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam,
  #                     demand_diffs_subdir, "demand_diffs_allregions_LPRparams.RDS"))
  amboutput_diffs_Ref_ML_MLparams <-
    readRDS(file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam,
                      demand_both_subdir, "scenarios_diff", 
                      "demand_diffs_allregions_MLparams.RDS"))
  amboutput_diffs_Ref_ML_HDHPRparams <-
    readRDS(file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam,
                      demand_both_subdir, "scenarios_diff", 
                      "demand_diffs_allregions_HD_HPR_Qtot.RDS"))
  amboutput_diffs_Ref_ML_HDLPRparams <-
    readRDS(file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam,
                      demand_both_subdir, "scenarios_diff", 
                      "demand_diffs_allregions_HD_LPR_Qtot.RDS"))
  amboutput_diffs_Ref_ML_LDHPRparams <-
    readRDS(file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam,
                      demand_both_subdir, "scenarios_diff", 
                      "demand_diffs_allregions_LD_HPR_Qtot.RDS"))
  amboutput_diffs_Ref_ML_LDLPRparams <-
    readRDS(file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam,
                      demand_both_subdir, "scenarios_diff", 
                      "demand_diffs_allregions_LD_LPR_Qtot.RDS"))
}

# Initialize dfs of regional plotting data
p_all_regional_abs <- p_all_regional_abs_plain <- 
  p_all_regional_abs_plain_compare <- data.frame()
p_all_regional_diff <- p_all_regional_diff_plain <- 
  p_all_regional_diff_plain_compare <- data.frame()
p_all_regional_elast_abs <- p_all_regional_elast_abs_plain <- data.frame()
p_all_regional_elast_diff <- p_all_regional_elast_diff_plain <- data.frame()
p_all_regional_price_abs <- p_all_regional_price_abs_plain <- 
  p_all_regional_price_abs_plain_compare <- data.frame()
p_all_regional_price_diff <- p_all_regional_price_diff_plain <- 
  p_all_regional_price_diff_plain_compare <- data.frame()
p_all_decile_abs <- p_all_decile_abs_plain <- 
  p_all_decile_abs_plain_compare <- data.frame()
p_all_decile_diff <- p_all_decile_diff_plain <- data.frame()
p_all_regional_decile_abs <- p_all_regional_decile_abs_plain <- 
  p_all_regional_decile_abs_plain_compare <- data.frame()
p_all_regional_decile_diff <- p_all_regional_decile_diff_plain <- data.frame()
reg_ens_bc_targetyr <- data.frame()
reg_diffs_ens_bc_targetyr <- data.frame()
p_all_regional_decomp_abs <- data.frame()
p_all_regional_decomp_diff <- data.frame()

# Create plots by region
for(region_id in region_list) {
  
  message("Region ", region_id)
  
  if(ABS) {
    
    # Load regional ensemble data
    reg_ens_bc <- readRDS(
      file.path(reg_results_path_abs,  
                paste0("demand_R", region_id, scen_case_abs, ".RDS")))
    
    if(DEMAND_REG) {
      
      message(" Creating regional demand plot")
      
      # # with four scenarios each from ambrosia and GCAM
      p_reg <- plot_regional_comparison(
        demand_reg = reg_ens_bc,
        reg_num    = region_id,
        ci_level   = 0.90,

        # GCAM (set 1, orange)
        scen_solid_1              = gcamoutput_Ref_ML,
        scen_solid_name_1         = "GCAM ML",
        scen_dashed_dark_1        = gcamoutput_Ref_HD_HPR,
        scen_dashed_dark_name_1   = "GCAM HD-HPR",
        scen_dashed_light_1       = gcamoutput_Ref_HD_LPR,
        scen_dashed_light_name_1  = "GCAM HD-LPR",
        scen_dotted_dark_1        = gcamoutput_Ref_LD_HPR,
        scen_dotted_dark_name_1   = "GCAM LD-HPR",
        scen_dotted_light_1       = gcamoutput_Ref_LD_LPR,
        scen_dotted_light_name_1  = "GCAM LD-LPR",

        # ambrosia (set 2, blue)
        scen_solid_2              = amboutput_Ref_ML_MLparams,
        scen_solid_name_2         = "ambrosia ML",
        scen_dashed_dark_2        = amboutput_Ref_ML_HDHPRparams,
        scen_dashed_dark_name_2   = "ambrosia HD-HPR",
        scen_dashed_light_2       = amboutput_Ref_ML_HDLPRparams,
        scen_dashed_light_name_2  = "ambrosia HD-LPR",
        scen_dotted_dark_2        = amboutput_Ref_ML_LDHPRparams,
        scen_dotted_dark_name_2   = "ambrosia LD-HPR",
        scen_dotted_light_2       = amboutput_Ref_ML_LDLPRparams,
        scen_dotted_light_name_2  = "ambrosia LD-LPR",

        return_data = TRUE   # or FALSE to get a ggplot
      )

      # Accumulate results
      p_all_regional_abs <- bind_rows(p_all_regional_abs, p_reg)

      # plain version: ambrosia ML scenario only
      p_reg <- plot_regional_comparison(
        demand_reg = reg_ens_bc,
        reg_num    = region_id,
        ci_level   = 0.90,

        # ambrosia (set 2)
        scen_solid_2              = amboutput_Ref_ML_MLparams,
        scen_solid_name_2         = "ambrosia ML",

        return_data = TRUE   # or FALSE to get a ggplot
      )

      # Accumulate results
      p_all_regional_abs_plain <- bind_rows(p_all_regional_abs_plain, p_reg)
      
      # plain version with comparison: ambrosia and GCAM ML scenarios only
      p_reg <- plot_regional_comparison(
        demand_reg = reg_ens_bc,
        reg_num    = region_id,
        ci_level   = 0.90,
        
        # GCAM (set 1, orange)
        scen_solid_1              = gcamoutput_Ref_ML,
        scen_solid_name_1         = "GCAM ML",

        # ambrosia (set 2)
        scen_solid_2              = amboutput_Ref_ML_MLparams,
        scen_solid_name_2         = "ambrosia ML",
        
        return_data = TRUE   # or FALSE to get a ggplot
      )
      
      # Accumulate results
      p_all_regional_abs_plain_compare <- bind_rows(p_all_regional_abs_plain_compare, p_reg)
    }
    
    if (ELAST) {
      
      message(" Creating regional elasticity plot")
      
      # with four scenarios each from ambrosia and GCAM
      p_el <- plot_regional_elasticity_comparison(
        elast_reg = reg_ens_bc,
        reg_num   = region_id,
        ci_level  = 0.90,
        
        # ambrosia (set 2, blue)
        scen_solid_2              = amboutput_Ref_ML_MLparams,
        scen_solid_name_2         = "ambrosia ML",
        scen_dashed_dark_2        = amboutput_Ref_ML_HDHPRparams,
        scen_dashed_dark_name_2   = "ambrosia HD-HPR",
        scen_dashed_light_2       = amboutput_Ref_ML_HDLPRparams,
        scen_dashed_light_name_2  = "ambrosia HD-LPR",
        scen_dotted_dark_2        = amboutput_Ref_ML_LDHPRparams,
        scen_dotted_dark_name_2   = "ambrosia LD-HPR",
        scen_dotted_light_2       = amboutput_Ref_ML_LDLPRparams,
        scen_dotted_light_name_2  = "ambrosia LD-LPR",
        
        return_data = TRUE
      )
      
      # Accumulate results
      p_all_regional_elast_abs <- bind_rows(p_all_regional_elast_abs, p_el)
      
      # plain version: ambrosia ML scenario only
      p_el <- plot_regional_elasticity_comparison(
        elast_reg = reg_ens_bc,
        reg_num   = region_id,
        ci_level  = 0.90,
        
        # ambrosia (set 2, blue)
        scen_solid_2              = amboutput_Ref_ML_MLparams,
        scen_solid_name_2         = "ambrosia ML",

        return_data = TRUE
      )
      
      # Accumulate results
      p_all_regional_elast_abs_plain <- 
        bind_rows(p_all_regional_elast_abs_plain, p_el)
    }
    
    if(PRICE) {
      
      message(" Creating regional price plot")

      # plain version with comparison: ambrosia and GCAM ML scenarios only
      p_reg <- plot_regional_comparison(
        demand_reg = reg_ens_bc,
        reg_num    = region_id,
        ci_level   = 0.90,
        
        value_cols  = c("Y.region", "Ps", "Pn"),
        value_names = c("Income pc (region)", "Staples price (Ps)", "Non-staples price (Pn)"),
        
        # GCAM (set 1, orange)
        scen_solid_1              = gcamoutput_Ref_ML,
        scen_solid_name_1         = "GCAM ML",
        
        # ambrosia (set 2)
        scen_solid_2              = amboutput_Ref_ML_MLparams,
        scen_solid_name_2         = "ambrosia ML",
        
        return_data = TRUE   # or FALSE to get a ggplot
      )
      
      # Accumulate results
      p_all_regional_price_abs_plain_compare <- 
        bind_rows(p_all_regional_price_abs_plain_compare, p_reg)
      
    }
    
    if(DEMAND_DEC) {
      
      message(" Creating decile demand plot")
      
      # with four scenarios each from ambrosia and GCAM
      p_dec <- map_dfr(target_deciles, function(cg) {
        plot_decile_demand_comparison(
          demand_reg = reg_ens_bc,
          reg_num = region_id,
          consumer_group = cg,

          # GCAM (set 1, orange)
          scen_solid_1              = gcamoutput_Ref_ML,
          scen_solid_name_1         = "GCAM ML",
          scen_dashed_dark_1        = gcamoutput_Ref_HD_HPR,
          scen_dashed_dark_name_1   = "GCAM HD-HPR",
          scen_dashed_light_1       = gcamoutput_Ref_HD_LPR,
          scen_dashed_light_name_1  = "GCAM HD-LPR",
          scen_dotted_dark_1        = gcamoutput_Ref_LD_HPR,
          scen_dotted_dark_name_1   = "GCAM LD-HPR",
          scen_dotted_light_1       = gcamoutput_Ref_LD_LPR,
          scen_dotted_light_name_1  = "GCAM LD-LPR",

          # ambrosia (set 2, blue)
          scen_solid_2              = amboutput_Ref_ML_MLparams,
          scen_solid_name_2         = "ambrosia ML",
          scen_dashed_dark_2        = amboutput_Ref_ML_HDHPRparams,
          scen_dashed_dark_name_2   = "ambrosia HD-HPR",
          scen_dashed_light_2       = amboutput_Ref_ML_HDLPRparams,
          scen_dashed_light_name_2  = "ambrosia HD-LPR",
          scen_dotted_dark_2        = amboutput_Ref_ML_LDHPRparams,
          scen_dotted_dark_name_2   = "ambrosia LD-HPR",
          scen_dotted_light_2       = amboutput_Ref_ML_LDLPRparams,
          scen_dotted_light_name_2  = "ambrosia LD-LPR",

          ci_level = 0.90,
          return_data = TRUE
        ) %>%
          mutate(region_label = paste0("Region ", region_id))
      })

      # Accumulate results
      p_all_decile_abs <- bind_rows(p_all_decile_abs, p_dec)

      # plain version: ambrosia ML scenario only
      p_dec <- map_dfr(target_deciles, function(cg) {
        plot_decile_demand_comparison(
          demand_reg = reg_ens_bc,
          reg_num = region_id,
          consumer_group = cg,

          # ambrosia (set 2, blue)
          scen_solid_2              = amboutput_Ref_ML_MLparams,
          scen_solid_name_2         = "ambrosia ML",

          ci_level = 0.90,
          return_data = TRUE
        ) %>%
          mutate(region_label = paste0("Region ", region_id))
      })

      # Accumulate results
      p_all_decile_abs_plain <- bind_rows(p_all_decile_abs_plain, p_dec)

      # plain version with compare: ambrosia and GCAM ML scenarios only
      p_dec <- map_dfr(target_deciles, function(cg) {
        plot_decile_demand_comparison(
          demand_reg = reg_ens_bc,
          reg_num = region_id,
          consumer_group = cg,
          
          # GCAM (set 1, orange)
          scen_solid_1              = gcamoutput_Ref_ML,
          scen_solid_name_1         = "GCAM ML",
          
          # ambrosia (set 2, blue)
          scen_solid_2              = amboutput_Ref_ML_MLparams,
          scen_solid_name_2         = "ambrosia ML",
          
          ci_level = 0.90,
          return_data = TRUE
        ) %>%
          mutate(region_label = paste0("Region ", region_id))
      })
      
      # Accumulate results
      p_all_decile_abs_plain_compare <- bind_rows(p_all_decile_abs_plain_compare, p_dec)
    }
    
    if(DEMAND_REG_DEC) {
      
      message(" Creating region/decile demand plot")
      
      # with four scenarios each from ambrosia and GCAM
      p_reg_dec <- plot_region_and_decile_demand_comparison(
        demand_reg = reg_ens_bc,
        reg_num    = region_id,
        
        # GCAM (set 1, orange)
        scen_solid_1              = gcamoutput_Ref_ML,
        scen_solid_name_1         = "GCAM ML",
        scen_dashed_dark_1        = gcamoutput_Ref_HD_HPR,
        scen_dashed_dark_name_1   = "GCAM HD-HPR",
        scen_dashed_light_1       = gcamoutput_Ref_HD_LPR,
        scen_dashed_light_name_1  = "GCAM HD-LPR",
        scen_dotted_dark_1        = gcamoutput_Ref_LD_HPR,
        scen_dotted_dark_name_1   = "GCAM LD-HPR",
        scen_dotted_light_1       = gcamoutput_Ref_LD_LPR,
        scen_dotted_light_name_1  = "GCAM LD-LPR",
        
        # ambrosia (set 2, blue)
        scen_solid_2              = amboutput_Ref_ML_MLparams,
        scen_solid_name_2         = "ambrosia ML",
        scen_dashed_dark_2        = amboutput_Ref_ML_HDHPRparams,
        scen_dashed_dark_name_2   = "ambrosia HD-HPR",
        scen_dashed_light_2       = amboutput_Ref_ML_HDLPRparams,
        scen_dashed_light_name_2  = "ambrosia HD-LPR",
        scen_dotted_dark_2        = amboutput_Ref_ML_LDHPRparams,
        scen_dotted_dark_name_2   = "ambrosia LD-HPR",
        scen_dotted_light_2       = amboutput_Ref_ML_LDLPRparams,
        scen_dotted_light_name_2  = "ambrosia LD-LPR",
        
        ci_level    = 0.90,
        return_data = TRUE
      ) %>%
        mutate(region_label = paste0("Region ", region_id))
      
      # Accumulate results
      p_all_regional_decile_abs <- bind_rows(p_all_regional_decile_abs, p_reg_dec)
      
      # plain version: ambrosia ML scenario only
      p_reg_dec <- plot_region_and_decile_demand_comparison(
        demand_reg = reg_ens_bc,
        reg_num    = region_id,
        
        # ambrosia (set 2, blue)
        scen_solid_2              = amboutput_Ref_ML_MLparams,
        scen_solid_name_2         = "ambrosia ML",

        ci_level    = 0.90,
        return_data = TRUE
      ) %>%
        mutate(region_label = paste0("Region ", region_id))
      
      # Accumulate results
      p_all_regional_decile_abs_plain <- bind_rows(p_all_regional_decile_abs_plain, p_reg_dec)
      
      # plain version with compare: ambrosia and GCAM ML scenarios only
      p_reg_dec <- plot_region_and_decile_demand_comparison(
        demand_reg = reg_ens_bc,
        reg_num    = region_id,
        
        # GCAM (set 1, orange)
        scen_solid_1              = gcamoutput_Ref_ML,
        scen_solid_name_1         = "GCAM ML",

        # ambrosia (set 2, blue)
        scen_solid_2              = amboutput_Ref_ML_MLparams,
        scen_solid_name_2         = "ambrosia ML",

        ci_level    = 0.90,
        return_data = TRUE
      ) %>%
        mutate(region_label = paste0("Region ", region_id))
      
      # Accumulate results
      p_all_regional_decile_abs_plain_compare <- 
        bind_rows(p_all_regional_decile_abs_plain_compare, p_reg_dec)
    }
    
    if(BAR) {
      
      message(" Collecting regional demand for bar plot")
      
      # Accumulate demand in target year
      reg_ens_bc_targetyr <- 
        bind_rows(reg_ens_bc_targetyr,
                  reg_ens_bc %>% 
                    filter(year == target_year,
                           `gcam-consumer` == "FoodDemand_Group1"))
    }
    
    if(DECOMP) {
      
      # Load regional decomposition ensemble data
      reg_ens_price <- readRDS(
        file.path(reg_results_path_price_abs,  
                  paste0("demand_R", region_id, scen_case_abs, ".RDS")))
      reg_ens_income <- readRDS(
        file.path(reg_results_path_income_abs,  
                  paste0("demand_R", region_id, scen_case_abs, ".RDS")))
      reg_ens_scale <- readRDS(
        file.path(reg_results_path_scale_abs,  
                  paste0("demand_R", region_id, scen_case_abs, ".RDS")))
      
      # Inside the region loop, after loading demand_full and having the 3 sub-ensembles available:
      p_decomp <- plot_regional_uncertainty_decomposition(
        demand_full = reg_ens_bc,
        reg_num     = region_id,
        scen_ml     = amboutput_Ref_ML_MLparams,
        scen_ml_name = "Ambrosia ML",
        ens_price   = reg_ens_price,
        ens_income  = reg_ens_income,
        ens_scale   = reg_ens_scale,
        ci_level    = 0.90,
        return_data = TRUE
      )
      
      p_all_regional_decomp_abs <- bind_rows(p_all_regional_decomp_abs, p_decomp)
      
    }
  }
  
  if(DIFF) {
    
    # Load regional ensemble data
    reg_ens_bc <- readRDS(
      file.path(reg_results_path_diff,  
                paste0("demand_R", region_id, scen_case_diff, ".RDS")))
    
    if(DEMAND_REG) {
      
      message(" Creating regional demand difference plot")
      
      # with four scenarios each from ambrosia and GCAM
      p_reg <- plot_regional_comparison(
        demand_reg = reg_ens_bc,
        reg_num    = region_id,
        ci_level   = 0.90,
        
        # GCAM (set 1, orange)
        scen_solid_1              = gcamoutput_diffs_Ref_ML_HP,
        scen_solid_name_1         = "GCAM ML",
        scen_dashed_dark_1        = gcamoutput_diffs_Ref_HD_HPR_HP,
        scen_dashed_dark_name_1   = "GCAM HD-HPR",
        scen_dashed_light_1       = gcamoutput_diffs_Ref_HD_LPR_HP,
        scen_dashed_light_name_1  = "GCAM HD-LPR",
        scen_dotted_dark_1        = gcamoutput_diffs_Ref_LD_HPR_HP,
        scen_dotted_dark_name_1   = "GCAM LD-HPR",
        scen_dotted_light_1       = gcamoutput_diffs_Ref_LD_LPR_HP,
        scen_dotted_light_name_1  = "GCAM LD-LPR",
        
        # ambrosia (set 2, blue)
        scen_solid_2              = amboutput_diffs_Ref_ML_MLparams,
        scen_solid_name_2         = "ambrosia ML",
        scen_dashed_dark_2        = amboutput_diffs_Ref_ML_HDHPRparams,
        scen_dashed_dark_name_2   = "ambrosia HD-HPR",
        scen_dashed_light_2       = amboutput_diffs_Ref_ML_HDLPRparams,
        scen_dashed_light_name_2  = "ambrosia HD-LPR",
        scen_dotted_dark_2        = amboutput_diffs_Ref_ML_LDHPRparams,
        scen_dotted_dark_name_2   = "ambrosia LD-HPR",
        scen_dotted_light_2       = amboutput_diffs_Ref_ML_LDLPRparams,
        scen_dotted_light_name_2  = "ambrosia LD-LPR",
        
        return_data = TRUE   # or FALSE to get a ggplot
      )
      
      # Accumulate results
      p_all_regional_diff <- bind_rows(p_all_regional_diff, p_reg)
      
      # plain version: with ambrosia ML scenario only
      p_reg <- plot_regional_comparison(
        demand_reg = reg_ens_bc,
        reg_num    = region_id,
        ci_level   = 0.90,
        
        # ambrosia (set 2, blue)
        scen_solid_2              = amboutput_diffs_Ref_ML_MLparams,
        scen_solid_name_2         = "ambrosia ML",

        return_data = TRUE   # or FALSE to get a ggplot
      )
      
      # Accumulate results
      p_all_regional_diff_plain <- bind_rows(p_all_regional_diff_plain, p_reg)
      
      # plain version with compare: ambrosia and GCAM ML scenarios only
      p_reg <- plot_regional_comparison(
        demand_reg = reg_ens_bc,
        reg_num    = region_id,
        ci_level   = 0.90,
        
        # GCAM (set 1, orange)
        scen_solid_1              = gcamoutput_diffs_Ref_ML_HP,
        scen_solid_name_1         = "GCAM ML",

        # ambrosia (set 2, blue)
        scen_solid_2              = amboutput_diffs_Ref_ML_MLparams,
        scen_solid_name_2         = "ambrosia ML",
        
        return_data = TRUE   # or FALSE to get a ggplot
      )
      
      # Accumulate results
      p_all_regional_diff_plain_compare <- 
        bind_rows(p_all_regional_diff_plain_compare, p_reg)
      
    }
    
    if (ELAST) {
      
      message(" Creating regional elasticity difference plot")
      
      # with four scenarios each from ambrosia and GCAM
      p_el <- plot_regional_elasticity_comparison(
        elast_reg = reg_ens_bc,
        reg_num   = region_id,
        ci_level  = 0.90,
        
        # ambrosia (set 2, blue)
        scen_solid_2              = amboutput_diffs_Ref_ML_MLparams,
        scen_solid_name_2         = "ambrosia ML",
        scen_dashed_dark_2        = amboutput_diffs_Ref_ML_HDHPRparams,
        scen_dashed_dark_name_2   = "ambrosia HD-HPR",
        scen_dashed_light_2       = amboutput_diffs_Ref_ML_HDLPRparams,
        scen_dashed_light_name_2  = "ambrosia HD-LPR",
        scen_dotted_dark_2        = amboutput_diffs_Ref_ML_LDHPRparams,
        scen_dotted_dark_name_2   = "ambrosia LD-HPR",
        scen_dotted_light_2       = amboutput_diffs_Ref_ML_LDLPRparams,
        scen_dotted_light_name_2  = "ambrosia LD-LPR",
        
        return_data = TRUE
      )
      
      # Accumulate results
      p_all_regional_elast_diff <- bind_rows(p_all_regional_elast_diff, p_el)
      
      # plain version: with ambrosia ML scenario only
      p_el <- plot_regional_elasticity_comparison(
        elast_reg = reg_ens_bc,
        reg_num   = region_id,
        ci_level  = 0.90,
        
        # ambrosia (set 2, blue)
        scen_solid_2              = amboutput_diffs_Ref_ML_MLparams,
        scen_solid_name_2         = "ambrosia ML",

        return_data = TRUE
      )
      
      # Accumulate results
      p_all_regional_elast_diff_plain <- 
        bind_rows(p_all_regional_elast_diff_plain, p_el)
    }
    
    if(PRICE) {
      
      message(" Creating regional price difference plot")

      # plain version with comparison: ambrosia and GCAM ML scenarios only
      p_reg <- plot_regional_comparison(
        demand_reg = reg_ens_bc,
        reg_num    = region_id,
        ci_level   = 0.90,
        
        value_cols  = c("Y.region", "Ps", "Pn"),
        value_names = c("Income pc (region)", "Staples price (Ps)", "Non-staples price (Pn)"),
        
        # GCAM (set 1, orange)
        scen_solid_1              = gcamoutput_Ref_ML,
        scen_solid_name_1         = "GCAM ML",
        
        # ambrosia (set 2)
        scen_solid_2              = amboutput_Ref_ML_MLparams,
        scen_solid_name_2         = "ambrosia ML",
        
        return_data = TRUE   # or FALSE to get a ggplot
      )
      
      # Accumulate results
      p_all_regional_price_diff_plain_compare <- 
        bind_rows(p_all_regional_price_diff_plain_compare, p_reg)
      
    }
    
    if(DEMAND_DEC) {
      
      message(" Creating decile demand difference plot")
      
      # with four scenarios each from ambrosia and GCAM
      p_dec <- map_dfr(target_deciles, function(cg) {
        plot_decile_demand_comparison(
          demand_reg = reg_ens_bc,
          reg_num = region_id,
          consumer_group = cg,

          # GCAM (set 1, orange)
          scen_solid_1              = gcamoutput_diffs_Ref_ML_HP,
          scen_solid_name_1         = "GCAM ML",
          scen_dashed_dark_1        = gcamoutput_diffs_Ref_HD_HPR_HP,
          scen_dashed_dark_name_1   = "GCAM HD-HPR",
          scen_dashed_light_1       = gcamoutput_diffs_Ref_HD_LPR_HP,
          scen_dashed_light_name_1  = "GCAM HD-LPR",
          scen_dotted_dark_1        = gcamoutput_diffs_Ref_LD_HPR_HP,
          scen_dotted_dark_name_1   = "GCAM LD-HPR",
          scen_dotted_light_1       = gcamoutput_diffs_Ref_LD_LPR_HP,
          scen_dotted_light_name_1  = "GCAM LD-LPR",
          
          # ambrosia (set 2, blue)
          scen_solid_2              = amboutput_diffs_Ref_ML_MLparams,
          scen_solid_name_2         = "ambrosia ML",
          scen_dashed_dark_2        = amboutput_diffs_Ref_ML_HDHPRparams,
          scen_dashed_dark_name_2   = "ambrosia HD-HPR",
          scen_dashed_light_2       = amboutput_diffs_Ref_ML_HDLPRparams,
          scen_dashed_light_name_2  = "ambrosia HD-LPR",
          scen_dotted_dark_2        = amboutput_diffs_Ref_ML_LDHPRparams,
          scen_dotted_dark_name_2   = "ambrosia LD-HPR",
          scen_dotted_light_2       = amboutput_diffs_Ref_ML_LDLPRparams,
          scen_dotted_light_name_2  = "ambrosia LD-LPR",
          
          ci_level = 0.90,
          return_data = TRUE
        ) %>%
          mutate(region_label = paste0("Region ", region_id))
      })
      
      # Accumulate results
      p_all_decile_diff <- bind_rows(p_all_decile_diff, p_dec)
      
      # plain version: with ambrosia ML scenario only
      p_dec <- map_dfr(target_deciles, function(cg) {
        plot_decile_demand_comparison(
          demand_reg = reg_ens_bc,
          reg_num = region_id,
          consumer_group = cg,
          
          # ambrosia (set 2, blue)
          scen_solid_2              = amboutput_diffs_Ref_ML_MLparams,
          scen_solid_name_2         = "ambrosia ML",

          ci_level = 0.90,
          return_data = TRUE
        ) %>%
          mutate(region_label = paste0("Region ", region_id))
      })
      
      # Accumulate results
      p_all_decile_diff_plain <- bind_rows(p_all_decile_diff_plain, p_dec)
    }
    
    if(DEMAND_REG_DEC) {
      
      message(" Creating region/decile demand difference plot")
      
      # with four scenarios each from ambrosia and GCAM
      p_reg_dec <- plot_region_and_decile_demand_comparison(
        demand_reg = reg_ens_bc,
        reg_num = region_id,
        
        # GCAM (set 1, orange)
        scen_solid_1              = gcamoutput_diffs_Ref_ML_HP,
        scen_solid_name_1         = "GCAM ML",
        scen_dashed_dark_1        = gcamoutput_diffs_Ref_HD_HPR_HP,
        scen_dashed_dark_name_1   = "GCAM HD-HPR",
        scen_dashed_light_1       = gcamoutput_diffs_Ref_HD_LPR_HP,
        scen_dashed_light_name_1  = "GCAM HD-LPR",
        scen_dotted_dark_1        = gcamoutput_diffs_Ref_LD_HPR_HP,
        scen_dotted_dark_name_1   = "GCAM LD-HPR",
        scen_dotted_light_1       = gcamoutput_diffs_Ref_LD_LPR_HP,
        scen_dotted_light_name_1  = "GCAM LD-LPR",
        
        # ambrosia (set 2, blue)
        scen_solid_2              = amboutput_diffs_Ref_ML_MLparams,
        scen_solid_name_2         = "ambrosia ML",
        scen_dashed_dark_2        = amboutput_diffs_Ref_ML_HDHPRparams,
        scen_dashed_dark_name_2   = "ambrosia HD-HPR",
        scen_dashed_light_2       = amboutput_diffs_Ref_ML_HDLPRparams,
        scen_dashed_light_name_2  = "ambrosia HD-LPR",
        scen_dotted_dark_2        = amboutput_diffs_Ref_ML_LDHPRparams,
        scen_dotted_dark_name_2   = "ambrosia LD-HPR",
        scen_dotted_light_2       = amboutput_diffs_Ref_ML_LDLPRparams,
        scen_dotted_light_name_2  = "ambrosia LD-LPR",
        
        ci_level = 0.90,
        return_data = TRUE
      ) %>%
        mutate(region_label = paste0("Region ", region_id))
      
      # Accumulate results
      p_all_regional_decile_diff <- bind_rows(p_all_regional_decile_diff, p_reg_dec)
      
      # plain version: with ambrosia ML scenario only
      p_reg_dec <- plot_region_and_decile_demand_comparison(
        demand_reg = reg_ens_bc,
        reg_num = region_id,
        
        # ambrosia (set 2, blue)
        scen_solid_2              = amboutput_diffs_Ref_ML_MLparams,
        scen_solid_name_2         = "ambrosia ML",

        ci_level = 0.90,
        return_data = TRUE
      ) %>%
        mutate(region_label = paste0("Region ", region_id))
      
      # Accumulate results
      p_all_regional_decile_diff_plain <- bind_rows(p_all_regional_decile_diff_plain, p_reg_dec)
    }
    
    if(BAR) {
      
      message(" Collecting regional demand differences for bar plot")
      
      # Accumulate demand in target year
      reg_diffs_ens_bc_targetyr <- 
        bind_rows(reg_diffs_ens_bc_targetyr,
                  reg_ens_bc %>% 
                    filter(year == target_year,
                           `gcam-consumer` == "FoodDemand_Group1"))
    }
    
    if(DECOMP) {
      
      # Load regional decomposition ensemble data
      reg_ens_price <- readRDS(
        file.path(reg_results_path_price_diff,  
                  paste0("demand_R", region_id, scen_case_diff, ".RDS")))
      reg_ens_income <- readRDS(
        file.path(reg_results_path_income_diff,  
                  paste0("demand_R", region_id, scen_case_diff, ".RDS")))
      reg_ens_scale <- readRDS(
        file.path(reg_results_path_scale_diff,  
                  paste0("demand_R", region_id, scen_case_diff, ".RDS")))
      
      # Inside the region loop, after loading demand_full and having the 3 sub-ensembles available:
      p_decomp <- plot_regional_uncertainty_decomposition(
        demand_full = reg_ens_bc,
        reg_num     = region_id,
        scen_ml     =  amboutput_diffs_Ref_ML_MLparams,
        scen_ml_name = "Ambrosia ML",
        ens_price   = reg_ens_price,
        ens_income  = reg_ens_income,
        ens_scale   = reg_ens_scale,
        ci_level    = 0.90,
        return_data = TRUE
      )
      
      p_all_regional_decomp_diff <- bind_rows(p_all_regional_decomp_diff, p_decomp)
      
    }
    
  }
}

# Produce and save pdfs

if(ABS) {
  
  if(DEMAND_REG) {
    
    message("Creating regional demand pdf")
    
    plot_regional_comparison_pdf(
      p_all_regional_abs,
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = report_name_regional_abs,
      y_label = "Demand (kcal/day)"
    )

    plot_regional_comparison_pdf(
      p_all_regional_abs_plain,
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = report_name_regional_abs_plain,
      y_label = "Demand (kcal/day)"
    )
    
    plot_regional_comparison_pdf(
      p_all_regional_abs_plain_compare,
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = report_name_regional_abs_plain_compare,
      y_label = "Demand (kcal/day)"
    )
  }
  
  if (ELAST) {
    
    message("Creating regional elasticity pdf")
    
    plot_regional_elasticity_comparison_pdf(
      p_all = p_all_regional_elast_abs,
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = report_name_regional_elast_abs,
      y_step = 0.1,
      y_label = "Elasticity"
    )
    
    plot_regional_elasticity_comparison_pdf(
      p_all = p_all_regional_elast_abs_plain,
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = report_name_regional_elast_abs_plain,
      y_step = 0.1,
      y_label = "Elasticity"
    )
  }
  
  if(PRICE) {

    message("Creating regional price pdf")

    # If this is > 0, the pdf wrapper will drop/garble facets
    print(
      sum(is.na(factor(as.character(p_reg$demand_type),
                     levels = c("Y.region","Ps","Pn"))))
    )
    
    plot_regional_comparison_pdf(
      p_all_regional_price_abs_plain_compare,
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = report_name_regional_price_abs_plain_compare,
      value_cols  = c("Y.region", "Ps", "Pn"),
      value_names = c("Income pc (region)", "Staples price (Ps)", "Non-staples price (Pn)"),
      y_label = "Price (units)"
    )
  }
  
  if(DEMAND_DEC) {
    
    message("Creating decile demand pdf")
    
    plot_decile_demand_comparison_pdf(
      p_all = p_all_decile_abs,
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = report_name_decile_abs,
      y_label = "Demand (kcal/day)"
    )

    plot_decile_demand_comparison_pdf(
      p_all = p_all_decile_abs_plain,
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = report_name_decile_abs_plain,
      y_label = "Demand (kcal/day)"
    )
    
    plot_decile_demand_comparison_pdf(
      p_all = p_all_decile_abs_plain_compare,
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = report_name_decile_abs_plain_compare,
      y_label = "Demand (kcal/day)"
    )
  }
  
  if(DEMAND_REG_DEC) {
    
    message("Creating region/decile demand pdf")
    
    plot_region_and_decile_demand_comparison_pdf(
      p_all = p_all_regional_decile_abs,
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = report_name_regional_decile_abs,
      y_label = "Demand (kcal/day)"
    )
    
    plot_region_and_decile_demand_comparison_pdf(
      p_all = p_all_regional_decile_abs_plain,
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = report_name_regional_decile_abs_plain,
      y_label = "Demand (kcal/day)"
    )
    
    plot_region_and_decile_demand_comparison_pdf(
      p_all = p_all_regional_decile_abs_plain_compare,
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = report_name_regional_decile_abs_plain_compare,
      y_label = "Demand (kcal/day)"
    )
  }
  
  if(BAR) {
    
    message("Creating regional demand bar pdf")
    
    # Total demand in target year, central 90% CI, sorted by median
    p_reg_target <- plot_region_ci_bars_one_year(
      df = reg_ens_bc_targetyr,
      target_year = target_year,
      value_col = "Qtot.region",
      ci_level = 0.90,
      include_range = FALSE,
      show_median = TRUE,
      sort_by = "median",
      output_dir = output_dir,
      filename = report_name_regional_abs_bar,
      y_label = "Total demand (kcal/person/day)"
    )
  }
  
  if(DECOMP) {
    
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
      p_all = p_all_regional_decomp_abs,
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = report_name_regional_decomp_abs,
      color_override = color_override,
      linetype_override = linetype_override,
      ylimit_mode = "by_region",
      y_label = "Demand difference vs ML (kcal/day)"
    )
  }
}

if(DIFF) {
  
  if(DEMAND_REG) {
    
    message("Creating regional demand difference pdf")
    
    plot_regional_comparison_pdf(
      p_all_regional_diff,
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = report_name_regional_diff,
      y_label = "Demand difference (kcal/day)"
    )

    plot_regional_comparison_pdf(
      p_all_regional_diff_plain,
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = report_name_regional_diff_plain,
      y_label = "Demand difference (kcal/day)"
    )
    
    plot_regional_comparison_pdf(
      p_all_regional_diff_plain_compare,
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = report_name_regional_diff_plain_compare,
      y_label = "Demand difference (kcal/day)"
    )
  }
  
  if (ELAST) {
    
    message("Creating regional elasticity difference pdf")
    
    plot_regional_elasticity_comparison_pdf(
      p_all = p_all_regional_elast_diff,
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = report_name_regional_elast_diff,
      y_step = 0.1,
      y_label = "Elasticity difference"
    )
    
    plot_regional_elasticity_comparison_pdf(
      p_all = p_all_regional_elast_diff_plain,
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = report_name_regional_elast_diff_plain,
      y_step = 0.1,
      y_label = "Elasticity difference"
    )
  }
  
  if(PRICE) {
    
    message("Creating regional price difference pdf")
    
    # If this is > 0, the pdf wrapper will drop/garble facets
    print(
      sum(is.na(factor(as.character(p_reg$demand_type),
                     levels = c("Y.region","Ps","Pn"))))
    )
    
    plot_regional_comparison_pdf(
      p_all_regional_price_diff_plain_compare,
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = report_name_regional_price_diff_plain_compare,
      value_cols  = c("Y.region", "Ps", "Pn"),
      value_names = c("Income pc (region)", "Staples price (Ps)", "Non-staples price (Pn)"),
      y_label = "Price (units)"
    )
  }
  
  if(DEMAND_DEC) {
    
    message("Creating decile demand difference pdf")
    
    plot_decile_demand_comparison_pdf(
      p_all = p_all_decile_diff,
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = report_name_decile_diff,
      y_label = "Demand difference (kcal/day)"
    )
    
    plot_decile_demand_comparison_pdf(
      p_all = p_all_decile_diff_plain,
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = report_name_decile_diff_plain,
      y_label = "Demand difference (kcal/day)"
    )
  }
  
  if(DEMAND_REG_DEC) {
    
    message("Creating region/decile demand difference pdf")
    
    plot_region_and_decile_demand_comparison_pdf(
      p_all = p_all_regional_decile_diff,
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = report_name_regional_decile_diff,
      y_label = "Demand difference (kcal/day)"
    )
    
    plot_region_and_decile_demand_comparison_pdf(
      p_all = p_all_regional_decile_diff_plain,
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = report_name_regional_decile_diff_plain,
      y_label = "Demand difference (kcal/day)"
    )
  }
  
  if(BAR) {
    
    message("Creating regional demand difference bar pdf")
    
    # Demand differences in target year, central 90% CI, sorted by median
    p_reg_target <- plot_region_ci_bars_one_year(
      df = reg_diffs_ens_bc_targetyr,
      target_year = target_year,
      value_col = "Qtot.region",
      ci_level = 0.90,
      include_range = FALSE,
      show_median = TRUE,
      sort_by = "median",
      output_dir = output_dir,
      filename = report_name_regional_diff_bar,
      y_label = "Total demand (kcal/person/day)"
    )
  }
  
  if(DECOMP) {
    
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
      p_all = p_all_regional_decomp_diff,
      region_mapping = GCAM_region_ID_mapping,
      output_dir = output_dir,
      filename = report_name_regional_decomp_diff,
      color_override = color_override,
      linetype_override = linetype_override,
      ylimit_mode = "by_region",
      y_label = "Demand difference vs ML (kcal/day)"
    )
  }
}
