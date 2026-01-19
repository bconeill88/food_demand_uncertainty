# Plot comparison of demand for the individual scenarios (such as the HD and LD
# parameter scenarios) to a specified confidence interval of the demand ensemble, 
# both for regional demand and demand by decile.

# Load functions
source("R/common_definitions.R")
source("R/init_packages.R")
source("R/demand_with_intervals_plot_functions.R")

# Make sure packages are installed/loaded
ensure_package(tidyverse)
ensure_package(ggforce)
ensure_package(ggh4x)

# Indicate whether to plot absolute demand or demand differences
outcome <- "ABS"
# outcome <- "DIFF"

# Define directories and file names

# Directory containing regional ensemble results
reg_results_path <- case_when(
  outcome == "ABS" ~ file.path("data", "processed", procdata_dir,
                               procdata_subdir_RefMLgcam),
  outcome == "DIFF" ~ file.path("data", "processed", procdata_dir,
                                procdata_subdir_RefMLgcam, demand_diffs_subdir)
)

# Directory for report result
output_dir <- file.path("output", "reports", procdata_dir, procdata_subdir_RefMLgcam)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Report file names
report_name_regional <- case_when(
  outcome == "ABS" ~ "demand_regional_all_regions_ens_bc_HDLD_test.pdf",
  outcome == "DIFF" ~ "demand_diffs_regional_all_regions_ens_bc_HPRLPR.pdf"
)
report_name_decile <- case_when(
  outcome == "ABS" ~ "demand_decile_all_regions_ens_bc_HDLD_test.pdf",
  outcome == "DIFF" ~ "demand_diffs_decile_all_regions_ens_bc_HPRLPR.pdf"
)

# Scenario case to use in file names
scen_case <- case_when(
  outcome == "ABS" ~ "_ens_bc",
  outcome == "DIFF" ~ "_diffs_ens_bc"
)

# Regions and deciles to plot
region_list <- c(5) # 1:29, 31, 32)
target_deciles <- c("FoodDemand_Group1", "FoodDemand_Group2", "FoodDemand_Group3",
                    "FoodDemand_Group6", "FoodDemand_Group10")

# Load region mapping and max iteration frequency tables
load("./data/raw/GCAM_region_ID_mapping.Rdata")

# Load demand projections for individual scenarios
if(outcome == "ABS") {
  
  # GCAM results, from files produced by the 101 script
  gcamoutput_Ref_ML <-
    readRDS(file.path("data", "processed", procdata_dir, gcam_results_dir,
                      "gcamoutput_Ref_ML.RDS"))
  gcamoutput_Ref_HD <-
    readRDS(file.path("data", "processed", procdata_dir, gcam_results_dir,
                      "gcamoutput_Ref_HD.RDS"))
  gcamoutput_Ref_LD <-
    readRDS(file.path("data", "processed", procdata_dir, gcam_results_dir,
                      "gcamoutput_Ref_LD.RDS"))
  
  # ambrosia results, from files produced by the 07 parameter for intervals script
  amboutput_Ref_ML_MLparams <-
    readRDS(file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam,
                      "demand_allregions_MLparams.RDS"))
  amboutput_Ref_ML_HDparams <-
    readRDS(file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam,
                      "demand_allregions_HDparams.RDS"))
  amboutput_Ref_ML_LDparams <-
    readRDS(file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam,
                      "demand_allregions_LDparams.RDS"))
  amboutput_Ref_ML_HPRparams <-
    readRDS(file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam,
                      "demand_allregions_HPRparams.RDS"))
  amboutput_Ref_ML_LPRparams <-
    readRDS(file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam,
                      "demand_allregions_LPRparams.RDS"))
  
} else if(outcome == "DIFF") {
    
  # ambrosia results, from files produced by the 07 parameter for intervals script
  amboutput_diffs_Ref_ML_HPRparams <- 
    readRDS(file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam,
                      demand_diffs_subdir, "demand_diffs_allregions_HPRparams.RDS"))
  amboutput_diffs_Ref_ML_LPRparams <- 
    readRDS(file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam,
                      demand_diffs_subdir, "demand_diffs_allregions_LPRparams.RDS"))
  
}

# region_list <- c(1)

message("Creating regional plots ...")

# Plot all regions, regional total results
p_all_regional <- map_dfr(region_list, function(region_id) {
  
  reg_ens_bc <- readRDS(
    file.path(reg_results_path,  paste0("demand_R", region_id, scen_case, ".RDS")))
  
  message("Plotting region ", region_id)

  plot_regional_demand_comparison(
    demand_reg = reg_ens_bc,
    reg_num    = region_id,
#    sample_n   = 100,
#    ensemble_name = "Emulator",
    ci_level   = 0.90,
    # GCAM (set 1)
    scen_solid_1      = gcamoutput_Ref_ML,
    scen_solid_name_1 = "GCAM ML",
    scen_dashed_1     = gcamoutput_Ref_HD,
    scen_dashed_name_1= "GCAM HD",
    scen_dotted_1     = gcamoutput_Ref_LD,
    scen_dotted_name_1= "GCAM LD",
    # ambrosia (set 2)
    scen_solid_2      = amboutput_Ref_ML_MLparams,
    scen_solid_name_2 = "Ambrosia ML",
    scen_dashed_2     = amboutput_Ref_ML_HDparams,
    scen_dashed_name_2= "Ambrosia HD",
    scen_dotted_2     = amboutput_Ref_ML_LDparams,
    scen_dotted_name_2= "Ambrosia LD",
    return_data = TRUE   # or FALSE to get a ggplot
  )
})

message("Creating pdf")

plot_regional_demand_comparison_pdf(
  p_all_regional,
  region_mapping = GCAM_region_ID_mapping,
  output_dir = output_dir,
  filename = report_name_regional
)

if(FALSE) {
  
  message("Creating decile plots ...")
  
  p_all_decile <- map_dfr(region_list, function(region_id) {
    
    reg_ens_bc <- readRDS(
      file.path(reg_results_path,  paste0("demand_R", region_id, scen_case, ".RDS")))
    
    message("Plotting region ", region_id)
    
    map_dfr(target_deciles, function(cg) {
      plot_decile_demand_comparison(
        demand_reg = reg_ens_bc,
        reg_num = region_id,
        consumer_group = cg,
        # GCAM (set 1)
        scen_solid_1      = gcamoutput_Ref_ML,
        scen_solid_name_1 = "GCAM ML",
        scen_dashed_1     = gcamoutput_Ref_HD,
        scen_dashed_name_1= "GCAM HD",
        scen_dotted_1     = gcamoutput_Ref_LD,
        scen_dotted_name_1= "GCAM LD",
        # ambrosia (set 2)
        scen_solid_2      = amboutput_Ref_ML_MLparams,
        scen_solid_name_2 = "Ambrosia ML",
        scen_dashed_2     = amboutput_Ref_ML_HDparams,
        scen_dashed_name_2= "Ambrosia HD",
        scen_dotted_2     = amboutput_Ref_ML_LDparams,
        scen_dotted_name_2= "Ambrosia LD",
        ci_level = 0.90,
        return_data = TRUE
      ) %>%
        dplyr::mutate(region_label = paste0("Region ", region_id))  # or your mapping
    })
  })
  
  message("Creating pdf")
  
  plot_decile_demand_comparison_pdf(
    p_all = p_all_decile,
    region_mapping = GCAM_region_ID_mapping,
    output_dir = output_dir,
    filename = report_name_decile
  )

}
