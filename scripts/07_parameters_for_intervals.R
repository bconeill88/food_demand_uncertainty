# Identify and save parameter sets that correspond to intervals in outcomes
# representing uncertainty space (e.g., representing the top 5% of outcomes,
# or bottom 5%, etc.). This is currently applied either to demand outcomes to 
# derive high or low demand (HD, LD) parameter sets, or to differences in demand
# between scenarios with different prices to derive high or low price response
# (HPR, LPR) parameter sets.
#
# Also save demand projections associated with these individual scenarios, for all
# regions in a single file, for convenient use in other scripts. Currently this 
# also includes saving the ML demand projections in the same format.

# load functions
source("R/common_definitions.R")
source("R/parameters_for_intervals_functions.R")
source("R/init_packages.R")

# install or load packages as needed
ensure_package(tidyverse)

# Indicate whether to identify parameters for absolute demand or demand differences
ABS <- FALSE
DIFF <- FALSE
BOTH <- TRUE

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

# directory for parameter ensembles
input_dir <- file.path("data", "processed", procdata_dir, "inputs")

#
if(ABS) {
  
  # define names for high and low interval parameter sets
  hi_name <- "HD"
  lo_name <- "LD"

  # define scenario case; specific to demand or demand differences
  scen_case <- "_ens_bc"
  
  # path to location for saving results; specific to demand or demand differences
  results_path <- file.path("data", "processed", procdata_dir,
                            procdata_subdir_RefMLgcam, demand_abs_subdir)
  
} else if(DIFF) {
  
  # define names for high and low interval parameter sets
  hi_name <- "HPR"
  lo_name <- "LPR"
  
  # define scenario case; specific to demand or demand differences
  scen_case <- "_diffs_ens_bc"
  
  # path to location for saving results; specific to demand or demand differences
  results_path <- file.path("data", "processed", procdata_dir,
                            procdata_subdir_RefMLgcam, demand_diffs_subdir)
} else if(BOTH) {
  
  # Combined case names (used only for labeling downstream exports)
  combo_names <- c("HD_HPR", "HD_LPR", "LD_HPR", "LD_LPR")
  
  # Where to READ ABS ensembles
  abs_scen_case <- "_ens_bc"
  abs_dir <- file.path("data", "processed", procdata_dir, 
                       procdata_subdir_RefMLgcam, demand_abs_subdir)
  
  # Where to READ DIFF ensembles
  diff_scen_case <- "_diffs_ens_bc"
  diff_dir <- file.path("data", "processed", procdata_dir, 
                        procdata_subdir_RefMLgcam, demand_diffs_subdir)
  
  # Where to WRITE BOTH results (new subfolder is cleanest)
  results_path <- file.path("data", "processed", procdata_dir, 
                            procdata_subdir_RefMLgcam, demand_both_subdir)
  
  # Bounds for ABS side (HD/LD) and DIFF side (HPR/LPR)
  abs_bounds <- list(
    HD = list(min = hi_min, max = hi_max),
    LD = list(min = lo_min, max = lo_max)
  )
  
  diff_bounds <- list(
    HPR = list(min = hi_min, max = hi_max),
    LPR = list(min = lo_min, max = lo_max)
  )
  
  
} else {
  
  stop("Flag for identifying case ABS, DIFF, or BOTH not set correctly.")
}

#------------------------------------------------------------------------------
# ABS or DIFF: Build iteration table, export params, and save scenario projections
#------------------------------------------------------------------------------

if (ABS || DIFF) {
  
  # define corresponding column names used several times below
  hi_col_name <- paste0(hi_name, "_Qtot")
  lo_col_name <- paste0(lo_name, "_Qtot")
  
  message("Identifying parameters for H/L parameters")
  
  # calculate parameters by taking iteration that falls most frequently in high/low intervals
  # lapply(scen_list_demand,function(x)
  make_HL_params_freq(
    year_vals = year_iter, 
    data_dir = procdata_dir, 
    out_dir = results_path, 
    scen = "Ref_ML_gcam",
    scen_case = scen_case,
    regions = reg_list, 
    hi_interval_min = hi_min,
    hi_interval_max = hi_max,
    lo_interval_min = lo_min,
    lo_interval_max = lo_max,
    hi_param_name = hi_name,
    lo_param_name = lo_name,
    save_outputs = TRUE
  )
  
  message("Finished identifying parameters for H/L intervals")
  
  # create and save tables of frequencies and iteration numbers, as results
  
  # load iteration frequency results
  max_freqs <- readRDS(
    file.path(results_path,
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
    file.path(results_path, 
              paste0("iter_table_", hi_name, lo_name, "_Qtot.csv")), 
    row.names = FALSE)
  
  # load parameter data
  param_data_global_clean_sub <- 
    readRDS(file.path(input_dir, "param_data_global_clean_sub.RDS"))
  param_data_FE_clean_sub <- 
    readRDS(file.path(input_dir, "param_data_FE_clean_sub.RDS"))
  
  # get and save global parameters for max frequency iterations
  params_global_H_Qtot <- param_data_global_clean_sub %>%
    filter(iteration %in% unique(iter_table[[hi_col_name]])) %>%
    mutate(case = hi_col_name)
  params_global_L_Qtot <- param_data_global_clean_sub %>%
    filter(iteration %in% unique(iter_table[[lo_col_name]])) %>%
    mutate(case = lo_col_name)
  params_global_Qtot <- bind_rows(params_global_H_Qtot, params_global_L_Qtot)
  write.csv(
    params_global_Qtot, 
    file.path(results_path, 
              paste0("params_global_", hi_name, lo_name, "_Qtot.csv")), 
    row.names = FALSE)
  
  # get and save FE parameters for max frequency iterations
  params_FE_H_Qtot <- param_data_FE_clean_sub %>%
    filter(iteration %in% unique(iter_table[[hi_col_name]])) %>%
    mutate(case = hi_col_name)
  params_FE_L_Qtot <- param_data_FE_clean_sub %>%
    filter(iteration %in% unique(iter_table[[lo_col_name]])) %>%
    mutate(case = lo_col_name)
  params_FE_Qtot <- bind_rows(params_FE_H_Qtot, params_FE_L_Qtot)
  write.csv(
    params_FE_Qtot, 
    file.path(results_path, 
              paste0("params_FE_", hi_name, lo_name, "_Qtot.csv")), 
    row.names = FALSE)
}

#------------------------------------------------------------------------------
# BOTH: identify parameters that are jointly extreme in ABS demand and DIFF response
#------------------------------------------------------------------------------

if (BOTH) {
  
  message("Running BOTH (ABS ∩ DIFF) parameter identification")
  
  # ---- Run joint interval identification + frequency analysis ----
  # NOTE: data_dir should be procdata_dir (same as ABS/DIFF), not input_dir
  res_both <- make_BOTH_params_freq(
    year_vals      = year_iter,
    data_dir       = procdata_dir,
    abs_dir        = abs_dir,
    abs_scen_case  = abs_scen_case,
    diff_dir       = diff_dir,
    diff_scen_case = diff_scen_case,
    out_dir        = results_path,
    regions        = reg_list,
    abs_bounds     = abs_bounds,
    diff_bounds    = diff_bounds,
    save_outputs   = TRUE
  )
  
  message("Finished BOTH interval extraction + frequency analysis")
  
  # --------------------------------------------------------------------------
  # Create + save tables (parallel to ABS/DIFF branch)
  # --------------------------------------------------------------------------
  
  # load iteration frequency results
  max_freqs <- readRDS(file.path(results_path, "max_iter_frequencies_BOTH.RDS"))
  
  # if your BOTH max_freqs ever contains non-Qtot cases, uncomment:
  # max_freqs <- max_freqs %>% filter(str_detect(case, "_Qtot$"))
  
  message("Creating summary tables of max frequencies and max iterations (BOTH)")
  
  # table of frequencies of max-frequency iterations for each case
  max_freq_table <- max_freqs %>%
    select(GCAM_region_ID, case, freq) %>%
    pivot_wider(names_from = case, values_from = freq) %>%
    left_join(GCAM_region_ID_mapping, by = "GCAM_region_ID") %>%  # add region names
    relocate(region, .before = GCAM_region_ID) %>%
    arrange(GCAM_region_ID) %>%
    rename(ID = GCAM_region_ID)
  
  # table of iteration numbers of max-frequency iterations for each case
  iter_table <- max_freqs %>%
    select(GCAM_region_ID, case, max_iter) %>%
    pivot_wider(names_from = case, values_from = max_iter) %>%
    left_join(GCAM_region_ID_mapping, by = "GCAM_region_ID") %>%
    relocate(region, .before = GCAM_region_ID) %>%
    arrange(GCAM_region_ID) %>%
    rename(ID = GCAM_region_ID)
  
  # save images of tables if interactive setting, RDS files if on PIC
  if (interactive()) {
    ensure_package(gt)
    ensure_package(webshot2)
    
    gtsave(
      gt(max_freq_table) %>% tab_header(title = "Maximum Frequencies"),
      file.path(results_path, "table_max_frequencies_all_regions.png")
    )
    
    gtsave(
      gt(iter_table) %>% tab_header(title = "Iteration Numbers of Maximum Frequency"),
      file.path(results_path, "table_max_iterations_all_regions.png")
    )
  } else {
    saveRDS(max_freq_table, file.path(results_path, "table_max_frequencies_all_regions.RDS"))
    saveRDS(iter_table,     file.path(results_path, "table_max_iterations_all_regions.RDS"))
  }
  
  # save iteration table (BOTH-specific filename)
  write.csv(
    iter_table,
    file.path(results_path, "iter_table_BOTH_Qtot.csv"),
    row.names = FALSE
  )
  
  # --------------------------------------------------------------------------
  # Export parameters (global + FE), tagged by combined case
  # --------------------------------------------------------------------------
  
  message("Exporting global and FE parameter sets for BOTH cases")
  
  # Ensure all expected BOTH case columns exist (if any missing, fill with NA)
  for (cc in combo_names) {
    if (!cc %in% names(iter_table)) iter_table[[cc]] <- NA_integer_
  }
  
  iters_needed <- iter_table %>%
    select(all_of(combo_names)) %>%
    unlist(use.names = FALSE) %>%
    unique() %>%
    na.omit()
  
  param_data_global_clean_sub <- readRDS(file.path(input_dir, "param_data_global_clean_sub.RDS"))
  param_data_FE_clean_sub     <- readRDS(file.path(input_dir, "param_data_FE_clean_sub.RDS"))
  
  tag_tbl <- iter_table %>%
    select(ID, all_of(combo_names)) %>%
    pivot_longer(
      cols      = all_of(combo_names),
      names_to  = "case",
      values_to = "iteration"
    ) %>%
    filter(!is.na(iteration)) %>%
    distinct(case, iteration)
  
  params_global_both <- param_data_global_clean_sub %>%
    filter(iteration %in% iters_needed) %>%
    inner_join(tag_tbl, by = "iteration") %>%
    arrange(case, iteration)
  
  params_FE_both <- param_data_FE_clean_sub %>%
    filter(iteration %in% iters_needed) %>%
    inner_join(tag_tbl, by = "iteration") %>%
    arrange(case, iteration, GCAM_region_ID)
  
  write.csv(
    params_global_both,
    file.path(results_path, "params_global_BOTH_Qtot.csv"),
    row.names = FALSE
  )
  
  write.csv(
    params_FE_both,
    file.path(results_path, "params_FE_BOTH_Qtot.csv"),
    row.names = FALSE
  )
  
  # --------------------------------------------------------------------------
  # Save ABS and DIFF scenario projections for world iteration (ID == 33)
  # Write ONLY inside BOTH results_path
  # --------------------------------------------------------------------------
  
  message("Saving world-iteration demand and demand-diffs projections for BOTH cases")
  
  iter_world <- iter_table %>% filter(ID == 33)
  if (nrow(iter_world) != 1) {
    stop("Expected exactly one row with ID == 33 (World).")
  }
  
  scen_out_abs  <- file.path(results_path, "scenarios_abs")
  scen_out_diff <- file.path(results_path, "scenarios_diff")
  dir.create(scen_out_abs,  recursive = TRUE, showWarnings = FALSE)
  dir.create(scen_out_diff, recursive = TRUE, showWarnings = FALSE)
  
  for (cc in combo_names) {
    
    it <- iter_world[[cc]][1]
    if (is.na(it)) next
    
    # ABS demand levels (read from abs_dir, write to BOTH dir)
    scen_abs <- map_dfr(reg_list, function(r) {
      readRDS(file.path(abs_dir, paste0("demand_R", r, abs_scen_case, ".RDS"))) %>%
        filter(iteration == it)
    })
    
    saveRDS(
      scen_abs,
      file.path(scen_out_abs, paste0("demand_allregions_", cc, "_Qtot.RDS"))
    )
    
    # DIFF demand responses (read from diff_dir, write to BOTH dir)
    scen_diff <- map_dfr(reg_list, function(r) {
      readRDS(file.path(diff_dir, paste0("demand_R", r, diff_scen_case, ".RDS"))) %>%
        filter(iteration == it)
    })
    
    saveRDS(
      scen_diff,
      file.path(scen_out_diff, paste0("demand_diffs_allregions_", cc, "_Qtot.RDS"))
    )
  }
  
  message("Finished BOTH parameter identification and exports")
}


#------------------------------------------------------------------------------
# Load files for manual inspection and writing results for Kanishka to use in GCAM
#------------------------------------------------------------------------------

if(FALSE) {

# manually inspect this file to confirm that Qtot iterations are the best ones to 
# use for HD, LD parameters
max_freq_table <- readRDS(file.path(results_path, "table_max_frequencies_all_regions.RDS"))
# max_freq_table_jan25 <- 
#   readRDS("H:/My Drive/R projects/food_demand/food_demand_uncertainty/data/processed/update9_cnstrlam_agg32FE_24jan25/Ref_ML_gcam/ens_bc_20251009_221027/table_max_frequencies_all_regions.RDS")

# identify iteration numbers that correspond to desired HD and LD parameter sets; 
# region ID 33 represents iteration that does the best across all regions rather 
# than for a single region
iter_table <- read_csv(
  file.path(results_path, 
            paste0("iter_table_", hi_name, lo_name, "_Qtot.csv")), 
            show_col_types = FALSE)
iter_HL_Qtot_world <- iter_table %>% filter(ID == 33) %>% 
  select(all_of(c(hi_col_name, lo_col_name)))

# load global and FE parameters, then extract HD and LD parameter sets
params_global_Qtot <- read_csv(
  file.path(results_path, 
            paste0("params_global_", hi_name, lo_name, "_Qtot.csv")), 
            show_col_types = FALSE)
params_FE_Qtot <- read_csv(
  file.path(results_path,
            paste0("params_FE_", hi_name, lo_name, "_Qtot.csv")),
            show_col_types = FALSE)
params_global_Qtot_world <- params_global_Qtot %>% 
  filter(iteration %in% iter_HL_Qtot_world[1,])
params_FE_Qtot_world <- params_FE_Qtot %>% 
  filter(iteration %in% iter_HL_Qtot_world[1,])

# write these parameter sets to files for Kanishka
write.csv(
  params_global_Qtot_world,
  file.path(results_path, 
            paste0("params_global_", hi_name, lo_name, "_Qtot_world.csv")), 
            row.names = FALSE)
write.csv(
  params_FE_Qtot_world, 
  file.path(results_path, 
            paste0("params_FE_", hi_name, lo_name, "_Qtot_world.csv")), 
            row.names = FALSE)

# Write out files containing demand projections for H/L scenarios for all regions;
# extract projections from the ensemble results. Note that even if parameters are
# for intervals of differences (HPR, LPR), here we want the demand results, not
# the difference results, so that's hard wired.
scen_results_path <- 
  file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam)
scen_results <- map_dfr(reg_list, function(r) {
  readRDS(file.path(scen_results_path, paste0("demand_R", r, "_ens_bc.RDS"))) %>%
    filter(iteration %in% iter_HL_Qtot_world[1,])
})
hi_iter <- iter_HL_Qtot_world[[hi_col_name]][1]
hi_scen <- scen_results %>% filter(iteration == hi_iter)
lo_iter <- iter_HL_Qtot_world[[lo_col_name]][1]
lo_scen <- scen_results %>% filter(iteration == lo_iter)
saveRDS(hi_scen, file.path(scen_results_path,
                           paste0("demand_allregions_", hi_name, "params.RDS")))
saveRDS(lo_scen, file.path(scen_results_path,
                           paste0("demand_allregions_", lo_name, "params.RDS")))

# For parameters for intervals of differences, write out files containing demand 
# difference projections for H/L scenarios for all regions; extract projections 
# from the ensemble results.
if(hi_name == "HPR" | lo_name == "LPR"){
  scen_results_path <- 
    file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam,
              demand_diffs_subdir)
  scen_results <- map_dfr(reg_list, function(r) {
    readRDS(file.path(scen_results_path, 
                      paste0("demand_R", r, "_diffs_ens_bc.RDS"))) %>%
      filter(iteration %in% iter_HL_Qtot_world[1,])
  })
}
if(hi_name == "HPR") {
  hi_iter <- iter_HL_Qtot_world[[hi_col_name]][1]
  hi_scen <- scen_results %>% filter(iteration == hi_iter)
  saveRDS(hi_scen, file.path(scen_results_path,
                             paste0("demand_diffs_allregions_", hi_name, "params.RDS")))
}
if(lo_name == "LPR") {
  lo_iter <- iter_HL_Qtot_world[[lo_col_name]][1]
  lo_scen <- scen_results %>% filter(iteration == lo_iter)
  saveRDS(lo_scen, file.path(scen_results_path,
                             paste0("demand_diffs_allregions_", lo_name, "params.RDS")))
}

# For completeness, save demand (and demand difference) projections for ML scenario 
# in same form as other scenario results.
# Should move this somewhere more logical in final version of code.

# demand results
iter_ML <- readRDS(
  file.path("data", "processed", procdata_dir, "params_ML_intervals_global.RDS")) %>%
  filter(measure == "ML") %>%
  pull(iteration)
scen_results_path <- 
  file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam)
scen_results <- map_dfr(reg_list, function(r) {
  readRDS(file.path(scen_results_path, paste0("demand_R", r, "_ens_bc.RDS"))) %>%
    filter(iteration == iter_ML)
})
saveRDS(scen_results, file.path(scen_results_path,
                           paste0("demand_allregions_MLparams.RDS")))

# demand difference results
iter_ML <- readRDS(
  file.path("data", "processed", procdata_dir, "params_ML_intervals_global.RDS")) %>%
  filter(measure == "ML") %>%
  pull(iteration)
scen_results_path <- 
  file.path("data", "processed", procdata_dir, procdata_subdir_RefMLgcam,
            demand_diffs_subdir)
scen_results <- map_dfr(reg_list, function(r) {
  readRDS(file.path(scen_results_path, 
                    paste0("demand_R", r, "_diffs_ens_bc.RDS"))) %>%
    filter(iteration == iter_ML)
})
saveRDS(scen_results, file.path(scen_results_path,
                                paste0("demand_diffs_allregions_MLparams.RDS")))

}