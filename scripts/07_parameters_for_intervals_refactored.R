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
ABS  <- TRUE
DIFF <- TRUE
BOTH <- TRUE

# define regions to run over
# get command line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  reg_list <- c(as.numeric(args[1]))
} else {
  reg_list <- c(1:32)
}

# directory for parameter ensembles
input_dir <- file.path("data", "processed", procdata_dir, clean_data_dir)

#------------------------------------------------------------------------------
# Helper functions
#------------------------------------------------------------------------------

# Create + save tables of frequencies and iteration numbers
# Returns a list(max_freq_table=..., iter_table=...)
make_and_save_summary_tables <- function(max_freqs, results_path, mapping_df,
                                         output_stub = NULL) {
  
  message("Creating summary tables of max frequencies and max iterations")
  
  # create table of frequencies of max frequency iterations for each case
  max_freq_table <- max_freqs %>%
    select(GCAM_region_ID, case, freq) %>%
    pivot_wider(names_from = case, values_from = freq) %>%
    left_join(mapping_df, by = "GCAM_region_ID") %>% # add region names
    relocate(region, .before = GCAM_region_ID) %>%
    arrange(GCAM_region_ID) %>%
    rename(ID = GCAM_region_ID)
  
  # create table of iteration numbers of max frequency iterations for each case
  iter_table <- max_freqs %>%
    select(GCAM_region_ID, case, max_iter) %>%
    pivot_wider(names_from = case, values_from = max_iter) %>%
    left_join(mapping_df, by = "GCAM_region_ID") %>%
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
  
  # Also write iteration CSV (keep existing naming logic where possible)
  if (!is.null(output_stub)) {
    write.csv(
      iter_table,
      file.path(results_path, paste0("iter_table_", output_stub, "_Qtot.csv")),
      row.names = FALSE
    )
  }
  
  list(max_freq_table = max_freq_table, iter_table = iter_table)
}

# Get and save parameters corresponding to max frequency iterations
# Exports both global + FE parameter csvs; returns list(params_global=..., params_FE=...)
export_params_for_iter_table <- function(iter_table, case_cols, input_dir,
                                         results_path, output_stub) {
  
  message("Getting parameters for max frequency iterations")
  
  iters_needed <- iter_table %>%
    select(all_of(case_cols)) %>%
    unlist(use.names = FALSE) %>%
    unique() %>%
    na.omit()
  
  param_data_global_clean_sub <-
    readRDS(file.path(input_dir, "param_data_global_clean_sub.RDS"))
  param_data_FE_clean_sub <-
    readRDS(file.path(input_dir, "param_data_FE_clean_sub.RDS"))
  
  tag_tbl <- iter_table %>%
    select(ID, all_of(case_cols)) %>%
    pivot_longer(
      cols      = all_of(case_cols),
      names_to  = "case",
      values_to = "iteration"
    ) %>%
    filter(!is.na(iteration)) %>%
    distinct(case, iteration)
  
  params_global <- param_data_global_clean_sub %>%
    filter(iteration %in% iters_needed) %>%
    inner_join(tag_tbl, by = "iteration") %>%
    arrange(case, iteration)
  
  params_FE <- param_data_FE_clean_sub %>%
    filter(iteration %in% iters_needed) %>%
    inner_join(tag_tbl, by = "iteration") %>%
    arrange(case, iteration, GCAM_region_ID)
  
  write.csv(
    params_global,
    file.path(results_path, paste0("params_global_", output_stub, "_Qtot.csv")),
    row.names = FALSE
  )
  
  write.csv(
    params_FE,
    file.path(results_path, paste0("params_FE_", output_stub, "_Qtot.csv")),
    row.names = FALSE
  )
  
  list(params_global = params_global, params_FE = params_FE)
}

# Save a reduced CSV containing ONLY the Qtot params for the "World" max-frequency
# iterations (i.e., the iterations in the ID==33 row of iter_table).
# Uses the already-tagged params_* tables returned by export_params_for_iter_table().
# These are the files used as input to GCAM runs.
save_world_maxfreq_param_csvs <- function(iter_table, case_cols,
                                          params_global, params_FE,
                                          results_path, output_stub,
                                          world_id = 33) {
  
  # Extract the world row (ID == 33)
  iter_world <- iter_table %>% filter(ID == world_id)
  if (nrow(iter_world) != 1) {
    stop("Expected exactly one row with ID == 33 (World).")
  }
  
  # Build a (case, iteration) table for world-only (skip NAs)
  world_cases <- iter_world %>%
    select(all_of(case_cols)) %>%
    pivot_longer(cols = all_of(case_cols),
                 names_to = "case",
                 values_to = "iteration") %>%
    filter(!is.na(iteration)) %>%
    distinct(case, iteration)
  
  # Subset global params for just these world iterations (Qtot-only cases)
  params_global_world <- params_global %>%
    semi_join(world_cases, by = c("case", "iteration")) %>%
    arrange(case, iteration)
  
  # Subset FE params for just these world iterations (Qtot-only cases)
  params_FE_world <- params_FE %>%
    semi_join(world_cases, by = c("case", "iteration")) %>%
    arrange(case, iteration, GCAM_region_ID)
  
  # Save reduced CSVs with the "_Qtot_world.csv" tag
  write.csv(
    params_global_world,
    file.path(results_path, paste0("params_global_", output_stub, "_Qtot_world.csv")),
    row.names = FALSE
  )
  
  write.csv(
    params_FE_world,
    file.path(results_path, paste0("params_FE_", output_stub, "_Qtot_world.csv")),
    row.names = FALSE
  )
}

# Save projections for identified parameter cases (world iteration) + ML
# - For ABS: writes demand_allregions_<CASE>.RDS and demand_allregions_MLparams.RDS
# - For DIFF: writes demand_diffs_allregions_<CASE>.RDS and demand_diffs_allregions_MLparams.RDS
save_case_and_ml_projections <- function(iter_table,
                                         reg_list,
                                         results_path,
                                         read_dir,
                                         scen_case,
                                         case_cols,
                                         file_prefix,     # "demand_allregions_" or "demand_diffs_allregions_"
                                         ml_file_prefix) {# same as file_prefix, but explicit to keep intent clear
  
  message("Saving projections for identified parameters and ML")
  
  iter_world <- iter_table %>% filter(ID == 33)
  if (nrow(iter_world) != 1) {
    stop("Expected exactly one row with ID == 33 (World).")
  }
  
  # ---- Save projections for identified parameter cases (world iterations) ----
  for (cc in case_cols) {
    
    it <- iter_world[[cc]][1]
    
    if (is.na(it)) {
      message("No iteration identified for case ", cc)
      next
    }
    
    scen_results <- map_dfr(reg_list, function(r) {
      readRDS(file.path(read_dir, paste0("demand_R", r, scen_case, ".RDS"))) %>%
        filter(iteration == it)
    })
    
    # file name strips "_Qtot" to match older naming in downstream scripts
    cc_clean <- gsub("_Qtot$", "", cc)
    
    saveRDS(
      scen_results,
      file.path(results_path, paste0(file_prefix, cc_clean, "params.RDS"))
    )
  }
  
  # ---- Save ML projections in same form as other scenario results ----
  iter_ML <- readRDS(
    file.path("data", "processed", procdata_dir, param_intervals_dir, "params_ML_intervals_global.RDS")
  ) %>%
    filter(measure == "ML") %>%
    pull(iteration)
  
  scen_ml <- map_dfr(reg_list, function(r) {
    readRDS(file.path(read_dir, paste0("demand_R", r, scen_case, ".RDS"))) %>%
      filter(iteration == iter_ML)
  })
  
  saveRDS(
    scen_ml,
    file.path(results_path, paste0(ml_file_prefix, "MLparams.RDS"))
  )
}

#------------------------------------------------------------------------------
# Case configuration (so ABS + DIFF + BOTH can all run in one script execution)
#------------------------------------------------------------------------------

# Where ABS ensembles live / what names to use
abs_cfg <- list(
  enabled    = ABS,
  hi_name    = "HD",
  lo_name    = "LD",
  scen_case  = "_ens_bc",
  results_path = file.path("data", "processed", procdata_dir,
                           procdata_subdir_RefMLgcam, demand_abs_subdir),
  # for ABS, read_dir is same as results_path (this is where demand_R*.RDS live)
  read_dir   = NULL, # filled after results_path exists
  output_stub = "HDLD",
  file_prefix = "demand_allregions_",        # output files used downstream
  ml_prefix   = "demand_allregions_"
)
abs_cfg$read_dir <- abs_cfg$results_path

# Where DIFF ensembles live / what names to use
diff_cfg <- list(
  enabled    = DIFF,
  hi_name    = "HPR",
  lo_name    = "LPR",
  scen_case  = "_diffs_ens_bc",
  results_path = file.path("data", "processed", procdata_dir,
                           procdata_subdir_RefMLgcam, demand_diffs_subdir),
  read_dir   = NULL,
  output_stub = "HPRLPR",
  file_prefix = "demand_diffs_allregions_",
  ml_prefix   = "demand_diffs_allregions_"
)
diff_cfg$read_dir <- diff_cfg$results_path

# BOTH configuration (existing logic preserved)
both_cfg <- list(
  enabled = BOTH,
  combo_names = c("HD_HPR", "HD_LPR", "LD_HPR", "LD_LPR"),
  
  abs_scen_case  = "_ens_bc",
  abs_dir = file.path("data", "processed", procdata_dir,
                      procdata_subdir_RefMLgcam, demand_abs_subdir),
  
  diff_scen_case = "_diffs_ens_bc",
  diff_dir = file.path("data", "processed", procdata_dir,
                       procdata_subdir_RefMLgcam, demand_diffs_subdir),
  
  results_path = file.path("data", "processed", procdata_dir,
                           procdata_subdir_RefMLgcam, demand_both_subdir),
  
  abs_bounds = list(
    HD = list(min = hi_min, max = hi_max),
    LD = list(min = lo_min, max = lo_max)
  ),
  diff_bounds = list(
    HPR = list(min = hi_min, max = hi_max),
    LPR = list(min = lo_min, max = lo_max)
  )
)

#------------------------------------------------------------------------------
# ABS: Build iteration table, export params, and save scenario projections
#------------------------------------------------------------------------------

if (abs_cfg$enabled) {
  
  message("ABS case:")
  message("Identifying parameters for H/L outcomes")
  
  make_HL_params_freq(
    year_vals = year_iter,
    data_dir = procdata_dir,
    out_dir = abs_cfg$results_path,
    scen = "Ref_ML_gcam",
    scen_case = abs_cfg$scen_case,
    regions = reg_list,
    hi_interval_min = hi_min,
    hi_interval_max = hi_max,
    lo_interval_min = lo_min,
    lo_interval_max = lo_max,
    hi_param_name = abs_cfg$hi_name,
    lo_param_name = abs_cfg$lo_name,
    save_outputs = TRUE
  )
  
  message("Finished identifying parameters")
  
  # load iteration frequency results
  max_freqs <- readRDS(
    file.path(abs_cfg$results_path,
              paste0("max_iter_frequencies", abs_cfg$scen_case, ".RDS"))
  )
  
  # Create + save tables
  tbls <- make_and_save_summary_tables(
    max_freqs = max_freqs,
    results_path = abs_cfg$results_path,
    mapping_df = GCAM_region_ID_mapping,
    output_stub = abs_cfg$output_stub
  )
  iter_table <- tbls$iter_table
  
  # Export parameters (Qtot-only cases)
  hi_col_name <- paste0(abs_cfg$hi_name, "_Qtot")
  lo_col_name <- paste0(abs_cfg$lo_name, "_Qtot")
  case_cols <- c(hi_col_name, lo_col_name)
  
  # Export parameters (Qtot-only cases)
  param_out <- export_params_for_iter_table(
    iter_table   = iter_table,
    case_cols    = case_cols,
    input_dir    = input_dir,
    results_path = abs_cfg$results_path,
    output_stub  = abs_cfg$output_stub
  )
  
  # Save reduced world-only Qtot CSVs (ID==33 max-frequency iterations)
  save_world_maxfreq_param_csvs(
    iter_table     = iter_table,
    case_cols      = case_cols,
    params_global  = param_out$params_global,
    params_FE      = param_out$params_FE,
    results_path   = abs_cfg$results_path,
    output_stub    = abs_cfg$output_stub
  )
  
  # Save scenario projections for identified parameters + ML
  save_case_and_ml_projections(
    iter_table     = iter_table,
    reg_list       = reg_list,
    results_path   = abs_cfg$results_path,
    read_dir       = abs_cfg$read_dir,
    scen_case      = abs_cfg$scen_case,
    case_cols      = case_cols,
    file_prefix    = abs_cfg$file_prefix,
    ml_file_prefix = abs_cfg$ml_prefix
  )
}

#------------------------------------------------------------------------------
# DIFF: Build iteration table, export params, and save scenario projections
#------------------------------------------------------------------------------

if (diff_cfg$enabled) {
  
  message("DIFF case:")
  message("Identifying parameters for H/L outcomes")
  
  make_HL_params_freq(
    year_vals = year_iter,
    data_dir = procdata_dir,
    out_dir = diff_cfg$results_path,
    scen = "Ref_ML_gcam",
    scen_case = diff_cfg$scen_case,
    regions = reg_list,
    hi_interval_min = hi_min,
    hi_interval_max = hi_max,
    lo_interval_min = lo_min,
    lo_interval_max = lo_max,
    hi_param_name = diff_cfg$hi_name,
    lo_param_name = diff_cfg$lo_name,
    save_outputs = TRUE
  )
  
  message("Finished identifying parameters")
  
  # load iteration frequency results
  max_freqs <- readRDS(
    file.path(diff_cfg$results_path,
              paste0("max_iter_frequencies", diff_cfg$scen_case, ".RDS"))
  )
  
  # Create + save tables
  tbls <- make_and_save_summary_tables(
    max_freqs = max_freqs,
    results_path = diff_cfg$results_path,
    mapping_df = GCAM_region_ID_mapping,
    output_stub = diff_cfg$output_stub
  )
  iter_table <- tbls$iter_table
  
  # Export parameters (Qtot-only cases)
  hi_col_name <- paste0(diff_cfg$hi_name, "_Qtot")
  lo_col_name <- paste0(diff_cfg$lo_name, "_Qtot")
  case_cols <- c(hi_col_name, lo_col_name)
  
  param_out <- export_params_for_iter_table(
    iter_table   = iter_table,
    case_cols    = case_cols,
    input_dir    = input_dir,
    results_path = diff_cfg$results_path,
    output_stub  = diff_cfg$output_stub
  )
  
  save_world_maxfreq_param_csvs(
    iter_table     = iter_table,
    case_cols      = case_cols,
    params_global  = param_out$params_global,
    params_FE      = param_out$params_FE,
    results_path   = diff_cfg$results_path,
    output_stub    = diff_cfg$output_stub
  )
  
  # Save scenario projections for identified parameters + ML
  save_case_and_ml_projections(
    iter_table     = iter_table,
    reg_list       = reg_list,
    results_path   = diff_cfg$results_path,
    read_dir       = diff_cfg$read_dir,
    scen_case      = diff_cfg$scen_case,
    case_cols      = case_cols,
    file_prefix    = diff_cfg$file_prefix,
    ml_file_prefix = diff_cfg$ml_prefix
  )
}

#------------------------------------------------------------------------------
# BOTH: Build iteration table, export params, and save scenario projections
# (Your existing structure is preserved; only adds ML projection saves here.)
#------------------------------------------------------------------------------

if (both_cfg$enabled) {
  
  message("BOTH case:")
  message("Identifying parameters for H/L outcomes for abs and diff in demand")
  
  # calculate parameters by taking iteration that falls most frequently in high/low
  # intervals of BOTH absolute and difference in demand
  res_both <- make_BOTH_params_freq(
    year_vals      = year_iter,
    data_dir       = procdata_dir,
    abs_dir        = both_cfg$abs_dir,
    abs_scen_case  = both_cfg$abs_scen_case,
    diff_dir       = both_cfg$diff_dir,
    diff_scen_case = both_cfg$diff_scen_case,
    out_dir        = both_cfg$results_path,
    regions        = reg_list,
    abs_bounds     = both_cfg$abs_bounds,
    diff_bounds    = both_cfg$diff_bounds,
    save_outputs   = TRUE
  )
  
  message("Finished identifying parameters")
  
  # load iteration frequency results
  max_freqs <- readRDS(file.path(both_cfg$results_path, "max_iter_frequencies_BOTH.RDS"))
  
  # Create + save tables (keep BOTH-specific CSV naming)
  tbls <- make_and_save_summary_tables(
    max_freqs = max_freqs,
    results_path = both_cfg$results_path,
    mapping_df = GCAM_region_ID_mapping,
    output_stub = NULL
  )
  iter_table <- tbls$iter_table
  
  write.csv(
    iter_table,
    file.path(both_cfg$results_path, "iter_table_BOTH_Qtot.csv"),
    row.names = FALSE
  )
  
  # Export params for BOTH (Qtot-only cases)
  combo_col_names <- paste0(both_cfg$combo_names, "_Qtot")
  
  param_out <- export_params_for_iter_table(
    iter_table   = iter_table,
    case_cols    = combo_col_names,
    input_dir    = input_dir,
    results_path = both_cfg$results_path,
    output_stub  = "BOTH"
  )
  
  save_world_maxfreq_param_csvs(
    iter_table     = iter_table,
    case_cols      = combo_col_names,
    params_global  = param_out$params_global,
    params_FE      = param_out$params_FE,
    results_path   = both_cfg$results_path,
    output_stub    = "BOTH"
  )
  
  # Save projections (existing logic), plus save ML projections in same folders
  message("Saving projections for identified parameters")
  
  iter_world <- iter_table %>% filter(ID == 33)
  if (nrow(iter_world) != 1) {
    stop("Expected exactly one row with ID == 33 (World).")
  }
  
  scen_out_abs  <- file.path(both_cfg$results_path, "scenarios_abs")
  scen_out_diff <- file.path(both_cfg$results_path, "scenarios_diff")
  dir.create(scen_out_abs,  recursive = TRUE, showWarnings = FALSE)
  dir.create(scen_out_diff, recursive = TRUE, showWarnings = FALSE)
  
  for (cc in combo_col_names) {
    
    it <- iter_world[[cc]][1]
    
    if (is.na(it)) {
      message("No iteration identified for case ", cc)
      next
    }
    
    # ABS demand levels (read from abs_dir, write to BOTH dir)
    scen_abs <- map_dfr(reg_list, function(r) {
      readRDS(file.path(both_cfg$abs_dir, paste0("demand_R", r, both_cfg$abs_scen_case, ".RDS"))) %>%
        filter(iteration == it)
    })
    
    saveRDS(
      scen_abs,
      file.path(scen_out_abs, paste0("demand_allregions_", cc, ".RDS"))
    )
    
    # DIFF demand responses (read from diff_dir, write to BOTH dir)
    scen_diff <- map_dfr(reg_list, function(r) {
      readRDS(file.path(both_cfg$diff_dir, paste0("demand_R", r, both_cfg$diff_scen_case, ".RDS"))) %>%
        filter(iteration == it)
    })
    
    saveRDS(
      scen_diff,
      file.path(scen_out_diff, paste0("demand_diffs_allregions_", cc, ".RDS"))
    )
  }
  
  # --------------------------------------------------------------------------
  # ALSO save ML projections for BOTH case (abs + diff), same "scenarios_*" folders
  # --------------------------------------------------------------------------
  
  iter_ML <- readRDS(
    file.path("data", "processed", procdata_dir, param_intervals_dir, "params_ML_intervals_global.RDS")
  ) %>%
    filter(measure == "ML") %>%
    pull(iteration)
  
  scen_ml_abs <- map_dfr(reg_list, function(r) {
    readRDS(file.path(both_cfg$abs_dir, paste0("demand_R", r, both_cfg$abs_scen_case, ".RDS"))) %>%
      filter(iteration == iter_ML)
  })
  saveRDS(
    scen_ml_abs,
    file.path(scen_out_abs, "demand_allregions_MLparams.RDS")
  )
  
  scen_ml_diff <- map_dfr(reg_list, function(r) {
    readRDS(file.path(both_cfg$diff_dir, paste0("demand_R", r, both_cfg$diff_scen_case, ".RDS"))) %>%
      filter(iteration == iter_ML)
  })
  saveRDS(
    scen_ml_diff,
    file.path(scen_out_diff, "demand_diffs_allregions_MLparams.RDS")
  )
}

