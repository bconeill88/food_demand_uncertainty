# ------------------------------------------------------------------------------
# Parameter interval helper functions
# ------------------------------------------------------------------------------
#
# Helper functions for identifying parameter iterations that are representative
# of high/low food demand outcomes, high/low price responses, and joint
# absolute-demand/price-response cases. The functions also create summary
# tables, export selected parameter sets, and save demand projections for the
# selected iterations.
#
# Functions included:
# - make_HL_params_freq(): Orchestrates interval extraction, frequency counting,
#   and parameter-table creation for one high/low outcome family, such as ABS
#   demand or DIFF price-response outcomes.
# - make_BOTH_params_freq(): Orchestrates the corresponding workflow for joint
#   ABS and DIFF cases such as HD_HPR and LD_LPR.
# - get_demand_intervals(): Reads regional ensemble files and tags iterations
#   that fall within high/low quantile intervals for Qs, Qn, Qtot, and QsQn.
# - get_demand_intervals_both(): Reads ABS and DIFF regional ensemble files and
#   tags iterations that satisfy both interval conditions for the same
#   region/year.
# - compute_max_iteration_frequencies(): Finds the most frequently selected
#   iteration for each ABS or DIFF case, by region and globally.
# - compute_max_iteration_frequencies_both(): Finds the most frequently selected
#   iteration for each BOTH case, by region and globally.
# - build_full_frequency_table(): Builds a detailed frequency table for ABS or
#   DIFF cases across regions, income categories, and food-demand types.
# - build_full_frequency_table_both(): Builds a detailed frequency table for
#   BOTH cases and flags maximum-frequency iterations.
# - build_parameter_tables(): Extracts global and food-expenditure parameter
#   rows for selected maximum-frequency iterations.
# - make_and_save_summary_tables(): Creates and saves wide summary tables for
#   maximum frequencies and selected iterations.
# - export_params_for_iter_table(): Exports parameter CSVs for selected
#   iterations listed in an iteration summary table.
# - save_world_maxfreq_param_csvs(): Saves reduced World-level Qtot parameter
#   CSVs used as GCAM run inputs.
# - save_case_and_ml_projections(): Saves all-region demand projections for
#   selected cases and the ML parameter set.
#
# ------------------------------------------------------------------------------

# Identify high/low interval parameter sets for one outcome family.
make_HL_params_freq <- function(
    year_vals,
    data_dir,
    out_dir,
    scen,
    scen_case,
    regions,
    hi_interval_min,
    hi_interval_max,
    lo_interval_min,
    lo_interval_max,
    hi_param_name,
    lo_param_name,
    save_outputs = TRUE
) {
  
  message("Calculating ", hi_param_name, " and ", lo_param_name, 
          " parameters for scenario ", scen)
  
  # Central output directory path
  if (save_outputs) {
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  }
  
  # Load parameter files
  param_file_global <- file.path("data", "processed", data_dir, clean_data_dir, "param_data_global_clean_sub.RDS")
  param_file_FE     <- file.path("data", "processed", data_dir, clean_data_dir, "param_data_FE_clean_sub.RDS")
  param_data_global_clean_sub <- readRDS(param_file_global)
  param_data_FE_clean_sub <- readRDS(param_file_FE)
  
  # Define interval bounds
  bounds <- setNames(
    list(
      H = list(min = hi_interval_min, max = hi_interval_max),
      L = list(min = lo_interval_min, max = lo_interval_max)),
    c(hi_param_name, lo_param_name)
  )

  message("  Extracting demand in ", hi_param_name, "/", lo_param_name, 
          " intervals for all regions and years")
  
  demand_intervals <- get_demand_intervals(year_vals, out_dir, scen_case, regions, 
                                           bounds, hi_param_name, lo_param_name)
  
  if (save_outputs) {
    saveRDS(demand_intervals, 
            file.path(out_dir, paste0("demand_intervals", scen_case, ".RDS")))
  }
  
  message("  Computing maximum-frequency iterations in ", hi_param_name, "/", 
          lo_param_name, " intervals")
  
  max_iter_frequencies <- compute_max_iteration_frequencies(demand_intervals,
                                                            hi_param_name,
                                                            lo_param_name)
  
  if (save_outputs) {
    saveRDS(max_iter_frequencies, 
            file.path(out_dir, paste0("max_iter_frequencies", scen_case, ".RDS")))
  }
  
  message("  Building full iteration frequency table")
  
  full_freq_table <- build_full_frequency_table(demand_intervals, max_iter_frequencies, 
                                                regions, hi_param_name, lo_param_name)
  
  if (save_outputs) {
    saveRDS(full_freq_table, 
            file.path(out_dir, paste0("full_freq_table", scen_case, ".RDS")))
  }
  
  message("  Building parameter tables")
  
  param_tables <- build_parameter_tables(
    max_iter_frequencies,
    param_data_global_clean_sub,
    param_data_FE_clean_sub
  )
  
  if (save_outputs) {
    saveRDS(param_tables$param_table_global, 
            file.path(out_dir, paste0("param_table_global", scen_case, ".RDS")))
    saveRDS(param_tables$param_table_FE, 
            file.path(out_dir, paste0("param_table_FE", scen_case, ".RDS")))
  }
}

# ------------------------------------------------------------------------------

# Identify parameter sets that jointly satisfy absolute-demand and price-response intervals.
make_BOTH_params_freq <- function(
    year_vals,
    data_dir,          # needed to load parameter files
    abs_dir,
    abs_scen_case,
    diff_dir,
    diff_scen_case,
    out_dir,
    regions,
    abs_bounds,
    diff_bounds,
    save_outputs = TRUE
) {
  
  both_case <- "_BOTH"
  
  message("Calculating HD/LD - HPR/LPR parameter sets")
  
  if (save_outputs) {
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  }
  
  # load parameter files
  param_file_global <- file.path("data", "processed", data_dir, clean_data_dir, 
                                 "param_data_global_clean_sub.RDS")
  param_file_FE     <- file.path("data", "processed", data_dir, clean_data_dir, 
                                 "param_data_FE_clean_sub.RDS")
  param_data_global_clean_sub <- readRDS(param_file_global)
  param_data_FE_clean_sub     <- readRDS(param_file_FE)
  
  message("  Extracting outcomes in intervals for all regions and years")
  
  demand_intervals_both <- get_demand_intervals_both(
    year_vals      = year_vals,
    abs_dir        = abs_dir,
    abs_scen_case  = abs_scen_case,
    diff_dir       = diff_dir,
    diff_scen_case = diff_scen_case,
    regions        = regions,
    abs_bounds     = abs_bounds,
    diff_bounds    = diff_bounds,
    fd_types       = c("Qtot")
  )
  
  if (save_outputs) {
    saveRDS(demand_intervals_both,
            file.path(out_dir, paste0("demand_intervals", both_case, ".RDS")))
  }
  
  message("  Computing maximum-frequency iterations in intervals")
  
  max_iter_frequencies <- compute_max_iteration_frequencies_both(demand_intervals_both)
  
  if (save_outputs) {
    saveRDS(max_iter_frequencies,
            file.path(out_dir, paste0("max_iter_frequencies", both_case, ".RDS")))
  }
  
  message("  Building full iteration frequency table")
  
  full_freq_table <- build_full_frequency_table_both(
    demand_intervals_both,
    max_iter_frequencies,
    regions
  )
  
  if (save_outputs) {
    saveRDS(full_freq_table,
            file.path(out_dir, paste0("full_freq_table", both_case, ".RDS")))
  }
  
  message("  Building parameter tables")
  
  param_tables <- build_parameter_tables(
    max_iter_frequencies,
    param_data_global_clean_sub,
    param_data_FE_clean_sub
  )
  
  if (save_outputs) {
    saveRDS(param_tables$param_table_global,
            file.path(out_dir, paste0("param_table_global", both_case, ".RDS")))
    saveRDS(param_tables$param_table_FE,
            file.path(out_dir, paste0("param_table_FE", both_case, ".RDS")))
  }
  
  invisible(list(
    demand_intervals_both  = demand_intervals_both,
    max_iter_frequencies   = max_iter_frequencies,
    full_freq_table        = full_freq_table,
    param_table_global     = param_tables$param_table_global,
    param_table_FE         = param_tables$param_table_FE
  ))
}

# ------------------------------------------------------------------------------

# Extract iterations falling within high/low intervals for ABS or DIFF outcomes.
get_demand_intervals <- function(year_vals, out_dir, scen_case, regions, bounds,
                                 hi_param_name, lo_param_name) {
  
  # loop over regions
  demand_intervals_list <- map(regions, function(r) {
    
    # get demand ensemble for region
    demand_reg_path <- file.path(out_dir, paste0("demand_R", r, scen_case, ".RDS"))
    demand_reg <- readRDS(demand_reg_path)
    
    # loop over years
    map_dfr(year_vals, function(year) {
      
      demand_year <- demand_reg[demand_reg$year == year, ]
      d_unique <- demand_year %>%
        distinct(iteration, year, GCAM_region_ID, Y.region, Qs.region, Qn.region, Qtot.region)
      
      # ---- Regular quantiles (Qs, Qn, Qtot) --------------------
      regular_cases <- expand.grid(c(hi_param_name, lo_param_name), 
                                   c("Qs", "Qn", "Qtot")) %>%
        split(seq(nrow(.)))
      
      regular_results <- map_dfr(regular_cases, function(case_pair) {
        interval <- as.character(case_pair[[1]])
        fd_type  <- as.character(case_pair[[2]])
        
        col <- paste0(fd_type, ".region")
        d_min <- quantile(d_unique[[col]], probs = bounds[[interval]]$min / 100)
        d_max <- quantile(d_unique[[col]], probs = bounds[[interval]]$max / 100)
        
        d_unique[d_unique[[col]] >= d_min & d_unique[[col]] <= d_max, ] %>%
          mutate(measure = paste0(interval, "_", fd_type)) %>%
          relocate(measure)
      })
      
      # ---- Special case: QsQn (intersection) --------------------
      qsqn_cases <- expand.grid(c(hi_param_name, lo_param_name), "QsQn") %>%
        split(seq(nrow(.)))
      
      qsqn_results <- map_dfr(qsqn_cases, function(case_pair) {
        interval <- as.character(case_pair[[1]])
        
        d_min_Qs <- quantile(d_unique$Qs.region, probs = bounds[[interval]]$min / 100)
        d_max_Qs <- quantile(d_unique$Qs.region, probs = bounds[[interval]]$max / 100)
        d_min_Qn <- quantile(d_unique$Qn.region, probs = bounds[[interval]]$min / 100)
        d_max_Qn <- quantile(d_unique$Qn.region, probs = bounds[[interval]]$max / 100)
        
        d_unique %>%
          filter(
            Qs.region >= d_min_Qs, Qs.region <= d_max_Qs,
            Qn.region >= d_min_Qn, Qn.region <= d_max_Qn
          ) %>%
          mutate(measure = paste0(interval, "_QsQn")) %>%
          relocate(measure)
      })
      
      bind_rows(regular_results, qsqn_results)
    })
  })
  
  bind_rows(demand_intervals_list)
}

# ------------------------------------------------------------------------------

# Extract iterations satisfying both ABS and DIFF interval conditions.
get_demand_intervals_both <- function(
    year_vals,
    abs_dir,
    abs_scen_case,
    diff_dir,
    diff_scen_case,
    regions,
    abs_bounds,
    diff_bounds,
    abs_hi_name = "HD",
    abs_lo_name = "LD",
    diff_hi_name = "HPR",
    diff_lo_name = "LPR",
    fd_types = c("Qtot")  # keep small/fast; can expand to c("Qs","Qn","Qtot","QsQn")
) {
  
  # All four combined cases
  combo_grid <- expand.grid(
    abs  = c(abs_hi_name, abs_lo_name),
    diff = c(diff_hi_name, diff_lo_name),
    stringsAsFactors = FALSE
  )
  
  out_list <- map(regions, function(r) {
    
    # Read each file ONCE per region
    abs_path  <- file.path(abs_dir,  paste0("demand_R", r, abs_scen_case,  ".RDS"))
    diff_path <- file.path(diff_dir, paste0("demand_R", r, diff_scen_case, ".RDS"))
    
    demand_abs  <- readRDS(abs_path)
    demand_diff <- readRDS(diff_path)
    
    map_dfr(year_vals, function(yy) {
      
      a <- demand_abs %>%
        filter(year == yy) %>%
        distinct(iteration, year, GCAM_region_ID, Y.region,
                 Qs.region, Qn.region, Qtot.region) %>%
        rename(
          Qs.region_abs   = Qs.region,
          Qn.region_abs   = Qn.region,
          Qtot.region_abs = Qtot.region
        )
      
      # Note: drop Y.region from DIFF (it is difference in income, i.e. zero) so 
      # ABS version is the only one carried forward
      d <- demand_diff %>%
      filter(year == yy) %>%
        distinct(iteration, year, GCAM_region_ID,
                 Qs.region, Qn.region, Qtot.region) %>%
        rename(
          Qs.region_diff   = Qs.region,
          Qn.region_diff   = Qn.region,
          Qtot.region_diff = Qtot.region
        )
      
      # Inner-join aligns the same iteration/year/region across ABS and DIFF
      ad <- inner_join(a, d, by = c("iteration", "year", "GCAM_region_ID"))
      
      if (nrow(ad) == 0) return(NULL)
      
      # For each combo (e.g., HD_HPR) and each fd_type (default Qtot),
      # compute bounds separately in ABS and DIFF, then intersect by filtering ad
      map_dfr(seq_len(nrow(combo_grid)), function(i) {
        
        abs_lab  <- combo_grid$abs[[i]]
        diff_lab <- combo_grid$diff[[i]]
        combo    <- paste(abs_lab, diff_lab, sep = "_")
        
        map_dfr(fd_types, function(fd) {
          
          if (fd == "QsQn") {
            # If you ever enable this, implement intersection logic analogous to your current QsQn code
            stop("QsQn not implemented in get_demand_intervals_both() yet.")
          }
          
          col_abs  <- paste0(fd, ".region_abs")
          col_diff <- paste0(fd, ".region_diff")
          
          a_min <- quantile(ad[[col_abs]],  probs = abs_bounds[[abs_lab]]$min  / 100, na.rm = TRUE)
          a_max <- quantile(ad[[col_abs]],  probs = abs_bounds[[abs_lab]]$max  / 100, na.rm = TRUE)
          d_min <- quantile(ad[[col_diff]], probs = diff_bounds[[diff_lab]]$min / 100, na.rm = TRUE)
          d_max <- quantile(ad[[col_diff]], probs = diff_bounds[[diff_lab]]$max / 100, na.rm = TRUE)
          
          ad %>%
            filter(
              .data[[col_abs]]  >= a_min, .data[[col_abs]]  <= a_max,
              .data[[col_diff]] >= d_min, .data[[col_diff]] <= d_max
            ) %>%
            transmute(
              measure = paste0(combo, "_", fd),
              iteration, year, GCAM_region_ID, Y.region,
              Qs.region = Qs.region_abs,
              Qn.region = Qn.region_abs,
              Qtot.region = Qtot.region_abs
            )
        })
      })
    })
  })
  
  bind_rows(out_list)
}

# ------------------------------------------------------------------------------

# Select the most frequent iterations for high/low ABS or DIFF interval cases.
compute_max_iteration_frequencies <- function(demand_result, hi_param_name, 
                                              lo_param_name) {
  
  # interval/food type cases expressed as list of pairs
  grid <- expand.grid(c(hi_param_name, lo_param_name), c("Qs", "Qn", "Qtot", "QsQn"))
  case_list <- split(grid, seq(nrow(grid)))
  
  # for each case calculate max iteration frequencies for each region and globally
  max_iteration_frequencies <- map_dfr(case_list, function(case_pair) {
    
    interval <- as.character(case_pair[[1]])
    fd_type  <- as.character(case_pair[[2]])
    case <- paste0(interval, "_", fd_type)
    
    # filter demand_result for the selected case
    demand_subset <- demand_result %>%
      filter(measure == case)
    
    if (nrow(demand_subset) == 0) {
      warning(glue::glue("No results found for case: {case}"))
      return(NULL)
    }
    
    # ---- REGIONAL max frequency ----
    regional_max <- demand_subset %>%
      distinct(GCAM_region_ID, year, iteration) %>%
      count(GCAM_region_ID, iteration, name = "freq") %>%
      group_by(GCAM_region_ID) %>%
      arrange(desc(freq), .by_group = TRUE) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(case = case)
    
    # ---- GLOBAL max frequency (region 33) ----
    global_max <- demand_subset %>%
      distinct(GCAM_region_ID, year, iteration) %>%
      count(iteration, name = "freq") %>%
      arrange(desc(freq)) %>%
      slice(1) %>%
      mutate(
        GCAM_region_ID = 33,
        case = case
      )
    
    # Combine regional + global
    bind_rows(regional_max, global_max) %>%
      select(case, GCAM_region_ID, max_iter = iteration, freq) %>%
      arrange(case, GCAM_region_ID, )
  })
}

# ------------------------------------------------------------------------------

# Select the most frequent iterations for BOTH interval cases.
compute_max_iteration_frequencies_both <- function(demand_result_both) {
  
  cases <- sort(unique(demand_result_both$measure))
  
  map_dfr(cases, function(case) {
    
    demand_subset <- demand_result_both %>% filter(measure == case)
    if (nrow(demand_subset) == 0) return(NULL)
    
    regional_max <- demand_subset %>%
      distinct(GCAM_region_ID, year, iteration) %>%
      count(GCAM_region_ID, iteration, name = "freq") %>%
      group_by(GCAM_region_ID) %>%
      arrange(desc(freq), .by_group = TRUE) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(case = case)
    
    global_max <- demand_subset %>%
      distinct(GCAM_region_ID, year, iteration) %>%
      count(iteration, name = "freq") %>%
      arrange(desc(freq)) %>%
      slice(1) %>%
      mutate(GCAM_region_ID = 33, case = case)
    
    bind_rows(regional_max, global_max) %>%
      select(case, GCAM_region_ID, max_iter = iteration, freq)
  })
}

# ------------------------------------------------------------------------------

# Build the detailed frequency table for ABS or DIFF interval cases.
build_full_frequency_table <- function(demand_result, max_iterations, regions,
                                       hi_param_name, lo_param_name) {

  # list of cases for which we want to find an iteration that falls within it
  # with maximum frequency
  params   <- c(hi_param_name, lo_param_name)
  suffixes <- c("Qs", "Qn", "Qtot", "QsQn")
  case_list <- as.vector(outer(params, suffixes, paste, sep = "_"))
  
  # define elements for constructing a table to hold the frequency results
  conditions <- list(
    case = case_list,
    GCAM_region_ID = regions,
    income_cat = c("poor", "middle", "rich"),
    food_type = c("Qs", "Qn", "Qtot")
  )

  # set up table as combinations of conditions, plus columns for iteration number
  # and frequency, initialized to NA, to fill in below
  full_freq_table <- expand.grid(conditions, stringsAsFactors = FALSE) %>%
    mutate(max_iteration = NA, frequency = NA)
  
  # add income category to demand results
  demand_result <- demand_result %>%
    mutate(income_cat = case_when(
      Y.region < 10 ~ "poor",
      Y.region >= 10 & Y.region < 50 ~ "middle",
      Y.region >= 50 ~ "rich"
    ))

  # assign iteration and frequency to each row in the table
  for (c in seq_len(nrow(full_freq_table))) {

    # define some values from this row; case is from the case_list
    row <- full_freq_table[c, ]
    case <- row$case
    reg <- row$GCAM_region_ID
    inc_cat <- row$income_cat
    fd_type <- row$food_type
    
    # the type of demand results we want to look for in demand_results
    # (which interval, and which food type; frequency of all food types
    # are calculated for each "case" in the case_list
    target_case <- paste0(substr(case, 1, 2), "_", fd_type)

    # identify the iteration number that has the max frequency for this case
    max_iter <- max_iterations$max_iter[
      max_iterations$case == row$case &
        max_iterations$GCAM_region_ID == row$GCAM_region_ID]
    
    # fill in the iteration number and its frequency in the table
    full_freq_table[c, "max_iteration"] <- max_iter
    full_freq_table[c, "frequency"] <- demand_result %>%
      filter(
        measure == target_case,
        GCAM_region_ID == reg,
        income_cat == inc_cat,
        iteration == max_iter
      ) %>%
      nrow()
  }
  
  # add global results (Region 33)
  global_freq_table <- full_freq_table %>%
    filter(GCAM_region_ID %in% regions) %>%
    group_by(case, income_cat, food_type) %>%
    summarise(frequency = sum(frequency), .groups = "drop") %>%
    mutate(GCAM_region_ID = 33)
  
  # combine with regional results
  bind_rows(full_freq_table, global_freq_table) %>%
    arrange(case, GCAM_region_ID, food_type, income_cat)
}

# ------------------------------------------------------------------------------

# Build the detailed frequency table for BOTH interval cases.
build_full_frequency_table_both <- function(demand_intervals, max_iter_frequencies, regions) {
  
  cases <- sort(unique(demand_intervals$measure))
  
  map_dfr(cases, function(cc) {
    
    di <- demand_intervals %>%
      filter(measure == cc)
    
    # frequency of each iteration by region (across all years included)
    freq_tbl <- di %>%
      distinct(GCAM_region_ID, year, iteration) %>%
      count(GCAM_region_ID, iteration, name = "freq")
    
    # max iteration per region for this case (includes World=33 if present)
    max_tbl <- max_iter_frequencies %>%
      filter(case == cc) %>%
      select(GCAM_region_ID, max_iter, max_freq = freq)
    
    freq_tbl %>%
      left_join(max_tbl, by = "GCAM_region_ID") %>%
      mutate(
        case = cc,
        is_max_iter = (iteration == max_iter)
      ) %>%
      select(case, GCAM_region_ID, iteration, freq, max_iter, max_freq, is_max_iter)
  })
}

# ------------------------------------------------------------------------------

# Build parameter tables for selected maximum-frequency iterations.
build_parameter_tables <- function(max_iterations, param_global, param_FE) {
  
  list(
    param_table_global = map_dfr(seq_len(nrow(max_iterations)), function(i) {
      it   <- max_iterations$max_iter[[i]]
      case <- max_iterations$case[[i]]
      freq <- max_iterations$freq[[i]]
      
      param_global %>%
        filter(iteration == it) %>%
        mutate(measure = case, freq = freq) %>%
        relocate(measure)
    }),
    
    param_table_FE = map_dfr(seq_len(nrow(max_iterations)), function(i) {
      it   <- max_iterations$max_iter[[i]]
      case <- max_iterations$case[[i]]
      freq <- max_iterations$freq[[i]]
      
      param_FE %>%
        filter(iteration == it) %>%
        mutate(measure = case, freq = freq) %>%
        relocate(measure)
    })
  )
}

# ------------------------------------------------------------------------------

# Create and save summary tables of maximum frequencies and selected iterations.
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

# ------------------------------------------------------------------------------

# Export global and food-expenditure parameters for selected iterations.
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

# ------------------------------------------------------------------------------

# Save reduced parameter CSVs for World-level Qtot selected iterations.
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

# ------------------------------------------------------------------------------

# Save all-region projections for selected parameter cases and ML parameters.
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
  if (nrow(iter_world) != 1) stop("Expected exactly one row with ID == 33 (World).")
  
  # ---- Map case column -> iteration (world row) ----
  case_to_iter <- iter_world %>%
    select(all_of(case_cols)) %>%
    slice(1)
  
  iters_cases <- case_to_iter %>%
    unlist(use.names = TRUE) %>%
    as.integer()
  
  # ---- Identify ML iteration once ----
  iter_ML <- readRDS(
    file.path("data", "processed", procdata_dir, param_intervals_dir, "params_ML_intervals_global.RDS")
  ) %>%
    filter(measure == "ML") %>%
    pull(iteration)
  
  if (length(iter_ML) != 1 || is.na(iter_ML)) {
    stop("Expected exactly one non-NA ML iteration.")
  }
  
  # ---- Read each region file, filter to all iterations we will save ----
  iters_needed <- unique(na.omit(c(iters_cases, iter_ML)))
  
  message("  Reading regional files once and filtering to needed iterations")
  scen_needed <- map_dfr(reg_list, function(r) {
    readRDS(file.path(read_dir, paste0("demand_R", r, scen_case, ".RDS"))) %>%
      filter(iteration %in% iters_needed)
  })
  
  # Split for extraction
  scen_by_iter <- split(scen_needed, scen_needed$iteration)
  
  # ---- Save projections for identified parameter cases ----
  for (cc in case_cols) {
    
    it <- iters_cases[[cc]]
    
    if (is.na(it)) {
      message("  No iteration identified for case ", cc)
      next
    }
    
    cc_clean <- gsub("_Qtot$", "", cc)
    
    saveRDS(
      scen_by_iter[[as.character(it)]],
      file.path(results_path, paste0(file_prefix, cc_clean, "params.RDS"))
    )
  }
  
  # ---- Save ML projections ----
  saveRDS(
    scen_by_iter[[as.character(iter_ML)]],
    file.path(results_path, paste0(ml_file_prefix, "MLparams.RDS"))
  )
}

