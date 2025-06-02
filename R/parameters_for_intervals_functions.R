# function for calculating parameters representative of high demand (HD) and low
# demand (LD) outcomes for an ensemble of demand results based on parameters
# indicated by "data_dir" and income and prices from "scen". 
# Method selects demand iterations that fall in high and low intervals of demand
# defined by percentiles given in the _min and _max arguments, for region numbers
# contained in "regions" and years given in "year_vals".
# Results are saved in sub-directory given by "data_dir" and "scen", with "scen_case"
# appended to the file name.

make_HD_LD_params_freq <- function(
    year_vals,
    data_dir,
    scen,
    scen_case,
    regions,
    hi_interval_min,
    hi_interval_max,
    lo_interval_min,
    lo_interval_max,
    save_outputs = TRUE
) {
  
  message("Calculating HD and LD parameters for scenario ", scen)
  
  # Central output directory path
  out_dir <- file.path("data", "processed", data_dir, scen)
  if (save_outputs) {
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  }
  
  # Load parameter files
  param_file_global <- file.path("data", "processed", data_dir, "inputs", "param_data_global_clean_sub.RData")
  param_file_FE     <- file.path("data", "processed", data_dir, "inputs", "param_data_FE_clean_sub.RData")
  load(param_file_global)
  load(param_file_FE)
  
  # Define interval bounds
  bounds <- list(
    HD = list(min = hi_interval_min, max = hi_interval_max),
    LD = list(min = lo_interval_min, max = lo_interval_max)
  )
  
  message("  Extracting demand in HD/LD intervals for all regions and years")
  
  demand_intervals <- get_demand_intervals(year_vals, data_dir, scen, regions, bounds)
  
  if (save_outputs) {
    saveRDS(demand_intervals, 
            file.path(out_dir, paste0("demand_intervals", scen_case, ".RDS")))
  }
  
  message("  Computing maximum-frequency iterations in HD/LD intervals")
  
  max_iter_frequencies <- compute_max_iteration_frequencies(demand_intervals, data_dir, scen)
  
  if (save_outputs) {
    saveRDS(max_iter_frequencies, 
            file.path(out_dir, paste0("max_iter_frequencies", scen_case, ".RDS")))
  }
  
  message("  Building full iteration frequency table")
  
  full_freq_table <- build_full_frequency_table(demand_intervals, max_iter_frequencies, regions)
  
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

# function for getting iterations that are members of certain "cases" defined as the value of
# a type of food demand falling in given quantile; food types include Qn, Qs, Qtot, and Qs and
# Qn simultaneously; quantiles include those defined for high demand and low demand as passed
# in "bounds"
get_demand_intervals <- function(year_vals, data_dir, scen, regions, bounds) {
  
  # loop over regions
  demand_intervals_list <- map(regions, function(r) {
    
    # get demand ensemble for region
    demand_reg_path <- file.path("data", "processed", data_dir, scen, paste0("demand_R", r, "_ens_bc.RDS"))
    demand_reg <- readRDS(demand_reg_path)
    
    # loop over years
    map_dfr(year_vals, function(year) {
      
      demand_year <- demand_reg[demand_reg$year == year, ]
      d_unique <- demand_year %>%
        distinct(iteration, year, GCAM_region_ID, Y.region, Qs.region, Qn.region, Qtot.region)
      
      # ---- Regular quantiles (Qs, Qn, Qtot) --------------------
      regular_cases <- expand.grid(c("HD", "LD"), c("Qs", "Qn", "Qtot")) %>%
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
      qsqn_cases <- expand.grid(c("HD", "LD"), "QsQn") %>%
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


# given a data frame with iterations tagged by intervals they fall within, 
# calculate the iteration that appears with the highest frequency across 
# years and deciles, for each region individually and for all regions combined,
# and return as a data frame
compute_max_iteration_frequencies <- function(demand_result, data_dir, scen) {
  
  # interval/food type cases expressed as list of pairs
  grid <- expand.grid(c("HD", "LD"), c("Qs", "Qn", "Qtot", "QsQn"))
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


build_full_frequency_table <- function(demand_result, max_iterations, regions) {

  # list of cases for which we want to find an iteration that falls within it
  # with maximum frequency
  case_list <- c("HD_Qs", "HD_Qn", "HD_Qtot", "HD_QsQn",
                 "LD_Qs", "LD_Qn", "LD_Qtot", "LD_QsQn")
  
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


build_parameter_tables <- function(max_iterations, param_global, param_FE, data_dir, scen) {
  
  list(
    params_global <- map_dfr(seq_len(nrow(max_iterations)), function(i) {
      param_global[param_global$iteration == max_iterations[i, "max_iter"], ] %>%
        mutate(measure = max_iterations[i, "case"], freq = max_iterations[i, "freq"]) %>%
        relocate(measure)
    }),
    params_FE <- map_dfr(seq_len(nrow(max_iterations)), function(i) {
      param_FE[param_FE$iteration == max_iterations[i, "max_iter"], ] %>%
        mutate(measure = max_iterations[i, "case"], freq = max_iterations[i, "freq"]) %>%
        relocate(measure)
    })
  )
}


# function for plotting results

plot_demand_measures_with_global <- function(
    freq_table,
    demand_result,
    region,
    measures,
    data_dir,
    scen,
    sample_n = 100,
    title = TRUE
) {
  stopifnot(length(measures) == 2)
  
  # Load full regional ensemble for sample lines
  demand_reg_path <- file.path("data", "processed", data_dir, scen, paste0("demand_R", region, "_ens_bc.RDS"))
  demand_reg <- readRDS(demand_reg_path)
  
  # Sample 100 unique iterations
  sampled_iterations <- demand_reg %>%
    distinct(iteration, year, Qs.region, Qn.region, Qtot.region) %>%
    pull(iteration) %>%
    unique() %>%
    sample(size = min(sample_n, length(.)))
  
  sampled_data <- demand_reg %>%
    filter(iteration %in% sampled_iterations) %>%
    distinct(iteration, year, Qs.region, Qn.region, Qtot.region) %>%
    tidyr::pivot_longer(
      cols = c(Qs.region, Qn.region, Qtot.region),
      names_to = "demand_type",
      values_to = "demand_value"
    )
  
  # Grab both regional + global iterations per measure
  iter_info <- freq_table %>%
    filter(GCAM_region_ID %in% c(region, 33), case %in% measures) %>%
    mutate(source = ifelse(GCAM_region_ID == region, "regional", "global"))
  
  if (nrow(iter_info) != 4) {
    stop(glue::glue("Expected 4 rows (2 cases x 2 sources), got {nrow(iter_info)}"))
  }
  
  # Get demand for both regional + global iterations
  demand_compare <- map_dfr(seq_len(nrow(iter_info)), function(i) {
    row <- iter_info[i, ]
    if (row$source == "regional") {
      # pull from demand_result (filtered)
      demand_result %>%
        filter(
          GCAM_region_ID == region,
          measure == row$case,
          iteration == row$max_iter
        ) %>%
        mutate(source = row$source)
    } else {
      # pull from full regional ensemble (unfiltered)
      # get the demand type from the measure
      fd_type <- stringr::str_extract(row$case, "Qs|Qn|Qtot")
      col_name <- paste0(fd_type, ".region")
      
      demand_reg %>%
        filter(iteration == row$max_iter) %>%
        distinct(iteration, year, GCAM_region_ID, Qs.region, Qn.region, Qtot.region) %>%
        mutate(
          measure = row$case,
          source = row$source
        )
    }
  })
  
  # Pivot to long format
  plot_data <- demand_compare %>%
    tidyr::pivot_longer(
      cols = c(Qs.region, Qn.region, Qtot.region),
      names_to = "demand_type",
      values_to = "demand_value"
    )
  
  # Plot: background ensemble + overlaid lines
  ggplot2::ggplot() +
    # Background sample
    ggplot2::geom_line(
      data = sampled_data,
      ggplot2::aes(x = year, y = demand_value, group = interaction(iteration, demand_type)),
      color = "gray85", linewidth = 0.4, alpha = 0.5
    ) +
    # Main demand lines
    ggplot2::geom_line(
      data = plot_data,
      ggplot2::aes(x = year, y = demand_value, color = measure, linetype = source),
      linewidth = 1.2
    ) +
    ggplot2::facet_wrap(~ demand_type, scales = "free_y", nrow = 1) +
    ggplot2::labs(
      x = "Year",
      y = "Demand",
      color = "Measure",
      linetype = "Source",
      title = if (title) glue::glue("Region {region} | Regional vs Global Max Iteration Intervals") else NULL
    ) +
    ggplot2::theme_minimal(base_size = 13)
}






# function for calculating parameters representative of high demand (HD) and low
# demand (LD) outcomes for a given food type (fd_type), for a given income
# scenario (inc_scen), for a list of regions (regions), and price scenario (pr_scen).
# Selects demand iterations that fall in specified high and low quantile intervals at
# each income level, then takes median of parameter values that produced those
# iterations. Saves results (median parameter values at each income level) to a
# single file containing both global and FE parameters in the folder corresponding
# to the price scenario.
make_HD_LD_params_median <- function(fd_type,inc_scen,pr_scen,regions,hi_interval_min,
                                     hi_interval_max,lo_interval_min,lo_interval_max) {
  
  # load parameter files
  load(paste(analysis_dir,input_path,"param_data_global_clean_sub.RData",sep="/"))
  load(paste(analysis_dir,input_path,"param_data_FE_clean_sub.RData",sep="/"))
  
  result <- data.frame()
  # region loop
  for(i in 1:length(regions)) {
    
    r <- regions[i]
    # load regional demand file for specific scenario
    load(paste0(analysis_dir,"/",results_path,"/",pr_scen,"/demand_R",r,".RData"))
    
    reg_result <- lapply(inc_scen,function(x) {
      
      # get demand results for specific income level
      d_reg_at_income <- demand_reg[demand_reg$Y == x,]
      # get demand results from high interval and associated parameter samples
      d_hi_min <- quantile(d_reg_at_income[[fd_type]],probs = hi_interval_min/100)
      d_hi_max <- quantile(d_reg_at_income[[fd_type]],probs = hi_interval_max/100)
      d_hi_interval <- d_reg_at_income[d_reg_at_income[[fd_type]] > d_hi_min &
                                         d_reg_at_income[[fd_type]] <= d_hi_max,]
      param_global_hi_interval <-
        subset(param_data_global_clean_sub,iteration %in% d_hi_interval$iteration)
      param_FE_hi_interval <-
        subset(param_data_FE_clean_sub,iteration %in% d_hi_interval$iteration &
                 GCAM_region_ID == r)
      # join global and FE parameters; this does not guarantee match in iteration
      # number between the parameters, but for the purpose of taking medians that's ok
      param_hi_interval <-
        param_global_hi_interval %>%
        mutate(staples_FE = param_FE_hi_interval$staples_FE) %>%
        relocate(staples_FE)
      # get demand results from low interval and associated parameter samples
      d_lo_min <- quantile(d_reg_at_income[[fd_type]],probs = lo_interval_min/100)
      d_lo_max <- quantile(d_reg_at_income[[fd_type]],probs = lo_interval_max/100)
      d_lo_interval <- d_reg_at_income[d_reg_at_income[[fd_type]] >= d_lo_min &
                                         d_reg_at_income[[fd_type]] < d_lo_max,]
      param_global_lo_interval <-
        subset(param_data_global_clean_sub,iteration %in% d_lo_interval$iteration)
      param_FE_lo_interval <-
        subset(param_data_FE_clean_sub,iteration %in% d_lo_interval$iteration &
                 GCAM_region_ID == r)
      # join global and FE parameters; this does not guarantee match in iteration
      # number between the parameters, but for the purpose of taking medians that's ok
      param_lo_interval <-
        param_global_lo_interval %>%
        mutate(staples_FE = param_FE_lo_interval$staples_FE) %>%
        relocate(staples_FE)
      # get medians of parameter conditional distributions (should try modes)
      bind_rows(
        param_hi_centralval <- param_hi_interval %>%
          summarize(across(c('staples_FE':'pnscl'),median)) %>%
          mutate(Y=x,GCAM_region_ID=r,measure="HD") %>%
          relocate(measure),
        param_lo_centralval <- param_lo_interval %>%
          summarize(across(c('staples_FE':'pnscl'),median)) %>%
          mutate(Y=x,GCAM_region_ID=r,measure="LD") %>%
          relocate(measure))
    }) %>%
      bind_rows() # bind results for each income level together
    result <- bind_rows(result,reg_result) # accumulate results for regions
  }
  save(result,file = paste0(analysis_dir,"/",results_path,"/",pr_scen,"/",
                            "params_HDLD_",fd_type,"_median.RData"))
}

