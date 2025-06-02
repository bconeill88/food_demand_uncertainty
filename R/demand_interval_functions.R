
# identify and save uncertainty intervals for demand and elasticities ----------
# ----

# function for identifying ML demand, calculating independent and identifying
# joint confidence intervals in demand  for a given independent confidence interval 
# (ci), price/income scenario(s), and list of regions. independent confidence 
# intervals are calculated directly from the demand results, they do not represent 
# the demand that would result from the independent confidence intervals of the 
# underlying parameters. in contrast, the joint uncertainty intervals represent 
# the demand associated with the parameter iterations identified as representing 
# specific intervals, passed in "paramintervals"; if two scenarios are specified, 
# intervals are identified for the differences in demand between them; if scen2 is 
# "None", intervals are identified for demand in scen1; results are saved in a
# subdirectory to data/processed given by scen1, with "case" added to the file name
make_demand_intervals <- function(
    ci,
    scen1,
    scen2,
    case,
    regions,
    paramintervals,
    measures = c("LPR", "HPR", "LIR", "HIR", "LSR", "HSR"),
    data_dir
) {
  for (region_id in regions) {
    
    # Read demand or demand difference data
    if (scen2 == "None") {
      file_path <- file.path("data/processed", data_dir, scen1, 
                             paste0("demand_R", region_id, case, ".RDS"))
      outcome_reg <- readRDS(file_path)
    } else {
      file_path <- file.path(
        "data/processed", data_dir, scen1,
        paste0("demand_diffs_", scen1, "_", scen2, "_R", region_id, case, ".RDS")
      )
      outcome_reg <- readRDS(file_path)
    }
    
    # ML + independent interval results
    intervals <- make_demand_ml_intervals_region(outcome_reg, ci)
    
    # Add joint uncertainty measures
    for (measure in measures) {
      
      target_iteration <- paramintervals$iteration[paramintervals$measure == measure]
      
      joint_interval <- outcome_reg %>%
        filter(iteration == target_iteration) %>%
        mutate(measure = measure) %>%
        relocate(measure)
      
      intervals <- bind_rows(intervals, joint_interval)
    }
    
    # Save results
    if (scen2 == "None") {
      output_file <- 
        file.path("data/processed", data_dir, scen1,
                  paste0("demand_intervals_R", region_id, case, ".RDS")
      )
    } else {
      output_file <- 
        file.path("data/processed", data_dir, scen1,
                  paste0("demand_diffs_intervals_", scen1, "_", scen2, "_R", region_id, case, ".RDS")
      )
    }
    
    saveRDS(intervals, file = output_file)
  }
}

# function for calculating ML and independent confidence intervals for demand
# data for a single region and a given interval, for each decile; use method from 
# Edmonds et al: take the range of all variables after dropping samples below the 
# interval likelihood scores
make_demand_ml_intervals_region <- function(demand_data, ci) {
  
  # Identify ML iteration (choose first if multiple max LLs)
  i_max_ll <- demand_data$iteration[which.max(demand_data$LL)]
  
  demand_ml <- demand_data %>%
    filter(iteration == i_max_ll) %>%
    mutate(measure = "ML")
  
  # Filter iterations within confidence interval
  quant_ll <- quantile(demand_data$LL, probs = (1 - ci / 100))
  
  demand_ci <- demand_data %>%
    filter(LL > quant_ll)
  
  # Choose grouping column dynamically
  group_col <- if ("year" %in% names(demand_ci)) "year" else "Y"
  
  # get demand for max and min of confidence interval for each decile
  demand_max <- demand_ci %>%
    group_by(.data[[group_col]], `gcam-consumer`) %>%
    summarise(across(-c(GCAM_region_ID:region), max), .groups = "drop") %>%
    mutate(measure = "HI")
  
  demand_min <- demand_ci %>%
    group_by(.data[[group_col]], `gcam-consumer`) %>%
    summarise(across(-c(GCAM_region_ID:region), min), .groups = "drop") %>%
    mutate(measure = "LI")
  
  # Combine results and reattach region identifiers
  bind_rows(demand_ml, demand_min, demand_max) %>%
    mutate(
      GCAM_region_ID = demand_ci$GCAM_region_ID[[1]],
      region = demand_ci$region[[1]]
    ) %>%
    relocate(measure)
}


