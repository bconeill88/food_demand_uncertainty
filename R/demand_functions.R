# ------------------------------------------------------------------------------
# Demand calculation helper functions
# ------------------------------------------------------------------------------
#
# Helper functions for calculating food demand, elasticities, and base-year bias
# correction terms using ambrosia. The primary entry point used by
# 03_calculate_demand_GCAM_ML.R is food.dmnd.ens_bc().
#
# Functions in this file:
# - food.dmnd.ens_bc(): Runs a bias-corrected demand ensemble for selected regions
#   and saves or returns regional demand and elasticity results.
# - get_bias_terms_solve(): Solves for base-year regional bias adders that make
#   modeled demand match reference demand in the calibration year.
# - food.dmnd.wrapper(): Applies food.dmnd() across regions and parameter
#   iterations, adds elasticities and metadata, and saves or returns results.
# - calc_income_elast(): Calculates staples and non-staples income elasticities
#   from a vec2param() parameter structure.
# - calc_price_elast(): Calculates own- and cross-price elasticities using demand
#   output and a vec2param() parameter structure.

# ------------------------------------------------------------------------------
# Main bias-corrected ensemble workflow
# ------------------------------------------------------------------------------
# Function to calculate food demand for an ensemble of parameter values, with each
# ensemble member bias corrected to the base-year total regional staples and
# non-staples demand of a reference scenario. Takes as input global and regional
# parameter ensembles; scenario income and price data; a list of region numbers;
# output directory; scenario name; case tag; base year for bias correction;
# minimum values for staples and non-staples consumption; and whether results
# should be saved or returned.
#
# Bias correction is carried out by solving for bias adders that, when passed to
# the demand function, produce demand equal to reference demand in the base year.
# The solve is implemented in get_bias_terms_solve().
food.dmnd.ens_bc <- function(globalparams, regparams, inputdata, regionIDs, 
                             output_dir, gcam_dir, scen, case, baseyr, 
                             Qs_min, Qn_min,
                             save_result = TRUE) {
  
  # Remove bias terms from input data if present, to avoid confusion
  if ("RBs" %in% names(inputdata)) { inputdata <- select(inputdata, -RBs) }
  if ("RBn" %in% names(inputdata)) { inputdata <- select(inputdata, -RBn) }
  
  # Get gcam reference scenario results to correct to; only total regional demand
  # is used, so doesn't matter which GCAM scenario is used; we use Ref_ML here
  demand_ref_baseyr <- 
    readRDS(file.path("data/processed", output_dir, gcam_dir,
             "gcamoutput_Ref_ML.RDS")
    ) %>%
    filter(year == baseyr) %>%
    select(GCAM_region_ID, region, `gcam-consumer`, year,
           Qs.region, Qn.region)
  
  # Calculate decile-specific bias terms for each ensemble member
  cal <- get_bias_terms_solve(
    demand_ref_baseyr = demand_ref_baseyr,
    globalparams = globalparams,
    regparams = regparams,
    inputdata = inputdata,
    regions = regionIDs,
    output_dir = output_dir,
    scen = scen,
    case = case,
    baseyr = baseyr,
    Qs_min = Qs_min, 
    Qn_min = Qn_min
  )
  
  bias_terms <- cal$bias_terms
  failures   <- cal$failures
  
  # Check whether there are any successful solves, stop if not
  if (is.null(bias_terms) || nrow(bias_terms) == 0 || 
      !all(c("iteration","region") %in% names(bias_terms))) {
    stop("No successful calibrations; bias_terms is empty. See failures output.")
  }
  
  # Identify which iterations successfully calibrated for ALL regions
  # (iterations with any failed region are excluded from projection)
  target_regions <- inputdata %>%
    filter(GCAM_region_ID %in% regionIDs, year == baseyr) %>%
    pull(region) %>%
    unique()
  
  ok_iters <- bias_terms %>%
    distinct(iteration, region) %>%
    count(iteration, name = "n_regions_ok") %>%
    filter(n_regions_ok == length(target_regions)) %>%
    pull(iteration)
  
  message(
    "Iterations with successful calibration for all regions: ",
    length(ok_iters), " / ", length(unique(globalparams$iteration)), "."
  )
  
  if (!is.null(failures) && nrow(failures) > 0) {
    message("Failed (region, iteration) pairs: ", nrow(failures), ".")
  }
  
  # demand projections
  if(TRUE) {
    
    # Filter parameters and bias terms to fully successful iterations only
    globalparams_ok <- globalparams %>% filter(iteration %in% ok_iters)
    bias_terms_ok   <- bias_terms %>% filter(iteration %in% ok_iters)
    
    # Use the bias terms calculated in the base year to calculate demand in all future 
    # years, applying minimum demand constraints in those years as well (since they 
    # are part of the food.dmnd() function).
    food.dmnd.wrapper(
      globalparams = globalparams_ok,
      regparams = regparams,
      biasdata = bias_terms_ok,
      Qs_min = Qs_min,
      Qn_min = Qn_min,
      inputdata = inputdata %>% filter(year >= baseyr),
      regions = regionIDs,
      output_dir = output_dir,
      scen = scen,
      case = case, 
      progress = TRUE,
      save_result = save_result
    )
  }
}

# ------------------------------------------------------------------------------
# Base-year bias calibration
# ------------------------------------------------------------------------------
# Function to calculate bias terms for each iteration by solving for the bias
# adders that make modeled base-year demand match reference base-year demand.
# The solver starts from zero bias adders, repeatedly recalculates demand, and
# updates the bias adders using the difference between predicted and reference
# regional demand until the specified tolerance is reached or max_iterations is hit.
get_bias_terms_solve <- function(demand_ref_baseyr, globalparams, regparams, 
                                 inputdata, regions, output_dir, scen, case, baseyr, 
                                 Qs_min, Qn_min,
                                 tol = 0.01, max_iterations = 20) {
  
  # define list of region names, iterations, and their combinations to loop over
  region_names <- inputdata %>%
    filter(GCAM_region_ID %in% regions) %>%
    pull(region) %>%
    unique()
  
  iterations <- globalparams$iteration %>% unique()
  regs_iters <- expand_grid(iter = iterations, reg_nm = region_names)
  
  # get input data for the base year for demand calculations
  inputdata_baseyr <- inputdata %>% filter(year == baseyr)
  
  # guard against input data not being available 
  if (nrow(inputdata_baseyr) == 0) {
    stop("No rows in inputdata for baseyr = ", baseyr,
         ". Upstream input file is missing base-year data.")
  }
  
  # loop over all regions and iterations, applying function to solve for bias terms 
  # for a given region and iteration in a single year; collect results into a single df;
  # display progress using progressr package
  message("\nCalibrating base year demand for all regions and iterations...\n")
  
  # loop over each region and iteration
  res <- pmap(regs_iters, function(iter, reg_nm) {
    
    # initialize iteration counter for THIS (region, iteration) solve
    # (previously this counter was global, which caused premature failure)
    local_iter <- 0L
    
    # initialize guess of bias adders at zero
    # define by consumer group, region, and iteration; need these columns in call to 
    # food.dmnd.wrapper() below
    bias_terms_next <- expand_grid(
      `gcam-consumer` = inputdata_baseyr$`gcam-consumer` %>% unique(),
      iteration = iter,
      region = reg_nm
    ) %>%
      # join to GCAM_region_ID
      left_join(
        inputdata_baseyr %>%
          select(GCAM_region_ID, region) %>%
          distinct(),
        by = "region"
      ) %>%
      # assign zeros
      mutate(RBs = 0, RBn = 0)
    
    # diagnostics code, including helper functions
    
    # keep a convergence trace for logging if this solve fails
    keep_iters <- c(1:max_iterations)
    #   sort(unique(c(
    #   1:20,
    #   seq(10, 100, by = 10),
    #   seq(100, max_iterations, by = 100)
    # )))
    
    conv_trace <- list()
    
    # collapse bias terms to one regional record, for one consumer group, because
    # we will only be using regional demand, not consumer-specific (CURRENT: no 
    # diffs exist yet)
    collapse_current_to_region <- function(df) {
      df %>%
        select(iteration, GCAM_region_ID, region, RBs, RBn) %>%
        distinct() %>%
        slice(1)
    }
    
    # collapse bias terms to one regional record for same reason (NEXT: diffs exist)
    collapse_next_to_region <- function(df) {
      df %>%
        select(iteration, GCAM_region_ID, region, diff_s, diff_n, RBs, RBn) %>%
        distinct() %>%
        slice(1)
    }
    
    # extract one regional row with absolute levels of demand (ref and predicted) 
    # for this step (same reason: only need regional demand, not consumer-specific)
    get_regional_levels <- function(reg_nm, demand_ref_baseyr, demand) {
      
      ref_reg <- demand_ref_baseyr %>%
        filter(region == reg_nm) %>%
        select(region, Qs.region.ref = Qs.region, Qn.region.ref = Qn.region) %>%
        distinct() %>%
        slice(1)
      
      pred_reg <- demand %>%
        filter(region == reg_nm) %>%
        select(region, Qs.region.predict = Qs.region, Qn.region.predict = Qn.region) %>%
        distinct() %>%
        slice(1)
      
      ref_reg %>% inner_join(pred_reg, by = "region")
    }
    
    # get range of decile-level demand
    get_decile_spread <- function(reg_nm, demand) {
      d <- demand %>% filter(region == reg_nm)
      tibble(
        Qs_min_dec = min(d$Qs, na.rm = TRUE),
        Qs_max_dec = max(d$Qs, na.rm = TRUE),
        Qn_min_dec = min(d$Qn, na.rm = TRUE),
        Qn_max_dec = max(d$Qn, na.rm = TRUE)
      )
    }
    
    # get budget constraint diagnostics
    get_budget_diag <- function(reg_nm, demand) {
      d <- demand %>% filter(region == reg_nm)
      
      tibble(
        alpha_m_min = min(d$alpha.m, na.rm = TRUE),
        alpha_m_med = median(d$alpha.m, na.rm = TRUE),
        alpha_m_max = max(d$alpha.m, na.rm = TRUE),
        food_share_min = 1 - max(d$alpha.m, na.rm = TRUE),
        food_share_med = 1 - median(d$alpha.m, na.rm = TRUE),
        food_share_max = 1 - min(d$alpha.m, na.rm = TRUE),
        budget_binds_any = any((1 - d$alpha.m) >= (1 - 1e-6), na.rm = TRUE)
      )
    }
    
    # build a 2-row table (staples / nonstaples) for one iteration step
    make_trace_rows <- function(step, bias_current_reg, bias_next_reg, levels_reg) {
      
      tibble(
        iter_step = step,
        food_type = c("staples", "nonstaples"),
        
        Q_ref  = c(levels_reg$Qs.region.ref[[1]],     levels_reg$Qn.region.ref[[1]]),
        Q_pred = c(levels_reg$Qs.region.predict[[1]], levels_reg$Qn.region.predict[[1]]),
        
        bias_terms_current = c(bias_current_reg$RBs[[1]], bias_current_reg$RBn[[1]]),
        bias_terms_next    = c(bias_next_reg$RBs[[1]],    bias_next_reg$RBn[[1]]),
        
        difference = c(bias_next_reg$diff_s[[1]], bias_next_reg$diff_n[[1]])
      )
    }
    
    # end of diagnostics code
    
    # compute once + validate
    reg_rows <- inputdata_baseyr %>% filter(region == reg_nm)
    stopifnot(nrow(reg_rows) > 0)
    
    reg_id <- reg_rows %>% slice(1) %>% pull(GCAM_region_ID)
    stopifnot(length(reg_id) == 1, !is.na(reg_id))
    
    # infinite loop, break on convergence or max iterations
    repeat {
      
      local_iter <- local_iter + 1L
      
      # update bias terms for this iteration
      bias_terms_current <- bias_terms_next
      
      # calculate demand with current bias terms
      demand <- food.dmnd.wrapper(
        globalparams = globalparams %>% filter(iteration == iter),
        regparams = regparams %>% filter(iteration == iter),
        biasdata = bias_terms_current,
        Qs_min = Qs_min,
        Qn_min = Qn_min,
        inputdata = inputdata_baseyr,
        regions = reg_id,
        output_dir = output_dir,
        scen = scen,
        case = case,
        progress = FALSE,
        save_result = FALSE
      )
      
      # --- DIAGNOSTIC: verify demand actually has rows for this region ---
      d_reg <- demand %>% filter(region == reg_nm)
      
      if (nrow(d_reg) == 0) {
        message("DIAG: EMPTY demand for region='", reg_nm, "', iter=", iter,
                " | demand rows=", nrow(demand),
                " | unique(demand$region) sample=",
                paste(utils::head(sort(unique(demand$region)), 10), collapse = ", "),
                " | unique(demand$GCAM_region_ID) sample=",
                paste(utils::head(sort(unique(demand$GCAM_region_ID)), 10), collapse = ", ")
        )
        stop("Demand returned no rows for this region label; likely region-name mismatch or missing region column.")
      }
      
      # Also check reference data presence (same idea)
      ref_reg <- demand_ref_baseyr %>% filter(region == reg_nm)
      if (nrow(ref_reg) == 0) {
        message("DIAG: EMPTY demand_ref_baseyr for region='", reg_nm,
                "' | unique(demand_ref_baseyr$region) sample=",
                paste(utils::head(sort(unique(demand_ref_baseyr$region)), 10), collapse = ", ")
        )
        stop("Reference base-year demand missing for this region label.")
      }
      
      # calculate difference from observed (reference) consumption and update bias terms
      bias_terms_next <- demand_ref_baseyr %>%
        inner_join(
          demand,
          by = c("GCAM_region_ID", "region", "gcam-consumer", "year"),
          suffix = c(".ref", ".predict")
        ) %>%
        # calculate differences in regional demand
        mutate(
          diff_s = Qs.region.predict - Qs.region.ref,
          diff_n = Qn.region.predict - Qn.region.ref,
          # update values of bias terms with these differences
          RBs = RBs - diff_s,
          RBn = RBn - diff_n
        ) %>%
        # keep only what we need for tolerance check and updated bias terms
        select(iteration, GCAM_region_ID, region,
               `gcam-consumer`, diff_s, diff_n, RBs, RBn) %>%
        distinct()
      
      # record trace rows for selected iteration steps only
      if (local_iter %in% keep_iters) {
        
        bias_current_reg <- collapse_current_to_region(bias_terms_current)
        bias_next_reg    <- collapse_next_to_region(bias_terms_next)
        
        levels_reg <- get_regional_levels(
          reg_nm = reg_nm,
          demand_ref_baseyr = demand_ref_baseyr,
          demand = demand
        )
        
        # range of demand over deciles
        spread_reg <- get_decile_spread(reg_nm, demand)
        
        # budget constraint diagnostics
        budget_diag <- get_budget_diag(reg_nm, demand)
        
        conv_trace[[length(conv_trace) + 1L]] <- make_trace_rows(
          step = local_iter,
          bias_current_reg = bias_current_reg,
          bias_next_reg = bias_next_reg,
          levels_reg = levels_reg
        ) %>%
          mutate(
            Qs_min_dec = spread_reg$Qs_min_dec[[1]],
            Qs_max_dec = spread_reg$Qs_max_dec[[1]],
            Qn_min_dec = spread_reg$Qn_min_dec[[1]],
            Qn_max_dec = spread_reg$Qn_max_dec[[1]],
            alpha_m_min = budget_diag$alpha_m_min[[1]],
            alpha_m_med = budget_diag$alpha_m_med[[1]],
            alpha_m_max = budget_diag$alpha_m_max[[1]],
            food_share_min = budget_diag$food_share_min[[1]],
            food_share_med = budget_diag$food_share_med[[1]],
            food_share_max = budget_diag$food_share_max[[1]],
            budget_binds_any = budget_diag$budget_binds_any[[1]]
          )
      }
      
      # if abs value of the difference is within tolerance, return bias values
      # NOTE: tolerance check corrected to use max(abs(.)) rather than abs(max(.))
      if (max(abs(bias_terms_next$diff_s), na.rm = TRUE) <= tol &&
          max(abs(bias_terms_next$diff_n), na.rm = TRUE) <= tol) {
        
        return(list(
          ok = TRUE,
          bias = bias_terms_next %>%
            select(iteration, GCAM_region_ID, region,
                   `gcam-consumer`, RBs, RBn),
          fail = NULL
        ))
      }
      
      # tolerance not achieved but max iterations reached
      # log failure and return status instead of stopping execution
      if (local_iter >= max_iterations) {
        
        msg <- paste0(
          "Maximum iterations reached when calibrating demand for ",
          reg_nm, " and iteration ", iter, "."
        )
        message(msg)
        
        # print regional convergence trace table to the log (selected steps only)
        if (length(conv_trace) > 0) {
          
          trace_tbl <- bind_rows(conv_trace) %>%
            arrange(iter_step, food_type)
          
          message("Convergence trace (regional; selected steps) for ",
                  reg_nm, ", iteration ", iter, ":")
          
          print(trace_tbl, n = Inf, width = Inf)
          
        } else {
          message("No convergence trace captured (unexpected).")
        }
        
        return(list(
          ok = FALSE,
          bias = NULL,
          fail = tibble(region = reg_nm, iteration = iter, reason = msg)
        ))
      }
    }
  })
  
  
  # collect successful bias terms
  bias_terms <- res %>%
    keep(~ isTRUE(.x$ok)) %>%
    map("bias") %>%
    bind_rows()
  
  # collect failures
  failures <- res %>%
    keep(~ !isTRUE(.x$ok)) %>%
    map("fail") %>%
    bind_rows()
  
  message(
    "Calibration complete. Failed (region, iteration) solves: ",
    ifelse(is.null(failures), 0L, nrow(failures)), "."
  )
  
  return(list(
    bias_terms = bias_terms,
    failures = failures
  ))
}

# ------------------------------------------------------------------------------
# Regional demand calculation wrapper
# ------------------------------------------------------------------------------
# Function for calculating regional food demand and elasticities with the fixed
# effects demand model for one or more parameter samples, one set of input
# assumptions (prices and income), and one or more regions.
#
# globalparams are the demand-function parameters. They vary by iteration and
# apply to all regions and consumer groups.
#
# regparams are fixed effects. They vary by iteration and region and are common
# to all consumer groups.
#
# inputdata contains prices and income. Prices are region-specific; income can be
# regional or consumer-group-specific, depending on the input data structure.
#
# biasdata contains bias adders by region, iteration, and, when present,
# consumer group.
#
# Qs_min and Qn_min are the minimum demand thresholds imposed in food.dmnd().
# This code assumes a local ambrosia version in which food.dmnd() takes these
# values as arguments.
#
# The function saves regional results to data/processed/<output_dir>/<scen>/ in
# a timestamped subdirectory unless save_result = FALSE or scen = "Obs", in which
# case results are returned.
food.dmnd.wrapper <- function(globalparams, regparams, biasdata, Qs_min, Qn_min,
                              inputdata, regions, output_dir, scen, case, 
                              progress = TRUE, save_result = TRUE) {
  
  if(progress) message("\nCalculating demand for scenario ", scen, ", case ", case)
  
  # if no regional parameters df, which would occur if the demand model has no fixed
  # effects, initialize to zero for each region and iteration
  if (is.null(regparams)) {
    regparams <- data.frame(
      GCAM_region_ID = rep(regions,nrow(globalparams)),
      iteration = rep(globalparams$iteration, each = length(regions)),
      staples_FE = rep(0, length(regions) * nrow(globalparams)),
      stringsAsFactors = FALSE)
  } 
  
  # initialize in case results will be returned rather than saved to files
  all_demand_results <- list()
  
  # Create sub-directory with time stamp for saving all regional results
  if(save_result) {
    timestamp <- format(Sys.time(), tz = "America/New_York", "%Y%m%d_%H%M%S")
    subdir_name <- paste0(case, "_", timestamp)
    pathname <- file.path("data/processed", output_dir, scen, subdir_name)
    dir.create(pathname, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Loop over regions
  for (r in seq_along(regions)) {
    
    region_id <- regions[r]
    if(progress) message("  Starting region ", region_id)
    
    # extract region-specific price/income data
    inputdata_region <- inputdata %>% filter(GCAM_region_ID == region_id)
    if (nrow(inputdata_region) == 0) stop("No inputdata for region ", region_id)
    
    # detect whether input is by consumer group or already regional totals
    has_consumer <- "gcam-consumer" %in% names(inputdata_region)
    
    # standardize income column so downstream code can always use inputdata_region$Y
    if (!("Y" %in% names(inputdata_region))) {
      if ("Y.region" %in% names(inputdata_region)) {
        inputdata_region <- inputdata_region %>% mutate(Y = .data$Y.region)
      } else {
        stop("inputdata_region must contain either 'Y' or 'Y.region' for region ", region_id)
      }
    }
    
    # extract region-specific bias data for all iterations
    biasdata_region <- biasdata %>% filter(GCAM_region_ID == region_id)
    if (nrow(biasdata_region) == 0) stop("No biasdata for region ", region_id)
    
    # extract region-specific parameters for all iterations
    regparams_region <- regparams %>% filter(GCAM_region_ID == region_id)
    
    # split globalparams by iteration
    globalparams_byiter <- split(globalparams, globalparams$iteration)
    # remove names to ensure imap_dfr() works with numeric indices
    names(globalparams_byiter) <- NULL
    
    # calculate demand for all iterations
    demand_reg <- imap_dfr(globalparams_byiter, function(gparams, i) {
      
      # progress tracking
      if (progress && i %% 500 == 0) {
        message("    reached iteration ", i, " for calculating demand")
      }
      
      # prepare parameter structure
      param_structure <- vec2param(as.vector(t(select(gparams, As:pnscl))))
      
      # extract regional parameter values for this iteration
      regparams_reg_iter <- regparams_region %>% 
        filter(iteration == gparams$iteration)
      
      # extract regional bias values for this iteration
      biasdata_reg_iter <- biasdata_region %>% 
        filter(iteration == gparams$iteration)
      
      # if consumer groups are present in data, match consumer ordering so that
      # when passed to food.dmnd, vectorization will match the variables correctly
      if(has_consumer && "gcam-consumer" %in% names(biasdata_reg_iter)) {
        
        consumer_order <- match(inputdata_region$`gcam-consumer`, 
                                biasdata_reg_iter$`gcam-consumer`)
        stopifnot(!any(is.na(consumer_order)))  # ensure no unmatched groups
        biasdata_reg_iter <- biasdata_reg_iter[consumer_order, ]
      }
      
      # calculate demand for this iteration
      demand_reg_iter <- food.dmnd(inputdata_region$Ps, inputdata_region$Pn, 
                                   inputdata_region$Y, params = param_structure,
                                   rgn = region_id, # rgn argument, not needed
                                   regparams_reg_iter$staples_FE, 
                                   biasdata_reg_iter$RBs, 
                                   biasdata_reg_iter$RBn,
                                   Qs_min, Qn_min) # %>%
      
      demand_reg_iter <- demand_reg_iter %>%
        
        # add iteration number, likelihood value
        mutate(iteration = gparams$iteration,
               LL = gparams$LL,
               RBs = rep(biasdata_reg_iter$RBs, length.out = n()),
               RBn = rep(biasdata_reg_iter$RBn, length.out = n())) %>%
        # add elasticities
        bind_cols(calc_income_elast(inputdata_region$Y, param_structure)) %>%
        # this must be done separately so that income elasticities are available
        bind_cols(calc_price_elast(., param_structure)) %>%
        # add input data
        bind_cols(select(inputdata_region, any_of(c("Y","Ps","Pn","Y.region"))))
      
      # include additional columns if present
      if (has_consumer) {
        demand_reg_iter <- bind_cols(select(inputdata_region, `gcam-consumer`), demand_reg_iter)
      }
      if ("year" %in% colnames(inputdata_region)) {
        demand_reg_iter <- bind_cols(select(inputdata_region, year), demand_reg_iter)
      }
      
      return(demand_reg_iter)
    })
    
    # finalize regional results
    demand_reg <- demand_reg %>%
      mutate(
        Qtot = Qs + Qn,
        alpha.t = alpha.s + alpha.n,
        GCAM_region_ID = unique(regparams_region$GCAM_region_ID),
        region = unique(regparams_region$region)
      ) %>%
      group_by(across(any_of(c("region", "year", "iteration")))) %>%
      # add regional demand per cap -- unweighted average across deciles
      # (if no deciles present, mean will simply return the single value)
      mutate(
        Qs.region = if_else(is.na(Qs), NA_real_, mean(Qs)),
        Qn.region = if_else(is.na(Qn), NA_real_, mean(Qn))
      ) %>%
      ungroup() %>%
      mutate(Qtot.region = Qs.region + Qn.region) %>%
      select(
        any_of(c("GCAM_region_ID","region","year","gcam-consumer",
                 "Y","Ps","Pn",
                 "Qs","Qn","Qm","Qtot",
                 "RBs","RBn",
                 "Y.region","Qs.region","Qn.region","Qtot.region",
                 "alpha.s","alpha.n","alpha.m","alpha.t",
                 "eta.s","eta.n","elast.ss","elast.nn","elast.sn","elast.ns",
                 "LL","iteration")),
        everything()
      )
    
    # conditionally save regional result to sub-directory created above
    if (save_result) {
      separator <- ifelse(case == "", "", "_")
      saveRDS(demand_reg, 
              file = file.path(pathname, paste0("demand_R", region_id, separator, 
                                                case, ".RDS")))
      if(progress) message("   Saved demand results for region ", region_id)
      
    }
    
    # conditionally store regional result in named list
    if (!save_result || scen == "Obs") {
      all_demand_results[[as.character(region_id)]] <- demand_reg
    }
    
  } # end region loop
  
  # return concatenated result if requested
  if (!save_result || scen == "Obs") {
    return(bind_rows(all_demand_results))
  }
}

# ------------------------------------------------------------------------------
# Elasticity helpers
# ------------------------------------------------------------------------------
# Function to calculate income elasticities for staples and non-staples from a
# vector of incomes and a parameter structure returned by vec2param().
calc_income_elast <- function(income,param_structure) {
  
  #calculate all income elasticities
  eta.s <- param_structure$yfunc[[1]](Y=income,FALSE)
  eta.n <- param_structure$yfunc[[2]](Y=income,FALSE)
  # create and return results
  inc_elast.df <- data.frame(eta.s,eta.n)
}


# Function to calculate own- and cross-price elasticities from a food_demand data
# frame and a parameter structure returned by vec2param(). Income elasticities
# must already be present in food_demand.
calc_price_elast <- function(food_demand,param_structure) {
  
  #calculate all price elasticities
  elasticities <- calc1eps(food_demand$alpha.s, food_demand$alpha.n,
                           food_demand$eta.s, food_demand$eta.n,
                           param_structure$xi)
  # extract the different elasticities; each one follows another
  len <- nrow(food_demand)
  elast.ss <- elasticities[1:len]
  elast.ns <- elasticities[(len+1):(2*len)]
  elast.sn <- elasticities[(2*len+1):(3*len)]
  elast.nn <- elasticities[(3*len+1):(4*len)]
  # create and return results
  elast.df <- data.frame(elast.ss,elast.nn,elast.sn,elast.ns)
}
