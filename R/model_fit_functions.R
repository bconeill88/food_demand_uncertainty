# ------------------------------------------------------------------------------
# Model-fit and model-comparison helper functions
# ------------------------------------------------------------------------------
#
# Helper functions for calculating diagnostics and summary tables used by
# plot_main_results.R. These functions compute paired fit metrics, compare GCAM
# and ambrosia scatter outputs, evaluate model results against observations, and
# format metric tables for reporting.
#
# Functions included:
#   calc_ccc()                    - Computes Lin's concordance correlation coefficient.
#   calc_fit_metrics()            - Computes grouped paired fit metrics for truth/prediction columns.
#   check_fit_inputs()            - Summarizes finite pairs and near-zero values for paired inputs.
#   format_fit_table()            - Standardizes labels, ordering, and column names in metric tables.
#   finalize_fit_table()          - Rounds and orders formatted metric tables for reporting.
#   calc_scatter_fit_metrics()    - Computes fit metrics for GCAM-vs-ambrosia scatter data.
#   check_scatter_fit_inputs()    - Runs paired-input diagnostics for scatter comparison data.
#   calc_model_vs_obs_metrics()   - Computes model-vs-observation metrics for demand variables.
#   check_model_vs_obs_inputs()   - Runs paired-input diagnostics for model-vs-observation data.
#   calc_ratio_metrics()          - Computes grouped prediction/truth ratio summaries.
#   make_metrics_summary_table()  - Builds concise summary tables from regional and decile metrics.
#
# Notes:
#   - calc_model_vs_obs_metrics(), check_model_vs_obs_inputs(), and
#     check_scatter_fit_inputs() are not called by plot_main_results.R, but they
#     are consistent with the file purpose and useful as diagnostics or optional
#     model-fit workflows.
#   - calc_ratio_metrics() is currently used for difference comparisons only.
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Core paired metric calculations
# ------------------------------------------------------------------------------

# Compute Lin's concordance correlation coefficient for paired finite values.
calc_ccc <- function(x, y) {
  
  # Keep finite complete pairs
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  
  # Need at least two points
  if (length(x) < 2) return(NA_real_)
  
  # Compute CCC
  mx  <- mean(x)
  my  <- mean(y)
  vx  <- var(x)
  vy  <- var(y)
  cxy <- cov(x, y)
  
  2 * cxy / (vx + vy + (mx - my)^2)
}

# General paired metrics

# Compute standard paired fit metrics for truth and prediction columns, optionally by group.
calc_fit_metrics <- function(
    df,
    truth_col,
    pred_col,
    group_cols = NULL,
    near_zero = 1e-8
) {
  
  # Check columns
  req <- c(truth_col, pred_col)
  miss <- setdiff(req, names(df))
  if (length(miss) > 0) {
    stop("calc_fit_metrics(): missing columns: ", paste(miss, collapse = ", "))
  }
  
  # Keep finite pairs and build error terms
  dat <- df %>%
    filter(
      is.finite(.data[[truth_col]]),
      is.finite(.data[[pred_col]])
    ) %>%
    mutate(
      truth = .data[[truth_col]],
      pred  = .data[[pred_col]],
      err   = pred - truth,
      abs_err = abs(err),
      sq_err  = err^2,
      ape = if_else(abs(truth) > near_zero, 100 * abs_err / abs(truth), NA_real_),
      smape = if_else(
        abs(truth) + abs(pred) > near_zero,
        100 * 2 * abs_err / (abs(truth) + abs(pred)),
        NA_real_
      ),
      sign_agree = sign(truth) == sign(pred)
    )
  
  # Stop if nothing remains
  if (nrow(dat) == 0) {
    stop("calc_fit_metrics(): no finite paired rows remain.")
  }
  
  # Add overall grouping if none requested
  if (is.null(group_cols) || length(group_cols) == 0) {
    dat <- dat %>% mutate(.overall = "Overall")
    group_cols <- ".overall"
  }
  
  # Summarize fit
  dat %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      n = n(),
      bias = mean(err, na.rm = TRUE),
      mae = mean(abs_err, na.rm = TRUE),
      rmse = sqrt(mean(sq_err, na.rm = TRUE)),
      nrmse_mean = rmse / mean(abs(truth), na.rm = TRUE),
      median_ae = median(abs_err, na.rm = TRUE),
      smape = mean(smape, na.rm = TRUE),
      mape = mean(ape, na.rm = TRUE),
      cor_pearson = if (n() >= 2) cor(truth, pred, method = "pearson") else NA_real_,
      cor_spearman = if (n() >= 2) cor(truth, pred, method = "spearman") else NA_real_,
      ccc = calc_ccc(truth, pred),
      sign_agreement = mean(sign_agree, na.rm = TRUE),
      .groups = "drop"
    )
}

# Diagnostics for paired fit inputs

# Summarize completeness and near-zero denominator counts for paired fit inputs.
check_fit_inputs <- function(df, truth_col, pred_col, near_zero = 1e-8) {
  
  # Check columns
  req <- c(truth_col, pred_col)
  miss <- setdiff(req, names(df))
  if (length(miss) > 0) {
    stop("check_fit_inputs(): missing columns: ", paste(miss, collapse = ", "))
  }
  
  # Summarize paired data quality
  tibble(
    n_total = nrow(df),
    n_finite_pairs = sum(is.finite(df[[truth_col]]) & is.finite(df[[pred_col]])),
    n_truth_near_zero = sum(is.finite(df[[truth_col]]) & abs(df[[truth_col]]) <= near_zero),
    n_pred_near_zero  = sum(is.finite(df[[pred_col]]) & abs(df[[pred_col]]) <= near_zero)
  )
}

# Metrics for scatter comparison objects

# ------------------------------------------------------------------------------
# Table formatting utilities
# ------------------------------------------------------------------------------

# Standardize labels, ordering, and column names in metric tables for manuscript use.
format_fit_table <- function(df) {
  
  out <- df
  
  # ---- Standardize demand type labels ----
  if ("demand_type" %in% names(out)) {
    out <- out %>%
      mutate(
        demand_type = recode(
          as.character(demand_type),
          "Qs" = "Staples",
          "Qn" = "Non-staples",
          "Qtot" = "Total",
          "Qs.region" = "Staples",
          "Qn.region" = "Non-staples",
          "Qtot.region" = "Total",
          .default = as.character(demand_type)
        )
      )
  }
  
  # ---- Standardize decile labels ----
  if ("consumer_group" %in% names(out)) {
    out <- out %>%
      mutate(
        consumer_group = recode(
          as.character(consumer_group),
          "FoodDemand_Group1" = "1",
          "FoodDemand_Group2" = "2",
          "FoodDemand_Group3" = "3",
          "FoodDemand_Group4" = "4",
          "FoodDemand_Group5" = "5",
          "FoodDemand_Group6" = "6",
          "FoodDemand_Group7" = "7",
          "FoodDemand_Group8" = "8",
          "FoodDemand_Group9" = "9",
          "FoodDemand_Group10" = "10",
          .default = as.character(consumer_group)
        )
      )
  }
  
  # ---- Rename columns for manuscript import ----
  out <- out %>%
    rename(
      `Demand type` = demand_type,
      N = n,
      Bias = bias,
      MAE = mae,
      RMSE = rmse,
      NRMSE = nrmse_mean,
      `Median AE` = median_ae,
      sMAPE = smape,
      MAPE = mape,
      `Correlation (Pearson)` = cor_pearson,
      `Correlation (Spearman)` = cor_spearman,
      CCC = ccc,
      `Sign agreement` = sign_agreement
    )
  
  if ("scenario_name" %in% names(out)) {
    out <- out %>% rename(Parameters = scenario_name)
  }
  
  if ("case" %in% names(out)) {
    out <- out %>% rename(Parameters = case)
  }
  
  if ("consumer_group" %in% names(out)) {
    out <- out %>% rename(Decile = consumer_group)
  }
  
  # ---- Parameter labels and ordering ----
  if ("Parameters" %in% names(out)) {
    
    # Recode known parameter labels to cleaner manuscript labels
    param_vals <- recode(
      as.character(out$Parameters),
      "ML" = "ML",
      "HD_HPR_Qtot" = "HD-HPR",
      "HD_LPR_Qtot" = "HD-LPR",
      "LD_HPR_Qtot" = "LD-HPR",
      "LD_LPR_Qtot" = "LD-LPR",
      .default = as.character(out$Parameters)
    )
    
    # Preferred ordering, keeping any extras if they appear
    preferred <- c("ML", "HD-HPR", "HD-LPR", "LD-HPR", "LD-LPR")
    extras <- setdiff(unique(param_vals), preferred)
    param_levels <- c(preferred[preferred %in% unique(param_vals)], sort(extras))
    
    out <- out %>%
      mutate(Parameters = factor(param_vals, levels = param_levels))
  }
  
  # ---- Demand type ordering ----
  if ("Demand type" %in% names(out)) {
    out <- out %>%
      mutate(
        `Demand type` = factor(
          as.character(`Demand type`),
          levels = c("Staples", "Non-staples", "Total")
        )
      )
  }
  
  # ---- Decile ordering ----
  if ("Decile" %in% names(out)) {
    dec_vals <- as.character(out$Decile)
    dec_levels <- c(as.character(1:10), "All")
    dec_levels <- dec_levels[dec_levels %in% unique(dec_vals)]
    
    out <- out %>%
      mutate(Decile = factor(dec_vals, levels = dec_levels))
  }
  
  # ---- Sort on final column names ----
  sort_cols <- intersect(c("Parameters", "Demand type", "Decile"), names(out))
  if (length(sort_cols) > 0) {
    out <- out %>% arrange(across(all_of(sort_cols)))
  }
  
  # ---- Put key columns first ----
  first_cols <- intersect(c("Parameters", "Demand type", "Decile"), names(out))
  other_cols <- setdiff(names(out), first_cols)
  out <- out %>% select(all_of(first_cols), all_of(other_cols))
  
  return(out)
}

# Round and order formatted metric tables for model-fit or model-comparison outputs.
finalize_fit_table <- function(df, table_type = c("model_fit", "model_comp_abs", "model_comp_diff")) {
  
  table_type <- match.arg(table_type)
  
  out <- df
  
  # ---- Round numeric columns ----
  if ("N" %in% names(out)) out$N <- as.integer(out$N)
  
  round_3 <- intersect(
    c("RMSE", "MAE", "Bias", "Correlation (Pearson)", "CCC", "MAPE", "SMAPE"),
    names(out)
  )
  if (length(round_3) > 0) {
    out <- out %>%
      mutate(across(all_of(round_3), ~ round(.x, 3)))
  }
  
  if ("Sign agreement" %in% names(out)) {
    out$`Sign agreement` <- round(out$`Sign agreement`, 3)
  }
  
  # ---- Put main manuscript columns first ----
  first_cols <- switch(
    table_type,
    model_fit = c(
      "Parameters", "Demand type", "N", "RMSE", "MAE", "MAPE",
      "Bias", "Correlation (Pearson)"
    ),
    model_comp_abs = c(
      "Parameters", "Demand type", "Decile", "N", "RMSE", "MAE",
      "Bias", "Correlation (Pearson)", "CCC"
    ),
    model_comp_diff = c(
      "Parameters", "Demand type", "Decile", "N", "RMSE", "MAE",
      "Bias", "Sign agreement", "Correlation (Pearson)", "CCC"
    )
  )
  
  first_cols <- intersect(first_cols, names(out))
  other_cols <- setdiff(names(out), first_cols)
  
  out <- out %>% select(all_of(first_cols), all_of(other_cols))
  
  return(out)
}


# Metrics for model vs observations

# ------------------------------------------------------------------------------
# Metrics for GCAM-vs-ambrosia scatter comparisons
# ------------------------------------------------------------------------------

# Compute model-comparison metrics from scatter-plot data with x_value as truth and y_value as prediction.
calc_scatter_fit_metrics <- function(
    scatter_df,
    group_cols = c("scenario_name", "demand_type"),
    near_zero = 1e-8,
    add_decile_all = TRUE,
    comparison_type = c("absolute", "difference"),
    output_dir_tables = NULL,
    filename = NULL
) {
  
  # Check required columns
  req <- c("x_value", "y_value")
  miss <- setdiff(req, names(scatter_df))
  if (length(miss) > 0) {
    stop("calc_scatter_fit_metrics(): missing columns: ", paste(miss, collapse = ", "))
  }
  
  comparison_type <- match.arg(comparison_type)
  
  # Main grouped metrics
  out_main <- calc_fit_metrics(
    df = scatter_df,
    truth_col = "x_value",
    pred_col  = "y_value",
    group_cols = group_cols,
    near_zero = near_zero
  )
  
  out <- out_main
  
  # Add pooled "All" row across deciles if deciles are part of grouping
  if (add_decile_all && "consumer_group" %in% group_cols && "consumer_group" %in% names(scatter_df)) {
    
    group_cols_all <- setdiff(group_cols, "consumer_group")
    
    out_all <- calc_fit_metrics(
      df = scatter_df,
      truth_col = "x_value",
      pred_col  = "y_value",
      group_cols = group_cols_all,
      near_zero = near_zero
    ) %>%
      mutate(consumer_group = "All")
    
    out <- bind_rows(out_main, out_all)
  }
  
  # Clean labels / sort / rename columns
  out <- format_fit_table(out)
  
  out <- finalize_fit_table(
    out,
    table_type = if (comparison_type == "absolute") "model_comp_abs" else "model_comp_diff"
  )
  
  # Save to CSV if requested
  if (!is.null(output_dir_tables)) {
    
    if (is.null(filename)) {
      filename <- "metrics_scatter.csv"
    }
    
    dir.create(output_dir_tables, recursive = TRUE, showWarnings = FALSE)
    write.csv(out, file.path(output_dir_tables, filename), row.names = FALSE)
  }
  
  return(out)
}

# Diagnostics for scatter comparison objects

# Run paired-input diagnostics for scatter-plot comparison data.
check_scatter_fit_inputs <- function(
    scatter_df,
    near_zero = 1e-8
) {
  
  # Run diagnostics
  check_fit_inputs(
    df = scatter_df,
    truth_col = "x_value",
    pred_col  = "y_value",
    near_zero = near_zero
  )
}

# Clean and sort fit tables for manuscript import

# ------------------------------------------------------------------------------
# Metrics for model-vs-observation comparisons
# ------------------------------------------------------------------------------

# Compute model-vs-observation fit metrics for selected food-demand variables.
calc_model_vs_obs_metrics <- function(
    demand_model_vs_obs,
    demand_types = c("Qs", "Qn", "Qtot"),
    group_cols = c("case"),
    near_zero = 1e-8,
    output_dir_tables = NULL,
    filename = NULL
) {
  
  # Check requested demand types
  valid_types <- c("Qs", "Qn", "Qtot")
  bad_types <- setdiff(demand_types, valid_types)
  if (length(bad_types) > 0) {
    stop(
      "calc_model_vs_obs_metrics(): invalid demand_types: ",
      paste(bad_types, collapse = ", ")
    )
  }
  
  # Build model/obs mapping
  col_map <- tibble(
    demand_type = c("Qs", "Qn", "Qtot"),
    truth_col   = c("Qs.obs", "Qn.obs", "Qtot.obs"),
    pred_col    = c("Qs.region", "Qn.region", "Qtot.region")
  ) %>%
    filter(demand_type %in% demand_types)
  
  # Confirm needed columns exist
  needed_cols <- unique(c(col_map$truth_col, col_map$pred_col, group_cols))
  missing_cols <- setdiff(needed_cols, names(demand_model_vs_obs))
  if (length(missing_cols) > 0) {
    stop(
      "calc_model_vs_obs_metrics(): missing columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  # Compute metrics for each demand type
  out <- lapply(seq_len(nrow(col_map)), function(i) {
    
    this_type <- col_map$demand_type[i]
    truth_col <- col_map$truth_col[i]
    pred_col  <- col_map$pred_col[i]
    
    calc_fit_metrics(
      df = demand_model_vs_obs,
      truth_col = truth_col,
      pred_col  = pred_col,
      group_cols = group_cols,
      near_zero = near_zero
    ) %>%
      mutate(demand_type = this_type, .before = 1)
  }) %>%
    bind_rows()
  
  # Clean labels / sort / rename columns
  out <- format_fit_table(out)
  out <- finalize_fit_table(out, table_type = "model_fit")
  
  # Save to CSV if requested
  if (!is.null(output_dir_tables)) {
    
    if (is.null(filename)) {
      if (length(demand_types) == 3) {
        filename <- "metrics_model_vs_obs_all.csv"
      } else {
        filename <- paste0(
          "metrics_model_vs_obs_",
          paste(demand_types, collapse = "_"),
          ".csv"
        )
      }
    }
    
    dir.create(output_dir_tables, recursive = TRUE, showWarnings = FALSE)
    write.csv(out, file.path(output_dir_tables, filename), row.names = FALSE)
  }
  
  return(out)
}

# Diagnostics for model vs observations

# Run paired-input diagnostics for model-vs-observation demand comparisons.
check_model_vs_obs_inputs <- function(
    demand_model_vs_obs,
    demand_types = c("Qs", "Qn", "Qtot"),
    near_zero = 1e-8
) {
  
  # Check requested demand types
  valid_types <- c("Qs", "Qn", "Qtot")
  bad_types <- setdiff(demand_types, valid_types)
  if (length(bad_types) > 0) {
    stop(
      "check_model_vs_obs_inputs(): invalid demand_types: ",
      paste(bad_types, collapse = ", ")
    )
  }
  
  # Build model/obs column mapping
  col_map <- tibble(
    demand_type = c("Qs", "Qn", "Qtot"),
    truth_col   = c("Qs.obs", "Qn.obs", "Qtot.obs"),
    pred_col    = c("Qs.region", "Qn.region", "Qtot.region")
  ) %>%
    filter(demand_type %in% demand_types)
  
  # Confirm needed columns exist
  needed_cols <- unique(c(col_map$truth_col, col_map$pred_col))
  missing_cols <- setdiff(needed_cols, names(demand_model_vs_obs))
  if (length(missing_cols) > 0) {
    stop(
      "check_model_vs_obs_inputs(): missing columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  # Run checks for each demand type and combine
  out <- lapply(seq_len(nrow(col_map)), function(i) {
    
    this_type <- col_map$demand_type[i]
    truth_col <- col_map$truth_col[i]
    pred_col  <- col_map$pred_col[i]
    
    check_fit_inputs(
      df = demand_model_vs_obs,
      truth_col = truth_col,
      pred_col  = pred_col,
      near_zero = near_zero
    ) %>%
      mutate(demand_type = this_type, .before = 1)
  }) %>%
    bind_rows()
  
  return(out)
}

# ------------------------------------------------------------------------------
# Ratio metrics and summary tables
# ------------------------------------------------------------------------------

# Compute grouped ratios of prediction to truth after filtering near-zero denominators.
calc_ratio_metrics <- function(
    df,
    truth_col,
    pred_col,
    group_cols = NULL,
    near_zero = 1e-6,
    output_dir_tables = NULL,
    filename = NULL
) {
  
  dat <- df %>%
    filter(
      is.finite(.data[[truth_col]]),
      is.finite(.data[[pred_col]]),
      abs(.data[[truth_col]]) > near_zero
    ) %>%
    mutate(
      truth = .data[[truth_col]],
      pred  = .data[[pred_col]],
      ratio = pred / truth
    )
  
  if (nrow(dat) == 0) {
    stop("calc_ratio_metrics(): no rows remain after denominator filtering.")
  }
  
  if (is.null(group_cols) || length(group_cols) == 0) {
    dat <- dat %>% mutate(.overall = "Overall")
    group_cols <- ".overall"
  }
  
  out <- dat %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      n_ratio = n(),
      mean_ratio = mean(ratio, na.rm = TRUE),
      median_ratio = median(ratio, na.rm = TRUE),
      p10_ratio = quantile(ratio, 0.10, na.rm = TRUE, names = FALSE),
      p90_ratio = quantile(ratio, 0.90, na.rm = TRUE, names = FALSE),
      .groups = "drop"
    )
  
  # ---- Save to CSV if requested ----
  if (!is.null(output_dir_tables)) {
    
    if (is.null(filename)) {
      filename <- "metrics_ratio.csv"
    }
    
    dir.create(output_dir_tables, recursive = TRUE, showWarnings = FALSE)
    
    write.csv(out, file.path(output_dir_tables, filename), row.names = FALSE)
  }
  
  return(out)
}

# Create concise summary table from existing regional + decile metrics

# Create a concise manuscript summary table from regional and decile metric tables.
make_metrics_summary_table <- function(
    metrics_reg,
    metrics_dec,
    output_dir = NULL,
    filename = NULL
) {
  
  # Keep just Decile 1 and All from decile results
  dec_keep <- metrics_dec %>%
    filter(Decile %in% c("1", "All"))
  
  # Add matching Decile column to regional results
  reg_keep <- metrics_reg %>%
    mutate(Decile = "Region")
  
  # Combine and order
  out <- bind_rows(dec_keep, reg_keep) %>%
    mutate(
      Decile = factor(Decile, levels = c("1", "All", "Region"))
    ) %>%
    arrange(Parameters, `Demand type`, Decile) %>%
    select(
      Parameters, `Demand type`, Decile, N,
      RMSE, MAE, Bias, `Correlation (Pearson)`, CCC, NRMSE,
      `Median AE`, sMAPE, MAPE, `Correlation (Spearman)`,
      `Sign agreement`
    )
  
  # Optional save
  if (!is.null(filename)) {
    if (is.null(output_dir)) {
      stop("make_metrics_summary_table(): output_dir must be provided when filename is not NULL.")
    }
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    write.csv(out, file.path(output_dir, filename), row.names = FALSE)
  }
  
  out
}

