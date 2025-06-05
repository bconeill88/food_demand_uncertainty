# function for plotting comparison of decile demand, regional demand, and decile
# bias adders between GCAM and ambrosia results; produces panels on each page for
# staples and non-staples (columns) and historical, base year, and projected time
# periods (rows); historical leaves out year 1975; three pages, one for each 
# variable; saved to output/reports
plot_gcam_amb_comparison <- function(df, base_year, end_year, output_file) {
  
  # Ensure output directory exists
  dir.create("output/reports", recursive = TRUE, showWarnings = FALSE)
  
  # Define base variable groups
  var_groups <- list(
    "Food demand, deciles" = c("Qs", "Qn"),
    "Food demand, regional totals" = c("Qs.region", "Qn.region"),
    "Regional bias, deciles" = c("RBs", "RBn")
  )
  
  # Conditionally add price comparison group
  price_vars <- c("Ps", "Pn")
  price_suffixes <- c("_gcam", "_amb")
  all_price_cols_exist <- all(sapply(paste0(rep(price_vars, each = 2), price_suffixes), \(col) col %in% names(df)))
  
  if (all_price_cols_exist) {
    var_groups[["Price comparison"]] <- price_vars
  }
  
  # Time periods
  time_periods <- list(
    "Historical" = function(df) filter(df, year > 1975 & year < base_year),
    "Base year" = function(df) filter(df, year == base_year),
    "Projected" = function(df) filter(df, year >= base_year & year <= end_year)
  )
  
  # Open PDF
  pdf_path <- file.path("output/reports", output_file)
  pdf(pdf_path, width = 10, height = 12)  # 2 columns x 3 rows layout
  
  # Plot loop
  for (group_name in names(var_groups)) {
    vars <- var_groups[[group_name]]
    plots <- list()
    
    for (period_label in names(time_periods)) {
      df_period <- time_periods[[period_label]](df)
      
      for (v in vars) {
        plot <- ggplot(df_period, aes_string(x = paste0(v, "_gcam"), y = paste0(v, "_amb"))) +
          geom_point(alpha = 0.5) +
          geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
          labs(
            title = paste(group_name, "|", v, "|", period_label),
            x = paste0(v, "_gcam"),
            y = paste0(v, "_amb")
          ) +
          theme_minimal()
        
        plots[[length(plots) + 1]] <- plot
      }
    }
    
    page_plot <- wrap_plots(plots, ncol = 2, nrow = 3)
    print(page_plot)
  }
  
  dev.off()
  message("PDF saved to ", pdf_path)
}


# function for plotting comparison of decile demand, regional demand, and prices
# between two different GCAM scenarios; produces panels on each page for
# staples and non-staples (columns) and historical, base year, and projected time
# periods (rows); historical leaves out year 1975; three pages, one for each 
# variable; saved to output/reports
plot_gcam_scenario_comparison <- function(df1, df2, base_year, end_year, 
                                          output_file, group_filter_num = 1) {
  
  # Create output directory if needed
  dir.create("output/reports", recursive = TRUE, showWarnings = FALSE)
  
  # Variable groups to plot
  var_groups <- list(
    "Food demand, deciles" = c("Qs", "Qn"),
    "Food demand, regional totals" = c("Qs.region", "Qn.region"),
    "Prices" = c("Ps", "Pn")
  )
  
  # Time period filters
  time_periods <- list(
    "Historical" = function(df) filter(df, year > 1975 & year < base_year),
    "Base year"  = function(df) filter(df, year == base_year),
    "Projected"  = function(df) filter(df, year >= base_year & year <= end_year)
  )
  
  # Format group label (e.g., "FoodDemand_Group1")
  group_filter <- paste0("FoodDemand_Group", group_filter_num)
  
  # Define join keys (adjust if needed)
  join_keys <- c("GCAM_region_ID", "region", "gcam-consumer", "year")
  
  # Identify shared variable names to rename
  value_vars <- setdiff(intersect(names(df1), names(df2)), join_keys)
  
  # Rename variable columns before join
  df1_renamed <- df1 %>%
    rename_with(~ paste0(., "_gcam1"), .cols = value_vars)
  df2_renamed <- df2 %>%
    rename_with(~ paste0(., "_gcam2"), .cols = value_vars)
  
  # Merge
  df_joined <- full_join(df1_renamed, df2_renamed, by = join_keys)
  
  # Output path
  pdf_path <- file.path("output/reports", output_file)
  pdf(pdf_path, width = 10, height = 12)
  
  for (group_name in names(var_groups)) {
    vars <- var_groups[[group_name]]
    plots <- list()
    
    for (period_label in names(time_periods)) {
      # Apply time period filter
      df_period <- time_periods[[period_label]](df_joined)
      
      # Filter by consumer group only for regional totals and prices
      if (group_name %in% c("Food demand, regional totals", "Prices")) {
        df_period <- df_period %>%
          filter(`gcam-consumer` == group_filter | is.na(`gcam-consumer`))
      }
      
      for (v in vars) {
        var1 <- paste0(v, "_gcam1")
        var2 <- paste0(v, "_gcam2")
        
        if (all(c(var1, var2) %in% names(df_period))) {
          df_plot <- df_period %>%
            filter(!is.na(.data[[var1]]) & !is.na(.data[[var2]]))
          
          p <- ggplot(df_plot, aes_string(x = var1, y = var2)) +
            geom_point(alpha = 0.5) +
            geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
            labs(
              title = paste(group_name, "|", v, "|", period_label),
              x = paste0(v, " (Scenario 1)"),
              y = paste0(v, " (Scenario 2)")
            ) +
            theme_minimal()
          
          plots[[length(plots) + 1]] <- p
        } else {
          warning("Variable ", v, " missing in ", period_label, "; adding placeholder.")
          plots[[length(plots) + 1]] <- ggplot() + theme_void() +
            labs(title = paste("Missing:", v, "in", period_label))
        }
      }
    }
    
    # Pad to 6 plots if needed
    while (length(plots) < 6) {
      plots[[length(plots) + 1]] <- ggplot() + theme_void()
    }
    
    page_plot <- wrap_plots(plots, ncol = 2, nrow = 3)
    print(page_plot)
  }
  
  dev.off()
  message("PDF saved to ", pdf_path)
}

