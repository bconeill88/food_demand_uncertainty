# Function to subtract all elements of two income/price scenarios of food demand and
# elasticities (scen1 - scen2) for regions defined in "regions", keeping columns
# that should not be subtracted
subtract_demand_dfs <- function(scen1, case1, path1, 
                                scen2, case2, path2,
                                regions) {
  
  # Columns that define corresponding rows
  by_cols <- c("GCAM_region_ID", "region", "year", "gcam-consumer", "iteration")
  
  for (r in seq_along(regions)) {
    
    # Load regional demand for two scenarios to subtract
    df1 <- readRDS(file.path(path1, paste0("demand_R", regions[r], case1, ".RDS")))
    df2 <- readRDS(file.path(path2, paste0("demand_R", regions[r], case2, ".RDS")))
    
    # Safely drop rgn if present
    df1_clean <- df1 %>% select(-any_of("rgn"))
    df2_clean <- df2 %>% select(-any_of("rgn"))
    
    # Identify common numeric columns to subtract, excluding keys and LL
    common <- intersect(names(df1_clean), names(df2_clean))
    numeric_common <- setdiff(common, c(by_cols, "LL"))
    numeric_common <- numeric_common[
      sapply(numeric_common, function(v)
        is.numeric(df1_clean[[v]]) && is.numeric(df2_clean[[v]])
      )
    ]
    
    # Columns that will be kept in the diff output
    kept_cols <- c(by_cols, "LL", numeric_common)
    all_input_cols <- union(names(df1_clean), names(df2_clean))
    dropped_cols <- setdiff(all_input_cols, kept_cols)
    
    if (length(dropped_cols) > 0) {
      message("Region ", regions[r],
              ": dropping columns from diff: ",
              paste(dropped_cols, collapse = ", "))
    }
    
    # Join the two data frames on key columns
    joined <- inner_join(
      df1_clean,
      df2_clean,
      by = by_cols,
      suffix = c(".df1", ".df2")
    )
    
    # Build list of difference columns (df1 - df2) in base R
    diff_list <- setNames(
      lapply(numeric_common, function(col) {
        joined[[paste0(col, ".df1")]] - joined[[paste0(col, ".df2")]]
      }),
      numeric_common
    )
    
    # Assemble final diffs_reg: keys + LL from df1 + numeric differences
    diffs_reg <- cbind(
      joined[, by_cols, drop = FALSE],
      LL = joined[["LL.df1"]],
      as.data.frame(diff_list, check.names = FALSE)
    )
    
    # Save in scen1 directory
    saveRDS(
      diffs_reg,
      file.path(path2, paste0("diffs_", scen1), 
                paste0("demand_diffs_R", regions[r], case2, ".RDS"))
    )
  }
}


