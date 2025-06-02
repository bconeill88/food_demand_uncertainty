apply_min_demand <- function(demand_reg, Qs_min, Qn_min, alloc_thresh) {
  unmet_demand <- TRUE
  
  while (unmet_demand) {
    
    # Flag under-threshold deciles and donors
    demand_reg <- demand_reg %>%
      mutate(
        Qs_needs_alloc = Qs < Qs_min,
        Qn_needs_alloc = Qn < Qn_min,
        Qs_alloc_flag = Qs > Qs_min * alloc_thresh,
        Qn_alloc_flag = Qn > Qn_min * alloc_thresh
      )
    
    # Compute shortfalls and donor counts per group
    realloc <- demand_reg %>%
      group_by(iteration, year) %>%
      summarize(
        Qs_shortfall = sum(Qs_min - Qs[Qs_needs_alloc], na.rm = TRUE),
        Qn_shortfall = sum(Qn_min - Qn[Qn_needs_alloc], na.rm = TRUE),
        Qs_alloc_donors = sum(Qs_alloc_flag, na.rm = TRUE),
        Qn_alloc_donors = sum(Qn_alloc_flag, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Merge reallocations and apply updates
    demand_reg <- demand_reg %>%
      left_join(realloc, by = c("iteration", "year")) %>%
      mutate(
        Qs = ifelse(Qs < Qs_min, Qs_min, Qs),
        Qn = ifelse(Qn < Qn_min, Qn_min, Qn),
        Qs = ifelse(Qs_alloc_flag & !is.na(Qs_shortfall) & Qs_alloc_donors > 0,
                    Qs - Qs_shortfall / Qs_alloc_donors, Qs),
        Qn = ifelse(Qn_alloc_flag & !is.na(Qn_shortfall) & Qn_alloc_donors > 0,
                    Qn - Qn_shortfall / Qn_alloc_donors, Qn)
      ) %>%
      # Clean up
      select(-Qs_needs_alloc, -Qn_needs_alloc,
             -Qs_alloc_flag, -Qn_alloc_flag,
             -Qs_shortfall, -Qn_shortfall,
             -Qs_alloc_donors, -Qn_alloc_donors)
    
    # Check for unmet demand
    unmet_demand <- any(demand_reg$Qs < Qs_min | demand_reg$Qn < Qn_min)
  }
  
  # Recalculate Qtot
  demand_reg <- demand_reg %>%
    mutate(Qtot = Qs + Qn)
  
  return(demand_reg)
}
