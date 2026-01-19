# functions supporting the 08_plot_demand_with_intervals.R main script

# Regional demand: single-region plot that shows regional demand over time with
# separate panels for Qs, Qn, and Qtotal, for a sub-sample of the ambrosia
# ensemble. Optionally, it also includes three single scenarios: a reference
# case as a solid line, and alternative scenarios (such as high and low demand) as 
# dashed and dotted lines. Single scenarios are specified to be either from GCAM or 
# ambrosia and scenario data is assumed to contain both, using _gcam and _amb variable
# name extensions.
plot_regional_demand_comparison <- function(
    demand_reg, reg_num, sample_n = 100, ensemble_name,
    scen_solid_1  = NULL, scen_solid_name_1  = NULL,
    scen_dashed_1 = NULL, scen_dashed_name_1 = NULL,
    scen_dotted_1 = NULL, scen_dotted_name_1 = NULL,
    scen_solid_2  = NULL, scen_solid_name_2  = NULL,
    scen_dashed_2 = NULL, scen_dashed_name_2 = NULL,
    scen_dotted_2 = NULL, scen_dotted_name_2 = NULL,
    ci_level = 0.90,
    return_data = FALSE
) {
  
  stopifnot(ci_level > 0, ci_level < 1)
  
  # --- Ensemble ribbons (range + central CI), years >= 2015 ---
  # Match the scenario filter as requested:
  ensemble_df <- demand_reg %>%
    filter(
      year >= 2015
    )
  
  if ("gcam-consumer" %in% names(ensemble_df)) {
    ensemble_df <- ensemble_df %>% filter(`gcam-consumer` == "FoodDemand_Group1")
  }
  
  # If demand_reg contains multiple regions, enforce region filter when available
  if ("GCAM_region_ID" %in% names(ensemble_df)) {
    ensemble_df <- ensemble_df %>% filter(GCAM_region_ID == reg_num)
  }
  
  alpha <- (1 - ci_level) / 2
  
  ensemble_long <- ensemble_df %>%
    select(iteration, year, Qs.region, Qn.region, Qtot.region) %>%
    pivot_longer(
      c(Qs.region, Qn.region, Qtot.region),
      names_to  = "demand_type",
      values_to = "demand_value"
    ) %>%
    mutate(
      GCAM_region_ID = as.character(reg_num),
      demand_type = factor(demand_type, levels = c("Qs.region", "Qn.region", "Qtot.region"))
    )
  
  # Summarise across iterations for ribbons
  ensemble_band <- ensemble_long %>%
    group_by(GCAM_region_ID, year, demand_type) %>%
    summarise(
      ymin = min(demand_value, na.rm = TRUE),
      ymax = max(demand_value, na.rm = TRUE),
      lo   = quantile(demand_value, probs = alpha,     na.rm = TRUE, names = FALSE),
      hi   = quantile(demand_value, probs = 1 - alpha, na.rm = TRUE, names = FALSE),
      .groups = "drop"
    )
  
  ci_label <- paste0(round(ci_level * 100), "% CI")
  
  # Convert bands into "ribbon rows" that can live in p_all
  ribbon_data <- bind_rows(
    ensemble_band %>%
      transmute(
        GCAM_region_ID,
        year,
        demand_type,
        band = "Range",
        ribbon_ymin = ymin,
        ribbon_ymax = ymax
      ),
    ensemble_band %>%
      transmute(
        GCAM_region_ID,
        year,
        demand_type,
        band = ci_label,
        ribbon_ymin = lo,
        ribbon_ymax = hi
      )
  ) %>%
    mutate(
      # line-related fields present for compatibility but unused for ribbons
      demand_value  = NA_real_,
      scenario_name = NA_character_,
      model         = "ensemble_band",
      line_role     = NA_character_,
      iteration     = NA_integer_
    )
  
  # --- Helper for single scenarios (set 1 vs set 2) ---
  reshape_scenario <- function(scen_df, label, model_tag, role) {
    if (is.null(scen_df) || is.null(label)) return(tibble())
    
    scen_df %>%
      filter(
        `gcam-consumer` == "FoodDemand_Group1",
        GCAM_region_ID  == reg_num,
        year >= 2015
      ) %>%
      select(year, Qs.region, Qn.region, Qtot.region) %>%
      pivot_longer(
        c(Qs.region, Qn.region, Qtot.region),
        names_to  = "demand_type",
        values_to = "demand_value"
      ) %>%
      mutate(
        GCAM_region_ID = as.character(reg_num),
        scenario_name  = label,
        model          = model_tag,  # "set1" or "set2"
        line_role      = role,       # "solid", "dashed", "dotted"
        iteration      = NA_integer_,
        band           = NA_character_,
        ribbon_ymin    = NA_real_,
        ribbon_ymax    = NA_real_,
        demand_type    = factor(demand_type, levels = c("Qs.region", "Qn.region", "Qtot.region"))
      )
  }
  
  # --- Build comparison scenario data for any non-NULL inputs ---
  comparison_data <- tibble()
  
  # Set 1
  comparison_data <- bind_rows(
    comparison_data,
    reshape_scenario(scen_solid_1,  scen_solid_name_1,  "set1", "solid"),
    reshape_scenario(scen_dashed_1, scen_dashed_name_1, "set1", "dashed"),
    reshape_scenario(scen_dotted_1, scen_dotted_name_1, "set1", "dotted")
  )
  
  # Set 2
  comparison_data <- bind_rows(
    comparison_data,
    reshape_scenario(scen_solid_2,  scen_solid_name_2,  "set2", "solid"),
    reshape_scenario(scen_dashed_2, scen_dashed_name_2, "set2", "dashed"),
    reshape_scenario(scen_dotted_2, scen_dotted_name_2, "set2", "dotted")
  )
  
  # Combine ribbons + scenarios
  out <- bind_rows(ribbon_data, comparison_data)
  
  if (return_data) {
    return(out)
  }
  
  # If plotting a single region directly (rare in your workflow), render it here:
  # (Most of the time you return_data=TRUE and use the PDF function.)
  scen_levels <- unique(out$scenario_name)
  scen_levels <- scen_levels[!is.na(scen_levels)]
  
  # Line types from line_role; default solid
  lt_map <- setNames(rep("solid", length(scen_levels)), scen_levels)
  lr <- out %>% filter(!is.na(line_role)) %>% distinct(scenario_name, line_role)
  lt_map[lr$scenario_name] <- lr$line_role
  
  # Colors: set1 orange, set2 blue
  color_map <- setNames(rep("#e6550d", length(scen_levels)), scen_levels)
  model_by_scen <- out %>%
    filter(!is.na(scenario_name), !is.na(model)) %>%
    group_by(scenario_name) %>%
    summarise(model = first(model), .groups = "drop")
  
  set2_names <- model_by_scen$scenario_name[model_by_scen$model == "set2"]
  color_map[intersect(names(color_map), set2_names)] <- "#3182bd"
  
  ggplot() +
    geom_ribbon(
      data = out %>% filter(!is.na(band)),
      aes(x = year, ymin = ribbon_ymin, ymax = ribbon_ymax, fill = band)
    ) +
    geom_line(
      data = out %>% filter(!is.na(scenario_name)),
      aes(x = year, y = demand_value, color = scenario_name, linetype = scenario_name),
      linewidth = 0.8
    ) +
    facet_wrap(~ demand_type, scales = "free_y") +
    scale_linetype_manual(values = lt_map) +
    scale_color_manual(values = color_map) +
    scale_fill_manual(
      name   = "Ensemble",
      values = c("Range" = "grey85", ci_label = "grey70")
    ) +
    guides(
      fill     = guide_legend(order = 1),
      color    = guide_legend(order = 2),
      linetype = guide_legend(order = 2)
    ) +
    labs(x = "Year", y = "Demand (kcal/day)", color = "Scenario", linetype = "Scenario") +
    theme_minimal()
}

# Regional demand: multi-page PDF; takes data frame of plots produced by
# plot_regional_demand_comparison and combines them into a single multi-region pdf,
# with each region labeled and a single legend per page. Currently legend is correct
# except all legend keys have thin lines, when the GCAM scenarios should have thick
# lines
plot_regional_demand_comparison_pdf <- function(
    p_all,
    region_mapping,
    output_dir,
    cols_per_page = 3,
    rows_per_page = 4,
    filename = "demand_all_regions_ens_bc.pdf",
    ensemble_name = "Emulator"   # retained for compatibility; not used for ribbons
) {
  
  # Helper: round up ymax to a nice increment
  nice_ymax <- function(x, step = 0.25) {
    if (is.na(x) || !is.finite(x)) return(NA_real_)
    ceiling(x / step) * step
  }
  
  # Join region names and set factor order for demand_type
  p_all <- p_all %>%
    mutate(GCAM_region_ID = as.character(GCAM_region_ID)) %>%
    left_join(
      region_mapping %>% mutate(GCAM_region_ID = as.character(GCAM_region_ID)),
      by = "GCAM_region_ID"
    ) %>%
    mutate(
      region_label = if_else(is.na(region), GCAM_region_ID, region),
      demand_type  = factor(demand_type, levels = c("Qs.region", "Qn.region", "Qtot.region"))
    ) %>%
    select(-region)
  
  # ---- Facet strip logic: region name only above Staples (Qs.region) ----
  p_all <- p_all %>%
    mutate(
      demand_name = dplyr::case_when(
        as.character(demand_type) == "Qs.region"   ~ "Staples",
        as.character(demand_type) == "Qn.region"   ~ "Non-staples",
        as.character(demand_type) == "Qtot.region" ~ "Total",
        TRUE ~ as.character(demand_type)
      ),
      facet_id = paste(region_label, as.character(demand_type), sep = "__"),
      facet_strip = if_else(
        as.character(demand_type) == "Qs.region",
        paste0(region_label, "\n", demand_name),  # region name only on first panel
        paste0("\n", demand_name)                 # blank region line, preserve strip height
      )
    ) %>%
    arrange(region_label, demand_type) %>%
    mutate(facet_id = factor(facet_id, levels = unique(facet_id)))
  
  strip_map <- p_all %>%
    dplyr::distinct(facet_id, facet_strip) %>%
    tibble::deframe()
  
  # Split into ribbons vs scenario lines
  p_ribbon <- p_all %>% filter(!is.na(band))
  p_lines  <- p_all %>% filter(!is.na(scenario_name))
  
  # Linetype map from line_role (scenario lines only)
  scen_levels <- sort(unique(p_lines$scenario_name))
  lt_map <- setNames(rep("solid", length(scen_levels)), scen_levels)
  lr_tbl <- p_lines %>%
    filter(!is.na(line_role)) %>%
    distinct(scenario_name, line_role)
  lt_map[lr_tbl$scenario_name] <- lr_tbl$line_role
  
  # Colors: set1 orange; set2 blue (hard-wired)
  color_map <- setNames(rep("#e6550d", length(scen_levels)), scen_levels)
  model_by_scen <- p_lines %>%
    filter(!is.na(scenario_name), !is.na(model)) %>%
    group_by(scenario_name) %>%
    summarise(model = first(model), .groups = "drop")
  
  set2_names <- model_by_scen$scenario_name[model_by_scen$model == "set2"]
  color_map[intersect(names(color_map), set2_names)] <- "#3182bd"
  
  # Fill map for ribbons. Ensure legend order is Range first, then CI.
  band_levels <- unique(p_ribbon$band)
  band_levels <- band_levels[!is.na(band_levels)]
  band_levels <- c("Range", setdiff(band_levels, "Range"))
  
  fill_values <- setNames(rep("grey70", length(band_levels)), band_levels)
  if ("Range" %in% names(fill_values)) fill_values["Range"] <- "grey85"
  
  p_ribbon <- p_ribbon %>%
    mutate(band = factor(band, levels = band_levels))
  
  # ---- Harmonized y-axis limits (global across all regions) ----
  ymax_sn <- p_all %>%
    filter(demand_type %in% c("Qs.region", "Qn.region")) %>%
    summarise(mx = max(c(ribbon_ymax, demand_value), na.rm = TRUE)) %>%
    pull(mx) %>%
    nice_ymax(step = 0.25)
  
  ymax_tot <- p_all %>%
    filter(demand_type %in% c("Qtot.region")) %>%
    summarise(mx = max(c(ribbon_ymax, demand_value), na.rm = TRUE)) %>%
    pull(mx) %>%
    nice_ymax(step = 0.25)
  
  # ---- Force per-facet y ranges via geom_blank() (works with ggforce pagination) ----
  min_year <- suppressWarnings(min(p_all$year, na.rm = TRUE))
  if (!is.finite(min_year)) min_year <- 2015
  
  facet_tbl <- p_all %>%
    distinct(facet_id, demand_type)
  
  blank_df <- facet_tbl %>%
    mutate(
      ymax = if_else(as.character(demand_type) == "Qtot.region", ymax_tot, ymax_sn)
    ) %>%
    select(facet_id, ymax) %>%
    tidyr::uncount(weights = 2, .id = "k") %>%
    mutate(
      year = min_year,
      demand_value = if_else(k == 1, 0, ymax)
    ) %>%
    select(facet_id, year, demand_value)
  
  # Base plot; paginate with ggforce::facet_wrap_paginate()
  base <- ggplot() +
    # Invisible points to force y-axis to [0, ymax_*] per facet
    geom_blank(
      data = blank_df,
      aes(x = year, y = demand_value, group = facet_id)
    ) +
    # Ribbons (behind lines)
    geom_ribbon(
      data = p_ribbon,
      aes(
        x = year,
        ymin = ribbon_ymin,
        ymax = ribbon_ymax,
        fill = band,
        group = interaction(facet_id, band)
      )
    ) +
    # Scenario lines
    geom_line(
      data = p_lines,
      aes(
        x = year,
        y = demand_value,
        color = scenario_name,
        linetype = scenario_name,
        group = interaction(facet_id, scenario_name)
      ),
      linewidth = 0.8
    ) +
    scale_linetype_manual(values = lt_map) +
    scale_color_manual(values = color_map) +
    scale_fill_manual(name = "Ensemble", values = fill_values) +
    guides(
      fill     = guide_legend(order = 1),
      color    = guide_legend(order = 2),
      linetype = guide_legend(order = 2)
    ) +
    labs(
      x = "Year",
      y = "Demand (kcal/day)",
      color = "Scenario",
      linetype = "Scenario"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      strip.text = element_text(size = 10)
    )
  
  # Determine number of pages
  tmp <- base + ggforce::facet_wrap_paginate(
    ~ facet_id,
    ncol = cols_per_page,
    nrow = rows_per_page,
    scales = "free_y",
    labeller = labeller(facet_id = as_labeller(strip_map)),
    page = 1
  )
  n_pg <- ggforce::n_pages(tmp)
  
  pdf(file.path(output_dir, filename), width = 8.5, height = 11)
  for (pg in seq_len(n_pg)) {
    p <- base + ggforce::facet_wrap_paginate(
      ~ facet_id,
      ncol = cols_per_page,
      nrow = rows_per_page,
      scales = "free_y",
      labeller = labeller(facet_id = as_labeller(strip_map)),
      page = pg
    )
    print(p)
  }
  dev.off()
}

# =============================================================================
# Decile demand comparison plots (range + CI ribbons + scenario overlays)
# =============================================================================

plot_decile_demand_comparison <- function(
    demand_reg,
    reg_num,
    consumer_group,
    
    # set1 (orange)
    scen_solid_1  = NULL, scen_solid_name_1  = NULL,
    scen_dashed_1 = NULL, scen_dashed_name_1 = NULL,
    scen_dotted_1 = NULL, scen_dotted_name_1 = NULL,
    
    # set2 (blue)
    scen_solid_2  = NULL, scen_solid_name_2  = NULL,
    scen_dashed_2 = NULL, scen_dashed_name_2 = NULL,
    scen_dotted_2 = NULL, scen_dotted_name_2 = NULL,
    
    ci_level = 0.90,
    return_data = FALSE
) {
  
  stopifnot(ci_level > 0 && ci_level < 1)
  
  required_cols <- c("GCAM_region_ID", "year", "iteration",
                     "gcam-consumer", "Qs", "Qn", "Qtot")
  missing_cols <- setdiff(required_cols, names(demand_reg))
  if (length(missing_cols) > 0) {
    stop("plot_decile_demand_comparison(): missing columns: ",
         paste(missing_cols, collapse = ", "))
  }
  
  # ---------------------------------------------------------------------------
  # Helper: reshape scenario to long form
  reshape_scenario <- function(df, scen_name, model_set, line_role) {
    if (is.null(df) || is.null(scen_name)) return(NULL)
    
    df %>%
      filter(
        year >= 2015,
        GCAM_region_ID == reg_num,
        `gcam-consumer` == consumer_group
      ) %>%
      select(GCAM_region_ID, year, Qs, Qn, Qtot) %>%
      pivot_longer(
        cols = c(Qs, Qn, Qtot),
        names_to = "demand_type",
        values_to = "demand_value"
      ) %>%
      mutate(
        scenario_name = scen_name,
        model = model_set,
        line_role = line_role,
        band = NA_character_,
        ribbon_ymin = NA_real_,
        ribbon_ymax = NA_real_
      )
  }
  
  # ---------------------------------------------------------------------------
  # Ensemble ribbons
  ens_long <- demand_reg %>%
    filter(
      year >= 2015,
      GCAM_region_ID == reg_num,
      `gcam-consumer` == consumer_group
    ) %>%
    select(GCAM_region_ID, year, iteration, Qs, Qn, Qtot) %>%
    pivot_longer(
      cols = c(Qs, Qn, Qtot),
      names_to = "demand_type",
      values_to = "demand_value"
    )
  
  if (nrow(ens_long) == 0) {
    out_empty <- tibble(
      GCAM_region_ID = integer(),
      year = integer(),
      demand_type = character(),
      demand_value = numeric(),
      band = character(),
      ribbon_ymin = numeric(),
      ribbon_ymax = numeric(),
      scenario_name = character(),
      model = character(),
      line_role = character(),
      consumer_group = character()
    )
    if (return_data) return(out_empty)
    return(ggplot() + theme_void())
  }
  
  alpha <- (1 - ci_level) / 2
  ci_label <- sprintf("%d%% CI", round(ci_level * 100))
  
  ribbon_data <- ens_long %>%
    group_by(GCAM_region_ID, year, demand_type) %>%
    summarise(
      y_min = min(demand_value, na.rm = TRUE),
      y_max = max(demand_value, na.rm = TRUE),
      y_lo  = quantile(demand_value, probs = alpha,     na.rm = TRUE),
      y_hi  = quantile(demand_value, probs = 1 - alpha, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      scenario_name = NA_character_,
      model = NA_character_,
      line_role = NA_character_
    ) %>%
    transmute(
      GCAM_region_ID, year, demand_type,
      demand_value = NA_real_,
      band = "Range",
      ribbon_ymin = y_min,
      ribbon_ymax = y_max,
      scenario_name, model, line_role
    ) %>%
    bind_rows(
      ens_long %>%
        group_by(GCAM_region_ID, year, demand_type) %>%
        summarise(
          ribbon_ymin = quantile(demand_value, probs = alpha,     na.rm = TRUE),
          ribbon_ymax = quantile(demand_value, probs = 1 - alpha, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          demand_value = NA_real_,
          band = ci_label,
          scenario_name = NA_character_,
          model = NA_character_,
          line_role = NA_character_
        )
    )
  
  # ---------------------------------------------------------------------------
  # Scenario overlays
  comparison_data <- bind_rows(
    reshape_scenario(scen_solid_1,  scen_solid_name_1,  "set1", "solid"),
    reshape_scenario(scen_dashed_1, scen_dashed_name_1, "set1", "dashed"),
    reshape_scenario(scen_dotted_1, scen_dotted_name_1, "set1", "dotted"),
    reshape_scenario(scen_solid_2,  scen_solid_name_2,  "set2", "solid"),
    reshape_scenario(scen_dashed_2, scen_dashed_name_2, "set2", "dashed"),
    reshape_scenario(scen_dotted_2, scen_dotted_name_2, "set2", "dotted")
  )
  
  out <- bind_rows(ribbon_data, comparison_data) %>%
    mutate(
      consumer_group = consumer_group,
      demand_type = factor(demand_type, levels = c("Qs", "Qn", "Qtot"))
    )
  
  if (return_data) return(out)
  
  # ---------------------------------------------------------------------------
  # Plot (single consumer group)
  set1_col <- "#e6550d"
  set2_col <- "#3182bd"
  
  scen_map <- out %>%
    filter(!is.na(scenario_name)) %>%
    distinct(scenario_name, model, line_role) %>%
    mutate(
      color_val = if_else(model == "set2", set2_col, set1_col),
      lty_val = case_when(
        line_role == "dashed" ~ "dashed",
        line_role == "dotted" ~ "dotted",
        TRUE ~ "solid"
      )
    )
  
  col_vals <- setNames(scen_map$color_val, scen_map$scenario_name)
  lty_vals <- setNames(scen_map$lty_val,   scen_map$scenario_name)
  
  ggplot(out) +
    geom_ribbon(
      data = filter(out, !is.na(band)),
      aes(x = year, ymin = ribbon_ymin, ymax = ribbon_ymax, fill = band),
      alpha = 0.6
    ) +
    geom_line(
      data = filter(out, is.na(band) & !is.na(scenario_name)),
      aes(x = year, y = demand_value, color = scenario_name, linetype = scenario_name),
      linewidth = 0.7
    ) +
    facet_wrap(~ demand_type, ncol = 3, scales = "free_y") +
    scale_color_manual(values = col_vals, drop = FALSE) +
    scale_linetype_manual(values = lty_vals, drop = FALSE) +
    scale_fill_manual(values = c("Range" = "grey80", ci_label = "grey60"), drop = FALSE) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      strip.background = element_rect(fill = "grey95"),
      panel.grid.minor = element_blank()
    ) +
    labs(
      title = sprintf("Decile demand comparison: region %s, %s", reg_num, consumer_group),
      x = NULL, y = NULL, color = NULL, linetype = NULL, fill = NULL
    )
}


plot_decile_demand_comparison_pdf <- function(
    p_all,
    region_mapping,
    output_dir,
    filename = "demand_decile_all_regions_ens_bc.pdf",
    ensemble_name = "Emulator"    # retained for compatibility; not used for ribbons
) {
  
  # ---- Join region names and set factor order for demand_type (same approach as regional) ----
  p_all <- p_all %>%
    mutate(GCAM_region_ID = as.character(GCAM_region_ID)) %>%
    left_join(
      region_mapping %>% mutate(GCAM_region_ID = as.character(GCAM_region_ID)),
      by = "GCAM_region_ID"
    ) %>%
    mutate(
      region_label = if_else(is.na(region), GCAM_region_ID, region),
      demand_type  = factor(demand_type, levels = c("Qs", "Qn", "Qtot"))
    ) %>%
    select(-region)
  
  # ---- Map consumer groups to Decile labels; set row order ----
  p_all <- p_all %>%
    mutate(
      decile_name = case_when(
        as.character(consumer_group) == "FoodDemand_Group1"  ~ "Decile 1",
        as.character(consumer_group) == "FoodDemand_Group2"  ~ "Decile 2",
        as.character(consumer_group) == "FoodDemand_Group3"  ~ "Decile 3",
        as.character(consumer_group) == "FoodDemand_Group6"  ~ "Decile 6",
        as.character(consumer_group) == "FoodDemand_Group10" ~ "Decile 10",
        TRUE ~ as.character(consumer_group)
      ),
      consumer_group = factor(
        as.character(consumer_group),
        levels = c("FoodDemand_Group1", "FoodDemand_Group2", "FoodDemand_Group3",
                   "FoodDemand_Group6", "FoodDemand_Group10")
      )
    )
  
  # ---- Facet strip logic (regional analogue)
  # Each panel has its own strip; Decile label appears only above Staples (Qs) panel in each row.
  p_all <- p_all %>%
    mutate(
      demand_name = case_when(
        as.character(demand_type) == "Qs"   ~ "Staples",
        as.character(demand_type) == "Qn"   ~ "Non-staples",
        as.character(demand_type) == "Qtot" ~ "Total",
        TRUE ~ as.character(demand_type)
      ),
      facet_id = paste(decile_name, as.character(demand_type), sep = "__"),
      facet_strip = if_else(
        as.character(demand_type) == "Qs",
        paste0(decile_name, "\n", demand_name),  # decile label only on left panel
        paste0("\n", demand_name)                # blank decile line, preserve strip height
      )
    ) %>%
    arrange(region_label, consumer_group, demand_type) %>%
    mutate(facet_id = factor(facet_id, levels = unique(facet_id)))
  
  strip_map <- p_all %>%
    distinct(facet_id, facet_strip) %>%
    tibble::deframe()
  
  # ---- Split into ribbons vs scenario lines ----
  p_ribbon <- p_all %>% filter(!is.na(band))
  p_lines  <- p_all %>% filter(!is.na(scenario_name))
  
  # Linetype map from line_role (scenario lines only)
  scen_levels <- sort(unique(p_lines$scenario_name))
  lt_map <- setNames(rep("solid", length(scen_levels)), scen_levels)
  lr_tbl <- p_lines %>%
    filter(!is.na(line_role)) %>%
    distinct(scenario_name, line_role)
  lt_map[lr_tbl$scenario_name] <- lr_tbl$line_role
  
  # Colors: set1 orange; set2 blue (hard-wired; same as regional)
  color_map <- setNames(rep("#e6550d", length(scen_levels)), scen_levels)
  model_by_scen <- p_lines %>%
    filter(!is.na(scenario_name), !is.na(model)) %>%
    group_by(scenario_name) %>%
    summarise(model = first(model), .groups = "drop")
  
  set2_names <- model_by_scen$scenario_name[model_by_scen$model == "set2"]
  color_map[intersect(names(color_map), set2_names)] <- "#3182bd"
  
  # Fill map for ribbons. Ensure legend order is Range first, then CI.
  band_levels <- unique(p_ribbon$band)
  band_levels <- band_levels[!is.na(band_levels)]
  band_levels <- c("Range", setdiff(band_levels, "Range"))
  
  fill_values <- setNames(rep("grey70", length(band_levels)), band_levels)
  if ("Range" %in% names(fill_values)) fill_values["Range"] <- "grey85"
  
  p_ribbon <- p_ribbon %>%
    mutate(band = factor(band, levels = band_levels))
  
  # ---- Portrait PDF, one page per region ----
  pdf(file.path(output_dir, filename), width = 8.5, height = 11)
  
  for (reg in sort(unique(p_all$region_label))) {
    
    df_ribbon <- p_ribbon %>% filter(region_label == reg)
    df_lines  <- p_lines  %>% filter(region_label == reg)
    
    base <- ggplot() +
      # Ribbons (behind lines)
      geom_ribbon(
        data = df_ribbon,
        aes(
          x = year,
          ymin = ribbon_ymin,
          ymax = ribbon_ymax,
          fill = band,
          group = interaction(facet_id, band)
        )
      ) +
      # Scenario lines
      geom_line(
        data = df_lines,
        aes(
          x = year,
          y = demand_value,
          color = scenario_name,
          linetype = scenario_name,
          group = interaction(facet_id, scenario_name)
        ),
        linewidth = 0.8
      ) +
      scale_linetype_manual(values = lt_map) +
      scale_color_manual(values = color_map) +
      scale_fill_manual(name = "Ensemble", values = fill_values) +
      guides(
        fill     = guide_legend(order = 1),
        color    = guide_legend(order = 2),
        linetype = guide_legend(order = 2)
      ) +
      labs(
        x = "Year",
        y = "Demand (kcal/day)",
        color = "Scenario",
        linetype = "Scenario"
      ) +
      ggtitle(reg) +
      theme_minimal(base_size = 13) +
      theme(
        strip.text = element_text(size = 10)
      )
    
    # Fixed 5 rows x 3 cols on one page; no pagination needed
    p <- base +
      facet_wrap(
        ~ facet_id,
        ncol = 3,
        nrow = 5,
        scales = "free_y",
        labeller = labeller(facet_id = as_labeller(strip_map))
      )
    
    print(p)
  }
  
  dev.off()
}





# # Decile-based comparison plot
# plot_decile_demand_comparison <- function(freq_table, global_iter_row,
#                                           demand_reg, region, consumer_group,
#                                           sample_n = 100, return_data = FALSE) {
#   demand_reg_grp <- demand_reg %>%
#     filter(`gcam-consumer` == consumer_group)
# 
#   sampled_iterations <- demand_reg_grp %>%
#     distinct(iteration) %>%
#     pull(iteration) %>%
#     unique() %>%
#     sample(size = min(sample_n, length(.)))
# 
#   sampled_data <- demand_reg_grp %>%
#     filter(iteration %in% sampled_iterations) %>%
#     select(iteration, year, `gcam-consumer`, Qs, Qn, Qtot) %>%
#     pivot_longer(cols = c(Qs, Qn, Qtot),
#                  names_to = "demand_type", values_to = "demand_value") %>%
#     mutate(GCAM_region_ID = as.character(region))
# 
#   regional_compare <- demand_reg_grp %>%
#     filter(iteration %in% c(freq_table$HD_Qtot, freq_table$LD_Qtot)) %>%
#     mutate(
#       measure = if_else(iteration == freq_table$HD_Qtot, "HD_Qtot", "LD_Qtot"),
#       scenario_source = "regional"
#     )
# 
#   global_compare <- demand_reg_grp %>%
#     filter(iteration %in% c(global_iter_row$HD_Qtot, global_iter_row$LD_Qtot)) %>%
#     mutate(
#       measure = if_else(iteration == global_iter_row$HD_Qtot, "HD_Qtot", "LD_Qtot"),
#       scenario_source = "global"
#     )
# 
#   demand_compare <- bind_rows(regional_compare, global_compare) %>%
#     select(iteration, year, `gcam-consumer`, Qs, Qn, Qtot, measure, scenario_source) %>%
#     pivot_longer(cols = c(Qs, Qn, Qtot),
#                  names_to = "demand_type", values_to = "demand_value") %>%
#     mutate(GCAM_region_ID = as.character(region))
# 
#   if (return_data) {
#     return(bind_rows(
#       sampled_data %>% mutate(scenario_source = "sample", measure = NA),
#       demand_compare
#     ))
#   }
# 
#   ggplot() +
#     geom_line(data = sampled_data, aes(x = year, y = demand_value, group = interaction(iteration, demand_type)),
#               color = "gray85", size = 0.4, alpha = 0.5) +
#     geom_line(data = demand_compare, aes(x = year, y = demand_value, color = measure, linetype = scenario_source),
#               size = 1.2) +
#     facet_wrap(~ demand_type, scales = "free_y") +
#     scale_color_manual(values = c("HD_Qtot" = "#1b9e77", "LD_Qtot" = "#d95f02")) +
#     labs(x = "Year", y = "Demand", color = "Model", linetype = "Scenario") +
#     theme_minimal()
# }
# 
# # Decile-based PDF
# plot_decile_demand_comparison_pdf <- function(p_all, region_mapping, output_dir,
#                                               cols_per_page = 3, rows_per_page = 5,
#                                               filename = "demand_by_decile.pdf") {
#   region_mapping <- region_mapping %>%
#     mutate(GCAM_region_ID = as.character(GCAM_region_ID))
# 
#   p_all <- p_all %>%
#     mutate(GCAM_region_ID = as.character(GCAM_region_ID)) %>%
#     left_join(region_mapping, by = "GCAM_region_ID") %>%
#     mutate(region_label = if_else(is.na(region), GCAM_region_ID, region)) %>%
#     select(-region)
# 
#   p_all <- p_all %>%
#     mutate(
#       region_label = factor(region_label),
#       consumer_group = factor(consumer_group,
#                               levels = c("FoodDemand_Group1", "FoodDemand_Group2", "FoodDemand_Group3",
#                                          "FoodDemand_Group6", "FoodDemand_Group10")),
#       demand_type = factor(demand_type, levels = c("Qs", "Qn", "Qtot")),
#       scenario_source = factor(scenario_source, levels = c("sample", "regional", "global"))
#     )
# 
#   region_levels <- sort(unique(p_all$region_label))
#   p_all <- p_all %>% mutate(region_label = factor(region_label, levels = region_levels))
# 
#   pdf(file.path(output_dir, filename), width = 8.5, height = 11)
# 
#   for (region in levels(p_all$region_label)) {
#     plot_data <- p_all %>% filter(region_label == region)
# 
#     p <- ggplot(plot_data, aes(x = year, y = demand_value)) +
#       geom_line(
#         data = filter(plot_data, scenario_source == "sample"),
#         aes(group = interaction(iteration, demand_type)),
#         color = "gray80", size = 0.4
#       ) +
#       geom_line(
#         data = filter(plot_data, scenario_source %in% c("regional", "global")),
#         aes(color = measure, linetype = scenario_source),
#         size = 1.2
#       ) +
#       facet_wrap(~ consumer_group + demand_type,
#                  ncol = cols_per_page, nrow = rows_per_page,
#                  scales = "free_y") +
#       scale_color_manual(values = c("HD_Qtot" = "#1b9e77", "LD_Qtot" = "#d95f02")) +
#       scale_linetype_manual(values = c("regional" = "dashed", "global" = "solid")) +
#       labs(x = "Year", y = "Demand", color = "Model", linetype = "Scenario",
#            title = paste("Demand Projections by Decile:", region)) +
#       theme_minimal(base_size = 13)
# 
#     print(p)
#   }
# 
#   dev.off()
# }
