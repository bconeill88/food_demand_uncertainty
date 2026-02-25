# Functions supporting the 08_plot_demand_with_intervals.R main script

# =============================================================================
# Helper functions for the plotting functions located further below
# =============================================================================

# Helper: round up ymax to a nice increment; for use in pdf creation with plots
# sharing same ymax.
nice_ymax <- function(x, step = 0.25) {
  if (is.na(x) || !is.finite(x)) return(NA_real_)
  ceiling(x / step) * step
}

# Helper: round limits outward to a "nice" increment (works for negative values too)
nice_limits <- function(xmin, xmax, step = 0.1) {
  if (!is.finite(xmin) || !is.finite(xmax)) return(c(NA_real_, NA_real_))
  c(floor(xmin / step) * step, ceiling(xmax / step) * step)
}

# Helper: build ensemble uncertainty ribbons (range + central CI, or CI-only)
build_ensemble_ribbon_data <- function(
    df,
    reg_num,
    demand_cols,
    demand_levels,
    ci_level = 0.90,
    year_min,
    consumer_col = "gcam-consumer",
    consumer_value = "FoodDemand_Group1",
    region_col = "GCAM_region_ID",
    include_range = TRUE
) {
  
  stopifnot(ci_level > 0, ci_level <= 1)
  
  # Filter to plotting domain (years, consumer group, region)
  out_df <- df %>%
    filter(year >= year_min)
  
  if (consumer_col %in% names(out_df)) {
    out_df <- out_df %>% filter(.data[[consumer_col]] == consumer_value)
  }
  if (region_col %in% names(out_df)) {
    out_df <- out_df %>% filter(.data[[region_col]] == reg_num)
  }
  
  # CI parameters
  alpha    <- (1 - ci_level) / 2
  ci_label <- paste0(round(ci_level * 100), "% CI")
  
  # Long-form ensemble draws (per iteration)
  ensemble_long <- out_df %>%
    select(iteration, year, all_of(demand_cols)) %>%
    pivot_longer(
      cols      = all_of(demand_cols),
      names_to  = "demand_type",
      values_to = "demand_value"
    ) %>%
    mutate(
      GCAM_region_ID = as.integer(reg_num),
      demand_type    = factor(demand_type, levels = demand_levels)
    )
  
  # Range + CI bounds by year and demand type
  ensemble_band <- ensemble_long %>%
    group_by(GCAM_region_ID, year, demand_type) %>%
    summarise(
      ymin = min(demand_value, na.rm = TRUE),
      ymax = max(demand_value, na.rm = TRUE),
      lo   = quantile(demand_value, probs = alpha,     na.rm = TRUE, names = FALSE),
      hi   = quantile(demand_value, probs = 1 - alpha, na.rm = TRUE, names = FALSE),
      .groups = "drop"
    )
  
  # Assemble ribbon rows
  ribbon_rows <- bind_rows(
    if (include_range) {
      ensemble_band %>%
        transmute(
          GCAM_region_ID, year, demand_type,
          band        = "Range",
          ribbon_ymin = ymin,
          ribbon_ymax = ymax
        )
    } else {
      NULL
    },
    ensemble_band %>%
      transmute(
        GCAM_region_ID, year, demand_type,
        band        = ci_label,
        ribbon_ymin = lo,
        ribbon_ymax = hi
      )
  )
  
  ribbon_data <- ribbon_rows %>%
    mutate(
      demand_value  = NA_real_,
      scenario_name = NA_character_,
      model         = "ensemble_band",
      line_role     = NA_character_,
      iteration     = NA_integer_
    )
  
  list(
    ribbon_data = ribbon_data,
    ci_label    = ci_label
  )
}

# Helper: CI bound lines (lower + upper) as scenario lines, with one legend entry
build_ci_bound_lines <- function(
    df,
    reg_num,
    demand_cols,
    demand_levels,
    label,
    ci_level = 0.90,
    year_min,
    consumer_col = "gcam-consumer",
    consumer_value = "FoodDemand_Group1",
    region_col = "GCAM_region_ID",
    model = "set2",
    line_role = "dashed"
) {
  
  stopifnot(ci_level > 0, ci_level <= 1)
  
  # Filter to plotting domain (years, consumer group, region)
  out_df <- df %>%
    filter(year >= year_min)
  
  if (consumer_col %in% names(out_df)) {
    out_df <- out_df %>% filter(.data[[consumer_col]] == consumer_value)
  }
  if (region_col %in% names(out_df)) {
    out_df <- out_df %>% filter(.data[[region_col]] == reg_num)
  }
  
  # CI parameters
  alpha <- (1 - ci_level) / 2
  
  # Long-form draws
  long <- out_df %>%
    select(iteration, year, all_of(demand_cols)) %>%
    pivot_longer(
      cols = all_of(demand_cols),
      names_to = "demand_type",
      values_to = "demand_value"
    ) %>%
    mutate(
      GCAM_region_ID = as.integer(reg_num),
      demand_type    = factor(demand_type, levels = demand_levels)
    )
  
  # Quantiles per year/type
  q <- long %>%
    group_by(GCAM_region_ID, year, demand_type) %>%
    summarise(
      lo = quantile(demand_value, probs = alpha,     na.rm = TRUE, names = FALSE),
      hi = quantile(demand_value, probs = 1 - alpha, na.rm = TRUE, names = FALSE),
      .groups = "drop"
    )
  
  # Two lines, same scenario_name; bound_id keeps them separate for grouping
  bind_rows(
    q %>%
      transmute(
        GCAM_region_ID, year, demand_type,
        demand_value  = lo,
        scenario_name = label,
        model         = model,
        line_role     = line_role,
        bound_id      = "lo",
        iteration     = NA_integer_,
        band          = NA_character_,
        ribbon_ymin   = NA_real_,
        ribbon_ymax   = NA_real_
      ),
    q %>%
      transmute(
        GCAM_region_ID, year, demand_type,
        demand_value  = hi,
        scenario_name = label,
        model         = model,
        line_role     = line_role,
        bound_id      = "hi",
        iteration     = NA_integer_,
        band          = NA_character_,
        ribbon_ymin   = NA_real_,
        ribbon_ymax   = NA_real_
      )
  )
}

# Helper: 
reshape_scenario_long <- function(
    df,
    scen_name,
    model_set,
    line_role,
    reg_num,
    year_min,
    demand_cols,
    demand_levels,
    consumer_group = NULL,
    consumer_col = "gcam-consumer",
    region_col = "GCAM_region_ID"
) {
  if (is.null(df) || is.null(scen_name)) return(NULL)
  
  reg_num_int <- as.integer(reg_num)
  
  out <- df %>%
    mutate(
      !!region_col := as.integer(.data[[region_col]])
    ) %>%
    filter(
      year >= year_min,
      .data[[region_col]] == reg_num_int
    )
  
  if (!is.null(consumer_group) && consumer_col %in% names(out)) {
    out <- out %>% filter(.data[[consumer_col]] == consumer_group)
  }
  
  out %>%
    select(all_of(c(region_col, "year", demand_cols))) %>%
    pivot_longer(
      cols = all_of(demand_cols),
      names_to = "demand_type",
      values_to = "demand_value"
    ) %>%
    mutate(
      GCAM_region_ID = reg_num_int,
      demand_type = factor(demand_type, levels = demand_levels),
      consumer_group = consumer_group,
      scenario_name = scen_name,
      model = model_set,
      line_role = line_role,
      iteration = NA_integer_,
      band = NA_character_,
      ribbon_ymin = NA_real_,
      ribbon_ymax = NA_real_
    )
}

# Helper: lighten a hex color by blending toward white
lighten_hex <- function(hex, amount = 0.45) {
  # amount: 0 = no change, 1 = white
  rgb <- grDevices::col2rgb(hex) / 255
  rgb2 <- rgb + (1 - rgb) * amount
  grDevices::rgb(rgb2[1,], rgb2[2,], rgb2[3,])
}

# Helper: scenario style maps (colors + linetypes), supports *_dark / *_light roles
make_scenario_style_maps <- function(
    p_lines,
    set1_col = "#e6550d",
    set2_col = "#3182bd",
    light_amount = 0.45
) {
  
  scen_levels <- sort(unique(p_lines$scenario_name))
  scen_levels <- scen_levels[!is.na(scen_levels)]
  
  # ---- Linetypes (map your semantic roles to ggplot linetypes) ----
  role_to_lty <- function(role) {
    if (is.na(role) || role == "") return("solid")
    switch(
      role,
      "solid"         = "solid",
      "dashed_dark"   = "dashed",
      "dashed_light"  = "longdash",
      "dotted_dark"   = "dotted",
      "dotted_light"  = "dotdash",
      # fallback for any legacy roles you still pass
      "dashed"        = "dashed",
      "dotted"        = "dotted",
      "solid"
    )
  }
  
  lt_map <- setNames(rep("solid", length(scen_levels)), scen_levels)
  lr_tbl <- p_lines %>%
    filter(!is.na(line_role)) %>%
    distinct(scenario_name, line_role)
  
  if (nrow(lr_tbl) > 0) {
    lt_map[lr_tbl$scenario_name] <- vapply(lr_tbl$line_role, role_to_lty, character(1))
  }
  
  # ---- Colors (set1 orange, set2 blue; light/dark shades via role) ----
  # Base by set membership
  model_by_scen <- p_lines %>%
    filter(!is.na(scenario_name), !is.na(model)) %>%
    group_by(scenario_name) %>%
    summarise(model = first(model), .groups = "drop")
  
  base_col <- setNames(rep(set1_col, length(scen_levels)), scen_levels)
  set2_names <- model_by_scen$scenario_name[model_by_scen$model == "set2"]
  if (length(set2_names) > 0) {
    base_col[intersect(names(base_col), set2_names)] <- set2_col
  }
  
  # Lighten if role ends with "_light"
  role_by_scen <- p_lines %>%
    filter(!is.na(scenario_name)) %>%
    group_by(scenario_name) %>%
    summarise(line_role = first(na.omit(line_role)), .groups = "drop")
  
  color_map <- base_col
  if (nrow(role_by_scen) > 0) {
    light_names <- role_by_scen$scenario_name[grepl("_light$", role_by_scen$line_role)]
    if (length(light_names) > 0) {
      color_map[intersect(names(color_map), light_names)] <-
        vapply(base_col[intersect(names(base_col), light_names)],
               lighten_hex, character(1), amount = light_amount)
    }
  }
  
  list(color_map = color_map, lt_map = lt_map)
}

# Helper: 
make_band_fill_map <- function(p_ribbon,
                               range_col = "grey85",
                               other_col = "grey70") {
  
  band_levels <- unique(p_ribbon$band)
  band_levels <- band_levels[!is.na(band_levels)]
  band_levels <- c("Range", setdiff(band_levels, "Range"))
  
  fill_values <- setNames(rep(other_col, length(band_levels)), band_levels)
  if ("Range" %in% names(fill_values)) fill_values["Range"] <- range_col
  
  list(
    band_levels = band_levels,
    fill_values = fill_values
  )
}

# Helper: compute per-region quantiles (CI and optionally range) for a single year;
# for use with bar plots of demand range by region
build_year_ci_bar_data <- function(
    df,
    target_year,
    value_col,                      # e.g., "Qs.region", "Qn.region", "Qtot.region", or a diff variable
    ci_level = 0.90,
    include_range = FALSE,
    consumer_col = "gcam-consumer",
    consumer_value = "FoodDemand_Group1",
    region_col = "GCAM_region_ID",
    iteration_col = "iteration"
) {
  
  stopifnot(ci_level > 0, ci_level <= 1)
  stopifnot(value_col %in% names(df))
  
  alpha <- (1 - ci_level) / 2
  
  out <- df %>%
    filter(.data$year == target_year)
  
  # optional filter for consumer group if column exists
  if (consumer_col %in% names(out)) {
    out <- out %>% filter(.data[[consumer_col]] == consumer_value)
  }
  
  # guardrails
  required_cols <- c(region_col, iteration_col, "year", value_col)
  missing_cols  <- setdiff(required_cols, names(out))
  if (length(missing_cols) > 0) {
    stop("build_year_ci_bar_data(): missing columns: ",
         paste(missing_cols, collapse = ", "))
  }
  
  # summarize across iterations within region for the target year
  sum_ci <- out %>%
    group_by(across(all_of(region_col))) %>%
    summarise(
      n_draws = n(),
      med = stats::quantile(.data[[value_col]], probs = 0.5, na.rm = TRUE, names = FALSE),
      lo  = stats::quantile(.data[[value_col]], probs = alpha, na.rm = TRUE, names = FALSE),
      hi  = stats::quantile(.data[[value_col]], probs = 1 - alpha, na.rm = TRUE, names = FALSE),
      ymin = if (include_range) min(.data[[value_col]], na.rm = TRUE) else NA_real_,
      ymax = if (include_range) max(.data[[value_col]], na.rm = TRUE) else NA_real_,
      .groups = "drop"
    ) %>%
    rename(GCAM_region_ID = all_of(region_col)) %>%
    mutate(
      GCAM_region_ID = as.integer(GCAM_region_ID),
      target_year    = as.integer(target_year),
      value_col      = value_col,
      ci_level       = ci_level,
      ci_label       = paste0(round(ci_level * 100), "% CI")
    )
  
  sum_ci
}

# Helper: convert plot data to be relative to ML (ML becomes 0)
make_relative_to_ml <- function(p_all, ml_scenario_name) {
  
  # Extract ML baseline by year and demand_type
  ml_base <- p_all %>%
    filter(!is.na(scenario_name),
           scenario_name == ml_scenario_name) %>%
    select(GCAM_region_ID, year, demand_type, ml_value = demand_value) %>%
    distinct()
  
  if (nrow(ml_base) == 0) {
    stop("make_relative_to_ml(): ML scenario not found: ", ml_scenario_name)
  }
  
  # Join baseline and subtract for lines and ribbons
  p_all %>%
    left_join(ml_base, by = c("GCAM_region_ID", "year", "demand_type")) %>%
    mutate(
      demand_value = if_else(!is.na(demand_value), demand_value - ml_value, demand_value),
      ribbon_ymin  = if_else(!is.na(ribbon_ymin),  ribbon_ymin  - ml_value, ribbon_ymin),
      ribbon_ymax  = if_else(!is.na(ribbon_ymax),  ribbon_ymax  - ml_value, ribbon_ymax)
    ) %>%
    select(-ml_value)
}

# =============================================================================
# Regional demand comparison plots (range + CI ribbons + scenario overlays)
# =============================================================================

# Regional demand: single-region plot that shows regional demand over time with
# separate panels for Qs, Qn, and Qtotal, for a sub-sample of the ambrosia
# ensemble. Optionally, it also includes three single scenarios: a reference
# case as a solid line, and alternative scenarios (such as high and low demand) as 
# dashed and dotted lines. Single scenarios are specified to be either from GCAM or 
# ambrosia and scenario data is assumed to contain both, using _gcam and _amb variable
# name extensions.
plot_regional_comparison <- function(
    demand_reg, reg_num,
    
    # --- NEW: which three variables to plot (defaults preserve current behavior)
    value_cols  = c("Qs.region", "Qn.region", "Qtot.region"),
    value_names = c("Qs.region", "Qn.region", "Qtot.region"),  # facet labels
    
    # set1 (orange)
    scen_solid_1         = NULL, scen_solid_name_1         = NULL,
    scen_dashed_dark_1   = NULL, scen_dashed_dark_name_1   = NULL,
    scen_dashed_light_1  = NULL, scen_dashed_light_name_1  = NULL,
    scen_dotted_dark_1   = NULL, scen_dotted_dark_name_1   = NULL,
    scen_dotted_light_1  = NULL, scen_dotted_light_name_1  = NULL,
    
    # set2 (blue)
    scen_solid_2         = NULL, scen_solid_name_2         = NULL,
    scen_dashed_dark_2   = NULL, scen_dashed_dark_name_2   = NULL,
    scen_dashed_light_2  = NULL, scen_dashed_light_name_2  = NULL,
    scen_dotted_dark_2   = NULL, scen_dotted_dark_name_2   = NULL,
    scen_dotted_light_2  = NULL, scen_dotted_light_name_2  = NULL,
    
    ci_level = 0.90,
    return_data = FALSE,
    y_label = "Demand (kcal/day)"
) {
  
  stopifnot(ci_level > 0, ci_level <= 1)
  stopifnot(length(value_cols) == 3, length(value_names) == 3)
  stopifnot(all(value_cols %in% names(demand_reg)))
  
  # define minimum year for x-axis as largest year divisible by 5 that is <= base year
  min_demand_yr <- min(demand_reg$year, na.rm = TRUE)
  min_plot_yr <- min_demand_yr - (min_demand_yr %% 5)
  
  # --- Ensemble ribbons (range + central CI), years >= base year ---
  ribbon_res <- build_ensemble_ribbon_data(
    df             = demand_reg,
    reg_num        = reg_num,
    demand_cols    = value_cols,
    demand_levels  = value_cols,
    ci_level       = ci_level,
    year_min       = min_plot_yr,
    consumer_col   = "gcam-consumer",
    consumer_value = "FoodDemand_Group1",
    region_col     = "GCAM_region_ID"
  )
  
  ribbon_data <- ribbon_res$ribbon_data
  ci_label    <- ribbon_res$ci_label
  
  # --- Build comparison scenario data for any non-NULL inputs ---
  comparison_data <- bind_rows(
    # set1
    reshape_scenario_long(scen_solid_1,        scen_solid_name_1,        "set1", "solid",        reg_num, min_plot_yr,
                          value_cols, value_cols, consumer_group = "FoodDemand_Group1"),
    
    reshape_scenario_long(scen_dashed_dark_1,  scen_dashed_dark_name_1,  "set1", "dashed_dark",  reg_num, min_plot_yr,
                          value_cols, value_cols, consumer_group = "FoodDemand_Group1"),
    
    reshape_scenario_long(scen_dashed_light_1, scen_dashed_light_name_1, "set1", "dashed_light", reg_num, min_plot_yr,
                          value_cols, value_cols, consumer_group = "FoodDemand_Group1"),
    
    reshape_scenario_long(scen_dotted_dark_1,  scen_dotted_dark_name_1,  "set1", "dotted_dark",  reg_num, min_plot_yr,
                          value_cols, value_cols, consumer_group = "FoodDemand_Group1"),
    
    reshape_scenario_long(scen_dotted_light_1, scen_dotted_light_name_1, "set1", "dotted_light", reg_num, min_plot_yr,
                          value_cols, value_cols, consumer_group = "FoodDemand_Group1"),
    
    # set2
    reshape_scenario_long(scen_solid_2,        scen_solid_name_2,        "set2", "solid",        reg_num, min_plot_yr,
                          value_cols, value_cols, consumer_group = "FoodDemand_Group1"),
    
    reshape_scenario_long(scen_dashed_dark_2,  scen_dashed_dark_name_2,  "set2", "dashed_dark",  reg_num, min_plot_yr,
                          value_cols, value_cols, consumer_group = "FoodDemand_Group1"),
    
    reshape_scenario_long(scen_dashed_light_2, scen_dashed_light_name_2, "set2", "dashed_light", reg_num, min_plot_yr,
                          value_cols, value_cols, consumer_group = "FoodDemand_Group1"),
    
    reshape_scenario_long(scen_dotted_dark_2,  scen_dotted_dark_name_2,  "set2", "dotted_dark",  reg_num, min_plot_yr,
                          value_cols, value_cols, consumer_group = "FoodDemand_Group1"),
    
    reshape_scenario_long(scen_dotted_light_2, scen_dotted_light_name_2, "set2", "dotted_light", reg_num, min_plot_yr,
                          value_cols, value_cols, consumer_group = "FoodDemand_Group1")
  )
  
  if (is.null(comparison_data)) comparison_data <- tibble()
  
  # Combine ribbons + scenarios
  out <- bind_rows(ribbon_data, comparison_data)
  
  # --- Apply nice facet labels (still stored in demand_type) ---
  out <- out %>%
    mutate(
      demand_type = factor(as.character(demand_type),
                           levels = value_cols,
                           labels = value_names)
    )
  
  if (return_data) return(out)
  
  # --- single-region plotting ---
  p_ribbon <- out %>% filter(!is.na(band))
  p_lines  <- out %>% filter(!is.na(scenario_name))
  
  scen_styles <- make_scenario_style_maps(p_lines)
  
  ggplot() +
    geom_ribbon(
      data = p_ribbon,
      aes(x = year, ymin = ribbon_ymin, ymax = ribbon_ymax, fill = band)
    ) +
    geom_line(
      data = p_lines,
      aes(x = year, y = demand_value, color = scenario_name, linetype = scenario_name),
      linewidth = 0.8
    ) +
    facet_wrap(~ demand_type, scales = "free_y") +
    scale_linetype_manual(values = scen_styles$lt_map) +
    scale_color_manual(values = scen_styles$color_map) +
    scale_fill_manual(
      name   = "Ensemble",
      values = c("Range" = "grey85", ci_label = "grey70")
    ) +
    guides(
      fill     = guide_legend(order = 1),
      color    = guide_legend(order = 2),
      linetype = guide_legend(order = 2)
    ) +
    labs(x = "Year", y = y_label, color = "Scenario", linetype = "Scenario") +
    theme_minimal()
}

# Regional demand: multi-page PDF; takes data frame of plots produced by
# plot_regional_demand_comparison and combines them into a single multi-region pdf,
# with each region labeled and a single legend per page. Currently legend is correct
# except all legend keys have thin lines, when the GCAM scenarios should have thick
# lines
plot_regional_comparison_pdf <- function(
    p_all,
    region_mapping,
    output_dir,
    cols_per_page = 3,
    rows_per_page = 4,
    filename = "demand_all_regions_ens_bc.pdf",
    color_override = NULL,
    linetype_override = NULL,
    ylimit_mode = c("by_type_global", "by_region"),
    y_label = "Demand (kcal/day)",
    
    # which three variables + strip labels (defaults preserve demand behavior)
    value_cols  = c("Qs.region", "Qn.region", "Qtot.region"),
    value_names = c("Staples", "Non-staples", "Total"),
    
    # nice_ymax() step
    y_step = 0.25
) {
  
  ylimit_mode <- match.arg(ylimit_mode)
  stopifnot(length(value_cols) == 3, length(value_names) == 3)
  
  # ---- Join region names (do NOT factor demand_type yet) ----
  p_all <- p_all %>%
    mutate(GCAM_region_ID = as.character(GCAM_region_ID)) %>%
    left_join(
      region_mapping %>% mutate(GCAM_region_ID = as.character(GCAM_region_ID)),
      by = "GCAM_region_ID"
    ) %>%
    mutate(
      region_label = if_else(is.na(region), GCAM_region_ID, region)
    ) %>%
    select(-region)
  
  # ---- FIX 1: demand_type may be raw (value_cols) OR already prettified (value_names) ----
  dt_vals <- unique(as.character(p_all$demand_type))
  use_labels  <- all(dt_vals %in% value_names)
  type_levels <- if (use_labels) value_names else value_cols
  first_type  <- type_levels[1]
  
  p_all <- p_all %>%
    mutate(
      demand_type = factor(as.character(demand_type), levels = type_levels)
    )
  
  # ---- FIX 2: build facet ids/strips without double-recoding ----
  p_all <- p_all %>%
    mutate(
      demand_name = as.character(demand_type),
      facet_id    = paste(region_label, demand_name, sep = "__"),
      facet_strip = if_else(
        demand_name == first_type,
        paste0(region_label, "\n", demand_name),  # region name only on first panel
        paste0("\n", demand_name)                 # blank region line, preserve strip height
      )
    ) %>%
    arrange(region_label, demand_type) %>%
    mutate(facet_id = factor(facet_id, levels = unique(facet_id)))
  
  strip_map <- p_all %>%
    distinct(facet_id, facet_strip) %>%
    tibble::deframe()
  
  # Split into ribbons vs lines
  p_ribbon <- p_all %>% filter(!is.na(band))
  p_lines  <- p_all %>% filter(!is.na(scenario_name))
  
  # Ensure bound_id exists (some workflows don't create it)
  if (!("bound_id" %in% names(p_lines))) {
    p_lines <- p_lines %>% mutate(bound_id = NA_character_)
  }
  
  scen_styles <- make_scenario_style_maps(p_lines)
  lt_map    <- scen_styles$lt_map
  color_map <- scen_styles$color_map
  
  # Override colors for specific scenario names
  if (!is.null(color_override)) {
    nm <- intersect(names(color_override), names(color_map))
    if (length(nm) > 0) color_map[nm] <- color_override[nm]
  }
  
  # Override linetypes for specific scenario names
  if (!is.null(linetype_override)) {
    nm <- intersect(names(linetype_override), names(lt_map))
    if (length(nm) > 0) lt_map[nm] <- linetype_override[nm]
  }
  
  band_styles <- make_band_fill_map(p_ribbon)
  band_levels <- band_styles$band_levels
  fill_values <- band_styles$fill_values
  
  p_ribbon <- p_ribbon %>% mutate(band = factor(band, levels = band_levels))
  
  # Force per-facet y ranges using invisible points
  min_year <- suppressWarnings(min(p_all$year, na.rm = TRUE))
  if (!is.finite(min_year)) min_year <- 2020
  
  facet_tbl <- p_all %>% distinct(facet_id, demand_type, region_label)
  
  if (ylimit_mode == "by_type_global") {
    
    # FIX 3: call nice_ymax() on a scalar inside summarise()
    type_limits <- p_all %>%
      group_by(demand_type) %>%
      summarise(
        ymax = nice_ymax(max(c(ribbon_ymax, demand_value), na.rm = TRUE), step = y_step),
        .groups = "drop"
      )
    
    blank_df <- facet_tbl %>%
      left_join(type_limits, by = "demand_type") %>%
      select(facet_id, ymax) %>%
      tidyr::uncount(weights = 2, .id = "k") %>%
      mutate(
        year = min_year,
        demand_value = if_else(k == 1, 0, ymax)
      ) %>%
      select(facet_id, year, demand_value)
    
  } else if (ylimit_mode == "by_region") {
    
    # Compute per-region min/max across ALL three variables
    reg_limits <- p_all %>%
      group_by(region_label) %>%
      summarise(
        ymin = min(c(ribbon_ymin, demand_value), na.rm = TRUE),
        ymax = max(c(ribbon_ymax, demand_value), na.rm = TRUE),
        .groups = "drop"
      )
    
    blank_df <- facet_tbl %>%
      left_join(reg_limits, by = "region_label") %>%
      select(facet_id, ymin, ymax) %>%
      tidyr::uncount(weights = 2, .id = "k") %>%
      mutate(
        year = min_year,
        demand_value = if_else(k == 1, ymin, ymax)
      ) %>%
      select(facet_id, year, demand_value)
  }
  
  # Base plot: y-range forcing + ribbon + lines
  base <- ggplot() +
    geom_blank(
      data = blank_df,
      aes(x = year, y = demand_value, group = facet_id)
    ) +
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
    geom_line(
      data = p_lines,
      aes(
        x = year,
        y = demand_value,
        color = scenario_name,
        linetype = scenario_name,
        group = interaction(facet_id, scenario_name, coalesce(bound_id, "main"))
      ),
      linewidth = 0.8
    ) +
    scale_linetype_manual(values = lt_map) +
    scale_color_manual(values = color_map) +
    scale_fill_manual(name = "Ensemble", values = fill_values, drop = FALSE) +
    guides(
      fill     = guide_legend(order = 1),
      color    = guide_legend(order = 2),
      linetype = guide_legend(order = 2)
    ) +
    labs(
      x = "Year",
      y = y_label,
      color = "Scenario",
      linetype = "Scenario"
    ) +
    theme_minimal(base_size = 13) +
    theme(strip.text = element_text(size = 10))
  
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
# Regional elasticity comparison plots (range + CI ribbons + scenario overlays)
# =============================================================================

# Regional elasticities: single-region plot that shows uncertainty in regional price 
# and income elasticities over time with separate panels for own- and cross-price 
# elasticities and for income elasticities, for staples and non-staples. Optionally, 
# it also includes up to six scenarios: a reference case as a solid line, and 
# alternative scenarios (such as high and low demand) as dashed and dotted lines. 
# Single scenarios are organized in two groups of three, one that plots in blue and 
# one in orange.

# Regional elasticities: single-region builder (range + CI ribbons + scenario overlays)
plot_regional_elasticity_comparison <- function(
    elast_reg,                        # regional ensemble (iterations)
    reg_num,
    
    # elasticity columns in elast_reg and in scenario data frames
    elast_cols = c(
      "elast.ss",  # own-price staples
      "elast.nn",  # own-price non-staples
      "elast.sn",  # cross-price elasticity for staples
      "elast.ns",  # cross-price elasticity for non-staples
      "eta.s",   # income elasticity staples
      "eta.n"    # income elasticity non-staples
    ),
    
    # pretty facet names (same order as elast_cols)
    elast_names = c(
      "Own-price (Staples)",
      "Own-price (Non-staples)",
      "Cross-price (Staples)",
      "Cross-price (Non-staples)",
      "Income (Staples)",
      "Income (Non-staples)"
    ),
    
    # set1 (orange)
    scen_solid_1         = NULL, scen_solid_name_1         = NULL,
    scen_dashed_dark_1   = NULL, scen_dashed_dark_name_1   = NULL,
    scen_dashed_light_1  = NULL, scen_dashed_light_name_1  = NULL,
    scen_dotted_dark_1   = NULL, scen_dotted_dark_name_1   = NULL,
    scen_dotted_light_1  = NULL, scen_dotted_light_name_1  = NULL,
    
    # set2 (blue)
    scen_solid_2         = NULL, scen_solid_name_2         = NULL,
    scen_dashed_dark_2   = NULL, scen_dashed_dark_name_2   = NULL,
    scen_dashed_light_2  = NULL, scen_dashed_light_name_2  = NULL,
    scen_dotted_dark_2   = NULL, scen_dotted_dark_name_2   = NULL,
    scen_dotted_light_2  = NULL, scen_dotted_light_name_2  = NULL,
    
    ci_level = 0.90,
    consumer_col = "gcam-consumer",
    consumer_value = "FoodDemand_Group1",
    region_col = "GCAM_region_ID",
    return_data = FALSE,
    y_label = "Elasticity"    # only relevant if return_data = FALSE
) {
  
  stopifnot(ci_level > 0, ci_level <= 1)
  stopifnot(length(elast_cols) == 6)
  stopifnot(all(elast_cols %in% names(elast_reg)))
  
  # define minimum year for x-axis as largest year divisible by 5 that is <= base year
  min_demand_yr <- min(elast_reg$year, na.rm = TRUE)
  min_plot_yr <- min_demand_yr - (min_demand_yr %% 5)
  
  # --- Ensemble ribbons (range + central CI), years >= year_min ---
  ribbon_res <- build_ensemble_ribbon_data(
    df             = elast_reg,
    reg_num        = reg_num,
    demand_cols    = elast_cols,
    demand_levels  = elast_cols,
    ci_level       = ci_level,
    year_min       = min_plot_yr,
    consumer_col   = consumer_col,
    consumer_value = consumer_value,
    region_col     = region_col,
    include_range  = TRUE
  )
  
  ribbon_data <- ribbon_res$ribbon_data
  ci_label    <- ribbon_res$ci_label
  
  # --- Scenario overlays (same helper as demand plots) ---
  comparison_data <- bind_rows(
    # set1
    reshape_scenario_long(scen_solid_1,        scen_solid_name_1,        "set1", "solid",
                          reg_num = reg_num, year_min = min_plot_yr,
                          demand_cols = elast_cols, demand_levels = elast_cols,
                          consumer_group = consumer_value,
                          consumer_col = consumer_col, region_col = region_col),
    
    reshape_scenario_long(scen_dashed_dark_1,  scen_dashed_dark_name_1,  "set1", "dashed_dark",
                          reg_num = reg_num, year_min = min_plot_yr,
                          demand_cols = elast_cols, demand_levels = elast_cols,
                          consumer_group = consumer_value,
                          consumer_col = consumer_col, region_col = region_col),
    
    reshape_scenario_long(scen_dashed_light_1, scen_dashed_light_name_1, "set1", "dashed_light",
                          reg_num = reg_num, year_min = min_plot_yr,
                          demand_cols = elast_cols, demand_levels = elast_cols,
                          consumer_group = consumer_value,
                          consumer_col = consumer_col, region_col = region_col),
    
    reshape_scenario_long(scen_dotted_dark_1,  scen_dotted_dark_name_1,  "set1", "dotted_dark",
                          reg_num = reg_num, year_min = min_plot_yr,
                          demand_cols = elast_cols, demand_levels = elast_cols,
                          consumer_group = consumer_value,
                          consumer_col = consumer_col, region_col = region_col),
    
    reshape_scenario_long(scen_dotted_light_1, scen_dotted_light_name_1, "set1", "dotted_light",
                          reg_num = reg_num, year_min = min_plot_yr,
                          demand_cols = elast_cols, demand_levels = elast_cols,
                          consumer_group = consumer_value,
                          consumer_col = consumer_col, region_col = region_col),
    
    # set2
    reshape_scenario_long(scen_solid_2,        scen_solid_name_2,        "set2", "solid",
                          reg_num = reg_num, year_min = min_plot_yr,
                          demand_cols = elast_cols, demand_levels = elast_cols,
                          consumer_group = consumer_value,
                          consumer_col = consumer_col, region_col = region_col),
    
    reshape_scenario_long(scen_dashed_dark_2,  scen_dashed_dark_name_2,  "set2", "dashed_dark",
                          reg_num = reg_num, year_min = min_plot_yr,
                          demand_cols = elast_cols, demand_levels = elast_cols,
                          consumer_group = consumer_value,
                          consumer_col = consumer_col, region_col = region_col),
    
    reshape_scenario_long(scen_dashed_light_2, scen_dashed_light_name_2, "set2", "dashed_light",
                          reg_num = reg_num, year_min = min_plot_yr,
                          demand_cols = elast_cols, demand_levels = elast_cols,
                          consumer_group = consumer_value,
                          consumer_col = consumer_col, region_col = region_col),
    
    reshape_scenario_long(scen_dotted_dark_2,  scen_dotted_dark_name_2,  "set2", "dotted_dark",
                          reg_num = reg_num, year_min = min_plot_yr,
                          demand_cols = elast_cols, demand_levels = elast_cols,
                          consumer_group = consumer_value,
                          consumer_col = consumer_col, region_col = region_col),
    
    reshape_scenario_long(scen_dotted_light_2, scen_dotted_light_name_2, "set2", "dotted_light",
                          reg_num = reg_num, year_min = min_plot_yr,
                          demand_cols = elast_cols, demand_levels = elast_cols,
                          consumer_group = consumer_value,
                          consumer_col = consumer_col, region_col = region_col)
  )
  
  if (is.null(comparison_data)) comparison_data <- tibble()
  
  # --- Combine ribbons + scenarios; add display names for facet strips ---
  out <- bind_rows(ribbon_data, comparison_data) %>%
    mutate(
      demand_type = factor(demand_type, levels = elast_cols),
      elast_name  = factor(elast_names[match(as.character(demand_type), elast_cols)],
                           levels = elast_names)
    )
  
  if (return_data) return(out)
  
  # Optional single-region ggplot (kept consistent with your demand function)
  p_ribbon <- out %>% filter(!is.na(band))
  p_lines  <- out %>% filter(!is.na(scenario_name))
  
  scen_styles <- make_scenario_style_maps(p_lines)
  band_styles <- make_band_fill_map(p_ribbon)
  
  p_ribbon <- p_ribbon %>%
    mutate(band = factor(band, levels = band_styles$band_levels))
  
  ggplot() +
    geom_ribbon(
      data = p_ribbon,
      aes(x = year, ymin = ribbon_ymin, ymax = ribbon_ymax, fill = band,
          group = interaction(GCAM_region_ID, demand_type, band))
    ) +
    geom_line(
      data = p_lines,
      aes(x = year, y = demand_value, color = scenario_name, linetype = scenario_name,
          group = interaction(GCAM_region_ID, demand_type, scenario_name)),
      linewidth = 0.8
    ) +
    scale_linetype_manual(values = scen_styles$lt_map) +
    scale_color_manual(values = scen_styles$color_map) +
    scale_fill_manual(name = "Ensemble", values = band_styles$fill_values) +
    facet_wrap(~ elast_name, ncol = 3, scales = "free_y") +
    theme_minimal(base_size = 13) +
    labs(x = "Year", y = y_label, color = "Scenario", linetype = "Scenario")
}

# Regional elasticities: multi-page PDF; takes data frame of plots produced by
# plot_regional_elasticity_comparison and combines them into a single multi-region pdf,
# with each region labeled and a single legend per page.
plot_regional_elasticity_comparison_pdf <- function(
    p_all,
    region_mapping,
    output_dir,
    cols_per_page = 3,
    rows_per_page = 4,
    filename = "elasticities_all_regions_ens_bc.pdf",
    y_step = 0.1,
    y_label = "Elasticity"
) {
  
  # ---- Join region names and enforce elasticity ordering ----
  # demand_type levels in p_all should already be in desired order; enforce explicitly if needed.
  if (!is.factor(p_all$demand_type)) {
    p_all <- p_all %>% mutate(demand_type = factor(demand_type))
  }
  elast_levels <- levels(p_all$demand_type)
  
  p_all <- p_all %>%
    mutate(GCAM_region_ID = as.character(GCAM_region_ID)) %>%
    left_join(
      region_mapping %>% mutate(GCAM_region_ID = as.character(GCAM_region_ID)),
      by = "GCAM_region_ID"
    ) %>%
    mutate(
      region_label = if_else(is.na(region), GCAM_region_ID, region),
      demand_type  = factor(demand_type, levels = elast_levels)
    ) %>%
    select(-region)
  
  # Ensure bound_id exists for grouping (CI-bound lines etc.)
  if (!("bound_id" %in% names(p_all))) {
    p_all <- p_all %>% mutate(bound_id = NA_character_)
  }
  
  # ---- Pretty panel labels (use elast_name if present, else demand_type) ----
  # Also: region name only on the first panel of each region block.
  first_type <- elast_levels[[1]]  # should be "own staples" panel
  
  if ("elast_name" %in% names(p_all)) {
    name_map <- p_all %>% distinct(demand_type, elast_name) %>% tibble::deframe()
    p_all <- p_all %>%
      mutate(panel_name = as.character(name_map[as.character(demand_type)]))
  } else {
    p_all <- p_all %>% mutate(panel_name = as.character(demand_type))
  }
  
  p_all <- p_all %>%
    mutate(
      facet_id = paste(region_label, as.character(demand_type), sep = "__"),
      facet_strip = if_else(
        as.character(demand_type) == first_type,
        paste0(region_label, "\n", panel_name),  # region name only on first panel
        paste0("\n", panel_name)                 # blank region line to preserve strip height
      )
    ) %>%
    arrange(region_label, demand_type) %>%
    mutate(facet_id = factor(facet_id, levels = unique(facet_id)))
  
  strip_map <- p_all %>%
    distinct(facet_id, facet_strip) %>%
    tibble::deframe()
  
  # ---- Split ribbons vs lines ----
  p_ribbon <- p_all %>% filter(!is.na(band))
  p_lines  <- p_all %>% filter(!is.na(scenario_name))
  
  # ---- Global y-limits: SIX separate harmonizations (by elasticity type) ----
  type_limits <- p_all %>%
    group_by(demand_type) %>%
    summarise(
      ymin = min(c(ribbon_ymin, demand_value), na.rm = TRUE),
      ymax = max(c(ribbon_ymax, demand_value), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(
      lims = list(nice_limits(ymin, ymax, step = y_step)),
      ymin_nice = lims[[1]][1],
      ymax_nice = lims[[1]][2]
    ) %>%
    ungroup() %>%
    select(demand_type, ymin_nice, ymax_nice)
  
  # ---- Data to force per-facet y ranges via geom_blank() ----
  min_year <- suppressWarnings(min(p_all$year, na.rm = TRUE))
  if (!is.finite(min_year)) min_year <- 2020
  
  facet_tbl <- p_all %>% distinct(facet_id, demand_type)
  
  blank_df <- facet_tbl %>%
    left_join(type_limits, by = "demand_type") %>%
    select(facet_id, ymin_nice, ymax_nice) %>%
    tidyr::uncount(weights = 2, .id = "k") %>%
    mutate(
      year = min_year,
      demand_value = if_else(k == 1, ymin_nice, ymax_nice)
    ) %>%
    select(facet_id, year, demand_value)
  
  # ---- Linetype + color maps (scenario lines) ----
  scen_styles <- make_scenario_style_maps(p_lines)
  lt_map    <- scen_styles$lt_map
  color_map <- scen_styles$color_map
  
  # ---- Fill map for ribbons (Range first, then CI) ----
  band_styles <- make_band_fill_map(p_ribbon)
  band_levels <- band_styles$band_levels
  fill_values <- band_styles$fill_values
  
  p_ribbon <- p_ribbon %>%
    mutate(band = factor(band, levels = band_levels))
  
  # ---- Base plot: y-range forcing + ribbon + lines ----
  base <- ggplot() +
    geom_blank(
      data = blank_df,
      aes(x = year, y = demand_value, group = facet_id)
    ) +
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
    geom_line(
      data = p_lines,
      aes(
        x = year,
        y = demand_value,
        color = scenario_name,
        linetype = scenario_name,
        # group = interaction(facet_id, scenario_name, bound_id)
        group = interaction(facet_id, scenario_name, coalesce(bound_id, "main"))
      ),
      linewidth = 0.8
    ) +
    scale_linetype_manual(values = lt_map) +
    scale_color_manual(values = color_map) +
    scale_fill_manual(name = "Ensemble", values = fill_values, drop = FALSE) +
    guides(
      fill     = guide_legend(order = 1),
      color    = guide_legend(order = 2),
      linetype = guide_legend(order = 2)
    ) +
    labs(
      x = "Year",
      y = y_label,
      color = "Scenario",
      linetype = "Scenario"
    ) +
    theme_minimal(base_size = 13) +
    theme(strip.text = element_text(size = 10))
  
  # ---- Determine number of pages (same pattern as demand pdf function) ----
  tmp <- base + ggforce::facet_wrap_paginate(
    ~ facet_id,
    ncol = cols_per_page,
    nrow = rows_per_page,
    scales = "free_y",
    labeller = labeller(facet_id = as_labeller(strip_map)),
    page = 1
  )
  n_pg <- ggforce::n_pages(tmp)
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
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
    scen_solid_1         = NULL, scen_solid_name_1         = NULL,
    scen_dashed_dark_1   = NULL, scen_dashed_dark_name_1   = NULL,
    scen_dashed_light_1  = NULL, scen_dashed_light_name_1  = NULL,
    scen_dotted_dark_1   = NULL, scen_dotted_dark_name_1   = NULL,
    scen_dotted_light_1  = NULL, scen_dotted_light_name_1  = NULL,
    
    # set2 (blue)
    scen_solid_2         = NULL, scen_solid_name_2         = NULL,
    scen_dashed_dark_2   = NULL, scen_dashed_dark_name_2   = NULL,
    scen_dashed_light_2  = NULL, scen_dashed_light_name_2  = NULL,
    scen_dotted_dark_2   = NULL, scen_dotted_dark_name_2   = NULL,
    scen_dotted_light_2  = NULL, scen_dotted_light_name_2  = NULL,
    
    ci_level = 0.90,
    return_data = FALSE,
    y_label = "Demand (kcal/day)"   # only relevant if return_data = FALSE
) {
  
  stopifnot(ci_level > 0 && ci_level <= 1)
  
  required_cols <- c("GCAM_region_ID", "year", "iteration",
                     "gcam-consumer", "Qs", "Qn", "Qtot")
  missing_cols <- setdiff(required_cols, names(demand_reg))
  if (length(missing_cols) > 0) {
    stop("plot_decile_demand_comparison(): missing columns: ",
         paste(missing_cols, collapse = ", "))
  }
  
  # define minimum year for x-axis as largest year divisible by 5 that is <= base year
  min_demand_yr <- min(demand_reg$year, na.rm = TRUE)
  min_plot_yr <- min_demand_yr - (min_demand_yr %% 5)
  
  # ---------------------------------------------------------------------------
  # Ensemble ribbons
  ribbon_res <- build_ensemble_ribbon_data(
    df             = demand_reg,
    reg_num        = reg_num,
    demand_cols    = c("Qs", "Qn", "Qtot"),
    demand_levels  = c("Qs", "Qn", "Qtot"),
    ci_level       = ci_level,
    year_min       = min_plot_yr,
    consumer_col   = "gcam-consumer",
    consumer_value = consumer_group,
    region_col     = "GCAM_region_ID"
  )
  
  ribbon_data <- ribbon_res$ribbon_data
  ci_label    <- ribbon_res$ci_label  # keep if you reference it later for fills/legend
  
  if (nrow(ribbon_data) == 0) {
    out_empty <- tibble(
      GCAM_region_ID = character(),
      year = integer(),
      demand_type = factor(character(), levels = c("Qs", "Qn", "Qtot")),
      demand_value = numeric(),
      band = character(),
      ribbon_ymin = numeric(),
      ribbon_ymax = numeric(),
      scenario_name = character(),
      model = character(),
      line_role = character(),
      iteration = integer(),
      consumer_group = character()
    )
    if (return_data) return(out_empty)
    return(ggplot() + theme_void())
  }
  
  # The decile PDF code expects this column to exist
  ribbon_data <- ribbon_data %>%
    mutate(
      consumer_group = consumer_group,
      demand_type = factor(demand_type, levels = c("Qs", "Qn", "Qtot"))
    )
  
  # ---------------------------------------------------------------------------
  # Scenario overlays
  comparison_data <- bind_rows(
    # set1
    reshape_scenario_long(scen_solid_1,        scen_solid_name_1,        "set1", "solid",
                          reg_num = reg_num, year_min = min_plot_yr,
                          demand_cols = c("Qs", "Qn", "Qtot"),
                          demand_levels = c("Qs", "Qn", "Qtot"),
                          consumer_group = consumer_group),
    
    reshape_scenario_long(scen_dashed_dark_1,  scen_dashed_dark_name_1,  "set1", "dashed_dark",
                          reg_num = reg_num, year_min = min_plot_yr,
                          demand_cols = c("Qs", "Qn", "Qtot"),
                          demand_levels = c("Qs", "Qn", "Qtot"),
                          consumer_group = consumer_group),
    
    reshape_scenario_long(scen_dashed_light_1, scen_dashed_light_name_1, "set1", "dashed_light",
                          reg_num = reg_num, year_min = min_plot_yr,
                          demand_cols = c("Qs", "Qn", "Qtot"),
                          demand_levels = c("Qs", "Qn", "Qtot"),
                          consumer_group = consumer_group),
    
    reshape_scenario_long(scen_dotted_dark_1,  scen_dotted_dark_name_1,  "set1", "dotted_dark",
                          reg_num = reg_num, year_min = min_plot_yr,
                          demand_cols = c("Qs", "Qn", "Qtot"),
                          demand_levels = c("Qs", "Qn", "Qtot"),
                          consumer_group = consumer_group),
    
    reshape_scenario_long(scen_dotted_light_1, scen_dotted_light_name_1, "set1", "dotted_light",
                          reg_num = reg_num, year_min = min_plot_yr,
                          demand_cols = c("Qs", "Qn", "Qtot"),
                          demand_levels = c("Qs", "Qn", "Qtot"),
                          consumer_group = consumer_group),
    
    # set2
    reshape_scenario_long(scen_solid_2,        scen_solid_name_2,        "set2", "solid",
                          reg_num = reg_num, year_min = min_plot_yr,
                          demand_cols = c("Qs", "Qn", "Qtot"),
                          demand_levels = c("Qs", "Qn", "Qtot"),
                          consumer_group = consumer_group),
    
    reshape_scenario_long(scen_dashed_dark_2,  scen_dashed_dark_name_2,  "set2", "dashed_dark",
                          reg_num = reg_num, year_min = min_plot_yr,
                          demand_cols = c("Qs", "Qn", "Qtot"),
                          demand_levels = c("Qs", "Qn", "Qtot"),
                          consumer_group = consumer_group),
    
    reshape_scenario_long(scen_dashed_light_2, scen_dashed_light_name_2, "set2", "dashed_light",
                          reg_num = reg_num, year_min = min_plot_yr,
                          demand_cols = c("Qs", "Qn", "Qtot"),
                          demand_levels = c("Qs", "Qn", "Qtot"),
                          consumer_group = consumer_group),
    
    reshape_scenario_long(scen_dotted_dark_2,  scen_dotted_dark_name_2,  "set2", "dotted_dark",
                          reg_num = reg_num, year_min = min_plot_yr,
                          demand_cols = c("Qs", "Qn", "Qtot"),
                          demand_levels = c("Qs", "Qn", "Qtot"),
                          consumer_group = consumer_group),
    
    reshape_scenario_long(scen_dotted_light_2, scen_dotted_light_name_2, "set2", "dotted_light",
                          reg_num = reg_num, year_min = min_plot_yr,
                          demand_cols = c("Qs", "Qn", "Qtot"),
                          demand_levels = c("Qs", "Qn", "Qtot"),
                          consumer_group = consumer_group)
  )
  
  if (is.null(comparison_data)) comparison_data <- tibble()
  
  out <- bind_rows(ribbon_data, comparison_data) %>%
    mutate(demand_type = factor(demand_type, levels = c("Qs", "Qn", "Qtot")))
  
  if (return_data) return(out)
  
  # ---------------------------------------------------------------------------
  # Plot (single consumer group)
  p_ribbon <- out %>% filter(!is.na(band))
  p_lines  <- out %>% filter(!is.na(scenario_name))
  
  scen_styles <- make_scenario_style_maps(p_lines)
  
  ggplot() +
    geom_ribbon(
      data = p_ribbon,
      aes(x = year, ymin = ribbon_ymin, ymax = ribbon_ymax, fill = band),
      alpha = 0.6
    ) +
    geom_line(
      data = p_lines,
      aes(x = year, y = demand_value, color = scenario_name, linetype = scenario_name),
      linewidth = 0.7
    ) +
    facet_wrap(~ demand_type, ncol = 3, scales = "free_y") +
    scale_color_manual(values = scen_styles$color_map, drop = FALSE) +
    scale_linetype_manual(values = scen_styles$lt_map, drop = FALSE) +
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
      x = NULL, y = y_label, color = NULL, linetype = NULL, fill = NULL
    )
}

plot_decile_demand_comparison_pdf <- function(
    p_all,
    region_mapping,
    output_dir,
    filename = "demand_decile_all_regions_ens_bc.pdf",
    y_label = "Demand (kcal/day)"
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
  
  # ---- Harmonized y-axis limits (global across all regions/deciles) ----
  ymax_sn <- p_all %>%
    filter(demand_type %in% c("Qs", "Qn")) %>%
    summarise(mx = max(c(ribbon_ymax, demand_value), na.rm = TRUE)) %>%
    pull(mx) %>%
    nice_ymax(step = 0.25)
  
  ymax_tot <- p_all %>%
    filter(demand_type %in% c("Qtot")) %>%
    summarise(mx = max(c(ribbon_ymax, demand_value), na.rm = TRUE)) %>%
    pull(mx) %>%
    nice_ymax(step = 0.25)
  
  # ---- Data to force facet y ranges via geom_blank() ----
  min_year <- suppressWarnings(min(p_all$year, na.rm = TRUE))
  if (!is.finite(min_year)) min_year <- 2020
  
  # Linetype + color maps (scenario lines)
  scen_styles <- make_scenario_style_maps(p_lines)
  lt_map    <- scen_styles$lt_map
  color_map <- scen_styles$color_map
  
  # Fill map for ribbons (Range first, then CI)
  band_styles <- make_band_fill_map(p_ribbon)
  band_levels <- band_styles$band_levels
  fill_values <- band_styles$fill_values
  
  p_ribbon <- p_ribbon %>%
    mutate(band = factor(band, levels = band_levels))
  
  # ---- Portrait PDF, one page per region ----
  pdf(file.path(output_dir, filename), width = 8.5, height = 11)
  
  for (reg in sort(unique(p_all$region_label))) {
    
    df_ribbon <- p_ribbon %>% filter(region_label == reg)
    df_lines  <- p_lines  %>% filter(region_label == reg)
    
    facet_tbl <- bind_rows(
      df_ribbon %>% select(facet_id, demand_type),
      df_lines  %>% select(facet_id, demand_type)
    ) %>% distinct()
    
    blank_df <- facet_tbl %>%
      mutate(
        ymax = if_else(as.character(demand_type) == "Qtot", ymax_tot, ymax_sn)
      ) %>%
      select(facet_id, ymax) %>%
      tidyr::uncount(weights = 2, .id = "k") %>%
      mutate(
        year = min_year,
        demand_value = if_else(k == 1, 0, ymax)
      ) %>%
      select(facet_id, year, demand_value)
    
    base <- ggplot() +
      geom_blank(
        data = blank_df,
        aes(x = year, y = demand_value, group = facet_id)
      ) +
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
        y = y_label,
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

# =============================================================================
# Combined region/decile demand comparison plots (range + CI ribbons + scenario overlays)
# =============================================================================

plot_region_and_decile_demand_comparison <- function(
    demand_reg,
    reg_num,
    
    # set1 (orange)
    scen_solid_1         = NULL, scen_solid_name_1         = NULL,
    scen_dashed_dark_1   = NULL, scen_dashed_dark_name_1   = NULL,
    scen_dashed_light_1  = NULL, scen_dashed_light_name_1  = NULL,
    scen_dotted_dark_1   = NULL, scen_dotted_dark_name_1   = NULL,
    scen_dotted_light_1  = NULL, scen_dotted_light_name_1  = NULL,
    
    # set2 (blue)
    scen_solid_2         = NULL, scen_solid_name_2         = NULL,
    scen_dashed_dark_2   = NULL, scen_dashed_dark_name_2   = NULL,
    scen_dashed_light_2  = NULL, scen_dashed_light_name_2  = NULL,
    scen_dotted_dark_2   = NULL, scen_dotted_dark_name_2   = NULL,
    scen_dotted_light_2  = NULL, scen_dotted_light_name_2  = NULL,
    
    ci_level = 0.90,
    return_data = TRUE
) {
  
  stopifnot(ci_level > 0 && ci_level <= 1)
  
  # ---- Guardrails: require BOTH regional + decile columns ----
  required_cols <- c(
    "GCAM_region_ID", "year", "iteration", "gcam-consumer",
    # decile cols
    "Qs", "Qn", "Qtot",
    # regional cols
    "Qs.region", "Qn.region", "Qtot.region"
  )
  missing_cols <- setdiff(required_cols, names(demand_reg))
  if (length(missing_cols) > 0) {
    stop("plot_decile_demand_comparison_3row(): missing columns: ",
         paste(missing_cols, collapse = ", "))
  }
  
  # define minimum year for x-axis as largest year divisible by 5 <= base year
  min_demand_yr <- min(demand_reg$year, na.rm = TRUE)
  min_plot_yr <- min_demand_yr - (min_demand_yr %% 5)
  
  # ============================================================================
  # Row 1: Regional total (uses *.region columns)
  #   - Keep consumer filter convention consistent with regional plots
  #   - Then rename demand_type to Qs/Qn/Qtot so columns match deciles
  # ============================================================================
  
  ribbon_reg <- build_ensemble_ribbon_data(
    df             = demand_reg,
    reg_num        = reg_num,
    demand_cols    = c("Qs.region", "Qn.region", "Qtot.region"),
    demand_levels  = c("Qs.region", "Qn.region", "Qtot.region"),
    ci_level       = ci_level,
    year_min       = min_plot_yr,
    consumer_col   = "gcam-consumer",
    consumer_value = "FoodDemand_Group1",
    region_col     = "GCAM_region_ID"
  )$ribbon_data %>%
    mutate(
      row_label = "Regional total",
      consumer_group = "RegionalTotal"
    ) %>%
    mutate(
      demand_type = recode(as.character(demand_type),
                           "Qs.region" = "Qs",
                           "Qn.region" = "Qn",
                           "Qtot.region" = "Qtot"),
      demand_type = factor(demand_type, levels = c("Qs", "Qn", "Qtot"))
    )
  
  lines_reg <- bind_rows(
    reshape_scenario_long(scen_solid_1,        scen_solid_name_1,        "set1", "solid",
                          reg_num = reg_num, year_min = min_plot_yr,
                          demand_cols = c("Qs.region", "Qn.region", "Qtot.region"),
                          demand_levels = c("Qs.region", "Qn.region", "Qtot.region"),
                          consumer_group = "FoodDemand_Group1"),
    reshape_scenario_long(scen_dashed_dark_1,  scen_dashed_dark_name_1,  "set1", "dashed_dark",
                          reg_num = reg_num, year_min = min_plot_yr,
                          demand_cols = c("Qs.region", "Qn.region", "Qtot.region"),
                          demand_levels = c("Qs.region", "Qn.region", "Qtot.region"),
                          consumer_group = "FoodDemand_Group1"),
    reshape_scenario_long(scen_dashed_light_1, scen_dashed_light_name_1, "set1", "dashed_light",
                          reg_num = reg_num, year_min = min_plot_yr,
                          demand_cols = c("Qs.region", "Qn.region", "Qtot.region"),
                          demand_levels = c("Qs.region", "Qn.region", "Qtot.region"),
                          consumer_group = "FoodDemand_Group1"),
    reshape_scenario_long(scen_dotted_dark_1,  scen_dotted_dark_name_1,  "set1", "dotted_dark",
                          reg_num = reg_num, year_min = min_plot_yr,
                          demand_cols = c("Qs.region", "Qn.region", "Qtot.region"),
                          demand_levels = c("Qs.region", "Qn.region", "Qtot.region"),
                          consumer_group = "FoodDemand_Group1"),
    reshape_scenario_long(scen_dotted_light_1, scen_dotted_light_name_1, "set1", "dotted_light",
                          reg_num = reg_num, year_min = min_plot_yr,
                          demand_cols = c("Qs.region", "Qn.region", "Qtot.region"),
                          demand_levels = c("Qs.region", "Qn.region", "Qtot.region"),
                          consumer_group = "FoodDemand_Group1"),
    
    reshape_scenario_long(scen_solid_2,        scen_solid_name_2,        "set2", "solid",
                          reg_num = reg_num, year_min = min_plot_yr,
                          demand_cols = c("Qs.region", "Qn.region", "Qtot.region"),
                          demand_levels = c("Qs.region", "Qn.region", "Qtot.region"),
                          consumer_group = "FoodDemand_Group1"),
    reshape_scenario_long(scen_dashed_dark_2,  scen_dashed_dark_name_2,  "set2", "dashed_dark",
                          reg_num = reg_num, year_min = min_plot_yr,
                          demand_cols = c("Qs.region", "Qn.region", "Qtot.region"),
                          demand_levels = c("Qs.region", "Qn.region", "Qtot.region"),
                          consumer_group = "FoodDemand_Group1"),
    reshape_scenario_long(scen_dashed_light_2, scen_dashed_light_name_2, "set2", "dashed_light",
                          reg_num = reg_num, year_min = min_plot_yr,
                          demand_cols = c("Qs.region", "Qn.region", "Qtot.region"),
                          demand_levels = c("Qs.region", "Qn.region", "Qtot.region"),
                          consumer_group = "FoodDemand_Group1"),
    reshape_scenario_long(scen_dotted_dark_2,  scen_dotted_dark_name_2,  "set2", "dotted_dark",
                          reg_num = reg_num, year_min = min_plot_yr,
                          demand_cols = c("Qs.region", "Qn.region", "Qtot.region"),
                          demand_levels = c("Qs.region", "Qn.region", "Qtot.region"),
                          consumer_group = "FoodDemand_Group1"),
    reshape_scenario_long(scen_dotted_light_2, scen_dotted_light_name_2, "set2", "dotted_light",
                          reg_num = reg_num, year_min = min_plot_yr,
                          demand_cols = c("Qs.region", "Qn.region", "Qtot.region"),
                          demand_levels = c("Qs.region", "Qn.region", "Qtot.region"),
                          consumer_group = "FoodDemand_Group1")
  )
  
  if (is.null(lines_reg)) lines_reg <- tibble()
  
  lines_reg <- lines_reg %>%
    mutate(
      row_label = "Regional total",
      consumer_group = "RegionalTotal"
    ) %>%
    mutate(
      demand_type = recode(as.character(demand_type),
                           "Qs.region" = "Qs",
                           "Qn.region" = "Qn",
                           "Qtot.region" = "Qtot"),
      demand_type = factor(demand_type, levels = c("Qs", "Qn", "Qtot"))
    )
  
  # ============================================================================
  # Rows 2–3: Decile 1 and Decile 10 (reuse your existing decile logic)
  # ============================================================================
  
  build_decile_row <- function(cg, row_label) {
    
    ribbon <- build_ensemble_ribbon_data(
      df             = demand_reg,
      reg_num        = reg_num,
      demand_cols    = c("Qs", "Qn", "Qtot"),
      demand_levels  = c("Qs", "Qn", "Qtot"),
      ci_level       = ci_level,
      year_min       = min_plot_yr,
      consumer_col   = "gcam-consumer",
      consumer_value = cg,
      region_col     = "GCAM_region_ID"
    )$ribbon_data %>%
      mutate(
        consumer_group = cg,
        row_label = row_label,
        demand_type = factor(demand_type, levels = c("Qs", "Qn", "Qtot"))
      )
    
    lines <- bind_rows(
      reshape_scenario_long(scen_solid_1,        scen_solid_name_1,        "set1", "solid",
                            reg_num = reg_num, year_min = min_plot_yr,
                            demand_cols = c("Qs", "Qn", "Qtot"),
                            demand_levels = c("Qs", "Qn", "Qtot"),
                            consumer_group = cg),
      reshape_scenario_long(scen_dashed_dark_1,  scen_dashed_dark_name_1,  "set1", "dashed_dark",
                            reg_num = reg_num, year_min = min_plot_yr,
                            demand_cols = c("Qs", "Qn", "Qtot"),
                            demand_levels = c("Qs", "Qn", "Qtot"),
                            consumer_group = cg),
      reshape_scenario_long(scen_dashed_light_1, scen_dashed_light_name_1, "set1", "dashed_light",
                            reg_num = reg_num, year_min = min_plot_yr,
                            demand_cols = c("Qs", "Qn", "Qtot"),
                            demand_levels = c("Qs", "Qn", "Qtot"),
                            consumer_group = cg),
      reshape_scenario_long(scen_dotted_dark_1,  scen_dotted_dark_name_1,  "set1", "dotted_dark",
                            reg_num = reg_num, year_min = min_plot_yr,
                            demand_cols = c("Qs", "Qn", "Qtot"),
                            demand_levels = c("Qs", "Qn", "Qtot"),
                            consumer_group = cg),
      reshape_scenario_long(scen_dotted_light_1, scen_dotted_light_name_1, "set1", "dotted_light",
                            reg_num = reg_num, year_min = min_plot_yr,
                            demand_cols = c("Qs", "Qn", "Qtot"),
                            demand_levels = c("Qs", "Qn", "Qtot"),
                            consumer_group = cg),
      
      reshape_scenario_long(scen_solid_2,        scen_solid_name_2,        "set2", "solid",
                            reg_num = reg_num, year_min = min_plot_yr,
                            demand_cols = c("Qs", "Qn", "Qtot"),
                            demand_levels = c("Qs", "Qn", "Qtot"),
                            consumer_group = cg),
      reshape_scenario_long(scen_dashed_dark_2,  scen_dashed_dark_name_2,  "set2", "dashed_dark",
                            reg_num = reg_num, year_min = min_plot_yr,
                            demand_cols = c("Qs", "Qn", "Qtot"),
                            demand_levels = c("Qs", "Qn", "Qtot"),
                            consumer_group = cg),
      reshape_scenario_long(scen_dashed_light_2, scen_dashed_light_name_2, "set2", "dashed_light",
                            reg_num = reg_num, year_min = min_plot_yr,
                            demand_cols = c("Qs", "Qn", "Qtot"),
                            demand_levels = c("Qs", "Qn", "Qtot"),
                            consumer_group = cg),
      reshape_scenario_long(scen_dotted_dark_2,  scen_dotted_dark_name_2,  "set2", "dotted_dark",
                            reg_num = reg_num, year_min = min_plot_yr,
                            demand_cols = c("Qs", "Qn", "Qtot"),
                            demand_levels = c("Qs", "Qn", "Qtot"),
                            consumer_group = cg),
      reshape_scenario_long(scen_dotted_light_2, scen_dotted_light_name_2, "set2", "dotted_light",
                            reg_num = reg_num, year_min = min_plot_yr,
                            demand_cols = c("Qs", "Qn", "Qtot"),
                            demand_levels = c("Qs", "Qn", "Qtot"),
                            consumer_group = cg)
    )
    
    if (is.null(lines)) lines <- tibble()
    
    lines <- lines %>%
      mutate(
        consumer_group = cg,
        row_label = row_label,
        demand_type = factor(demand_type, levels = c("Qs", "Qn", "Qtot"))
      )
    
    bind_rows(ribbon, lines)
  }
  
  out <- bind_rows(
    ribbon_reg, lines_reg,
    build_decile_row("FoodDemand_Group1",  "Decile 1"),
    build_decile_row("FoodDemand_Group10", "Decile 10")
  ) %>%
    mutate(
      # enforce consistent ordering
      row_label = factor(row_label, levels = c("Regional total", "Decile 1", "Decile 10")),
      demand_type = factor(demand_type, levels = c("Qs", "Qn", "Qtot"))
    )
  
  if (return_data) return(out)
  
  # (If you ever want a single-region ggplot preview, you can replicate your
  # existing decile preview logic here; most of your workflow is return_data=TRUE.)
  out
}

plot_region_and_decile_demand_comparison_pdf <- function(
    p_all,
    region_mapping,
    output_dir,
    filename = "demand_decile_3row_all_regions.pdf",
    y_label = "Demand (kcal/day)"
) {
  
  # ---- Join region names ----
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
  
  # ---- Facet strip logic: row label only above Staples (Qs) ----
  p_all <- p_all %>%
    mutate(
      demand_name = case_when(
        as.character(demand_type) == "Qs"   ~ "Staples",
        as.character(demand_type) == "Qn"   ~ "Non-staples",
        as.character(demand_type) == "Qtot" ~ "Total",
        TRUE ~ as.character(demand_type)
      ),
      facet_id = paste(as.character(row_label), as.character(demand_type), sep = "__"),
      facet_strip = if_else(
        as.character(demand_type) == "Qs",
        paste0(as.character(row_label), "\n", demand_name),
        paste0("\n", demand_name)
      )
    ) %>%
    arrange(region_label, row_label, demand_type) %>%
    mutate(facet_id = factor(facet_id, levels = unique(facet_id)))
  
  strip_map <- p_all %>%
    distinct(facet_id, facet_strip) %>%
    tibble::deframe()
  
  # ---- Split ribbons vs lines ----
  p_ribbon <- p_all %>% filter(!is.na(band))
  p_lines  <- p_all %>% filter(!is.na(scenario_name))
  
  # ---- Harmonized y-axis limits (global) ----
  ymax_sn <- p_all %>%
    filter(demand_type %in% c("Qs", "Qn")) %>%
    summarise(mx = max(c(ribbon_ymax, demand_value), na.rm = TRUE)) %>%
    pull(mx) %>%
    nice_ymax(step = 0.25)
  
  ymax_tot <- p_all %>%
    filter(demand_type %in% c("Qtot")) %>%
    summarise(mx = max(c(ribbon_ymax, demand_value), na.rm = TRUE)) %>%
    pull(mx) %>%
    nice_ymax(step = 0.25)
  
  min_year <- suppressWarnings(min(p_all$year, na.rm = TRUE))
  if (!is.finite(min_year)) min_year <- 2020
  
  # ---- Maps ----
  scen_styles <- make_scenario_style_maps(p_lines)
  lt_map    <- scen_styles$lt_map
  color_map <- scen_styles$color_map
  
  band_styles <- make_band_fill_map(p_ribbon)
  band_levels <- band_styles$band_levels
  fill_values <- band_styles$fill_values
  
  p_ribbon <- p_ribbon %>% mutate(band = factor(band, levels = band_levels))
  
  # ---- PDF: one page per region; use nrow=4 with dummy row padding ----
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  pdf(file.path(output_dir, filename), width = 8.5, height = 11)
  
  # one page per region
  for (reg in sort(unique(p_all$region_label))) {
    
    # ---- subset this region ----
    df_ribbon <- p_ribbon %>% filter(region_label == reg)
    df_lines  <- p_lines  %>% filter(region_label == reg)
    
    # ---- identify facets present for this region (for blank_df construction) ----
    facet_tbl <- bind_rows(
      df_ribbon %>% select(facet_id, demand_type),
      df_lines  %>% select(facet_id, demand_type)
    ) %>% distinct()
    
    # ---- force y ranges via geom_blank (per facet) ----
    blank_df <- facet_tbl %>%
      mutate(ymax = if_else(as.character(demand_type) == "Qtot", ymax_tot, ymax_sn)) %>%
      select(facet_id, ymax) %>%
      tidyr::uncount(weights = 2, .id = "k") %>%
      mutate(
        year = min_year,
        demand_value = if_else(k == 1, 0, ymax)
      ) %>%
      select(facet_id, year, demand_value)
    
    # ---- main 3-row faceted plot (x-axis labels land on row 3) ----
    p_main <- ggplot() +
      geom_blank(
        data = blank_df,
        aes(x = year, y = demand_value, group = facet_id)
      ) +
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
        y = y_label,
        color = "Scenario",
        linetype = "Scenario"
      ) +
      ggtitle(reg) +
      theme_minimal(base_size = 13) +
      theme(strip.text = element_text(size = 10)) +
      facet_wrap(
        ~ facet_id,
        ncol = 3,
        nrow = 3,
        scales = "free_y",
        labeller = labeller(facet_id = as_labeller(strip_map))
      )
    
    # ---- true blank 4th row (no axes/grid), keep 4-row page geometry ----
    # requires: library(patchwork)
    p_final <- p_main / plot_spacer() + plot_layout(heights = c(3, 1))
    
    print(p_final)
  }
  
  dev.off()
}

# =============================================================================
# Regional demand uncertainty bar plots 
# =============================================================================

# Demand uncertainty bar plots -------------------------------------------------

# Cross-region CI bar plot for a single year
# Main: plot and (optionally) save as PDF
plot_region_ci_bars_one_year <- function(
    df,
    target_year,
    value_col,                        # e.g., "Qtot.region" or "Qtot.diff" etc.
    ci_level = 0.90,
    include_range = FALSE,
    show_median = TRUE,
    sort_by = c("region_id", "median"), # ordering on x-axis
    consumer_col = "gcam-consumer",
    consumer_value = "FoodDemand_Group1",
    region_col = "GCAM_region_ID",
    iteration_col = "iteration",
    x_label = "Region",
    y_label = NULL,
    title = NULL,
    output_dir = NULL,
    filename = NULL,
    width = 11,
    height = 8.5
) {
  
  sort_by <- match.arg(sort_by)
  
  bar_df <- build_year_ci_bar_data(
    df = df,
    target_year = target_year,
    value_col = value_col,
    ci_level = ci_level,
    include_range = include_range,
    consumer_col = consumer_col,
    consumer_value = consumer_value,
    region_col = region_col,
    iteration_col = iteration_col
  )
  
  if (nrow(bar_df) == 0) {
    stop("plot_region_ci_bars_one_year(): no data after filtering to target_year = ", target_year,
         ". Check year availability and consumer group filters.")
  }
  
  # ordering of regions on the x-axis
  if (sort_by == "median") {
    bar_df <- bar_df %>% arrange(.data$med)
  } else {
    bar_df <- bar_df %>% arrange(.data$GCAM_region_ID)
  }
  
  bar_df <- bar_df %>%
    mutate(
      region_factor = factor(GCAM_region_ID, levels = unique(GCAM_region_ID))
    )
  
  if (is.null(y_label)) y_label <- value_col
  if (is.null(title)) {
    title <- sprintf("%s across regions in %d (%s)", value_col, target_year, bar_df$ci_label[[1]])
  }
  
  p <- ggplot2::ggplot(bar_df, ggplot2::aes(x = region_factor)) +
    
    # optional full range behind the CI (lighter, thicker)
    { if (include_range) ggplot2::geom_linerange(
      ggplot2::aes(ymin = ymin, ymax = ymax),
      linewidth = 1.2,
      alpha = 0.35
    ) } +
    
    # central CI
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = lo, ymax = hi),
      linewidth = 0.9
    ) +
    
    # optional median point
    { if (show_median) ggplot2::geom_point(
      ggplot2::aes(y = med),
      size = 1.8
    ) } +
    
    ggplot2::labs(
      title = title,
      x = x_label,
      y = y_label
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      panel.grid.major.x = ggplot2::element_blank()
    )
  
  # save if requested
  if (!is.null(output_dir) && !is.null(filename)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    ggplot2::ggsave(
      filename = file.path(output_dir, filename),
      plot = p,
      width = width,
      height = height,
      units = "in",
      device = cairo_pdf
    )
  }
  
  return(p)
}

# =============================================================================
# Uncertainty decomposition plots (CI ribbon + upper/lower bound overlays)
# =============================================================================

# Regional uncertainty decomposition: CI ribbon for full ensemble + ML + CI bounds 
# for 3 sub-ensembles
plot_regional_uncertainty_decomposition <- function(
    demand_full,
    reg_num,
    
    scen_ml,
    scen_ml_name = "Ambrosia ML",
    
    ens_price,
    ens_income,
    ens_scale,
    
    ci_level = 0.90,
    return_data = FALSE
) {
  
  stopifnot(ci_level > 0, ci_level <= 1)
  
  # Guardrails (regional format expects *.region columns)
  required_cols <- c("GCAM_region_ID", "year", "iteration", "gcam-consumer",
                     "Qs.region", "Qn.region", "Qtot.region")
  missing_cols <- setdiff(required_cols, names(demand_full))
  if (length(missing_cols) > 0) {
    stop("plot_regional_uncertainty_decomposition(): missing columns in demand_full: ",
         paste(missing_cols, collapse = ", "))
  }
  
  demand_cols   <- c("Qs.region", "Qn.region", "Qtot.region")
  demand_levels <- c("Qs.region", "Qn.region", "Qtot.region")
  
  # define minimum year for x-axis as largest year divisible by 5 that is <= base year
  min_demand_yr <- min(demand_full$year, na.rm = TRUE)
  min_plot_yr <- min_demand_yr - (min_demand_yr %% 5)
  
  # Full-ensemble ribbon (CI only)
  ribbon_res <- build_ensemble_ribbon_data(
    df             = demand_full,
    reg_num        = reg_num,
    demand_cols    = demand_cols,
    demand_levels  = demand_levels,
    ci_level       = ci_level,
    year_min       = min_plot_yr,
    consumer_col   = "gcam-consumer",
    consumer_value = "FoodDemand_Group1",
    region_col     = "GCAM_region_ID",
    include_range  = FALSE
  )
  ribbon_data <- ribbon_res$ribbon_data
  
  # ML scenario (standard helper)
  ml_data <- reshape_scenario_long(
    df            = scen_ml,
    scen_name     = scen_ml_name,
    model_set     = "set2",
    line_role     = "solid",
    reg_num       = reg_num,
    year_min      = min_plot_yr,
    demand_cols   = demand_cols,
    demand_levels = demand_levels,
    consumer_group = "FoodDemand_Group1"
  )
  
  # CI bound lines for decomposition ensembles (one legend entry per component)
  price_lines <- build_ci_bound_lines(
    df            = ens_price,
    reg_num       = reg_num,
    demand_cols   = demand_cols,
    demand_levels = demand_levels,
    label         = "Price-only CI",
    ci_level      = ci_level,
    year_min      = min_plot_yr,
    model         = "set2",
    line_role     = "dashed"
  )
  
  income_lines <- build_ci_bound_lines(
    df            = ens_income,
    reg_num       = reg_num,
    demand_cols   = demand_cols,
    demand_levels = demand_levels,
    label         = "Income-only CI",
    ci_level      = ci_level,
    year_min      = min_plot_yr,
    model         = "set2",
    line_role     = "dashed"
  )
  
  scale_lines <- build_ci_bound_lines(
    df            = ens_scale,
    reg_num       = reg_num,
    demand_cols   = demand_cols,
    demand_levels = demand_levels,
    label         = "Scale-only CI",
    ci_level      = ci_level,
    year_min      = min_plot_yr,
    model         = "set2",
    line_role     = "dashed"
  )
  
  # Standardized output table (compatible with your pdf pipeline)
  out <- bind_rows(
    ribbon_data,
    ml_data,
    price_lines,
    income_lines,
    scale_lines
  )
  
  # Convert to differences relative to ML (ML becomes zero)
  out <- make_relative_to_ml(out, ml_scenario_name = scen_ml_name)
  
  if (return_data) return(out)
  
  # Optional: single-region ggplot for quick checking
  p_ribbon <- out %>% filter(!is.na(band))
  p_lines  <- out %>% filter(!is.na(scenario_name))
  
  scen_styles <- make_scenario_style_maps(p_lines)
  band_styles <- make_band_fill_map(p_ribbon)
  
  p_ribbon <- p_ribbon %>%
    mutate(band = factor(band, levels = band_styles$band_levels))
  
  ggplot() +
    geom_ribbon(
      data = p_ribbon,
      aes(x = year, ymin = ribbon_ymin, ymax = ribbon_ymax, fill = band,
          group = interaction(GCAM_region_ID, demand_type, band))
    ) +
    geom_line(
      data = p_lines,
      aes(
        x = year,
        y = demand_value,
        color = scenario_name,
        linetype = scenario_name,
        group = interaction(GCAM_region_ID, demand_type, scenario_name, bound_id)
      ),
      linewidth = 0.8
    ) +
    scale_linetype_manual(values = scen_styles$lt_map) +
    scale_color_manual(values = scen_styles$color_map) +
    scale_fill_manual(name = "Ensemble", values = band_styles$fill_values) +
    facet_wrap(~ demand_type, scales = "free_y") +
    theme_minimal(base_size = 13)
}
