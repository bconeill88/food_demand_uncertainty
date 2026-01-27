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

# Helper: build ensemble uncertainty ribbons (range + central CI, or CI-only)
build_ensemble_ribbon_data <- function(
    df,
    reg_num,
    demand_cols,
    demand_levels,
    ci_level = 0.90,
    year_min = 2015,
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
    year_min = 2015,
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

# Helper: 
make_scenario_style_maps <- function(p_lines,
                                     set1_col = "#e6550d",
                                     set2_col = "#3182bd") {
  
  scen_levels <- sort(unique(p_lines$scenario_name))
  scen_levels <- scen_levels[!is.na(scen_levels)]
  
  # linetypes (default solid; override from line_role)
  lt_map <- setNames(rep("solid", length(scen_levels)), scen_levels)
  lr_tbl <- p_lines %>%
    filter(!is.na(line_role)) %>%
    distinct(scenario_name, line_role)
  if (nrow(lr_tbl) > 0) {
    lt_map[lr_tbl$scenario_name] <- lr_tbl$line_role
  }
  
  # colors (default set1; override for set2)
  color_map <- setNames(rep(set1_col, length(scen_levels)), scen_levels)
  model_by_scen <- p_lines %>%
    filter(!is.na(scenario_name), !is.na(model)) %>%
    group_by(scenario_name) %>%
    summarise(model = first(model), .groups = "drop")
  
  set2_names <- model_by_scen$scenario_name[model_by_scen$model == "set2"]
  if (length(set2_names) > 0) {
    color_map[intersect(names(color_map), set2_names)] <- set2_col
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
    dplyr::filter(.data$year == target_year)
  
  # optional filter for consumer group if column exists
  if (consumer_col %in% names(out)) {
    out <- out %>% dplyr::filter(.data[[consumer_col]] == consumer_value)
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
    dplyr::group_by(.data[[region_col]]) %>%
    dplyr::summarise(
      n_draws = dplyr::n(),
      med = stats::quantile(.data[[value_col]], probs = 0.5, na.rm = TRUE, names = FALSE),
      lo  = stats::quantile(.data[[value_col]], probs = alpha, na.rm = TRUE, names = FALSE),
      hi  = stats::quantile(.data[[value_col]], probs = 1 - alpha, na.rm = TRUE, names = FALSE),
      ymin = if (include_range) min(.data[[value_col]], na.rm = TRUE) else NA_real_,
      ymax = if (include_range) max(.data[[value_col]], na.rm = TRUE) else NA_real_,
      .groups = "drop"
    ) %>%
    dplyr::rename(GCAM_region_ID = .data[[region_col]]) %>%
    dplyr::mutate(
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
  
  stopifnot(ci_level > 0, ci_level <= 1)
  
  # --- Ensemble ribbons (range + central CI), years >= 2015 ---
  ribbon_res <- build_ensemble_ribbon_data(
    df            = demand_reg,
    reg_num       = reg_num,
    demand_cols   = c("Qs.region", "Qn.region", "Qtot.region"),
    demand_levels = c("Qs.region", "Qn.region", "Qtot.region"),
    ci_level      = ci_level,
    year_min      = 2015,
    consumer_col  = "gcam-consumer",
    consumer_value = "FoodDemand_Group1",
    region_col    = "GCAM_region_ID"
  )
  
  ribbon_data <- ribbon_res$ribbon_data
  ci_label    <- ribbon_res$ci_label
  
  # --- Build comparison scenario data for any non-NULL inputs ---
  comparison_data <- bind_rows(
    reshape_scenario_long(scen_solid_1,  scen_solid_name_1,  "set1", "solid",
                          reg_num = reg_num, year_min = 2015,
                          demand_cols = c("Qs.region", "Qn.region", "Qtot.region"),
                          demand_levels = c("Qs.region", "Qn.region", "Qtot.region"),
                          consumer_group = "FoodDemand_Group1"),
    
    reshape_scenario_long(scen_dashed_1, scen_dashed_name_1, "set1", "dashed",
                          reg_num = reg_num, year_min = 2015,
                          demand_cols = c("Qs.region", "Qn.region", "Qtot.region"),
                          demand_levels = c("Qs.region", "Qn.region", "Qtot.region"),
                          consumer_group = "FoodDemand_Group1"),
    
    reshape_scenario_long(scen_dotted_1, scen_dotted_name_1, "set1", "dotted",
                          reg_num = reg_num, year_min = 2015,
                          demand_cols = c("Qs.region", "Qn.region", "Qtot.region"),
                          demand_levels = c("Qs.region", "Qn.region", "Qtot.region"),
                          consumer_group = "FoodDemand_Group1"),
    
    reshape_scenario_long(scen_solid_2,  scen_solid_name_2,  "set2", "solid",
                          reg_num = reg_num, year_min = 2015,
                          demand_cols = c("Qs.region", "Qn.region", "Qtot.region"),
                          demand_levels = c("Qs.region", "Qn.region", "Qtot.region"),
                          consumer_group = "FoodDemand_Group1"),
    
    reshape_scenario_long(scen_dashed_2, scen_dashed_name_2, "set2", "dashed",
                          reg_num = reg_num, year_min = 2015,
                          demand_cols = c("Qs.region", "Qn.region", "Qtot.region"),
                          demand_levels = c("Qs.region", "Qn.region", "Qtot.region"),
                          consumer_group = "FoodDemand_Group1"),
    
    reshape_scenario_long(scen_dotted_2, scen_dotted_name_2, "set2", "dotted",
                          reg_num = reg_num, year_min = 2015,
                          demand_cols = c("Qs.region", "Qn.region", "Qtot.region"),
                          demand_levels = c("Qs.region", "Qn.region", "Qtot.region"),
                          consumer_group = "FoodDemand_Group1")
  )
  
  if (is.null(comparison_data)) comparison_data <- tibble()
  
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
    color_override = NULL,
    linetype_override = NULL,
    ylimit_mode = c("by_type_global", "by_region")
) {
  
  ylimit_mode <- match.arg(ylimit_mode)
  
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
  
  # Split into ribbons vs lines
  p_ribbon <- p_all %>% filter(!is.na(band))
  p_lines  <- p_all %>% filter(!is.na(scenario_name))
  
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
  if (!is.finite(min_year)) min_year <- 2015
  
  facet_tbl <- p_all %>% distinct(facet_id, demand_type, region_label)
  
  if (ylimit_mode == "by_type_global") {
    
    # Global limits by demand type group (your existing intent)
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
    
    blank_df <- facet_tbl %>%
      mutate(ymax = if_else(as.character(demand_type) == "Qtot.region", ymax_tot, ymax_sn)) %>%
      select(facet_id, ymax) %>%
      tidyr::uncount(weights = 2, .id = "k") %>%
      mutate(
        year = min_year,
        demand_value = if_else(k == 1, 0, ymax)
      ) %>%
      select(facet_id, year, demand_value)
    
  } else if (ylimit_mode == "by_region") {
    
    # Compute per-region min/max across ALL three demand types
    reg_limits <- p_all %>%
      group_by(region_label) %>%
      summarise(
        ymin = min(c(ribbon_ymin, demand_value), na.rm = TRUE),
        ymax = max(c(ribbon_ymax, demand_value), na.rm = TRUE),
        .groups = "drop"
      )
    
    # Apply the same [ymin, ymax] to every facet in that region
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
    # Force per-facet y-limits via invisible points
    geom_blank(
      data = blank_df,
      aes(x = year, y = demand_value, group = facet_id)
    ) +
    # CI ribbon (full ensemble)
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
    # Scenario + CI-boundary lines
    geom_line(
      data = p_lines,
      aes(
        x = year,
        y = demand_value,
        color = scenario_name,
        linetype = scenario_name,
        group = interaction(facet_id, scenario_name, bound_id)
      ),
      linewidth = 0.8
    ) +
    # Scales
    scale_linetype_manual(values = lt_map) +
    scale_color_manual(values = color_map) +
    scale_fill_manual(name = "Ensemble", values = fill_values, drop = FALSE) +
    # Legends
    guides(
      fill     = guide_legend(order = 1),
      color    = guide_legend(order = 2),
      linetype = guide_legend(order = 2)
    ) +
    # Labels + theme
    labs(
      x = "Year",
      y = "Demand difference vs ML (kcal/day)",
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
  
  stopifnot(ci_level > 0 && ci_level <= 1)
  
  required_cols <- c("GCAM_region_ID", "year", "iteration",
                     "gcam-consumer", "Qs", "Qn", "Qtot")
  missing_cols <- setdiff(required_cols, names(demand_reg))
  if (length(missing_cols) > 0) {
    stop("plot_decile_demand_comparison(): missing columns: ",
         paste(missing_cols, collapse = ", "))
  }
  
  # ---------------------------------------------------------------------------
  # Ensemble ribbons
  ribbon_res <- build_ensemble_ribbon_data(
    df             = demand_reg,
    reg_num        = reg_num,
    demand_cols    = c("Qs", "Qn", "Qtot"),
    demand_levels  = c("Qs", "Qn", "Qtot"),
    ci_level       = ci_level,
    year_min       = 2015,
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
    reshape_scenario_long(scen_solid_1,  scen_solid_name_1,  "set1", "solid",
                          reg_num = reg_num, year_min = 2015,
                          demand_cols = c("Qs", "Qn", "Qtot"),
                          demand_levels = c("Qs", "Qn", "Qtot"),
                          consumer_group = consumer_group),
    
    reshape_scenario_long(scen_dashed_1, scen_dashed_name_1, "set1", "dashed",
                          reg_num = reg_num, year_min = 2015,
                          demand_cols = c("Qs", "Qn", "Qtot"),
                          demand_levels = c("Qs", "Qn", "Qtot"),
                          consumer_group = consumer_group),
    
    reshape_scenario_long(scen_dotted_1, scen_dotted_name_1, "set1", "dotted",
                          reg_num = reg_num, year_min = 2015,
                          demand_cols = c("Qs", "Qn", "Qtot"),
                          demand_levels = c("Qs", "Qn", "Qtot"),
                          consumer_group = consumer_group),
    
    reshape_scenario_long(scen_solid_2,  scen_solid_name_2,  "set2", "solid",
                          reg_num = reg_num, year_min = 2015,
                          demand_cols = c("Qs", "Qn", "Qtot"),
                          demand_levels = c("Qs", "Qn", "Qtot"),
                          consumer_group = consumer_group),
    
    reshape_scenario_long(scen_dashed_2, scen_dashed_name_2, "set2", "dashed",
                          reg_num = reg_num, year_min = 2015,
                          demand_cols = c("Qs", "Qn", "Qtot"),
                          demand_levels = c("Qs", "Qn", "Qtot"),
                          consumer_group = consumer_group),
    
    reshape_scenario_long(scen_dotted_2, scen_dotted_name_2, "set2", "dotted",
                          reg_num = reg_num, year_min = 2015,
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
  
  # ---- Harmonized y-axis limits (global across all regions/deciles) ----
  ymax_sn <- p_all %>%
    dplyr::filter(demand_type %in% c("Qs", "Qn")) %>%
    dplyr::summarise(mx = max(c(ribbon_ymax, demand_value), na.rm = TRUE)) %>%
    dplyr::pull(mx) %>%
    nice_ymax(step = 0.25)
  
  ymax_tot <- p_all %>%
    dplyr::filter(demand_type %in% c("Qtot")) %>%
    dplyr::summarise(mx = max(c(ribbon_ymax, demand_value), na.rm = TRUE)) %>%
    dplyr::pull(mx) %>%
    nice_ymax(step = 0.25)
  
  # ---- Data to force facet y ranges via geom_blank() ----
  min_year <- suppressWarnings(min(p_all$year, na.rm = TRUE))
  if (!is.finite(min_year)) min_year <- 2015
  
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
    
    df_ribbon <- p_ribbon %>% dplyr::filter(region_label == reg)
    df_lines  <- p_lines  %>% dplyr::filter(region_label == reg)
    
    facet_tbl <- dplyr::bind_rows(
      df_ribbon %>% dplyr::select(facet_id, demand_type),
      df_lines  %>% dplyr::select(facet_id, demand_type)
    ) %>% dplyr::distinct()
    
    blank_df <- facet_tbl %>%
      dplyr::mutate(
        ymax = dplyr::if_else(as.character(demand_type) == "Qtot", ymax_tot, ymax_sn)
      ) %>%
      dplyr::select(facet_id, ymax) %>%
      tidyr::uncount(weights = 2, .id = "k") %>%
      dplyr::mutate(
        year = min_year,
        demand_value = dplyr::if_else(k == 1, 0, ymax)
      ) %>%
      dplyr::select(facet_id, year, demand_value)
    
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
    bar_df <- bar_df %>% dplyr::arrange(.data$med)
  } else {
    bar_df <- bar_df %>% dplyr::arrange(.data$GCAM_region_ID)
  }
  
  bar_df <- bar_df %>%
    dplyr::mutate(
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
    year_min = 2015,
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
  
  # Full-ensemble ribbon (CI only)
  ribbon_res <- build_ensemble_ribbon_data(
    df             = demand_full,
    reg_num        = reg_num,
    demand_cols    = demand_cols,
    demand_levels  = demand_levels,
    ci_level       = ci_level,
    year_min       = year_min,
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
    year_min      = year_min,
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
    year_min      = year_min,
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
    year_min      = year_min,
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
    year_min      = year_min,
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
