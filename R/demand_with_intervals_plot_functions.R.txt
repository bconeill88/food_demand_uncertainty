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
  scen_solid = NULL,  scen_solid_name  = NULL,
  scen_dashed = NULL, scen_dashed_name = NULL,
  scen_dotted = NULL, scen_dotted_name = NULL,
  scen_model = c("gcam","amb","both"),
  consumer_group = "FoodDemand_Group1",
  include_comparison_scenarios = TRUE,
  return_data = FALSE
) {
  scen_model <- match.arg(scen_model)

  # --- Ensemble (gray spaghetti) ---
  sampled_iterations <- demand_reg %>%
    distinct(iteration) %>% pull(iteration) %>% unique()
  sampled_iterations <- if (length(sampled_iterations) > 0) {
    sample(sampled_iterations, size = min(sample_n, length(sampled_iterations)))
  } else numeric(0)

  sampled_data <- demand_reg %>%
    filter(iteration %in% sampled_iterations) %>%
    select(iteration, year, Qs.region, Qn.region, Qtot.region) %>%
    tidyr::pivot_longer(c(Qs.region, Qn.region, Qtot.region),
                        names_to = "demand_type", values_to = "demand_value") %>%
    mutate(GCAM_region_ID = as.character(reg_num),
           scenario_name = ensemble_name,
           model = "ensemble",
           line_role = "solid",  # show spaghetti as solid (thin, gray)
           demand_type = factor(demand_type,
                                levels = c("Qs.region","Qn.region","Qtot.region")))

  # --- Helper to reshape one scenario set for a specific model ---
  reshape_scenario <- function(scen_df, label, model_tag, role) {
    if (is.null(scen_df) || is.null(label)) return(tibble())
    scen_df %>%
      filter(`gcam-consumer` == consumer_group, GCAM_region_ID == reg_num) %>%
      select(year,
             !!sym(paste0("Qs.region_",  model_tag)),
             !!sym(paste0("Qn.region_",  model_tag)),
             !!sym(paste0("Qtot.region_",model_tag))) %>%
      rename(Qs.region  = !!sym(paste0("Qs.region_",  model_tag)),
             Qn.region  = !!sym(paste0("Qn.region_",  model_tag)),
             Qtot.region= !!sym(paste0("Qtot.region_",model_tag))) %>%
      tidyr::pivot_longer(c(Qs.region, Qn.region, Qtot.region),
                          names_to = "demand_type", values_to = "demand_value") %>%
      mutate(scenario_name = label,
             GCAM_region_ID = as.character(reg_num),
             model = model_tag,
             line_role = role,
             iteration = NA_integer_,
             demand_type = factor(demand_type,
                                  levels = c("Qs.region","Qn.region","Qtot.region")))
  }

  # Label helper when duplicating GCAM names for Ambrosia in "both"
  amb_name <- function(x) if (is.null(x)) NULL else sub("(?i)gcam","Ambrosia", x)

  comparison_data <- tibble()
  if (include_comparison_scenarios) {
    if (scen_model %in% c("gcam","both")) {
      comparison_data <- bind_rows(
        comparison_data,
        reshape_scenario(scen_solid,  scen_solid_name,  "gcam", "solid"),
        reshape_scenario(scen_dashed, scen_dashed_name, "gcam", "dashed"),
        reshape_scenario(scen_dotted, scen_dotted_name, "gcam", "dotted")
      )
    }
    if (scen_model %in% c("amb","both")) {
      comparison_data <- bind_rows(
        comparison_data,
        reshape_scenario(scen_solid,  if (scen_model=="both") amb_name(scen_solid_name)  else scen_solid_name,  "amb", "solid"),
        reshape_scenario(scen_dashed, if (scen_model=="both") amb_name(scen_dashed_name) else scen_dashed_name, "amb", "dashed"),
        reshape_scenario(scen_dotted, if (scen_model=="both") amb_name(scen_dotted_name) else scen_dotted_name, "amb", "dotted")
      )
    }
  }

  out <- bind_rows(sampled_data, comparison_data)

  if (return_data) return(out)

  # If plotting a single region directly (rare in your workflow), keep styles consistent
  scen_levels <- unique(out$scenario_name)
  lt_map <- setNames(rep("solid", length(scen_levels)), scen_levels)
  # assign from line_role per scenario_name (where available)
  lr <- out %>% filter(!is.na(line_role)) %>%
    distinct(scenario_name, line_role)
  lt_map[lr$scenario_name] <- lr$line_role

  color_map <- setNames(rep("#e6550d", length(scen_levels)), scen_levels) # default GCAM orange
  for (s in names(color_map)) {
    if (identical(s, ensemble_name)) { color_map[s] <- "gray60"; next }
    if (any(out$model[out$scenario_name==s] == "amb")) color_map[s] <- "#3182bd" # amb blue
    if (any(out$model[out$scenario_name==s] == "gcam")) color_map[s] <- "#e6550d" # gcam orange
  }

  ggplot(out, aes(year, demand_value,
                  color = scenario_name, linetype = scenario_name)) +
    geom_line(data = out %>% filter(scenario_name == ensemble_name),
              aes(group = interaction(iteration, demand_type)),
              alpha = 0.6, linewidth = 0.3) +
    geom_line(data = out %>% filter(scenario_name != ensemble_name),
              linewidth = 0.8) +
    facet_wrap(~demand_type, scales = "free_y") +
    scale_linetype_manual(values = lt_map) +
    scale_color_manual(values = color_map) +
    labs(x = "Year", y = "Demand", color = "Scenario", linetype = "Scenario") +
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
  ensemble_name = "Emulator"
) {
  # Join region names and keep factor order for demand_type
  p_all <- p_all %>%
    mutate(GCAM_region_ID = as.character(GCAM_region_ID)) %>%
    left_join(region_mapping %>% mutate(GCAM_region_ID = as.character(GCAM_region_ID)),
              by = "GCAM_region_ID") %>%
    mutate(region_label = if_else(is.na(region), GCAM_region_ID, region)) %>%
    select(-region) %>%
    mutate(demand_type = factor(demand_type, levels = c("Qs.region","Qn.region","Qtot.region")))

  # Build linetype map from line_role carried in the data
  scen_levels <- sort(unique(p_all$scenario_name))
  lt_map <- setNames(rep("solid", length(scen_levels)), scen_levels)
  lr_tbl <- p_all %>% filter(!is.na(line_role)) %>%
    distinct(scenario_name, line_role)
  lt_map[lr_tbl$scenario_name] <- lr_tbl$line_role
  # Ensure ensemble is solid
  if (ensemble_name %in% names(lt_map)) lt_map[ensemble_name] <- "solid"

  # Colors: ensemble gray; GCAM orange; Ambrosia blue
  color_map <- setNames(rep("#e6550d", length(scen_levels)), scen_levels)
  for (s in names(color_map)) {
    if (identical(s, ensemble_name)) { color_map[s] <- "gray60"; next }
    # infer model from rows (robust to "both")
    if (any(p_all$model[p_all$scenario_name==s] == "amb")) color_map[s] <- "#3182bd"
    if (any(p_all$model[p_all$scenario_name==s] == "gcam")) color_map[s] <- "#e6550d"
  }

  # Base plot (full data); we’ll paginate with ggforce::n_pages()
  base <- ggplot(p_all, aes(x = year, y = demand_value,
                            color = scenario_name, linetype = scenario_name)) +
    # Ensemble spaghetti (use iteration grouping)
    geom_line(
      data = p_all %>% filter(scenario_name == ensemble_name),
      aes(group = interaction(region_label, demand_type, iteration)),
      alpha = 0.6, linewidth = 0.3
    ) +
    # Scenario lines (group by scenario per panel)
    geom_line(
      data = p_all %>% filter(scenario_name != ensemble_name),
      aes(group = interaction(region_label, demand_type, scenario_name)),
      linewidth = 0.8
    ) +
    scale_linetype_manual(values = lt_map) +
    scale_color_manual(values = color_map) +
    labs(x = "Year", y = "Demand", color = "Scenario", linetype = "Scenario") +
    theme_minimal(base_size = 13)

  # Determine how many pages we need and print them all
  tmp <- base + ggforce::facet_wrap_paginate(
    ~ region_label + demand_type,
    ncol = cols_per_page, nrow = rows_per_page,
    scales = "free_y", page = 1
  )
  n_pg <- ggforce::n_pages(tmp)

  pdf(file.path(output_dir, filename), width = 8.5, height = 11)
  for (pg in seq_len(n_pg)) {
    p <- base + ggforce::facet_wrap_paginate(
      ~ region_label + demand_type,
      ncol = cols_per_page, nrow = rows_per_page,
      scales = "free_y", page = pg
    )
    print(p)
  }
  dev.off()
}

# Decile-based comparison plot
plot_decile_demand_comparison <- function(freq_table, global_iter_row,
                                          demand_reg, region, consumer_group,
                                          sample_n = 100, return_data = FALSE) {
  demand_reg_grp <- demand_reg %>%
    filter(`gcam-consumer` == consumer_group)

  sampled_iterations <- demand_reg_grp %>%
    distinct(iteration) %>%
    pull(iteration) %>%
    unique() %>%
    sample(size = min(sample_n, length(.)))

  sampled_data <- demand_reg_grp %>%
    filter(iteration %in% sampled_iterations) %>%
    select(iteration, year, `gcam-consumer`, Qs, Qn, Qtot) %>%
    pivot_longer(cols = c(Qs, Qn, Qtot),
                 names_to = "demand_type", values_to = "demand_value") %>%
    mutate(GCAM_region_ID = as.character(region))

  regional_compare <- demand_reg_grp %>%
    filter(iteration %in% c(freq_table$HD_Qtot, freq_table$LD_Qtot)) %>%
    mutate(
      measure = if_else(iteration == freq_table$HD_Qtot, "HD_Qtot", "LD_Qtot"),
      scenario_source = "regional"
    )

  global_compare <- demand_reg_grp %>%
    filter(iteration %in% c(global_iter_row$HD_Qtot, global_iter_row$LD_Qtot)) %>%
    mutate(
      measure = if_else(iteration == global_iter_row$HD_Qtot, "HD_Qtot", "LD_Qtot"),
      scenario_source = "global"
    )

  demand_compare <- bind_rows(regional_compare, global_compare) %>%
    select(iteration, year, `gcam-consumer`, Qs, Qn, Qtot, measure, scenario_source) %>%
    pivot_longer(cols = c(Qs, Qn, Qtot),
                 names_to = "demand_type", values_to = "demand_value") %>%
    mutate(GCAM_region_ID = as.character(region))

  if (return_data) {
    return(bind_rows(
      sampled_data %>% mutate(scenario_source = "sample", measure = NA),
      demand_compare
    ))
  }

  ggplot() +
    geom_line(data = sampled_data, aes(x = year, y = demand_value, group = interaction(iteration, demand_type)),
              color = "gray85", size = 0.4, alpha = 0.5) +
    geom_line(data = demand_compare, aes(x = year, y = demand_value, color = measure, linetype = scenario_source),
              size = 1.2) +
    facet_wrap(~ demand_type, scales = "free_y") +
    scale_color_manual(values = c("HD_Qtot" = "#1b9e77", "LD_Qtot" = "#d95f02")) +
    labs(x = "Year", y = "Demand", color = "Model", linetype = "Scenario") +
    theme_minimal()
}

# Decile-based PDF
plot_decile_demand_comparison_pdf <- function(p_all, region_mapping, output_dir,
                                              cols_per_page = 3, rows_per_page = 5,
                                              filename = "demand_by_decile.pdf") {
  region_mapping <- region_mapping %>%
    mutate(GCAM_region_ID = as.character(GCAM_region_ID))

  p_all <- p_all %>%
    mutate(GCAM_region_ID = as.character(GCAM_region_ID)) %>%
    left_join(region_mapping, by = "GCAM_region_ID") %>%
    mutate(region_label = if_else(is.na(region), GCAM_region_ID, region)) %>%
    select(-region)

  p_all <- p_all %>%
    mutate(
      region_label = factor(region_label),
      consumer_group = factor(consumer_group,
                              levels = c("FoodDemand_Group1", "FoodDemand_Group2", "FoodDemand_Group3",
                                         "FoodDemand_Group6", "FoodDemand_Group10")),
      demand_type = factor(demand_type, levels = c("Qs", "Qn", "Qtot")),
      scenario_source = factor(scenario_source, levels = c("sample", "regional", "global"))
    )

  region_levels <- sort(unique(p_all$region_label))
  p_all <- p_all %>% mutate(region_label = factor(region_label, levels = region_levels))

  pdf(file.path(output_dir, filename), width = 8.5, height = 11)

  for (region in levels(p_all$region_label)) {
    plot_data <- p_all %>% filter(region_label == region)

    p <- ggplot(plot_data, aes(x = year, y = demand_value)) +
      geom_line(
        data = filter(plot_data, scenario_source == "sample"),
        aes(group = interaction(iteration, demand_type)),
        color = "gray80", size = 0.4
      ) +
      geom_line(
        data = filter(plot_data, scenario_source %in% c("regional", "global")),
        aes(color = measure, linetype = scenario_source),
        size = 1.2
      ) +
      facet_wrap(~ consumer_group + demand_type,
                 ncol = cols_per_page, nrow = rows_per_page,
                 scales = "free_y") +
      scale_color_manual(values = c("HD_Qtot" = "#1b9e77", "LD_Qtot" = "#d95f02")) +
      scale_linetype_manual(values = c("regional" = "dashed", "global" = "solid")) +
      labs(x = "Year", y = "Demand", color = "Model", linetype = "Scenario",
           title = paste("Demand Projections by Decile:", region)) +
      theme_minimal(base_size = 13)

    print(p)
  }

  dev.off()
}
