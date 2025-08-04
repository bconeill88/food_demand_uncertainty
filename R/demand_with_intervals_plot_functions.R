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
    scen_solid = NULL, scen_solid_name = NULL,
    scen_dashed = NULL, scen_dashed_name = NULL,
    scen_dotted = NULL, scen_dotted_name = NULL,
    scen_model = NULL, consumer_group = "FoodDemand_Group1",
    include_comparison_scenarios = TRUE,
    return_data = FALSE
) {
  
  # Sample iterations
  sampled_iterations <- demand_reg %>%
    distinct(iteration) %>%
    pull(iteration) %>%
    unique() %>%
    sample(size = min(sample_n, length(.)))
  
  # Sampled ensemble data
  sampled_data <- demand_reg %>%
    filter(iteration %in% sampled_iterations) %>%
    select(iteration, year, Qs.region, Qn.region, Qtot.region) %>%
    pivot_longer(cols = c(Qs.region, Qn.region, Qtot.region),
                 names_to = "demand_type", values_to = "demand_value") %>%
    mutate(GCAM_region_ID = as.character(reg_num),
           scenario_name = ensemble_name,
           demand_type = factor(demand_type,
                                levels = c("Qs.region", "Qn.region", "Qtot.region")))
  
  comparison_data <- tibble()
  
  if (include_comparison_scenarios) {
    reshape_scenario <- function(scen_data, linetype_label, model) {
      scen_data %>% 
        filter(`gcam-consumer` == consumer_group, GCAM_region_ID == reg_num) %>%
        select(year,
               !!sym(paste0("Qs.region_", model)),
               !!sym(paste0("Qn.region_", model)),
               !!sym(paste0("Qtot.region_", model))) %>%
        rename(Qs.region = !!sym(paste0("Qs.region_", model)),
               Qn.region = !!sym(paste0("Qn.region_", model)),
               Qtot.region = !!sym(paste0("Qtot.region_", model))) %>%
        pivot_longer(cols = c(Qs.region, Qn.region, Qtot.region),
                     names_to = "demand_type", values_to = "demand_value") %>%
        mutate(scenario_name = linetype_label,
               GCAM_region_ID = as.character(reg_num),
               demand_type = factor(demand_type,
                                    levels = c("Qs.region", "Qn.region", "Qtot.region")))
    }
    
    comparison_data <- bind_rows(
      reshape_scenario(scen_solid,  scen_solid_name,  scen_model),
      reshape_scenario(scen_dashed, scen_dashed_name, scen_model),
      reshape_scenario(scen_dotted, scen_dotted_name, scen_model)
    )
  }
  
  if (return_data) {
    return(bind_rows(
      sampled_data %>% mutate(line_type = "sample"),
      comparison_data
    ))
  }
  
  # Construct manual linetypes
  linetype_names <- c()
  linetype_values <- c()
  if (include_comparison_scenarios) {
    linetype_names <- c(scen_solid_name, scen_dashed_name, scen_dotted_name)
    linetype_values <- c("solid", "dashed", "dotted")
  }
  linetype_names <- c(linetype_names, ensemble_name)
  linetype_values <- c(linetype_values, "solid")
  manual_linetypes <- setNames(linetype_values, linetype_names)
  
  # Plot
  p <- ggplot() +
    geom_line(data = sampled_data,
              aes(x = year, y = demand_value,
                  group = interaction(iteration, demand_type),
                  linetype = scenario_name),
              color = "gray60", size = 0.4, alpha = 0.6)
  
  if (include_comparison_scenarios) {
    p <- p + geom_line(data = comparison_data,
                       aes(x = year, y = demand_value,
                           linetype = scenario_name, group = scenario_name),
                       color = "#e6550d", size = 1.2)
  }
  
  p +
    facet_wrap(~demand_type, scales = "free_y") +
    scale_linetype_manual(values = manual_linetypes) +
    labs(x = "Year", y = "Demand", linetype = "Scenario") +
    theme_minimal()
}

# original version, plotted ensemble and high/low scenarios using both global
# parameters and regionally-specific parameters
# plot_regional_demand_comparison <- function(freq_table, global_iter_row,
#                                             demand_reg, region, sample_n = 100,
#                                             return_data = FALSE) {
#   sampled_iterations <- demand_reg %>%
#     distinct(iteration) %>%
#     pull(iteration) %>%
#     unique() %>%
#     sample(size = min(sample_n, length(.)))
# 
#   sampled_data <- demand_reg %>%
#     filter(iteration %in% sampled_iterations) %>%
#     select(iteration, year, Qs.region, Qn.region, Qtot.region) %>%
#     pivot_longer(cols = c(Qs.region, Qn.region, Qtot.region),
#                  names_to = "demand_type", values_to = "demand_value") %>%
#     mutate(GCAM_region_ID = as.character(region))
# 
#   regional_compare <- demand_reg %>%
#     filter(iteration %in% c(freq_table$HD_Qtot, freq_table$LD_Qtot)) %>%
#     mutate(
#       measure = if_else(iteration == freq_table$HD_Qtot, "HD_Qtot", "LD_Qtot"),
#       source = "regional"
#     )
# 
#   global_compare <- demand_reg %>%
#     filter(iteration %in% c(global_iter_row$HD_Qtot, global_iter_row$LD_Qtot)) %>%
#     mutate(
#       measure = if_else(iteration == global_iter_row$HD_Qtot, "HD_Qtot", "LD_Qtot"),
#       source = "global"
#     )
# 
#   demand_compare <- bind_rows(regional_compare, global_compare) %>%
#     select(iteration, year, Qs.region, Qn.region, Qtot.region, measure, source) %>%
#     pivot_longer(cols = c(Qs.region, Qn.region, Qtot.region),
#                  names_to = "demand_type", values_to = "demand_value") %>%
#     mutate(GCAM_region_ID = as.character(region))
# 
#   if (return_data) {
#     return(bind_rows(
#       sampled_data %>% mutate(source = "sample", measure = NA),
#       demand_compare
#     ))
#   }
# 
#   ggplot() +
#     geom_line(data = sampled_data, aes(x = year, y = demand_value, group = interaction(iteration, demand_type)),
#               color = "gray85", size = 0.4, alpha = 0.5) +
#     geom_line(data = demand_compare, aes(x = year, y = demand_value, color = measure, linetype = source),
#               size = 1.2) +
#     facet_wrap(~ demand_type, scales = "free_y") +
#     scale_color_manual(values = c("HD_Qtot" = "#1b9e77", "LD_Qtot" = "#d95f02")) +
#     labs(x = "Year", y = "Demand", color = "Measure", linetype = "Source") +
#     theme_minimal()
# }

# Regional demand: multi-page PDF; takes data frame of plots produced by 
# plot_regional_demand_comparison and combines them into a single multi-region pdf,
# with each region labeled and a single legend per page. Currently legend is correct
# except all legend keys have thin lines, when the GCAM scenarios should have thick
# lines
plot_regional_demand_comparison_pdf <- function(p_all, region_mapping, output_dir,
                                                cols_per_page = 3, rows_per_page = 4,
                                                filename = "demand_all_regions_ens_bc.pdf",
                                                ensemble_name = "Emulator") {
  panels_per_page <- cols_per_page * rows_per_page

  # Prepare region labels and demand type ordering
  p_all <- p_all %>%
    mutate(GCAM_region_ID = as.character(GCAM_region_ID)) %>%
    left_join(region_mapping %>% mutate(GCAM_region_ID = as.character(GCAM_region_ID)), by = "GCAM_region_ID") %>%
    mutate(region_label = if_else(is.na(region), GCAM_region_ID, region)) %>%
    select(-region) %>%
    mutate(
      region_label = factor(region_label),
      demand_type = factor(demand_type, levels = c("Qs.region", "Qn.region", "Qtot.region"))
    )

  # Create grid of region × demand_type panels
  actual_regions <- unique(p_all$region_label[!grepl("^blank", p_all$region_label)])
  dummy_regions <- unique(p_all$region_label[grepl("^blank", p_all$region_label)])
  region_levels <- c(actual_regions, dummy_regions)

  panel_keys <- expand_grid(
    region_label = actual_regions,
    demand_type = levels(p_all$demand_type)
  )

  # Add dummy panels to pad last page if needed
  remainder <- nrow(panel_keys) %% panels_per_page
  if (remainder > 0) {
    n_dummy_panels <- panels_per_page - remainder
    dummy_rows <- expand_grid(
      region_label = paste0("blank", seq_len(ceiling(n_dummy_panels / 3))),
      demand_type = levels(p_all$demand_type)
    ) %>%
      head(n_dummy_panels)
    panel_keys <- bind_rows(panel_keys, dummy_rows)
  }

  panel_keys <- panel_keys %>%
    mutate(page = ceiling(row_number() / panels_per_page))

  # Open PDF
  pdf(file.path(output_dir, filename), width = 8.5, height = 11)

  for (pg in sort(unique(panel_keys$page))) {
    combos <- panel_keys %>% filter(page == pg)

    plot_data <- p_all %>%
      semi_join(combos, by = c("region_label", "demand_type"))

    # Create linetype mapping dynamically from scenario names
    linetype_levels <- unique(plot_data$scenario_name)
    manual_linetypes <- setNames(
      c("solid", "dashed", "dotted", "solid")[seq_along(linetype_levels)],
      sort(linetype_levels)
    )

    p <- ggplot(plot_data, aes(x = year, y = demand_value,
                           linetype = scenario_name,
                           color = scenario_name,
                           size = scenario_name)) +
  # Emulator (ensemble) layer
  geom_line(
    data = filter(plot_data, scenario_name == ensemble_name),
    aes(group = interaction(iteration, demand_type)),
    alpha = 0.6
  ) +
  # GCAM scenarios
  geom_line(
    data = filter(plot_data, scenario_name != ensemble_name),
    aes(group = scenario_name)
  ) +
  ggforce::facet_wrap_paginate(
    ~ region_label + demand_type,
    ncol = cols_per_page, nrow = rows_per_page,
    page = 1,
    scales = "free_y"
  ) +
  # Manual mappings
  scale_linetype_manual(values = manual_linetypes) +
  scale_color_manual(values = c(
    Emulator = "gray60",
    `GCAM HD` = "#e6550d",
    `GCAM LD` = "#e6550d",
    `GCAM ML` = "#e6550d"
  )) +
  scale_size_manual(values = c(
    Emulator = 0.4,
    `GCAM HD` = 1.2,
    `GCAM LD` = 1.2,
    `GCAM ML` = 1.2
  )) +
  labs(x = "Year", y = "Demand", linetype = "Scenario",
       color = "Scenario", size = "Scenario") +
  guides(size = "none") +
  theme_minimal(base_size = 13)

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
