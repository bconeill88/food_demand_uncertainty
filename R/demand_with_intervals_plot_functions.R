# functions supporting the 08_plot_demand_with_intervals.R main script

# Regional demand: single-region plot or data
plot_regional_demand_comparison <- function(freq_table, global_iter_row,
                                            demand_reg, region, sample_n = 100,
                                            return_data = FALSE) {
  sampled_iterations <- demand_reg %>%
    distinct(iteration) %>%
    pull(iteration) %>%
    unique() %>%
    sample(size = min(sample_n, length(.)))

  sampled_data <- demand_reg %>%
    filter(iteration %in% sampled_iterations) %>%
    select(iteration, year, Qs.region, Qn.region, Qtot.region) %>%
    pivot_longer(cols = c(Qs.region, Qn.region, Qtot.region),
                 names_to = "demand_type", values_to = "demand_value") %>%
    mutate(GCAM_region_ID = as.character(region))

  regional_compare <- demand_reg %>%
    filter(iteration %in% c(freq_table$HD_Qtot, freq_table$LD_Qtot)) %>%
    mutate(
      measure = if_else(iteration == freq_table$HD_Qtot, "HD_Qtot", "LD_Qtot"),
      source = "regional"
    )

  global_compare <- demand_reg %>%
    filter(iteration %in% c(global_iter_row$HD_Qtot, global_iter_row$LD_Qtot)) %>%
    mutate(
      measure = if_else(iteration == global_iter_row$HD_Qtot, "HD_Qtot", "LD_Qtot"),
      source = "global"
    )

  demand_compare <- bind_rows(regional_compare, global_compare) %>%
    select(iteration, year, Qs.region, Qn.region, Qtot.region, measure, source) %>%
    pivot_longer(cols = c(Qs.region, Qn.region, Qtot.region),
                 names_to = "demand_type", values_to = "demand_value") %>%
    mutate(GCAM_region_ID = as.character(region))

  if (return_data) {
    return(bind_rows(
      sampled_data %>% mutate(source = "sample", measure = NA),
      demand_compare
    ))
  }

  ggplot() +
    geom_line(data = sampled_data, aes(x = year, y = demand_value, group = interaction(iteration, demand_type)),
              color = "gray85", size = 0.4, alpha = 0.5) +
    geom_line(data = demand_compare, aes(x = year, y = demand_value, color = measure, linetype = source),
              size = 1.2) +
    facet_wrap(~ demand_type, scales = "free_y") +
    scale_color_manual(values = c("HD_Qtot" = "#1b9e77", "LD_Qtot" = "#d95f02")) +
    labs(x = "Year", y = "Demand", color = "Measure", linetype = "Source") +
    theme_minimal()
}

# Regional demand: multi-page PDF
plot_regional_demand_comparison_pdf <- function(p_all, region_mapping, output_dir,
                                                cols_per_page = 3, rows_per_page = 4,
                                                filename = "demand_all_regions_ens_bc.pdf") {
  panels_per_page <- cols_per_page * rows_per_page

  p_all <- p_all %>%
    mutate(GCAM_region_ID = as.character(GCAM_region_ID)) %>%
    left_join(region_mapping %>% mutate(GCAM_region_ID = as.character(GCAM_region_ID)), by = "GCAM_region_ID") %>%
    mutate(region_label = if_else(is.na(region), GCAM_region_ID, region)) %>%
    select(-region)

  actual_regions <- unique(p_all$region_label[!grepl("^blank", p_all$region_label)])
  dummy_regions <- unique(p_all$region_label[grepl("^blank", p_all$region_label)])
  region_levels <- c(actual_regions, dummy_regions)

  p_all <- p_all %>%
    mutate(
      region_label = factor(region_label, levels = region_levels),
      demand_type = factor(demand_type, levels = c("Qs.region", "Qn.region", "Qtot.region")),
      source = factor(source, levels = c("sample", "global", "regional"))
    )

  panel_keys <- expand_grid(
    region_label = actual_regions,
    demand_type = levels(p_all$demand_type)
  )

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

  pdf(file.path(output_dir, filename), width = 8.5, height = 11)

  for (pg in sort(unique(panel_keys$page))) {
    combos <- panel_keys %>% filter(page == pg)

    plot_data <- p_all %>%
      semi_join(combos, by = c("region_label", "demand_type"))

    p <- ggplot(plot_data, aes(x = year, y = demand_value)) +
      geom_line(
        data = filter(plot_data, source == "sample"),
        aes(group = interaction(iteration, demand_type)),
        color = "gray80", size = 0.4
      ) +
      geom_line(
        data = filter(plot_data, source %in% c("global", "regional")),
        aes(color = measure, linetype = source),
        size = 1.2
      ) +
      ggforce::facet_wrap_paginate(
        ~ region_label + demand_type,
        ncol = cols_per_page, nrow = rows_per_page,
        page = 1,
        scales = "free_y"
      ) +
      scale_color_manual(values = c("HD_Qtot" = "#1b9e77", "LD_Qtot" = "#d95f02")) +
      scale_linetype_manual(values = c("global" = "solid", "regional" = "dashed")) +
      labs(x = "Year", y = "Demand", color = "Measure", linetype = "Source") +
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
      source = "regional"
    )

  global_compare <- demand_reg_grp %>%
    filter(iteration %in% c(global_iter_row$HD_Qtot, global_iter_row$LD_Qtot)) %>%
    mutate(
      measure = if_else(iteration == global_iter_row$HD_Qtot, "HD_Qtot", "LD_Qtot"),
      source = "global"
    )

  demand_compare <- bind_rows(regional_compare, global_compare) %>%
    select(iteration, year, `gcam-consumer`, Qs, Qn, Qtot, measure, source) %>%
    pivot_longer(cols = c(Qs, Qn, Qtot),
                 names_to = "demand_type", values_to = "demand_value") %>%
    mutate(GCAM_region_ID = as.character(region))

  if (return_data) {
    return(bind_rows(
      sampled_data %>% mutate(source = "sample", measure = NA),
      demand_compare
    ))
  }

  ggplot() +
    geom_line(data = sampled_data, aes(x = year, y = demand_value, group = interaction(iteration, demand_type)),
              color = "gray85", size = 0.4, alpha = 0.5) +
    geom_line(data = demand_compare, aes(x = year, y = demand_value, color = measure, linetype = source),
              size = 1.2) +
    facet_wrap(~ demand_type, scales = "free_y") +
    scale_color_manual(values = c("HD_Qtot" = "#1b9e77", "LD_Qtot" = "#d95f02")) +
    labs(x = "Year", y = "Demand", color = "Measure", linetype = "Source") +
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
      source = factor(source, levels = c("sample", "regional", "global"))
    )

  region_levels <- sort(unique(p_all$region_label))
  p_all <- p_all %>% mutate(region_label = factor(region_label, levels = region_levels))

  pdf(file.path(output_dir, filename), width = 8.5, height = 11)

  for (region in levels(p_all$region_label)) {
    plot_data <- p_all %>% filter(region_label == region)

    p <- ggplot(plot_data, aes(x = year, y = demand_value)) +
      geom_line(
        data = filter(plot_data, source == "sample"),
        aes(group = interaction(iteration, demand_type)),
        color = "gray80", size = 0.4
      ) +
      geom_line(
        data = filter(plot_data, source %in% c("regional", "global")),
        aes(color = measure, linetype = source),
        size = 1.2
      ) +
      facet_wrap(~ consumer_group + demand_type,
                 ncol = cols_per_page, nrow = rows_per_page,
                 scales = "free_y") +
      scale_color_manual(values = c("HD_Qtot" = "#1b9e77", "LD_Qtot" = "#d95f02")) +
      scale_linetype_manual(values = c("regional" = "dashed", "global" = "solid")) +
      labs(x = "Year", y = "Demand", color = "Measure", linetype = "Source",
           title = paste("Demand Projections by Decile:", region)) +
      theme_minimal(base_size = 13)

    print(p)
  }

  dev.off()
}
