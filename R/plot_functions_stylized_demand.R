# ------------------------------------------------------------------------------
# Stylized demand plotting functions
# ------------------------------------------------------------------------------
#
# Helper functions for creating stylized demand and elasticity plots over income.
# These functions are used by plot_stylized_demand.R to compare this study's
# maximum-likelihood demand parameters with the 2017 and 2021 parameter sets.
#
# Functions included:
#   .require_plot_pkgs()
#     Check that packages required by the plotting functions are installed.
#
#   .study_style_maps()
#     Return labels and linetypes used to distinguish the 2026, 2017, and 2021
#     parameter sets.
#
#   .label_x_target()
#     Choose a target x-location for direct curve labels.
#
#   .make_label_df()
#     Build a label-position data frame for direct curve labels. Currently not
#     called by the plotting functions, but retained as a small labeling utility.
#
#   .build_demand_long()
#     Convert stylized staples and non-staples demand output from wide to long
#     format for plotting.
#
#   .build_elast_long()
#     Convert price and income elasticity output from wide to long format for
#     plotting.
#
#   plot_demand_vs_income()
#     Plot stylized staples and non-staples demand as functions of income, either
#     for this study alone or compared with the 2017 and 2021 parameter sets.
#
#   plot_elasticities_vs_income()
#     Plot stylized price and income elasticities as functions of income, either
#     for this study alone or compared with the 2017 and 2021 parameter sets.
#
#   make_demand_vs_income_pdf()
#     Save a two-panel PDF containing the demand-vs-income plot for this study
#     alone and the comparison across all three parameter sets.
#
#   make_elasticities_vs_income_pdf()
#     Save a two-panel PDF containing the elasticity-vs-income plot for this
#     study alone and the comparison across all three parameter sets.
# ------------------------------------------------------------------------------

# ==============================================================================
# Package and style helpers
# ==============================================================================

# Check that required plotting packages are installed.
.require_plot_pkgs <- function() {
  
  # Check required packages are available
  pkgs <- c("ggplot2","dplyr","tidyr","geomtextpath")
  
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  
  if(length(missing) > 0) {
    stop(
      "Missing packages: ",
      paste(missing, collapse = ", "),
      "\nInstall them first."
    )
  }
}

# Return labels and linetypes for the parameter-set comparison.
.study_style_maps <- function() {
  
  # Map study years to labels and line types
  lbl <- c("2026" = "This study",
           "2017" = "2017",
           "2021" = "2021")
  
  lty <- c("2026" = "solid",
           "2017" = "dashed",
           "2021" = "dotted")
  
  list(label_map = lbl, linetype_map = lty)
}

# ==============================================================================
# Label helpers
# ==============================================================================

# Choose an x-position for direct curve labels.
.label_x_target <- function(y_vec) {
  
  # Choose x-location for labels near right side of plot
  quantile(y_vec, probs = 0.85, na.rm = TRUE, names = FALSE)
}

# Build a data frame for direct curve labels. Currently retained but not called.
.make_label_df <- function(df,
                           x_col = "Y",
                           y_col = "value",
                           group_col = "series",
                           label_col = "series_label") {
  
  # Create one row per curve for placing labels
  
  x_tgt <- .label_x_target(df[[x_col]])
  
  df %>%
    group_by(.data[[group_col]]) %>%
    slice_min(order_by = abs(.data[[x_col]] - x_tgt),
              n = 1,
              with_ties = FALSE) %>%
    ungroup() %>%
    mutate(label_text = .data[[label_col]])
}

# ==============================================================================
# Data reshaping helpers
# ==============================================================================

# Convert stylized demand output to long format for plotting.
.build_demand_long <- function(df, study_key) {
  
  # Convert wide demand data to long format for plotting
  
  df %>%
    select(Y, Qs, Qn) %>%
    pivot_longer(cols = c(Qs, Qn),
                 names_to = "series",
                 values_to = "value") %>%
    mutate(
      study = study_key,
      series = factor(series, levels = c("Qs","Qn")),
      series_label = case_when(
        series == "Qs" ~ "Staples",
        series == "Qn" ~ "Non-staples",
        TRUE ~ as.character(series)
      )
    )
}

# Convert stylized elasticity output to long format for plotting.
.build_elast_long <- function(df, study_key) {
  
  price_vars  <- c("elast.ss","elast.nn","elast.sn","elast.ns")
  income_vars <- c("eta.s","eta.n")
  
  missing <- setdiff(c("Y",price_vars,income_vars), names(df))
  
  if(length(missing) > 0) {
    stop("Missing columns: ", paste(missing, collapse = ", "))
  }
  
  
  d_price <- df %>%
    select(Y, all_of(price_vars)) %>%
    pivot_longer(cols = all_of(price_vars),
                 names_to = "series",
                 values_to = "value") %>%
    mutate(panel = "Price elasticities")
  
  
  d_income <- df %>%
    select(Y, all_of(income_vars)) %>%
    pivot_longer(cols = all_of(income_vars),
                 names_to = "series",
                 values_to = "value") %>%
    mutate(panel = "Income elasticities")
  
  
  bind_rows(d_price, d_income) %>%
    mutate(
      study = study_key,
      panel = factor(panel,
                     levels = c("Price elasticities",
                                "Income elasticities")),
      series_label = case_when(
        series == "elast.ss" ~ "Own-price (Staples)",
        series == "elast.nn" ~ "Own-price (Non-staples)",
        series == "elast.sn" ~ "Cross-price (Staples)",
        series == "elast.ns" ~ "Cross-price (Non-staples)",
        series == "eta.s"    ~ "Income (Staples)",
        series == "eta.n"    ~ "Income (Non-staples)",
        TRUE ~ series
      )
    )
}

# ==============================================================================
# Plot builders
# ==============================================================================

# Plot stylized staples and non-staples demand over income.
plot_demand_vs_income <- function(demand_2026,
                                  demand_2017 = NULL,
                                  demand_2021 = NULL,
                                  include_all_three = FALSE,
                                  x_min = NULL,
                                  x_max = NULL,
                                  sqrt_x = FALSE,
                                  label_base_nudge = 0.03,
                                  label_slope_nudge = 0.20,
                                  label_gap_frac = 0.035) {
  
  .require_plot_pkgs()
  sty <- .study_style_maps()
  
  # Build data
  d26 <- .build_demand_long(demand_2026, "2026")
  d_all <- d26
  
  if(include_all_three) {
    if(is.null(demand_2017) || is.null(demand_2021)) {
      stop("include_all_three=TRUE requires demand_2017 and demand_2021")
    }
    d17 <- .build_demand_long(demand_2017, "2017")
    d21 <- .build_demand_long(demand_2021, "2021")
    d_all <- bind_rows(d26, d17, d21) %>%
      mutate(study = factor(study, levels = c("2026","2017","2021")))
  }
  
  # Apply x-range
  d_plot <- d_all
  if(!is.null(x_min)) d_plot <- d_plot %>% filter(Y >= x_min)
  if(!is.null(x_max)) d_plot <- d_plot %>% filter(Y <= x_max)
  if(nrow(d_plot) == 0) stop("No data remain after applying x_min/x_max.")
  
  d26_plot <- d_plot %>% filter(study == "2026")
  if(nrow(d26_plot) == 0) stop("No 2026 data remain after applying x_min/x_max.")
  
  # Choose label x location from plotted x-range, accounting for optional sqrt scale
  x_left_data  <- 0
  x_right_data <- if (!is.null(x_max)) x_max else max(d26_plot$Y, na.rm = TRUE)
  
  if (sqrt_x) {
    x_left_plot  <- sqrt(x_left_data)
    x_right_plot <- sqrt(x_right_data)
    x_tgt_plot   <- x_left_plot + 0.70 * (x_right_plot - x_left_plot)
    x_tgt        <- x_tgt_plot^2
  } else {
    x_tgt <- x_left_data + 0.70 * (x_right_data - x_left_data)
  }
  
  x_by_series <- d26_plot %>%
    group_by(series) %>%
    slice_min(order_by = abs(Y - x_tgt), n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(series, series_label, Y) %>%
    rename(series_key = series,
           Y_lab = Y)
  
  # Local slope (2026) near label x
  .local_slope <- function(df_series, x0) {
    df_series <- df_series %>% arrange(Y)
    i <- which.min(abs(df_series$Y - x0))
    if(length(i) == 0 || is.na(i)) return(0)
    i1 <- max(1, i - 1)
    i2 <- min(nrow(df_series), i + 1)
    dy <- df_series$value[i2] - df_series$value[i1]
    dx <- df_series$Y[i2] - df_series$Y[i1]
    if(is.na(dx) || dx == 0) return(0)
    abs(dy / dx)
  }
  
  lab_df <- x_by_series %>%
    rowwise() %>%
    mutate(
      y_max_at_x = {
        y0 <- Y_lab
        s0 <- series_key
        dd <- d_plot %>% filter(series == s0)
        dd2 <- dd %>%
          group_by(study) %>%
          slice_min(order_by = abs(Y - y0), n = 1, with_ties = FALSE) %>%
          ungroup()
        max(dd2$value, na.rm = TRUE)
      },
      slope_mag = {
        y0 <- Y_lab
        s0 <- series_key
        df_s <- d26_plot %>% filter(series == s0)
        .local_slope(df_s, y0)
      },
      label_text = series_label
    ) %>%
    ungroup()
  
  # Offsets
  y_rng  <- range(d_plot$value, na.rm = TRUE)
  y_span <- diff(y_rng)
  
  y_base <- label_base_nudge * y_span
  y_gap  <- label_gap_frac  * y_span  # ensures label isn't on top of curve
  
  s_max <- max(lab_df$slope_mag, na.rm = TRUE)
  if(is.na(s_max) || s_max == 0) s_max <- 1
  
  lab_df <- lab_df %>%
    mutate(
      y_off = y_gap + y_base * (1 + label_slope_nudge * (slope_mag / s_max))
    )
  
  p <- ggplot(
    d_plot,
    aes(x = Y,
        y = value,
        group = interaction(series, study))
  ) +
    geom_line(
      aes(linetype = study),
      linewidth = 0.9,
      color = "black"
    ) +
    labs(
      x = "Income per cap (10^3 1990 US$/year)",
      y = "Demand",
      linetype = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = if(include_all_three) "right" else "none",
      panel.grid.minor = element_blank(),
      axis.title = element_text(size = 10)
    ) +
    guides(
      linetype = guide_legend(override.aes = list(color = "black"))
    )
  
  if(include_all_three) {
    p <- p +
      scale_linetype_manual(
        values = sty$linetype_map,
        breaks = names(sty$linetype_map),
        labels = unname(sty$label_map[names(sty$linetype_map)])
      )
  } else {
    p <- p + scale_linetype_manual(values = c("2026" = "solid"), guide = "none")
  }
  
  if(!is.null(x_min) || !is.null(x_max)) {
    p <- p + coord_cartesian(xlim = c(x_min, x_max))
  }
  
  # Optional sqrt x scale with explicit tick marks
  if (sqrt_x) {
    
    x_upper <- if (!is.null(x_max)) x_max else max(d_plot$Y, na.rm = TRUE)
    
    sqrt_breaks <- c(
      0:5,
      seq(10, floor(x_upper / 5) * 5, by = 5)
    ) %>%
      unique() %>%
      .[. <= x_upper]
    
    p <- p + scale_x_sqrt(
      breaks = sqrt_breaks,
      labels = sqrt_breaks
    )
  }
  
  p +
    geom_text(
      data = lab_df,
      inherit.aes = FALSE,
      aes(x = Y_lab,
          y = y_max_at_x + y_off,
          label = label_text),
      color = "black",
      hjust = 0,
      size = 3.4
    )
}

# Plot stylized price and income elasticities over income.
plot_elasticities_vs_income <- function(demand_2026,
                                        demand_2017 = NULL,
                                        demand_2021 = NULL,
                                        include_all_three = FALSE,
                                        x_min = NULL,
                                        x_max = NULL,
                                        sqrt_x = FALSE) {
  
  .require_plot_pkgs()
  sty <- .study_style_maps()
  
  # Build long data
  d26 <- .build_elast_long(demand_2026, "2026")
  d_all <- d26
  
  if(include_all_three) {
    if(is.null(demand_2017) || is.null(demand_2021)) {
      stop("include_all_three=TRUE requires demand_2017 and demand_2021")
    }
    d17 <- .build_elast_long(demand_2017, "2017")
    d21 <- .build_elast_long(demand_2021, "2021")
    d_all <- bind_rows(d26, d17, d21) %>%
      mutate(study = factor(study, levels = c("2026","2017","2021")))
  }
  
  # Apply x-range for plotting
  d_plot <- d_all
  if(!is.null(x_min)) d_plot <- d_plot %>% filter(Y >= x_min)
  if(!is.null(x_max)) d_plot <- d_plot %>% filter(Y <= x_max)
  if(nrow(d_plot) == 0) stop("No data remain after applying x_min/x_max.")
  
  # Color mapping by series (adjust hex codes if desired)
  col_map <- c(
    # Income elasticities
    "eta.s"    = "black",
    "eta.n"    = "gray60",
    # Own-price
    "elast.ss" = "#0B3D91",  # dark blue
    "elast.nn" = "#7FB7FF",  # light blue
    # Cross-price
    "elast.sn" = "#5C2E00",  # dark brown
    "elast.ns" = "#C49A6C"   # light brown
  )
  
  # Pretty labels for series in the legend
  series_lab <- c(
    "elast.ss" = "Own-price (Staples)",
    "elast.nn" = "Own-price (Non-staples)",
    "elast.sn" = "Cross-price (Staples)",
    "elast.ns" = "Cross-price (Non-staples)",
    "eta.s"    = "Income (Staples)",
    "eta.n"    = "Income (Non-staples)"
  )
  
  p <- ggplot(
    d_plot,
    aes(
      x = Y,
      y = value,
      group = interaction(panel, series, study),
      linetype = study,
      color = series
    )
  ) +
    geom_hline(yintercept = 0, linewidth = 0.4, alpha = 0.4) +
    geom_line(linewidth = 0.85) +
    facet_wrap(~panel, ncol = 1, scales = "free_y") +
    scale_color_manual(
      values = col_map,
      breaks = names(series_lab),
      labels = unname(series_lab[names(series_lab)]),
      name = NULL
    ) +
    labs(
      x = "Income per cap (10^3 1990 US$/year)",
      y = "Elasticity",
      linetype = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "right",
      panel.grid.minor = element_blank(),
      axis.title = element_text(size = 10)
    )
  
  # Linetype mapping:
  # - If all-three, show all three
  # - If 2026-only, still show a linetype legend
  if(include_all_three) {
    
    p <- p +
      scale_linetype_manual(
        values = sty$linetype_map,
        breaks = names(sty$linetype_map),
        labels = unname(sty$label_map[names(sty$linetype_map)])
      ) +
      guides(
        linetype = guide_legend(override.aes = list(color = "black")),
        color    = guide_legend(override.aes = list(linetype = "solid"))
      )
    
  } else {
    
    p <- p +
      scale_linetype_manual(
        values = c("2026" = "solid"),
        guide = "none"
      ) +
      guides(
        color = guide_legend(override.aes = list(linetype = "solid"))
      )
  }
  
  # Enforce x-range visually
  if(!is.null(x_min) || !is.null(x_max)) {
    p <- p + coord_cartesian(xlim = c(x_min, x_max))
  }
  
  # Optional sqrt x scale with explicit tick marks
  if (sqrt_x) {
    
    x_upper <- if (!is.null(x_max)) x_max else max(d_plot$Y, na.rm = TRUE)
    
    sqrt_breaks <- c(
      0:5,
      seq(10, floor(x_upper / 5) * 5, by = 5)
    ) %>%
      unique() %>%
      .[. <= x_upper]
    
    p <- p + scale_x_sqrt(
      breaks = sqrt_breaks,
      labels = sqrt_breaks
    )
  }
  
  p
}

# ==============================================================================
# PDF writers
# ==============================================================================

# Save demand-vs-income plots for this study alone and for all parameter sets.
make_demand_vs_income_pdf <- function(demand_2026,
                                      demand_2017,
                                      demand_2021,
                                      output_file,
                                      x_min = NULL,
                                      x_max = NULL,
                                      sqrt_x = FALSE,
                                      width = 6.0,
                                      height = 8.5) {
  
  .require_plot_pkgs()
  
  # gridExtra is needed for stacking plots on one page
  if(!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("Missing package: gridExtra (install.packages('gridExtra'))")
  }
  
  p1 <- plot_demand_vs_income(
    demand_2026 = demand_2026,
    include_all_three = FALSE,
    x_min = x_min,
    x_max = x_max,
    sqrt_x = sqrt_x
  )
  
  p2 <- plot_demand_vs_income(
    demand_2026 = demand_2026,
    demand_2017 = demand_2017,
    demand_2021 = demand_2021,
    include_all_three = TRUE,
    x_min = x_min,
    x_max = x_max,
    sqrt_x = sqrt_x
  )
  
  pdf(output_file, width = width, height = height)
  on.exit(dev.off(), add = TRUE)
  
  # IMPORTANT: no grid.newpage() here (avoids blank first page)
  gridExtra::grid.arrange(p1, p2, ncol = 1, heights = c(1, 1))
}

# Save elasticity-vs-income plots for this study alone and for all parameter sets.
make_elasticities_vs_income_pdf <- function(demand_2026,
                                            demand_2017,
                                            demand_2021,
                                            output_file,
                                            x_min = NULL,
                                            x_max = NULL,
                                            sqrt_x = FALSE,
                                            width = 6.0,
                                            height = 8.5) {
  
  .require_plot_pkgs()
  
  if(!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("Missing package: gridExtra (install.packages('gridExtra'))")
  }
  grid_arrange <- get("grid.arrange", asNamespace("gridExtra"))
  
  p1 <- plot_elasticities_vs_income(
    demand_2026 = demand_2026,
    include_all_three = FALSE,
    x_min = x_min,
    x_max = x_max,
    sqrt_x = sqrt_x
  )
  
  p2 <- plot_elasticities_vs_income(
    demand_2026 = demand_2026,
    demand_2017 = demand_2017,
    demand_2021 = demand_2021,
    include_all_three = TRUE,
    x_min = x_min,
    x_max = x_max,
    sqrt_x = sqrt_x
  )
  
  pdf(output_file, width = width, height = height)
  on.exit(dev.off(), add = TRUE)
  
  grid_arrange(p1, p2, ncol = 1, heights = c(1, 1))
}

