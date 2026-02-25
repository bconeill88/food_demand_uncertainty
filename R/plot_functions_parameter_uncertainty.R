# Functions for creating various plots of uncertainty in parameters,
# currently bringing in and updating code from scripts/uncertainty_figures.R

# Parameter trace plots

# Plot traces for 9 global variables
# make_trace_plots_global <- function(param_data, title   = NULL, subtitle = NULL,) {
#   param_colnames <- select(param_data, c('As':'Pm')) %>% colnames()
#   param_data_long <- gather(param_data, key="parameter", value="value", param_colnames)
#   ggplot(param_data_long, aes(x=iteration, y=value)) +
#     geom_point(size=0.3) +
#     facet_wrap(~parameter, ncol=3, scales = "free") +
#     ggtitle(title, subtitle = subtitle)
# }
make_trace_plots_global <- function(
    param_data,
    title     = NULL,
    subtitle  = NULL,
    facet_ncol = 3
) {
  # Identify columns As:Pm (assumes they exist and are contiguous)
  param_cols <- names(param_data)[which(names(param_data) == "As") :
                                    which(names(param_data) == "Pm")]
  
  # Convert to long format
  param_data_long <- param_data %>%
    pivot_longer(cols = all_of(param_cols),
                 names_to  = "parameter",
                 values_to = "value")
  
  ggplot(param_data_long, aes(x = iteration, y = value)) +
    geom_point(size = 0.3) +
    facet_wrap(~ parameter, ncol = facet_ncol, scales = "free") +
    labs(
      title    = title,
      subtitle = subtitle
    )
}

# Plot traces for regional fixed effects, one plot per 16 regions
make_trace_plots_FE <- function(param_data,
                                title   = NULL,
                                subtitle = NULL,
                                regions_per_page = 16,
                                facet_ncol = 4) {
  # All regions, in a consistent order
  all_regions <- sort(unique(param_data$region))
  
  # If we fit on one page, just return a single plot in a list
  if (length(all_regions) <= regions_per_page) {
    p <- ggplot(param_data, aes(x = iteration, y = staples_FE)) +
      geom_point(size = 0.3) +
      facet_wrap(~ region, ncol = facet_ncol, scales = "free")
    return(list(p))
  }
  
  # Otherwise, split regions into chunks of size `regions_per_page`
  region_groups <- split(
    all_regions,
    ceiling(seq_along(all_regions) / regions_per_page)
  )
  
  # Make one plot per group
  plot_list <- lapply(region_groups, function(regs) {
    df_sub <- param_data %>%
      filter(region %in% regs)
    
    ggplot(df_sub, aes(x = iteration, y = staples_FE)) +
      geom_point(size = 0.3) +
      facet_wrap(~ region, ncol = facet_ncol, scales = "free") +
      ggtitle(title, subtitle = subtitle)
  })
  
  plot_list
}


# Parameter marginal densities

# Plot marginal densities for 9 global parameters with optional vertical lines to
# mark specific parameter values such as ML estimates
make_density_plots_global <- function(
    paramdata,                         
    param_cols  = c("As","ks","eps1n","xi.ss","xi.nn",
                    "xi.cross","lambda","An","Pm"),
    vline_values = NULL,              
    vline_labels = NULL,              
    vline_colors = NULL,              
    title   = NULL,
    subtitle = NULL,
    line_width = 1,
    font_size = 10,
    facet_ncol = 3
) {
  
  #------------------------------------------------------------
  # 1. Prepare parameter columns and long data
  #------------------------------------------------------------
  param_cols <- intersect(param_cols, names(paramdata))
  if (length(param_cols) == 0) {
    stop("No matching parameter columns found in paramdata.")
  }
  
  paramdata_long <- paramdata %>%
    select(all_of(param_cols)) %>%
    pivot_longer(cols = everything(),
                 names_to = "parameter",
                 values_to = "value")
  
  #------------------------------------------------------------
  # 2. Build vertical-line data (if any)
  #------------------------------------------------------------
  vline_df <- NULL
  
  if (!is.null(vline_values) && length(vline_values) > 0) {
    
    if (is.null(vline_labels) || length(vline_labels) != length(vline_values)) {
      vline_labels <- paste0("Set ", seq_along(vline_values))
    }
    
    make_vline_df <- function(x, label) {
      if (is.data.frame(x) || is.matrix(x)) {
        x <- x[1, param_cols, drop = FALSE]
        vals <- as.numeric(x[1, ])
        names(vals) <- param_cols
      } else {
        vals <- as.numeric(x[param_cols])
        names(vals) <- param_cols
      }
      
      tibble(
        parameter  = param_cols,
        line_value = vals,
        line_label = label
      )
    }
    
    vline_list <- Map(make_vline_df, vline_values, vline_labels)
    vline_df   <- bind_rows(vline_list)
    
    vline_df$line_label <- factor(vline_df$line_label, levels = vline_labels)
    
    if (is.null(vline_colors)) {
      base_cols <- c("red", "blue", "green")
      vline_colors <- base_cols[seq_along(vline_labels)]
      names(vline_colors) <- vline_labels
    } else if (is.null(names(vline_colors))) {
      names(vline_colors) <- vline_labels
    }
  }
  
  #------------------------------------------------------------
  # 3. Build plot
  #------------------------------------------------------------
  g <- ggplot(paramdata_long, aes(x = value)) +
    geom_density(linewidth = line_width, color = "black") +
    facet_wrap(~ parameter, ncol = facet_ncol, scales = "free") +
    theme(strip.text = element_text(size = font_size)) +
    ggtitle(title, subtitle = subtitle)
  
  if (!is.null(vline_df)) {
    g <- g +
      geom_vline(data = vline_df,
                 aes(xintercept = line_value, color = line_label),
                 linewidth = line_width) +
      scale_color_manual(name = "Estimate", values = vline_colors)
  }
  
  invisible(g)
}

# Plot marginal densities for regional fixed effect parameters with optional vertical 
# lines to mark specific parameter values such as ML estimates
make_density_plots_FE <- function(
    paramdata,                        
    value_col      = "staples_FE",    
    vline_values   = NULL,            
    vline_labels   = NULL,            
    vline_colors   = NULL,            
    title          = NULL,
    subtitle       = NULL,
    line_width = 1,
    font_size = 10,
    facet_ncol     = 4,
    regions_per_page = 16
) {
  #------------------------------------------------------------
  # 1. Basic checks
  #------------------------------------------------------------
  if (!value_col %in% names(paramdata)) {
    stop(paste("Column", value_col, "not found in paramdata."))
  }
  if (!"region" %in% names(paramdata)) {
    stop("Column 'region' must be present in paramdata.")
  }
  
  #------------------------------------------------------------
  # 2. Build vertical-line data (if any)
  #------------------------------------------------------------
  vline_df <- NULL
  
  if (!is.null(vline_values) && length(vline_values) > 0) {
    
    if (is.null(vline_labels) || length(vline_labels) != length(vline_values)) {
      vline_labels <- paste0("Set ", seq_along(vline_values))
    }
    
    make_vline_df <- function(x, label) {
      if (is.data.frame(x) || is.matrix(x)) {
        if (!all(c("region", value_col) %in% names(x))) {
          stop(paste("Each vertical-line data frame must contain 'region' and", value_col))
        }
        df_small <- x[, c("region", value_col), drop = FALSE]
        names(df_small)[names(df_small) == value_col] <- "line_value"
        df_small$line_label <- label
      } else {
        # Assume named numeric vector: names are regions
        df_small <- data.frame(
          region     = names(x),
          line_value = as.numeric(x),
          line_label = label,
          stringsAsFactors = FALSE
        )
      }
      df_small
    }
    
    vline_list <- Map(make_vline_df, vline_values, vline_labels)
    vline_df   <- bind_rows(vline_list)
    
    vline_df$line_label <- factor(vline_df$line_label, levels = vline_labels)
    
    if (is.null(vline_colors)) {
      base_cols <- c("red", "blue", "green")
      vline_colors <- base_cols[seq_along(vline_labels)]
      names(vline_colors) <- vline_labels
    } else if (is.null(names(vline_colors))) {
      names(vline_colors) <- vline_labels
    }
  }
  
  #------------------------------------------------------------
  # 3. Split regions into pages
  #------------------------------------------------------------
  all_regions <- sort(unique(paramdata$region))
  
  region_groups <- split(
    all_regions,
    ceiling(seq_along(all_regions) / regions_per_page)
  )
  
  #------------------------------------------------------------
  # 4. Build one plot per region group
  #------------------------------------------------------------
  plot_list <- lapply(region_groups, function(regs) {
    df_sub <- paramdata %>%
      filter(region %in% regs)
    
    g <- ggplot(df_sub, aes(x = .data[[value_col]])) +
      geom_density(linewidth = line_width, color = "black") +
      facet_wrap(~ region, ncol = facet_ncol, scales = "free") +
      theme(strip.text = element_text(size = font_size)) +
      labs(
        title    = title,
        subtitle = subtitle,
        x        = value_col,
        y        = "Density"
      )
    
    if (!is.null(vline_df)) {
      vline_sub <- vline_df %>%
        filter(region %in% regs)
      
      g <- g +
        geom_vline(data = vline_sub,
                   aes(xintercept = line_value, color = line_label),
                   linewidth = line_width) +
        scale_color_manual(name = "Estimate", values = vline_colors)
    }
    
    g
  })
  
  plot_list
}


# values of fixed effects

# function for plotting FE parameter values across regions, faceted for each case
# in the parameter data file passed as an argument
make_FE_values_plot <- function(
    paramdata,
    title,
    subtitle = NULL,
    facet_ncol = 3
) {
  # Ensure facet order
  paramdata$measure <- factor(
    paramdata$measure,
    levels = c("ML", "HI", "LI", "HPR", "LPR",
               "HIR", "LIR", "HSR", "LSR")
  )
  
  # Prepare axis label thinning: label every 5th region
  regions <- sort(unique(paramdata$GCAM_region_ID))
  axis_breaks <- regions
  axis_labels <- ifelse(regions %% 5 == 0, regions, "")
  
  ggplot(paramdata, aes(x = factor(GCAM_region_ID), y = staples_FE)) +
    geom_point(na.rm = TRUE) +
    facet_wrap(~ measure, ncol = facet_ncol, drop = FALSE) +
    theme(strip.text = element_text(size = 15)) +
    scale_x_discrete(breaks = axis_breaks, labels = axis_labels) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "GCAM region",
      y = "Fixed Effect"
    )
}
