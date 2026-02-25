
plot_model_vs_obs_scatter_rows_pdf <- function(
    df,
    output_dir,
    filename = "model_vs_obs_scatter_rows.pdf",
    
    case_col   = "case",
    
    # observed (x) and modeled (y) columns
    obs_cols   = c(Qs = "Qs.obs", Qn = "Qn.obs", Qtot = "Qtot.obs"),
    mod_cols   = c(Qs = "Qs.region", Qn = "Qn.region", Qtot = "Qtot.region"),
    
    cases      = NULL,   # optional subset of cases (character vector)
    
    x_label    = "Observed demand (kcal/day)",
    y_label    = "Modeled demand (kcal/day)",
    
    point_alpha = 0.6,
    point_size  = 0.9,
    
    width  = 8.5,
    height = 11
) {
  
  # --- Guardrails ------------------------------------------------------------
  req <- c(case_col, unname(obs_cols), unname(mod_cols))
  miss <- setdiff(req, names(df))
  if (length(miss) > 0) stop("Missing columns in df: ", paste(miss, collapse = ", "))
  
  # --- Subset cases if requested --------------------------------------------
  d <- df
  if (!is.null(cases)) d <- d[d[[case_col]] %in% cases, , drop = FALSE]
  if (nrow(d) == 0) stop("No rows after filtering; nothing to plot.")
  
  case_levels <- unique(as.character(d[[case_col]]))
  if (length(case_levels) == 0) stop("No cases found after filtering.")
  
  # --- Shared square limits across ALL panels -------------------------------
  x_all <- unlist(lapply(obs_cols, function(nm) d[[nm]]), use.names = FALSE)
  y_all <- unlist(lapply(mod_cols, function(nm) d[[nm]]), use.names = FALSE)
  ok <- is.finite(x_all) & is.finite(y_all)
  if (!any(ok)) stop("No finite x/y values after filtering; cannot plot.")
  lim <- range(c(x_all[ok], y_all[ok]), na.rm = TRUE)
  if (diff(lim) == 0) lim <- lim + c(-0.5, 0.5)
  
  # --- Helper: one row plot (1 case -> 3 facets) ----------------------------
  make_case_row_plot <- function(d_case, case_name) {
    
    var_levels <- names(obs_cols)
    
    long <- do.call(
      rbind,
      lapply(var_levels, function(v) {
        data.frame(
          var = v,
          x   = d_case[[obs_cols[[v]]]],
          y   = d_case[[mod_cols[[v]]]],
          stringsAsFactors = FALSE
        )
      })
    )
    long$var <- factor(long$var, levels = var_levels)
    
    ggplot(long, aes(x = x, y = y)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      geom_point(alpha = point_alpha, size = point_size, na.rm = TRUE) +
      facet_wrap(~ var, ncol = 3, drop = FALSE) +
      coord_equal(xlim = lim, ylim = lim) +
      labs(x = NULL, y = NULL) +                 # key change
      ggtitle(case_name) +
      theme_minimal(base_size = 11) +
      theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0, size = 9, margin = margin(b = 2)),
        plot.title.position = "panel",   # <-- key line: align title with panels
        strip.text = element_text(size = 9),
        plot.margin = margin(t = 2, r = 2, b = 2, l = 2)
      )
  }
  
  make_y_label_plot <- function(lbl) {
    ggplot() +
      annotate("text", x = 0, y = 0, label = lbl, angle = 90, hjust = 0.5, vjust = 0.5) +
      xlim(-1, 1) + ylim(-1, 1) +
      theme_void() +
      theme(plot.margin = margin(0, 0, 0, 0))
  }
  
  make_x_label_plot <- function(lbl) {
    ggplot() +
      annotate("text", x = 0, y = 0, label = lbl, hjust = 0.5, vjust = 0.5) +
      xlim(-1, 1) + ylim(-1, 1) +
      theme_void() +
      theme(plot.margin = margin(0, 0, 0, 0))
  }
  
  # --- Page chunks: 3 cases per page ----------------------------------------
  rows_per_page <- 3
  split_cases <- split(case_levels, ceiling(seq_along(case_levels) / rows_per_page))
  
  # --- Write PDF (and always close device) ----------------------------------
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(output_dir, filename)
  
  pdf(out_path, width = width, height = height, onefile = TRUE)
  on.exit(dev.off(), add = TRUE)
  
  for (case_page in split_cases) {
    
    row_plots <- lapply(case_page, function(cs) {
      make_case_row_plot(d[d[[case_col]] == cs, , drop = FALSE], cs)
    })
    
    # Pad to 3 rows with truly blank spacers
    if (length(row_plots) < rows_per_page) {
      n_pad <- rows_per_page - length(row_plots)
      row_plots <- c(row_plots, rep(list(patchwork::plot_spacer()), n_pad))
    }
    page_body <- row_plots[[1]] / row_plots[[2]] / row_plots[[3]]
    
    # Shared axis label plots (real ggplots, so widths behave)
    ylab_plot <- make_y_label_plot(y_label)
    xlab_plot <- make_x_label_plot(x_label)
    
    # Stack 3 case-rows
    page_body <- row_plots[[1]] / row_plots[[2]] / row_plots[[3]]
    
    # Add shared labels with explicit widths/heights
    top_row <- (ylab_plot | page_body) +
      patchwork::plot_layout(widths = c(1, 40))   # left label strip vs content
    
    bottom_row <- (patchwork::plot_spacer() | xlab_plot) +
      patchwork::plot_layout(widths = c(1, 40))
    
    page_plot <- (top_row / bottom_row) +
      patchwork::plot_layout(heights = c(60, 1))
    
    print(page_plot)
  }
  
  invisible(out_path)
}

plot_income_vs_demand_rows_pdf <- function(
    df,
    output_dir,
    filename = "income_vs_demand_obs_model.pdf",
    
    # layout
    rows_per_page = 3,
    
    # columns
    case_col   = "case",
    income_col = "Y.region",
    
    obs_cols   = c(Qs = "Qs.obs",   Qn = "Qn.obs",   Qtot = "Qtot.obs"),
    mod_cols   = c(Qs = "Qs.region", Qn = "Qn.region", Qtot = "Qtot.region"),
    
    # shared labels (one per page)
    x_label = "Income",
    y_label = "Demand (kcal/day)",
    
    # styling
    obs_color   = "grey70",
    model_color = "#E69F00",   # set-1 orange; replace with your exact constant if you have one
    point_alpha = 0.6,
    point_size  = 0.9,
    
    width  = 8.5,
    height = 11
) {
  
  # --- Guardrails ------------------------------------------------------------
  req <- c(case_col, income_col, unname(obs_cols), unname(mod_cols))
  miss <- setdiff(req, names(df))
  if (length(miss) > 0) stop("Missing columns in df: ", paste(miss, collapse = ", "))
  
  stopifnot(length(obs_cols) == 3, length(mod_cols) == 3)
  stopifnot(all(names(obs_cols) == names(mod_cols)))
  
  d <- df
  case_levels <- unique(as.character(d[[case_col]]))
  if (length(case_levels) == 0) stop("No cases found.")
  
  # --- Shared x/y limits across ALL panels ----------------------------------
  x_all <- d[[income_col]]
  y_all <- c(
    unlist(lapply(obs_cols, function(nm) d[[nm]]), use.names = FALSE),
    unlist(lapply(mod_cols, function(nm) d[[nm]]), use.names = FALSE)
  )
  
  okx <- is.finite(x_all)
  oky <- is.finite(y_all)
  if (!any(okx)) stop("No finite income values; cannot plot.")
  if (!any(oky)) stop("No finite demand values; cannot plot.")
  
  xlim <- range(x_all[okx], na.rm = TRUE)
  ylim <- range(y_all[oky], na.rm = TRUE)
  if (diff(xlim) == 0) xlim <- xlim + c(-0.5, 0.5)
  if (diff(ylim) == 0) ylim <- ylim + c(-0.5, 0.5)
  
  # --- label plots as real ggplots (width control works) ----------------------
  make_y_label_plot <- function(lbl) {
    ggplot() +
      annotate("text", x = 0, y = 0, label = lbl, angle = 90, hjust = 0.5, vjust = 0.5) +
      xlim(-1, 1) + ylim(-1, 1) +
      theme_void() +
      theme(plot.margin = margin(0, 0, 0, 0))
  }
  
  make_x_label_plot <- function(lbl) {
    ggplot() +
      annotate("text", x = 0, y = 0, label = lbl, hjust = 0.5, vjust = 0.5) +
      xlim(-1, 1) + ylim(-1, 1) +
      theme_void() +
      theme(plot.margin = margin(0, 0, 0, 0))
  }
  
  # --- one row plot per case: 3 facets (Qs/Qn/Qtot) ---------------------------
  make_case_row_plot <- function(d_case, case_name) {
    
    var_levels <- names(obs_cols)
    
    # long format with separate obs/model rows so we can control draw order
    long_obs <- do.call(
      rbind,
      lapply(var_levels, function(v) {
        data.frame(
          var = v,
          x   = d_case[[income_col]],
          y   = d_case[[obs_cols[[v]]]],
          type = "obs",
          stringsAsFactors = FALSE
        )
      })
    )
    
    long_mod <- do.call(
      rbind,
      lapply(var_levels, function(v) {
        data.frame(
          var = v,
          x   = d_case[[income_col]],
          y   = d_case[[mod_cols[[v]]]],
          type = "model",
          stringsAsFactors = FALSE
        )
      })
    )
    
    long <- rbind(long_obs, long_mod)
    long$var  <- factor(long$var, levels = var_levels)
    long$type <- factor(long$type, levels = c("obs", "model"))
    
    ggplot(long, aes(x = x, y = y)) +
      # observed first (grey)
      geom_point(
        data = subset(long, type == "obs"),
        color = obs_color,
        alpha = point_alpha,
        size  = point_size,
        na.rm = TRUE
      ) +
      # modeled on top (orange)
      geom_point(
        data = subset(long, type == "model"),
        color = model_color,
        alpha = point_alpha,
        size  = point_size,
        na.rm = TRUE
      ) +
      facet_wrap(~ var, ncol = 3, drop = FALSE) +
      coord_cartesian(xlim = xlim, ylim = ylim) +
      labs(x = NULL, y = NULL) +          # shared labels added at page-level
      ggtitle(case_name) +
      theme_minimal(base_size = 11) +
      theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0, size = 9, margin = margin(b = 2)),
        strip.text = element_text(size = 9),
        plot.margin = margin(t = 2, r = 2, b = 2, l = 2)
      )
  }
  
  # --- paginate cases: rows_per_page per page --------------------------------
  split_cases <- split(case_levels, ceiling(seq_along(case_levels) / rows_per_page))
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(output_dir, filename)
  
  pdf(out_path, width = width, height = height, onefile = TRUE)
  on.exit(dev.off(), add = TRUE)
  
  for (case_page in split_cases) {
    
    row_plots <- lapply(case_page, function(cs) {
      make_case_row_plot(d[d[[case_col]] == cs, , drop = FALSE], cs)
    })
    
    # pad to full page with truly blank rows
    if (length(row_plots) < rows_per_page) {
      n_pad <- rows_per_page - length(row_plots)
      row_plots <- c(row_plots, rep(list(patchwork::plot_spacer()), n_pad))
    }
    
    # stack row plots
    page_body <- Reduce(`/`, row_plots)
    
    # shared labels
    ylab_plot <- make_y_label_plot(y_label)
    xlab_plot <- make_x_label_plot(x_label)
    
    # add shared labels with explicit widths/heights
    top_row <- (ylab_plot | page_body) + patchwork::plot_layout(widths = c(1, 60))
    bottom_row <- (patchwork::plot_spacer() | xlab_plot) + patchwork::plot_layout(widths = c(1, 60))
    page_plot <- (top_row / bottom_row) + patchwork::plot_layout(heights = c(60, 1))
    
    print(page_plot)
  }
  
  invisible(out_path)
}