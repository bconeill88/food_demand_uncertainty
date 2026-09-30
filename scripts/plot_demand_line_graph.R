# ------------------------------------------------------------------------------
# Demand ensemble line plots
# ------------------------------------------------------------------------------
# Standalone presentation plotting script (figure not included in manuscript).
#
# Produces three PNG files for a selected region showing a random subsample of
# ensemble demand trajectories for staples, non-staples, and total demand.
# The same function can be used for absolute demand or demand differences.

# Load functions and common definitions
source("R/common_definitions.R")
source("R/init_packages.R")

# Load packages
ensure_package(tidyverse)
ensure_package(ggplot2)

# ------------------------------------------------------------------------------
# User settings
# ------------------------------------------------------------------------------

# Region to plot
region_id <- 2

# Number of ensemble iterations to plot
sample_n <- 100

# Set to "abs" for demand levels for Ref or High price scenario, or "diff" for 
# demand differences
plot_type <- "abs_Ref"

# Scenario/case to read
scen_case_abs  <- "_ens_bc"
scen_case_diff <- "_diffs_ens_bc"

# Input directories
input_dir_abs_Ref <- file.path("data", "processed", procdata_dir,
                           procdata_subdir_RefMLgcam, demand_abs_subdir)
input_dir_abs_High <- file.path("data", "processed", procdata_dir,
                                procdata_subdir_RefMLHPgcam, demand_abs_subdir)
input_dir_diff <- file.path("data", "processed", procdata_dir,
                            procdata_subdir_RefMLgcam, demand_diffs_subdir)

# Output directory
output_dir <- file.path("output", "figures")

# Optional reproducibility seed
set.seed(123)

# ------------------------------------------------------------------------------
# Plotting function
# ------------------------------------------------------------------------------

plot_demand_line_graph <- function(region_id,
                                   sample_n = 100,
                                   plot_type = c("abs_Ref", "abs_High", "diff"),
                                   input_dir_abs_Ref,
                                   input_dir_abs_High,
                                   input_dir_diff,
                                   scen_case_abs = "_ens_bc",
                                   scen_case_diff = "_diffs_ens_bc",
                                   output_dir,
                                   width = 7,
                                   height = 4.5,
                                   dpi = 300) {
  
  # Match plot type
  plot_type <- match.arg(plot_type)
  
  # Choose input file
  if (plot_type == "abs_Ref") {
    input_file <- file.path(input_dir_abs_Ref, paste0("demand_R", region_id, scen_case_abs, ".RDS"))
    y_label <- "Demand (10^3 cal/person/day)"
    file_stub <- paste0("demand_lines_Ref_R", region_id)
  } else if (plot_type == "abs_High") {
    input_file <- file.path(input_dir_abs_High, paste0("demand_R", region_id, scen_case_abs, ".RDS"))
    y_label <- "Demand (10^3 cal/person/day)"
    file_stub <- paste0("demand_lines_High_R", region_id)
  } else {
    input_file <- file.path(input_dir_diff, paste0("demand_R", region_id, scen_case_diff, ".RDS"))
    y_label <- "Demand difference (10^3 cal/person/day)"
    file_stub <- paste0("demand_diff_lines_R", region_id)
  }
  
  # Check input file
  if (!file.exists(input_file)) {
    stop("Input file not found: ", input_file)
  }
  
  # Create output directory
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Read regional ensemble
  demand_reg <- readRDS(input_file)
  
  # Check required columns
  required_cols <- c("iteration", "year", "gcam-consumer",
                     "Qs.region", "Qn.region", "Qtot.region")
  missing_cols <- setdiff(required_cols, names(demand_reg))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Select one consumer group to avoid duplicate regional values
  demand_reg <- demand_reg %>%
    filter(`gcam-consumer` == "FoodDemand_Group1")
  
  # Sample iterations
  available_iters <- sort(unique(demand_reg$iteration))
  if (length(available_iters) == 0) {
    stop("No iterations found after filtering regional ensemble.")
  }
  sample_iters <- sample(available_iters, size = min(sample_n, length(available_iters)))
  
  # Define variables to plot
  plot_specs <- tibble(
    value_col = c("Qs.region", "Qn.region", "Qtot.region"),
    title = c("Staples demand", "Non-staples demand", "Total demand"),
    filename = paste0(file_stub, c("_staples.png", "_nonstaples.png", "_total.png"))
  )
  
  # Make and save plots
  saved_files <- pmap_chr(plot_specs, function(value_col, title, filename) {
    
    plot_df <- demand_reg %>%
      filter(iteration %in% sample_iters) %>%
      select(iteration, year, value = all_of(value_col))
    
    p <- ggplot(plot_df, aes(x = year, y = value, group = iteration)) +
      geom_line(color = "gray30", linewidth = 0.25, alpha = 0.7) +
      labs(
        title = title,
        subtitle = paste0("Region ", region_id, "; ", length(sample_iters), " sampled iterations"),
        x = "Year",
        y = y_label
      ) +
      theme_classic(base_size = 12) +
      theme(
        legend.position = "none",
        panel.grid.minor = element_blank()
      )
    
    out_file <- file.path(output_dir, filename)
    ggsave(out_file, p, width = width, height = height, dpi = dpi)
    out_file
  })
  
  # Return output filenames
  invisible(saved_files)
}

# ------------------------------------------------------------------------------
# Run plot
# ------------------------------------------------------------------------------

plot_demand_line_graph(
  region_id = region_id,
  sample_n = sample_n,
  plot_type = "diff",
  input_dir_abs_Ref = input_dir_abs_Ref,
  input_dir_abs_High = input_dir_abs_High,
  input_dir_diff = input_dir_diff,
  scen_case_abs = scen_case_abs,
  scen_case_diff = scen_case_diff,
  output_dir = output_dir,
  width = 4.5,
  height = 4.5
)
