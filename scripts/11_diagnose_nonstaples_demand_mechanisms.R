# Add-on to diagnose_nonstaples_input_pathways.R; run from the project root.
# Uses its existing output and selected absolute demand files.
# Controlled evaluations retain saved bias adders and fixed effects; no recalibration.

# Load definitions and configure units, floor tolerances, and controlled evaluations.
library(tidyverse)
source("R/common_definitions.R")
previous_dir <- file.path("output", "reports", procdata_dir,
                          procdata_subdir_RefMLgcam, "diagnostics_price_response")
pathway_dir <- file.path(previous_dir, "input_pathways")
output_dir <- file.path(pathway_dir, "demand_mechanisms")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
run_price_switch <- TRUE
# Processed Qs and Qn are Mcal/person/day; 0.1 calorie equals 0.0001 Mcal.
staples_floor <- 0.6
nonstaples_floor <- 0
floor_tolerance <- 0.1 / 1000
reproduction_tolerance <- 1e-6

# Read the earlier scope and comparison pairs without changing helper conventions.
needed <- file.path(pathway_dir, c("absolute_pathways.csv", "demand_gaps_with_inputs.csv"))
if (any(!file.exists(needed))) stop("Run diagnose_nonstaples_input_pathways.R first.")
pathways <- read_csv(needed[1], show_col_types = FALSE)
pairs <- read_csv(needed[2], show_col_types = FALSE)
keys <- pairs %>% distinct(scenario, region, decile, year)
gcam_dir <- file.path("data", "processed", procdata_dir, gcam_results_dir)
amb_dir <- file.path("data", "processed", procdata_dir,
                     procdata_subdir_RefMLgcam, demand_both_subdir, "scenarios_abs")

# Read absolute demand, preserving the selected iteration and saved calibration terms.
raw_amb <- list()
read_demand <- function(path, s, condition, model) {
  if (!file.exists(path)) stop("Missing absolute demand file: ", path)
  d <- readRDS(path)
  required <- c("GCAM_region_ID", "gcam-consumer", "year", "Qs", "Qn")
  if (!is.data.frame(d) || !all(required %in% names(d))) {
    stop("Missing absolute demand columns in ", path, ": ", paste(setdiff(required, names(d)), collapse = ", "))
  }
  d <- d %>% mutate(region_id = GCAM_region_ID,
                    decile_id = as.integer(parse_number(as.character(`gcam-consumer`)))) %>%
    semi_join(keys %>% filter(scenario == s),
              by = c("region_id" = "region", "decile_id" = "decile", "year"))
  if (!nrow(d)) stop("No matched demand rows in ", path)
  dup <- d %>% count(region_id, decile_id, year) %>% filter(n > 1)
  if (nrow(dup)) stop("Duplicate absolute demand keys in ", path, "; inspect iterations before continuing.")
  if (model == "Ambrosia") raw_amb[[paste(s, condition)]] <<- d
  d %>% transmute(scenario = s, region = region_id, decile = decile_id, year,
                 condition = condition, model = model, Qs, Qn,
                 iteration = if ("iteration" %in% names(d)) d$iteration else NA_real_)
}

# Load both Reference and High Price demand for both models and both parameter cases.
demand <- map_dfr(c("HD-HPR", "LD-HPR"), function(s) {
  tag <- str_replace_all(s, "-", "_")
  map_dfr(c("Ref", "HP"), function(condition) {
    gcam_file <- paste0("gcamoutput_", condition, "_", tag, ".RDS")
    amb_prefix <- if (condition == "Ref") "Ref_ML_gcam_" else "Ref_ML_HP_gcam_"
    bind_rows(read_demand(file.path(gcam_dir, gcam_file), s, condition, "GCAM"),
              read_demand(file.path(amb_dir, paste0("demand_allregions_", amb_prefix, tag, "_Qtot.RDS")),
                          s, condition, "Ambrosia"))
  })
})

# Audit coverage and finite demand before interpreting floors or changes.
expected <- crossing(keys, condition = c("Ref", "HP"), model = c("GCAM", "Ambrosia"))
missing <- anti_join(expected, demand, by = c("scenario", "region", "decile", "year", "condition", "model"))
write_csv(missing, file.path(output_dir, "missing_demand_keys.csv"))
if (nrow(missing) || any(!is.finite(demand$Qs)) || any(!is.finite(demand$Qn))) {
  stop("Incomplete/nonfinite absolute demand; inspect missing_demand_keys.csv and source files.")
}

# Distinguish at-floor values from values below the floor, without clipping demand.
demand <- demand %>% mutate(Qtot = Qs + Qn,
  staples_at_floor = abs(Qs - staples_floor) <= floor_tolerance,
  staples_below_floor = Qs < staples_floor - floor_tolerance,
  nonstaples_at_floor = abs(Qn - nonstaples_floor) <= floor_tolerance,
  nonstaples_below_floor = Qn < nonstaples_floor - floor_tolerance)
write_csv(demand, file.path(output_dir, "absolute_demand_and_floors.csv"))

# Compute explicit HP-minus-Reference demand changes alongside earlier scatter values.
responses <- demand %>% select(-iteration) %>%
  pivot_wider(names_from = condition, values_from = c(Qs, Qn, Qtot,
    staples_at_floor, staples_below_floor, nonstaples_at_floor, nonstaples_below_floor)) %>%
  mutate(delta_Qs = Qs_HP - Qs_Ref, delta_Qn = Qn_HP - Qn_Ref,
         delta_Qtot = Qtot_HP - Qtot_Ref,
         staples_floor_transition = case_when(
           staples_at_floor_Ref & staples_at_floor_HP ~ "Both",
           staples_at_floor_Ref ~ "Reference only", staples_at_floor_HP ~ "HP only", TRUE ~ "Neither")) %>%
  left_join(pairs %>% select(scenario, region, decile, year, x_value, y_value, demand_gap),
            by = c("scenario", "region", "decile", "year"))
write_csv(responses, file.path(output_dir, "demand_changes_and_floor_transitions.csv"))

# Verify that explicit absolute-file differences agree with earlier scatter differences.
response_audit <- responses %>%
  mutate(scatter_delta = if_else(model == "GCAM", x_value, y_value),
         difference_from_scatter = delta_Qn - scatter_delta) %>%
  group_by(scenario, model) %>%
  summarise(max_abs_difference = max(abs(difference_from_scatter)), .groups = "drop")
write_csv(response_audit, file.path(output_dir, "absolute_vs_scatter_audit.csv"))
# Retain both values even if the scatter helper used a different sign or transformation.
if (any(response_audit$max_abs_difference > reproduction_tolerance)) {
  warning("Absolute HP-minus-Reference changes differ from scatter values; inspect absolute_vs_scatter_audit.csv.")
}
largest <- pairs %>% arrange(desc(abs(demand_gap))) %>% slice_head(n = 50) %>%
  select(scenario, region, decile, year, demand_gap) %>%
  left_join(responses, by = c("scenario", "region", "decile", "year", "demand_gap"))
write_csv(largest, file.path(output_dir, "largest_gaps_demand_and_floors.csv"))

# Measure the non-staples/staples relative-price change for each input pathway.
relative <- pathways %>% mutate(relative_price = if_else(staples_price > 0,
                                                        nonstaples_price / staples_price, NA_real_)) %>%
  select(scenario, region, decile, year, model, condition, relative_price,
         income, staples_price, nonstaples_price) %>%
  pivot_wider(names_from = condition,
              values_from = c(relative_price, income, staples_price, nonstaples_price)) %>%
  mutate(relative_price_pct_change = if_else(relative_price_Ref > 0,
          100 * (relative_price_HP / relative_price_Ref - 1), NA_real_))
write_csv(relative, file.path(output_dir, "relative_price_changes.csv"))

# Summarize demand direction and floor exposure using explicit absolute-file differences.
floor_summary <- responses %>% group_by(scenario, model, region, decile) %>%
  summarise(n_years = n(), n_staples_floor_ref = sum(staples_at_floor_Ref),
            n_staples_floor_hp = sum(staples_at_floor_HP),
            n_staples_below_floor = sum(staples_below_floor_Ref | staples_below_floor_HP),
            n_Qn_increases = sum(delta_Qn > 0), n_Qn_decreases = sum(delta_Qn < 0),
            .groups = "drop")
write_csv(floor_summary, file.path(output_dir, "floor_summary.csv"))

# Compare saved bias adders by matched key before controlled evaluations.
bias_comparison <- map_dfr(c("HD-HPR", "LD-HPR"), function(s) {
  ref <- raw_amb[[paste(s, "Ref")]]
  hp <- raw_amb[[paste(s, "HP")]]
  required <- c("RBs", "RBn", "iteration")
  if (!all(required %in% names(ref)) || !all(required %in% names(hp))) {
    stop("Saved demand lacks RBs, RBn, or iteration for ", s)
  }
  ref <- ref %>% transmute(region = region_id, decile = decile_id, year,
                           iteration_Ref = iteration, RBs_Ref = RBs, RBn_Ref = RBn)
  hp <- hp %>% transmute(region = region_id, decile = decile_id, year,
                         iteration_HP = iteration, RBs_HP = RBs, RBn_HP = RBn)
  ref %>% full_join(hp, by = c("region", "decile", "year")) %>%
    mutate(scenario = s, delta_RBs = RBs_HP - RBs_Ref, delta_RBn = RBn_HP - RBn_Ref)
})
write_csv(bias_comparison, file.path(output_dir, "bias_adder_comparison.csv"))
if (any(!is.finite(as.matrix(select(bias_comparison, iteration_Ref, iteration_HP,
                                    RBs_Ref, RBs_HP, RBn_Ref, RBn_HP))))) {
  stop("Unmatched or nonfinite saved bias terms; inspect bias_adder_comparison.csv.")
}
if (any(bias_comparison$iteration_Ref != bias_comparison$iteration_HP)) {
  stop("Reference and HP parameter iterations differ; inspect bias_adder_comparison.csv.")
}
bias_summary <- bias_comparison %>% group_by(scenario, region, decile) %>%
  summarise(max_abs_delta_RBs = max(abs(delta_RBs)), max_abs_delta_RBn = max(abs(delta_RBn)),
            range_ref_RBs = max(RBs_Ref) - min(RBs_Ref),
            range_ref_RBn = max(RBn_Ref) - min(RBn_Ref),
            range_hp_RBs = max(RBs_HP) - min(RBs_HP),
            range_hp_RBn = max(RBn_HP) - min(RBn_HP), .groups = "drop")
write_csv(bias_summary, file.path(output_dir, "bias_adder_summary.csv"))

# Evaluate ambrosia with stored inputs first, then GCAM prices and mixed-price checks.
# Baseline verification prevents attributing version/calibration mismatches to prices.
if (run_price_switch) {
  source("R/demand_functions.R")
  source("R/install_ambrosia_function.R")
  # Select the verified Windows package root explicitly.
  package_path <- if (.Platform$OS.type == "windows") ambrosia_path_windows else ambrosia_path_pic
  if (!isTRUE(install_ambrosia_once(path = package_path, force_install = FALSE))) {
    stop("Could not load ambrosia from: ", package_path)
  }
  global <- readRDS(file.path("data", "processed", procdata_dir, clean_data_dir,
                             "param_data_global_clean_sub.RDS"))
  fixed <- readRDS(file.path("data", "processed", procdata_dir, clean_data_dir,
                            "param_data_FE_clean_sub.RDS"))
  evaluate <- function(d, Ps, Pn, s, condition, treatment) {
    required <- c("iteration", "Y", "Ps", "Pn", "RBs", "RBn")
    if (!all(required %in% names(d))) stop("Missing saved ambrosia calibration/input columns: ",
                                           paste(setdiff(required, names(d)), collapse = ", "))
    if (any(!is.finite(as.matrix(d[required])))) stop("Nonfinite saved calibration/input values.")
    it <- unique(d$iteration)
    if (length(it) != 1) stop("Expected one selected parameter iteration per case/condition.")
    gp <- global %>% filter(iteration == it)
    if (nrow(gp) != 1) stop("Selected global parameter iteration missing or duplicated: ", it)
    param <- vec2param(as.vector(t(select(gp, As:pnscl))))
    map_dfr(unique(d$GCAM_region_ID), function(r) {
      idx <- which(d$GCAM_region_ID == r)
      fe <- fixed %>% filter(iteration == it, GCAM_region_ID == r)
      if (nrow(fe) != 1) stop("Selected fixed effect missing or duplicated for region ", r)
      q <- food.dmnd(Ps = Ps[idx], Pn = Pn[idx], Y = d$Y[idx], params = param,
                    rgn = r, staples_FE = fe$staples_FE,
                    bias_adder_s = d$RBs[idx], bias_adder_ns = d$RBn[idx])
      if (nrow(q) != length(idx) || !all(c("Qs", "Qn") %in% names(q))) stop("Unexpected food.dmnd output.")
      tibble(scenario = s, condition = condition, treatment = treatment, region = r,
             decile = d$decile_id[idx], year = d$year[idx], Qs = q$Qs, Qn = q$Qn,
             stored_Qs = d$Qs[idx], stored_Qn = d$Qn[idx])
    })
  }
  # Export the selected ambrosia parameters for comparison with GCAM input files.
  selected_iterations <- map_dfr(c("HD-HPR", "LD-HPR"), function(s) {
    tibble(scenario = s, iteration = unique(raw_amb[[paste(s, "Ref")]]$iteration))
  })
  write_csv(selected_iterations %>% left_join(global, by = "iteration"),
            file.path(output_dir, "selected_global_parameters.csv"))
  write_csv(selected_iterations %>% left_join(fixed, by = "iteration"),
            file.path(output_dir, "selected_regional_parameters.csv"))
  # Reproduce every saved observation before evaluating any switched prices.
  baseline <- map_dfr(c("HD-HPR", "LD-HPR"), function(s) {
    map_dfr(c("Ref", "HP"), function(condition) {
      d <- raw_amb[[paste(s, condition)]]
      evaluate(d, d$Ps, d$Pn, s, condition, "Stored prices")
    })
  }) %>% mutate(error_Qs = Qs - stored_Qs, error_Qn = Qn - stored_Qn)
  write_csv(baseline, file.path(output_dir, "baseline_reproduction.csv"))
  if (any(!is.finite(baseline$Qs)) || any(!is.finite(baseline$Qn)) ||
      max(abs(c(baseline$error_Qs, baseline$error_Qn))) > reproduction_tolerance) {
    stop("Saved demand was not reproduced: inspect baseline_reproduction.csv. Floor diagnostics are already saved.")
  }
  # Change both prices or just one price, preserving income, iteration, and bias adders.
  switched <- map_dfr(c("HD-HPR", "LD-HPR"), function(s) {
    map_dfr(c("Ref", "HP"), function(condition) {
      d <- raw_amb[[paste(s, condition)]]
      prices <- pathways %>% filter(scenario == s, condition == !!condition, model == "GCAM") %>%
        select(region, decile, year, staples_price, nonstaples_price)
      d <- d %>% left_join(prices, by = c("region_id" = "region", "decile_id" = "decile", "year"))
      if (any(!is.finite(d$staples_price)) || any(!is.finite(d$nonstaples_price))) stop("Missing switched prices.")
      bind_rows(evaluate(d, d$staples_price, d$nonstaples_price, s, condition, "Both GCAM prices"),
                evaluate(d, d$staples_price, d$Pn, s, condition, "GCAM staples price only"),
                evaluate(d, d$Ps, d$nonstaples_price, s, condition, "GCAM non-staples price only"))
    })
  })
  # Hold Reference bias adders fixed in both conditions, matched by region/decile/year.
  fixed_bias <- map_dfr(c("HD-HPR", "LD-HPR"), function(s) {
    ref_bias <- raw_amb[[paste(s, "Ref")]] %>%
      select(region_id, decile_id, year, RBs, RBn)
    map_dfr(c("Ref", "HP"), function(condition) {
      d <- raw_amb[[paste(s, condition)]] %>% select(-RBs, -RBn) %>%
        left_join(ref_bias, by = c("region_id", "decile_id", "year"))
      prices <- pathways %>% filter(scenario == s, condition == !!condition, model == "GCAM") %>%
        select(region, decile, year, staples_price, nonstaples_price)
      d <- d %>% left_join(prices, by = c("region_id" = "region", "decile_id" = "decile", "year"))
      bind_rows(evaluate(d, d$Ps, d$Pn, s, condition, "Reference bias; stored prices"),
                evaluate(d, d$staples_price, d$nonstaples_price, s, condition,
                         "Reference bias; GCAM prices"))
    })
  })
  results <- bind_rows(baseline %>% select(-error_Qs, -error_Qn), switched, fixed_bias)
  if (any(!is.finite(results$Qs)) || any(!is.finite(results$Qn))) {
    stop("Nonfinite controlled demand; inspect the input/bias comparisons.")
  }
  write_csv(results, file.path(output_dir, "controlled_absolute_demand.csv"))
  controlled <- results %>% select(-stored_Qs, -stored_Qn) %>%
    pivot_wider(names_from = condition, values_from = c(Qs, Qn)) %>%
    mutate(delta_Qs = Qs_HP - Qs_Ref, delta_Qn = Qn_HP - Qn_Ref)
  reference_changes <- responses %>% filter(model == "GCAM") %>%
    select(scenario, region, decile, year, gcam_delta_Qn = delta_Qn)
  controlled <- controlled %>% left_join(reference_changes, by = c("scenario", "region", "decile", "year")) %>%
    mutate(gap_to_gcam = delta_Qn - gcam_delta_Qn)
  write_csv(controlled, file.path(output_dir, "controlled_demand_changes.csv"))
  # Quantify the effect of changing adders at each of the two price pathways.
  bias_effects <- controlled %>% filter(treatment %in% c("Stored prices", "Both GCAM prices",
      "Reference bias; stored prices", "Reference bias; GCAM prices")) %>%
    select(scenario, region, decile, year, treatment, delta_Qn) %>%
    pivot_wider(names_from = treatment, values_from = delta_Qn) %>%
    mutate(bias_effect_stored_prices = `Stored prices` - `Reference bias; stored prices`,
           bias_effect_gcam_prices = `Both GCAM prices` - `Reference bias; GCAM prices`)
  write_csv(bias_effects, file.path(output_dir, "bias_effect_on_response.csv"))
  # Repeat the comparison for precisely the earlier 50 largest gaps.
  largest_keys <- pairs %>% arrange(desc(abs(demand_gap))) %>% slice_head(n = 50) %>%
    distinct(scenario, region, decile, year)
  write_csv(controlled %>% semi_join(largest_keys, by = c("scenario", "region", "decile", "year")),
            file.path(output_dir, "largest_gaps_controlled_changes.csv"))
  controlled_summary <- bind_rows(
    controlled %>% mutate(scope = "All selected cases"),
    controlled %>% semi_join(largest_keys, by = c("scenario", "region", "decile", "year")) %>%
      mutate(scope = "50 largest gaps")) %>%
    group_by(scope, scenario, treatment) %>%
    summarise(n = n(), n_opposite_sign = sum(delta_Qn * gcam_delta_Qn < 0),
              mae = mean(abs(gap_to_gcam)), .groups = "drop")
  write_csv(controlled_summary, file.path(output_dir, "controlled_response_summary.csv"))
  print(controlled_summary %>% filter(scope == "50 largest gaps"))
}

# Plot absolute demand in the two flagged regions, showing each model's floor status.
walk(c("HD-HPR", "LD-HPR"), function(s) {
  walk(c(4, 31), function(r) {
    d <- demand %>% filter(scenario == s, region == r) %>%
      pivot_longer(c(Qs, Qn), names_to = "food", values_to = "demand")
    if (!nrow(d)) return(invisible(NULL))
    p <- ggplot(d, aes(year, demand, colour = model, linetype = condition)) +
      geom_line() + facet_grid(food ~ decile, scales = "free_y") + theme_bw() +
      labs(title = paste(s, "region", r), x = "Year", y = "Mcal/person/day")
    ggsave(file.path(output_dir, paste0("absolute_demand_", s, "_R", r, ".pdf")), p, width = 20, height = 7)
  })
})

# Document interpretation and the limits of mixed-price experiments.
writeLines(c(
  "Demand changes here are explicitly HP minus Reference; earlier scatter values are retained separately.",
  "Floor status uses 0.6 Mcal staples and zero non-staples, with 0.1-calorie tolerance.",
  "At-floor output alone does not prove the floor caused an opposite response.",
  "Price switches retain saved iteration, regional fixed effect, income, and bias adders for each condition.",
  "Original price switches use each condition's saved adders; Reference-bias treatments use Reference adders in both.",
  "Reference adders are matched by region, decile, and year; variation across years is reported in bias_adder_summary.csv.",
  "No recalibration is performed. Bias-effect contrasts isolate adder differences at the specified price pathway.",
  "Selected parameter exports document ambrosia inputs; comparing GCAM parameters requires GCAM input files.",
  "The baseline must reproduce saved demand before switched results are produced.",
  "If both GCAM prices reverse ambrosia's response and narrow the gap, input prices contribute.",
  "If the response remains opposite, investigate demand implementation, constraints, and calibration.",
  "One-price switches show sensitivity; their effects are not an additive decomposition in a nonlinear model.",
  "Price switches alone cannot establish that floors cause the discrepancy."
), file.path(output_dir, "READ_ME.txt"))
print(floor_summary %>% filter(region %in% c(4, 31), decile <= 4))
message("Demand mechanism diagnostics written to: ", output_dir)
