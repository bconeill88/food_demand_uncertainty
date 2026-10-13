# Aggregate regional demand ensembles before calculating uncertainty intervals.
# Run from the project root. No ambrosia installation or new model runs needed.
# The 90% CI is the empirical 5th-95th percentile ensemble uncertainty interval,
# not a confidence interval for the ensemble mean.

# Load the project's definitions and analysis packages.
source("R/common_definitions.R")
source("R/init_packages.R")
ensure_package(tidyverse)

# Select regions and years; use 1:32 once all regional files are available.
reg_list <- 1:32
years_to_use <- NULL                  # NULL uses every available year
ci_level <- 0.90
save_draws <- TRUE

# Select ensemble directories; add other cases using the same structure.
cases <- list(
  Ref_ML_gcam = list(
    demand_path = file.path("data", "processed", procdata_dir,
                            procdata_subdir_RefMLgcam_new, demand_abs_subdir),
    suffix = "_ens_bc",
    population_file = file.path("data", "processed", procdata_dir,
                                 gcam_results_dir, "gcamoutput_Ref_ML.RDS")
  )
)
# For high-price results, copy this entry and use procdata_subdir_RefMLHPgcam
# and gcamoutput_HP_ML.RDS. For decompositions, change demand_path as appropriate.

# Set population metadata explicitly if automatic column identification fails.
# The default assumes GCAM population is in millions and demand in Mcal/person/day.
# If your population column contains persons, set population_multiplier <- 1.
population_column <- NULL
population_multiplier <- 1e6
demand_units <- "Mcal/person/day"
total_units <- "Mcal/day"
output_path <- file.path("data", "processed", procdata_dir,
                         "global_demand_uncertainty",
                         paste0("regions_", paste(reg_list, collapse = "-")))

# Validate selections before reading files.
stopifnot(length(reg_list) > 0, !anyNA(reg_list), !anyDuplicated(reg_list),
          all(reg_list %in% 1:32), ci_level > 0, ci_level < 1,
          length(population_multiplier) == 1, is.finite(population_multiplier),
          population_multiplier > 0)
coverage_label <- if (setequal(reg_list, 1:32)) "Global" else "Selected regions (partial global total)"

# Read one region, remove repeated decile totals, and diagnose inconsistent data.
read_region <- function(region_id, cfg) {
  filename <- file.path(cfg$demand_path,
                         paste0("demand_R", region_id, cfg$suffix, ".RDS"))
  if (!file.exists(filename)) stop("Missing selected-region file: ", filename)
  raw <- readRDS(filename)
  required <- c("GCAM_region_ID", "year", "iteration", "Qs.region", "Qn.region")
  if (!is.data.frame(raw) || !all(required %in% names(raw))) {
    stop("Unexpected demand structure in ", filename,
         "; required columns: ", paste(required, collapse = ", "))
  }
  if (nrow(raw) == 0 || anyNA(raw[required[1:3]]) ||
      any(raw$GCAM_region_ID != region_id)) stop("Invalid region or keys in ", filename)
  d <- raw %>% select(all_of(required)) %>% distinct()
  if (!is.null(years_to_use)) d <- d %>% filter(year %in% years_to_use)
  if (nrow(d) == 0) stop("No selected years in ", filename)
  duplicates <- d %>% count(GCAM_region_ID, year, iteration) %>% filter(n != 1)
  if (nrow(duplicates) > 0) {
    print(head(duplicates, 10))
    stop("Regional totals differ across decile rows in ", filename)
  }
  message("Region ", region_id, ": ", n_distinct(d$iteration),
          " iterations; ", n_distinct(d$year), " years; ",
          sum(!is.finite(d$Qs.region) | !is.finite(d$Qn.region)), " invalid rows.")
  d
}

# Read population once per region/year, without counting repeated decile rows.
read_population <- function(cfg, selected_years) {
  p <- readRDS(cfg$population_file)
  if (!is.data.frame(p) || !all(c("GCAM_region_ID", "year") %in% names(p))) {
    stop("Population input must contain GCAM_region_ID and year.")
  }
  col <- population_column
  if (is.null(col)) {
    candidates <- intersect(c("population", "population.region", "pop", "pop.region",
                               "Population", "Pop", "population_million"), names(p))
    if (length(candidates) != 1) {
      message("Available input columns: ", paste(names(p), collapse = ", "))
      stop("Set population_column to the REGION population column, or supply a separate population RDS.")
    }
    col <- candidates[[1]]
  }
  if (!col %in% names(p)) stop("Missing population column: ", col)
  p <- p %>% filter(GCAM_region_ID %in% reg_list, year %in% selected_years) %>%
    transmute(GCAM_region_ID, year, population_persons = .data[[col]] * population_multiplier) %>%
    distinct()
  dup <- p %>% count(GCAM_region_ID, year) %>% filter(n != 1)
  if (nrow(dup) > 0) stop("Population differs across rows for a region/year. Use total regional population.")
  expected <- expand_grid(GCAM_region_ID = reg_list, year = selected_years)
  if (nrow(anti_join(expected, p, by = c("GCAM_region_ID", "year"))) > 0 ||
      any(!is.finite(p$population_persons) | p$population_persons <= 0)) {
    stop("Missing, nonfinite, or nonpositive selected-region population.")
  }
  message("Population column: ", col, "; multiplier to persons: ", population_multiplier)
  print(p %>% group_by(year) %>% summarise(population_persons = sum(population_persons), .groups = "drop"))
  p
}

# Sum matching parameter iterations, retaining a fixed complete ensemble over time.
calculate_case <- function(case_name, cfg) {
  message("\nChecking ensemble: ", case_name)
  d <- bind_rows(lapply(reg_list, read_region, cfg = cfg))
  selected_years <- sort(unique(d$year))
  if (!is.null(years_to_use) && !setequal(selected_years, years_to_use)) {
    stop("Some requested years are absent from every region.")
  }
  p <- read_population(cfg, selected_years)
  checks <- d %>% group_by(iteration) %>%
    summarise(n_rows = n(), n_years = n_distinct(year),
              n_regions = n_distinct(GCAM_region_ID),
              all_finite = all(is.finite(Qs.region) & is.finite(Qn.region)), .groups = "drop") %>%
    mutate(retained = n_rows == length(reg_list) * length(selected_years) &
             n_years == length(selected_years) & n_regions == length(reg_list) & all_finite)
  keep_iterations <- checks$iteration[checks$retained]
  message("Retained ", length(keep_iterations), " / ", nrow(checks),
          " iterations complete and finite across every selected region and year.")
  if (length(keep_iterations) < 2) stop("Fewer than two complete iterations; cannot estimate ensemble uncertainty.")
  if (any(!checks$retained)) warning("Incomplete/nonfinite iterations excluded; see iteration diagnostics.")
  # Weight regional per-person demand before summing across regions.
  totals <- d %>% filter(iteration %in% keep_iterations) %>%
    left_join(p, by = c("GCAM_region_ID", "year")) %>%
    mutate(staples = Qs.region * population_persons,
           non_staples = Qn.region * population_persons) %>%
    group_by(year, iteration) %>%
    summarise(population_persons = sum(population_persons),
              staples = sum(staples), non_staples = sum(non_staples), .groups = "drop") %>%
    mutate(total = staples + non_staples)
  if (any(!is.finite(as.matrix(totals[c("staples", "non_staples", "total")])))) {
    stop("Nonfinite aggregated demand.")
  }

  # Report both absolute demand and population-weighted per-person demand.
  draws <- totals %>% pivot_longer(c(staples, non_staples, total),
                                   names_to = "food_type", values_to = "value")
  draws <- bind_rows(
    draws %>% mutate(measure = "total_demand", units = total_units),
    draws %>% mutate(value = value / population_persons,
                      measure = "per_capita_demand", units = demand_units)
  ) %>% mutate(scenario = case_name, coverage = coverage_label,
                region_ids = paste(reg_list, collapse = ","), n_regions = length(reg_list))
  tail_prob <- (1 - ci_level) / 2
  summary <- draws %>%
    group_by(scenario, coverage, region_ids, n_regions, year, population_persons,
             measure, units, food_type) %>%
    summarise(n_iterations = n(), minimum = min(value), maximum = max(value),
              range_width = maximum - minimum, mean = mean(value), median = median(value),
              ci_lower = quantile(value, tail_prob, names = FALSE),
              ci_upper = quantile(value, 1 - tail_prob, names = FALSE),
              ci_width = ci_upper - ci_lower, ci_level = ci_level, .groups = "drop")
  list(summary = summary, draws = draws,
       diagnostics = checks %>% mutate(scenario = case_name))
}

# Complete validation/calculation for all cases before writing any results.
results <- lapply(names(cases), function(nm) calculate_case(nm, cases[[nm]]))
summary_all <- bind_rows(lapply(results, function(x) x$summary))
diagnostics_all <- bind_rows(lapply(results, function(x) x$diagnostics))
dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
write.csv(summary_all, file.path(output_path, "global_demand_uncertainty_summary.csv"), row.names = FALSE)
write.csv(diagnostics_all, file.path(output_path, "global_demand_iteration_diagnostics.csv"), row.names = FALSE)
if (save_draws) saveRDS(bind_rows(lapply(results, function(x) x$draws)),
                        file.path(output_path, "global_demand_ensemble.RDS"))
saveRDS(list(regions = reg_list, years = years_to_use, cases = cases,
             ci_level = ci_level, population_column = population_column,
             population_multiplier = population_multiplier,
             demand_units = demand_units, total_units = total_units),
        file.path(output_path, "global_demand_settings.RDS"))
print(summary_all, n = 18, width = Inf)
message("Saved demand uncertainty results to ", output_path)
