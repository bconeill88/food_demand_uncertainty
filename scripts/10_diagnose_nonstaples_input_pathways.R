# Extend the earlier correlation diagnostics using existing absolute GCAM outputs.
# Run from the project root after diagnose_gcam_ambrosia_price_response.R.
# Ambrosia input assumptions come from ML GCAM, not the HD/LD GCAM runs.

# Load project definitions and set the diagnostic scope.
library(tidyverse)
source("R/common_definitions.R")
input_dir <- file.path("data", "processed", procdata_dir, gcam_results_dir)
previous_dir <- file.path("output", "reports", procdata_dir,
                          procdata_subdir_RefMLgcam, "diagnostics_price_response")
output_dir <- file.path(previous_dir, "input_pathways")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
focus_regions <- c(4, 31, 20)
# NULL uses every year present in the earlier matched decile comparisons.
years_keep <- NULL
# Set these explicitly if automatic column identification reports ambiguity.
region_col <- "GCAM_region_ID"
consumer_col <- "gcam-consumer"
income_col <- "Y"

# Require the earlier matched pairs so signs, units, and years remain identical.
pairs_file <- file.path(previous_dir, "pairs_returned.csv")
if (!file.exists(pairs_file)) stop("Run the earlier correlation diagnostic first: ", pairs_file)
pairs <- read_csv(pairs_file, show_col_types = FALSE) %>%
  filter(level == "Decile") %>%
  mutate(region = as.integer(region), year = as.numeric(year),
         decile = as.integer(parse_number(as.character(consumer_group))))
if (!is.null(years_keep)) pairs <- filter(pairs, year %in% years_keep)
if (anyNA(pairs$decile)) stop("Cannot identify deciles in pairs_returned.csv.")

# Select problematic deciles across all regions; retain full pathways for regions 4/31.
focus_keys <- pairs %>%
  filter((scenario == "HD-HPR" & decile %in% 1:4) |
           (scenario == "LD-HPR" & decile == 1) | region %in% focus_regions) %>%
  distinct(scenario, region, decile, year)

# Identify key columns without guessing between multiple plausible matches.
choose_col <- function(d, explicit, candidates, label) {
  if (!is.null(explicit)) {
    if (!explicit %in% names(d)) stop("Missing ", label, " column: ", explicit)
    return(explicit)
  }
  found <- intersect(candidates, names(d))
  if (length(found) != 1) stop("Set ", label, "_col explicitly. Available columns: ",
                             paste(names(d), collapse = ", "))
  found
}

# Validate numeric decile inputs and unique region/decile/year keys before joining.
read_inputs <- function(file) {
  path <- file.path(input_dir, file)
  if (!file.exists(path)) stop("Missing GCAM input: ", path)
  d <- readRDS(path)
  if (!is.data.frame(d)) stop("Expected a data frame in ", path)
  r <- choose_col(d, region_col, c("region", "region_ID", "region_id", "region.id",
                                  "GCAM_region_ID", "GCAM_region", "reg_num"), "region")
  c <- choose_col(d, consumer_col, c("gcam-consumer", "consumer_group", "consumer"), "consumer")
  required <- c("year", income_col, "Ps", "Pn")
  if (!all(required %in% names(d))) stop("Missing columns in ", file, ": ",
                                        paste(setdiff(required, names(d)), collapse = ", "))
  if (!all(vapply(d[required], is.numeric, logical(1)))) stop("Nonnumeric pathways in ", file)
  # Accept numeric region IDs only; named regions need an explicit mapping first.
  ids <- suppressWarnings(as.integer(as.character(d[[r]])))
  if (anyNA(ids)) stop("Region column contains names/noninteger IDs in ", file,
                      ". Use the project region mapping to supply numeric IDs.")
  out <- tibble(region = ids, decile = as.integer(parse_number(as.character(d[[c]]))),
                year = d$year, income = d[[income_col]], staples_price = d$Ps,
                nonstaples_price = d$Pn) %>%
    filter(year %in% pairs$year, region %in% pairs$region)
  if (anyNA(out$decile) || any(!out$decile %in% 1:10)) stop("Unexpected consumer labels in ", file)
  duplicates <- out %>% count(region, decile, year) %>% filter(n > 1)
  if (nrow(duplicates)) stop("Duplicate region/decile/year keys in ", file,
                            "; do not average parameter iterations or duplicated inputs.")
  out
}

# Read all six pathways: paired GCAM runs and the ML assumptions used by ambrosia.
files <- c(ML_Ref = "gcamoutput_Ref_ML.RDS", ML_HP = "gcamoutput_HP_ML.RDS",
           HD_Ref = "gcamoutput_Ref_HD_HPR.RDS", HD_HP = "gcamoutput_HP_HD_HPR.RDS",
           LD_Ref = "gcamoutput_Ref_LD_HPR.RDS", LD_HP = "gcamoutput_HP_LD_HPR.RDS")
inputs <- map(files, read_inputs)

# Assemble the four pathways for each parameter case, preserving unmatched keys.
pathways <- map_dfr(c("HD-HPR", "LD-HPR"), function(s) {
  tag <- if (s == "HD-HPR") "HD" else "LD"
  map_dfr(c("Ref", "HP"), function(condition) {
    map_dfr(c("GCAM", "Ambrosia inputs"), function(model) {
      id <- paste0(if (model == "GCAM") tag else "ML", "_", condition)
      keys <- focus_keys %>% filter(scenario == s)
      keys %>% left_join(inputs[[id]], by = c("region", "decile", "year")) %>%
        mutate(model = model, condition = condition, source_file = files[[id]])
    })
  })
})
write_csv(pathways, file.path(output_dir, "absolute_pathways.csv"))

# Audit missing/nonfinite values before interpreting pathway differences.
variables <- c("income", "staples_price", "nonstaples_price")
long <- pathways %>% pivot_longer(all_of(variables), names_to = "variable", values_to = "value")
audit <- long %>% group_by(scenario, model, condition, variable) %>%
  summarise(n = n(), missing_nonfinite = sum(!is.finite(value)),
            nonpositive = sum(is.finite(value) & value <= 0), .groups = "drop")
write_csv(audit, file.path(output_dir, "pathway_audit.csv"))
if (any(audit$missing_nonfinite > 0)) stop("Incomplete pathways: inspect pathway_audit.csv.")

# Calculate HP minus Reference in native units and percent; avoid zero denominators.
changes <- long %>% select(-source_file) %>%
  pivot_wider(names_from = condition, values_from = value) %>%
  mutate(change = HP - Ref, pct_change = if_else(Ref > 0, 100 * change / Ref, NA_real_))
write_csv(changes, file.path(output_dir, "hp_minus_reference.csv"))

# Compare the changes and absolute inputs faced by GCAM and ambrosia.
comparison <- changes %>% select(-pct_change) %>%
  mutate(model = if_else(model == "GCAM", "gcam", "amb_input")) %>%
  pivot_wider(names_from = model, values_from = c(Ref, HP, change)) %>%
  mutate(ref_input_gap = Ref_gcam - Ref_amb_input,
         hp_input_gap = HP_gcam - HP_amb_input,
         change_gap = change_gcam - change_amb_input,
         opposite_change_sign = change_gcam * change_amb_input < 0)
write_csv(comparison, file.path(output_dir, "pathway_mismatch.csv"))

# Join input changes to the exact demand-response gaps used in the earlier analysis.
features <- comparison %>%
  select(scenario, region, decile, year, variable, change_gcam, change_amb_input, change_gap) %>%
  pivot_wider(names_from = variable, values_from = c(change_gcam, change_amb_input, change_gap))
demand_pairs <- pairs %>% semi_join(focus_keys, by = c("scenario", "region", "decile", "year")) %>%
  mutate(demand_gap = y_value - x_value, opposite_demand_sign = x_value * y_value < 0) %>%
  left_join(features, by = c("scenario", "region", "decile", "year"))
write_csv(demand_pairs, file.path(output_dir, "demand_gaps_with_inputs.csv"))
write_csv(demand_pairs %>% arrange(desc(abs(demand_gap))) %>% slice_head(n = 50),
          file.path(output_dir, "largest_demand_gaps_with_inputs.csv"))

# Summarize pathway mismatches and their descriptive association with demand gaps.
safe_cor <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  if (length(x) < 3 || sd(x) == 0 || sd(y) == 0) return(NA_real_)
  cor(x, y)
}
summary <- comparison %>% group_by(scenario, region, decile, variable) %>%
  summarise(n_years = n(), max_abs_ref_gap = max(abs(ref_input_gap)),
            max_abs_hp_gap = max(abs(hp_input_gap)),
            mean_change_gcam = mean(change_gcam), mean_change_amb_input = mean(change_amb_input),
            n_opposite_change_sign = sum(opposite_change_sign), .groups = "drop")
write_csv(summary, file.path(output_dir, "pathway_summary.csv"))
associations <- demand_pairs %>%
  pivot_longer(starts_with("change_gap_"), names_to = "input_gap", values_to = "value") %>%
  group_by(scenario, decile, input_gap) %>%
  summarise(n_finite = sum(is.finite(value) & is.finite(demand_gap)),
            r_with_demand_gap = safe_cor(value, demand_gap), .groups = "drop")
write_csv(associations, file.path(output_dir, "input_gap_associations.csv"))

# Plot all deciles for each flagged region, with separate scales for each variable.
walk(c("HD-HPR", "LD-HPR"), function(s) {
  walk(focus_regions, function(r) {
    d <- long %>% filter(scenario == s, region == r)
    if (!nrow(d)) return(invisible(NULL))
    p <- ggplot(d, aes(year, value, colour = model, linetype = condition)) +
      geom_line() + facet_grid(variable ~ decile, scales = "free_y") + theme_bw() +
      labs(title = paste(s, "region", r), x = "Year", y = "Native processed-file units",
           subtitle = "Ambrosia inputs = ML GCAM assumptions")
    ggsave(file.path(output_dir, paste0("pathways_", s, "_region", r, ".pdf")),
           p, width = 20, height = 10)
    p_change <- ggplot(changes %>% filter(scenario == s, region == r),
                       aes(year, change, colour = model)) +
      geom_hline(yintercept = 0, colour = "grey70") + geom_line() +
      facet_grid(variable ~ decile, scales = "free_y") + theme_bw() +
      labs(title = paste(s, "region", r, "HP minus Reference"),
           x = "Year", y = "Change in native units")
    ggsave(file.path(output_dir, paste0("changes_", s, "_region", r, ".pdf")),
           p_change, width = 20, height = 10)
  })
})

# Explain interpretation without attributing causality to pathway correlations.
writeLines(c(
  "Ambrosia input pathways are read from Ref_ML and HP_ML GCAM files.",
  "HD/LD GCAM pathways are read from their own Reference and High Price runs.",
  "Focus: HD-HPR deciles 1-4 and LD-HPR decile 1 across regions; all deciles in regions 4/31.",
  "Demand gaps preserve the earlier scatter-helper sign and units.",
  "New pathway changes are explicitly HP minus Reference in native processed-file units.",
  "Income uses Y (decile per capita), not Y.region; staples prices matter through cross-price effects.",
  "Inspect audits, pathway mismatches, then largest_demand_gaps_with_inputs.csv.",
  "Equal signs do not imply equal magnitudes; examine both absolute levels and scenario changes.",
  "Input-gap associations are descriptive, with repeated years and no causal decomposition.",
  "Matching pathways would shift attention to demand implementation, floors, and bias correction.",
  "Different pathways identify a possible contributor; quantifying it requires controlled demand reruns."
), file.path(output_dir, "READ_ME.txt"))
print(audit)
print(summary %>% filter(region %in% focus_regions, decile <= 4))
message("Pathway diagnostics written to: ", output_dir)
