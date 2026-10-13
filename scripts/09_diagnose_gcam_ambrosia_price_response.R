# Diagnose negative GCAM/ambrosia non-staples price-response correlations.
# Run from the project root; uses the same scatter-data helper as plot_main_results.R.
# No demand calculations or GCAM runs are needed. Existing files are not modified.
# Differences retain the sign and units supplied by the existing comparison helper.
# Largest absolute gaps and strongest negative covariance contributions differ:
# omission tests assess influence, not whether any observation should be removed.

# Load project definitions and the existing scatter-data function.
library(tidyverse)
source("R/common_definitions.R")
source("R/plot_functions_main_results.R")

# Set diagnostic scope; NULL preserves every year returned by the scatter helper.
region_list <- 1:32
years_keep <- NULL
top_n <- 25
output_dir <- file.path("output", "reports", procdata_dir,
                        procdata_subdir_RefMLgcam, "diagnostics_price_response")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Specify exactly the difference files used in plot_main_results.R.
gcam_base <- file.path("data", "processed", procdata_dir, gcam_results_dir)
amb_base <- file.path("data", "processed", procdata_dir,
                      procdata_subdir_RefMLgcam_orig, demand_both_subdir, "scenarios_diff")
cases <- tibble(
  scenario = c("HD-HPR", "LD-HPR"),
  gcam_file = c("gcamoutput_diffs_Ref_HD_HPR_HP.RDS",
                "gcamoutput_diffs_Ref_LD_HPR_HP.RDS"),
  amb_file = c("demand_diffs_allregions_HD_HPR_Qtot.RDS",
               "demand_diffs_allregions_LD_HPR_Qtot.RDS")
)

# Check inputs before reading; report all missing files together.
paths <- c(file.path(gcam_base, cases$gcam_file), file.path(amb_base, cases$amb_file))
if (any(!file.exists(paths))) {
  stop("Missing comparison inputs:\n", paste(paths[!file.exists(paths)], collapse = "\n"),
       "\nCheck the filenames against your current plot_main_results.R.")
}

# Build matched pairs using the project's existing comparison logic.
paired <- map_dfr(seq_len(nrow(cases)), function(i) {
  gcam <- readRDS(file.path(gcam_base, cases$gcam_file[i]))
  amb <- readRDS(file.path(amb_base, cases$amb_file[i]))
  map_dfr(region_list, function(r) {
    map_dfr(c("Regional", "Decile"), function(level) {
      cols <- if (level == "Regional") "Qn.region" else "Qn"
      d <- plot_scenario_scatter(
        reg_num = r, value_cols = cols, value_names = "Non-staples",
        keep_all_consumers = level == "Decile",
        scen_solid_1 = gcam, scen_solid_name_1 = cases$scenario[i],
        scen_solid_2 = amb, scen_solid_name_2 = cases$scenario[i],
        return_data = TRUE
      )
      if (!all(c("x_value", "y_value", "year") %in% names(d))) {
        stop("Unexpected scatter-data columns: ", paste(names(d), collapse = ", "))
      }
      if (level == "Decile" && !"consumer_group" %in% names(d)) {
        stop("Decile scatter data lack consumer_group.")
      }
      if (level == "Regional") d$consumer_group <- "Regional"
      d %>% mutate(scenario = cases$scenario[i], level = level, region = r) %>%
        select(scenario, level, region, consumer_group, year, x_value, y_value)
    })
  })
})

# Audit returned pairs; duplicate keys are never silently averaged or dropped.
if (!is.null(years_keep)) paired <- paired %>% filter(year %in% years_keep)
write_csv(paired, file.path(output_dir, "pairs_returned.csv"))
duplicates <- paired %>% count(scenario, level, region, consumer_group, year) %>% filter(n > 1)
write_csv(duplicates, file.path(output_dir, "duplicate_keys.csv"))
if (nrow(duplicates)) stop("Duplicate pairs: inspect duplicate_keys.csv before interpreting results.")
audit <- paired %>% group_by(scenario, level) %>%
  summarise(n_returned = n(), n_finite = sum(is.finite(x_value) & is.finite(y_value)),
            n_excluded = n_returned - n_finite, .groups = "drop")
write_csv(audit, file.path(output_dir, "pair_audit.csv"))
paired <- paired %>% filter(is.finite(x_value), is.finite(y_value)) %>%
  mutate(gap = y_value - x_value, abs_gap = abs(gap),
         opposite_sign = x_value * y_value < 0)
if (!nrow(paired)) stop("No finite pairs remain.")

# Return NA for correlations with insufficient data or zero variance.
safe_cor <- function(x, y, method = "pearson") {
  if (length(x) < 3 || sd(x) == 0 || sd(y) == 0) return(NA_real_)
  cor(x, y, method = method)
}
fit_summary <- function(d) {
  tibble(n = nrow(d), pearson = safe_cor(d$x_value, d$y_value),
         spearman = safe_cor(d$x_value, d$y_value, "spearman"),
         mean_gap = mean(d$gap), mae = mean(d$abs_gap),
         rmse = sqrt(mean(d$gap^2)), n_opposite_sign = sum(d$opposite_sign))
}

# Reproduce regional and individual-decile correlations, plus the pooled decile result.
summary <- paired %>% group_by(scenario, level) %>%
  group_modify(~ fit_summary(.x)) %>% ungroup()
by_decile <- paired %>% filter(level == "Decile") %>%
  group_by(scenario, consumer_group) %>% group_modify(~ fit_summary(.x)) %>% ungroup()
write_csv(summary, file.path(output_dir, "correlation_summary.csv"))
write_csv(by_decile, file.path(output_dir, "correlation_by_decile.csv"))

# Rank gaps and covariance contributions within each scenario and comparison level.
# The contribution sum equals Pearson r (up to rounding), using the pooled means.
ranked <- paired %>% group_by(scenario, level) %>%
  mutate(cov_product = (x_value - mean(x_value)) * (y_value - mean(y_value)),
         cor_contribution = cov_product /
           sqrt(sum((x_value - mean(x_value))^2) * sum((y_value - mean(y_value))^2)),
         gap_rank = min_rank(desc(abs_gap)),
         negative_covariance_rank = min_rank(cov_product)) %>% ungroup()
write_csv(ranked, file.path(output_dir, "all_pairs_ranked.csv"))
write_csv(ranked %>% group_by(scenario, level) %>% slice_max(abs_gap, n = top_n, with_ties = FALSE) %>%
            ungroup(), file.path(output_dir, "largest_gaps.csv"))
write_csv(ranked %>% filter(cov_product < 0) %>% group_by(scenario, level) %>%
            slice_min(cov_product, n = top_n, with_ties = FALSE) %>% ungroup(),
          file.path(output_dir, "strongest_negative_contributions.csv"))

# Compare correlations after omitting each region, decile, or year.
# Positive change_in_r means that excluding this group makes r more positive.
omission <- function(d, field) {
  full <- safe_cor(d$x_value, d$y_value)
  map_dfr(unique(d[[field]]), function(g) {
    removed <- d[d[[field]] == g, , drop = FALSE]
    retained <- d[d[[field]] != g, , drop = FALSE]
    r_without <- safe_cor(retained$x_value, retained$y_value)
    tibble(omitted_dimension = field, omitted_group = as.character(g),
           n_removed = nrow(removed), n_retained = nrow(retained),
           r_full = full, r_without = r_without, change_in_r = r_without - full,
           removed_mae = mean(removed$abs_gap),
           removed_cor_contribution = sum(removed$cor_contribution))
  })
}
influence <- ranked %>% group_by(scenario, level) %>% group_modify(function(.x, .y) {
  dims <- c("region", "year")
  if (.y$level == "Decile") dims <- c(dims, "consumer_group")
  map_dfr(dims, function(field) omission(.x, field))
})
influence <- influence %>% ungroup() %>%
  arrange(scenario, level, omitted_dimension, desc(change_in_r))
write_csv(influence, file.path(output_dir, "group_omission_influence.csv"))

# Separate within-region/within-year relationships from pooled relationships.
# Demeaning removes group means; these are descriptive correlations, not significance tests.
within <- paired %>% group_by(scenario, level) %>% group_modify(function(.x, .y) {
  map_dfr(c("region", "year", "consumer_group"), function(field) {
    z <- .x %>% group_by(across(all_of(field))) %>%
      mutate(x_centered = x_value - mean(x_value), y_centered = y_value - mean(y_value)) %>%
      ungroup()
    tibble(centered_within = field,
           r_centered = safe_cor(z$x_centered, z$y_centered))
  })
}) %>% ungroup()
write_csv(within, file.path(output_dir, "within_group_correlations.csv"))

# Report regional influence separately within each decile, matching decile metrics.
within_decile_influence <- ranked %>% filter(level == "Decile") %>%
  group_by(scenario, consumer_group) %>% group_modify(function(.x, .y) {
    # Recenter contributions for the individual-decile correlation.
    .x$cor_contribution <- (.x$x_value - mean(.x$x_value)) * (.x$y_value - mean(.x$y_value)) /
      sqrt(sum((.x$x_value - mean(.x$x_value))^2) * sum((.x$y_value - mean(.x$y_value))^2))
    omission(.x, "region")
  }) %>% ungroup() %>% arrange(scenario, consumer_group, desc(change_in_r))
write_csv(within_decile_influence, file.path(output_dir, "region_influence_by_decile.csv"))

# Show GCAM versus ambrosia differences; label the largest gaps with region and year.
labels <- ranked %>% group_by(scenario, level) %>%
  slice_max(abs_gap, n = 5, with_ties = FALSE) %>% ungroup() %>%
  mutate(label = paste0("R", region, "/", consumer_group, "/", year))
p <- ggplot(ranked, aes(x_value, y_value)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  geom_hline(yintercept = 0, colour = "grey85") +
  geom_vline(xintercept = 0, colour = "grey85") +
  geom_point(aes(colour = consumer_group), alpha = 0.6, size = 1.3) +
  geom_text(data = labels, aes(label = label), size = 2.3, check_overlap = TRUE, vjust = -0.6) +
  facet_wrap(vars(scenario, level), scales = "free", ncol = 2) +
  labs(x = "GCAM demand difference (existing comparison units)",
       y = "ambrosia demand difference (existing comparison units)", colour = "Consumer group") +
  theme_bw()
ggsave(file.path(output_dir, "price_response_scatter.pdf"), p, width = 12, height = 9)

# Map typical discrepancies across regions and deciles (mean absolute gap over years).
heat <- ranked %>% filter(level == "Decile") %>%
  group_by(scenario, region, consumer_group) %>% summarise(mae = mean(abs_gap), .groups = "drop") %>%
  mutate(decile = parse_number(as.character(consumer_group)))
p_heat <- ggplot(heat, aes(factor(decile), factor(region), fill = mae)) +
  geom_tile() + facet_wrap(vars(scenario)) + scale_fill_viridis_c() +
  labs(x = "Income decile", y = "GCAM region ID", fill = "Mean absolute gap") + theme_bw()
ggsave(file.path(output_dir, "region_decile_gaps.pdf"), p_heat, width = 9, height = 10)

# Explain output interpretation and limitations alongside the diagnostic results.
writeLines(c(
  "Start with correlation_summary.csv and correlation_by_decile.csv.",
  "Regional correlations match the regional comparison; each decile has its own correlation.",
  "The pooled decile correlation is additional and may differ from the summary table.",
  "Confirm these correlations against existing metrics before interpreting influence.",
  "pair_audit.csv counts nonfinite pairs excluded; duplicate keys stop execution.",
  "largest_gaps.csv ranks absolute ambrosia-minus-GCAM differences.",
  "strongest_negative_contributions.csv identifies points making pooled covariance negative.",
  "group_omission_influence.csv ranks changes in r after dropping a region, decile, or year.",
  "region_influence_by_decile.csv repeats regional omission within each individual decile.",
  "Positive change_in_r: omitting that group increases r. r_without > 0 indicates a sign reversal.",
  "within_group_correlations.csv removes group means to investigate aggregation effects.",
  "Rows/years are equally weighted, matching the scatter comparison; no population weighting.",
  "Years are repeated observations, so these influence checks are descriptive, not independent tests.",
  "No percent gaps are used: demand differences close to zero make ratios unstable.",
  "The script diagnoses existing differences; it cannot establish a mechanism from differences alone.",
  "Check price/income changes and absolute demand in the flagged cases to investigate mechanisms.",
  "The data preparation and scatter helpers are reused without changing their sign or unit conventions."
), file.path(output_dir, "READ_ME.txt"))
print(audit)
print(summary)
print(by_decile)
message("Diagnostics written to: ", output_dir)
