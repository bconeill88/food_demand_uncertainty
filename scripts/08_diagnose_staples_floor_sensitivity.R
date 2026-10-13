# Diagnose staples demand thresholds using existing results only.
# Run from the project root; no demand calculations or model installation occur.
# Outcomes are equally weighted, not population weighted.

# Load the same paths used by the existing analysis scripts.
source("R/common_definitions.R")

# Choose thresholds, projection years, columns, and numerical tolerance.
thresholds_cal <- c(400, 600, 1000)
existing_floor_cal <- 600
years_use <- NULL                    # NULL: all saved years >= base_year
regions_use <- 1:32
deciles_use <- paste0("FoodDemand_Group", 1:10)
qs_obs_col <- "Qs"
qs_gcam_col <- "Qs"
qs_ensemble_col <- "Qs"             # Verify this is final, floored demand.
units_to_cal <- 1000                 # Saved units: 10^3 cal/person/day.
floor_tolerance_cal <- 2.5         # Used only for equality at the old floor.
diagnostics_only <- FALSE             # First run: inspect schemas and ranges.

# Specify the ten GCAM runs and the two full ambrosia ensembles.
processed_root <- file.path("data", "processed", procdata_dir)
obs_file <- file.path(processed_root, clean_data_dir, "obs_data.RDS")
gcam_cases <- c("Ref_ML", "Ref_HD_HPR", "Ref_HD_LPR", "Ref_LD_HPR",
                "Ref_LD_LPR", "HP_ML", "HP_HD_HPR", "HP_HD_LPR",
                "HP_LD_HPR", "HP_LD_LPR")
ensemble_dirs <- c(
   Ref = file.path(processed_root, procdata_subdir_RefMLgcam_new, demand_abs_subdir)
#  HP = file.path(processed_root, procdata_subdir_RefMLHPgcam, demand_abs_subdir)
)
output_dir <- file.path("output", "tables", procdata_dir,
                        "staples_floor_diagnostics")

# Build an explicit file manifest to avoid counting summaries or differences.
manifest <- data.frame(source = "Observations", scenario = "Observed",
                       file = obs_file, qs_col = qs_obs_col,
                       stringsAsFactors = FALSE)
manifest <- rbind(manifest, data.frame(
  source = "GCAM", scenario = gcam_cases,
  file = file.path(processed_root, gcam_results_dir,
                   paste0("gcamoutput_", gcam_cases, ".RDS")),
  qs_col = qs_gcam_col, stringsAsFactors = FALSE
))
for (scenario in names(ensemble_dirs)) {
  manifest <- rbind(manifest, data.frame(
    source = "Ambrosia", scenario = scenario,
    file = file.path(ensemble_dirs[[scenario]],
                     paste0("demand_R", regions_use, "_ens_bc.RDS")),
    qs_col = qs_ensemble_col, stringsAsFactors = FALSE
  ))
}
missing_files <- manifest$file[!file.exists(manifest$file)]
if (length(missing_files)) {
  stop("Missing required results files:\n", paste(missing_files, collapse = "\n"))
}

# Inspect one file per source/scenario before processing large ensembles.
if (diagnostics_only) {
  examples <- manifest[!duplicated(manifest[c("source", "scenario")]), ]
  for (i in seq_len(nrow(examples))) {
    spec <- examples[i, ]
    x <- readRDS(spec$file)
    cat("\n", spec$source, ": ", spec$scenario, "\n", spec$file, "\n", sep = "")
    if (!is.data.frame(x)) stop("Expected a data frame: ", spec$file)
    print(names(x))
    if (!spec$qs_col %in% names(x)) stop("Missing demand column: ", spec$qs_col)
    if (!is.numeric(x[[spec$qs_col]])) stop("Demand must be numeric.")
    print(summary(x[[spec$qs_col]] * units_to_cal))
    for (col in intersect(c("year", "gcam-consumer", "GCAM_region_ID"), names(x))) {
      cat(col, ": ", paste(sort(unique(x[[col]])), collapse = ", "), "\n", sep = "")
    }
    print(head(x[intersect(c("GCAM_region_ID", "year", "gcam-consumer",
                             "iteration", spec$qs_col), names(x))]))
  }
  message("Confirm Qs is final demand and units are 10^3 calories; then set diagnostics_only <- FALSE.")
} else {

  # Count strict threshold crossings; at 600, report floor hits as a proxy.
  count_thresholds <- function(z, censored) {
    valid <- is.finite(z)
    n_total <- length(z)
    n_valid <- sum(valid)
    z <- z[valid]
    result <- lapply(thresholds_cal, function(threshold) {
      if (censored && threshold < existing_floor_cal) {
        n_affected <- NA_real_
        interpretation <- "Unidentifiable from floored results"
      } else if (censored && threshold == existing_floor_cal) {
        n_affected <- sum(abs(z - existing_floor_cal) <= floor_tolerance_cal)
        interpretation <- "At existing floor; proxy for floor application"
      } else {
        n_affected <- sum(z < threshold)
        interpretation <- "Strictly below threshold"
      }
      data.frame(threshold_cal = threshold, n_total = n_total,
                 n_valid = n_valid, n_missing_nonfinite = n_total - n_valid,
                 n_affected = n_affected,
                 pct_total = if (n_total) 100 * n_affected / n_total else NA_real_,
                 pct_valid = if (n_valid) 100 * n_affected / n_valid else NA_real_,
                 interpretation = interpretation)
    })
    do.call(rbind, result)
  }

  # Process one file at a time; retain only small count tables and audit rows.
  count_parts <- list()
  audit_parts <- list()
  for (i in seq_len(nrow(manifest))) {
    spec <- manifest[i, ]
    message("Reading ", i, "/", nrow(manifest), ": ", basename(spec$file))
    x <- readRDS(spec$file)
    if (!is.data.frame(x)) stop("Expected a data frame: ", spec$file)
    censored <- spec$source != "Observations"
    keys <- c("GCAM_region_ID", "year")
    if (censored) keys <- c(keys, "gcam-consumer")
    if (spec$source == "Ambrosia") keys <- c(keys, "iteration")
    required <- unique(c(keys, spec$qs_col))
    absent <- setdiff(required, names(x))
    if (length(absent)) stop("Missing columns in ", spec$file, ": ", paste(absent, collapse = ", "))
    if (!is.numeric(x[[spec$qs_col]])) stop("Demand must be numeric: ", spec$file)

    # Select actual decile records and requested years; observations retain all years.
    n_read <- nrow(x)
    keep <- x$GCAM_region_ID %in% regions_use
    if (censored) {
      keep <- keep & x[["gcam-consumer"]] %in% deciles_use
      keep <- keep & !is.na(x$year) & x$year >= base_year
      if (!is.null(years_use)) keep <- keep & x$year %in% years_use
    }
    x <- x[keep, required, drop = FALSE]
    if (!nrow(x)) stop("No selected rows: ", spec$file)
    if (anyNA(x[keys])) stop("Missing outcome identifiers: ", spec$file)
    if (anyDuplicated(x[keys])) stop("Duplicate outcome keys: ", spec$file)
    if (spec$source == "Ambrosia") {
      expected_region <- regions_use[match(spec$file, manifest$file[manifest$source == "Ambrosia" &
                                                                 manifest$scenario == spec$scenario])]
      if (any(x$GCAM_region_ID != expected_region)) stop("Unexpected region in ", spec$file)
    }
    z <- x[[spec$qs_col]] * units_to_cal
    valid_z <- z[is.finite(z)]
    n_below_floor <- sum(valid_z < existing_floor_cal - floor_tolerance_cal)
    audit_parts[[i]] <- data.frame(
      source = spec$source, scenario = spec$scenario, file = spec$file,
      n_read = n_read, n_selected = nrow(x), n_missing_nonfinite = sum(!is.finite(z)),
      min_cal = if (length(valid_z)) min(valid_z) else NA_real_,
      max_cal = if (length(valid_z)) max(valid_z) else NA_real_,
      n_below_existing_floor = n_below_floor,
      n_at_existing_floor = sum(abs(valid_z - existing_floor_cal) <= floor_tolerance_cal),
      n_regions = length(unique(x$GCAM_region_ID)),
      n_deciles = if (censored) length(unique(x[["gcam-consumer"]])) else NA_integer_,
      n_iterations = if (spec$source == "Ambrosia") length(unique(x$iteration)) else NA_integer_,
      first_year = min(x$year), last_year = max(x$year)
    )
    if (censored && n_below_floor > 0) {
      print(audit_parts[[i]])
      stop("Values below 600 found. Check units, final demand column, and floor implementation before interpreting counts.")
    }

    # Keep region/decile/year detail; each ensemble iteration is one outcome.
    group_cols <- c("GCAM_region_ID", "year")
    if (censored) group_cols <- c(group_cols, "gcam-consumer")
    groups <- split(seq_len(nrow(x)), interaction(x[group_cols], drop = TRUE))
    per_file <- lapply(groups, function(rows) {
      ids <- x[rows[1], group_cols, drop = FALSE]
      if (!censored) ids[["gcam-consumer"]] <- "Observed regional mean"
      cbind(data.frame(source = spec$source, scenario = spec$scenario),
            ids[rep(1, length(thresholds_cal)), , drop = FALSE],
            count_thresholds(z[rows], censored))
    })
    count_parts[[i]] <- do.call(rbind, per_file)
    rm(x, z, valid_z, per_file, groups)
  }

  # Pool counts before computing percentages, preserving unavailable values.
  detail <- do.call(rbind, count_parts)
  audit <- do.call(rbind, audit_parts)
  pool_counts <- function(x, group_cols) {
    groups <- split(seq_len(nrow(x)), interaction(x[group_cols], drop = TRUE))
    out <- lapply(groups, function(rows) {
      part <- x[rows, ]
      ids <- part[1, group_cols, drop = FALSE]
      counts <- c("n_total", "n_valid", "n_missing_nonfinite", "n_affected")
      totals <- as.data.frame(lapply(part[counts], sum))
      totals$pct_total <- if (totals$n_total) 100 * totals$n_affected / totals$n_total else NA_real_
      totals$pct_valid <- if (totals$n_valid) 100 * totals$n_affected / totals$n_valid else NA_real_
      totals$interpretation <- part$interpretation[1]
      cbind(ids, totals)
    })
    do.call(rbind, out)
  }
  by_scenario_year <- pool_counts(detail, c("source", "scenario", "year", "threshold_cal"))
  by_scenario <- pool_counts(detail, c("source", "scenario", "threshold_cal"))
  overall <- pool_counts(detail, c("source", "threshold_cal"))

  # Save counts and input diagnostics for review; existing demand files are untouched.
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  tables <- list(overall = overall, by_scenario = by_scenario,
                 by_scenario_year = by_scenario_year,
                 by_region_decile_year = detail, input_audit = audit)
  for (name in names(tables)) {
    write.csv(tables[[name]], file.path(output_dir, paste0(name, ".csv")), row.names = FALSE)
  }
  print(overall, row.names = FALSE)
  message("Tables saved to ", output_dir)
  if (any(audit$n_missing_nonfinite > 0)) {
    warning("Missing/nonfinite demand exists: compare pct_total with pct_valid and inspect input_audit.csv.")
  }
}
