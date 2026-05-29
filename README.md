# Food Demand Uncertainty Analysis

## Overview

This repository contains code used to analyze uncertainty in projections of food demand using:

- the *ambrosia* food demand package

- income and price assumptions from GCAM scenario output

- an ensemble of food demand parameters from a Markov Chain Monte Carlo (MCMC) estimation based on historical observations of food demand, income, and prices.

The primary outputs are an ensemble of future staples, non-staples, and total food demand projections across 32 GCAM regions and 10 income groups per region. These ensembles are used to characterize food demand uncertainty and to identify a small number of parameter sets that represent that uncertainty that can be used in GCAM for scenario analysis.

------------------------------------------------------------------------

## Repository Structure

``` text
data/
  raw/            Input data files
  processed/      Intermediate and final processed data

R/
  Helper functions

scripts/
  Main analysis scripts

output/
  reports/        PDF reports containing sets of figures
  tables/         Summary tables and metrics
```

Directory names and file locations are defined centrally in `R/common_definitions.R`.

------------------------------------------------------------------------

## Data Requirements

The analysis expects the following input files in `data/raw/` and its subdirectories.

| File | Location | Purpose |
|----|----|----|
| `ambrosia_9_params_23Nov2025.dat` | `data/raw/mcmc_params/` | Global parameter ensemble from MCMC estimation |
| `FE_params_23Nov25.dat` | `data/raw/mcmc_params/` | Regional fixed-effect parameter ensemble from MCMC estimation |
| `Processed_group_data_13Jan25.csv` | `data/raw/obs_data/` | Historical observations of food demand, income, and prices used for estimation and validation |
| `GCAM_region_ID_mapping.RDS` | `data/raw/` | Mapping between GCAM region names and numeric region identifiers |
| `income_dist_by2021.csv` | `data/raw/final_rgcam_outputs/tables_7mar26/` | Income distribution assumptions used to construct decile-level results |
| GCAM output tables | `data/raw/final_rgcam_outputs/tables_7mar26/` | Regional income, food price, food demand, land-use, water-use, and related GCAM outputs processed by `06_process_gcam_results.R` |

## Analysis Workflow

### 01_clean_param_data.R

Reads raw MCMC parameter ensembles and observational data, performs cleaning and filtering, and creates analysis-ready datasets.

### 02_parameter_stats.R

Identifies maximum-likelihood (ML) parameter values and independent parameter confidence intervals.

### 03_calculate_demand.R

Runs the ambrosia demand model using MCMC parameter ensembles and GCAM income and price trajectories. Produces demand and elasticity projections.

### 04_calculate_demand_diffs.R

Calculates differences between ensembles of demand projections, comparing reference and high-price scenarios.

### 05_param_scenario_discovery.R

Identifies parameter sets that best represent high or low demand (HD, LD) across regions and time steps, or high or low responses to price changes (HPR, LPR), or combinations of both. Also identifies the ambrosia demand projections associated with those parameter sets.

### 06_process_gcam_results.R

Processes GCAM outputs from scenarios based on the identified parameter sets. These results are used for comparison to the ambrosia demand uncertainty ranges, and in some cases (particularly for ML parameters) as inputs to ambrosia runs.

### 07_model_vs_obs.R

Compares GCAM model predictions against historical observations and produces fit metrics and diagnostic plots.

------------------------------------------------------------------------

## Supporting Scripts

### plot_parameter_uncertainty.R

Produces parameter posterior and uncertainty visualizations.

### plot_stylized_demand.R

Produces stylized demand and elasticity comparisons across alternative parameterizations.

### plot_main_results.R

Produces the primary figures used in reporting results and in the manuscript.

### create_max_iter_table.R

Creates a .csv version of a summary tables describing parameter iterations most frequently associated with extreme outcomes, for use in the manuscript.

------------------------------------------------------------------------

## Main Scenario Types

| Scenario | Description                       |
|----------|-----------------------------------|
| ML       | Maximum-likelihood parameter set  |
| HD       | High-demand parameter set         |
| LD       | Low-demand parameter set          |
| HPR      | High price-response parameter set |
| LPR      | Low price-response parameter set  |

Combined scenarios include:

- HD-HPR
- HD-LPR
- LD-HPR
- LD-LPR

------------------------------------------------------------------------

## External Dependencies

Key dependencies include:

- R
- tidyverse
- ggplot2
- patchwork
- ggforce
- ggh4x
- progressr
- rgcam
- ambrosia

------------------------------------------------------------------------

## Reproducing the Analysis

Typical workflow:

1.  Run `01_clean_param_data.R`
2.  Run `02_parameter_stats.R` to identify and save the maximum likelihood (ML) parameter set
3.  Run `06_process_gcam_results.R` just for GCAM results employing the ML parameter set
4.  Run `03_calculate_demand.R` to generate all necessary ambrosia demand projections
5.  Run `04_calculate_demand_diffs.R` to calculate differences in demand between the Reference scenario (i.e., income and prices) and the High Price Conditions scenario
6.  Run `05_param_scenario_discovery.R` to identify parameter sets that best represent high/low demand and/or high/low price response
7.  Run `06_process_gcam_results.R` for GCAM results employing the identified parameter sets
8.  Run:
    - `07_model_vs_obs.R` to calculate and plot comparison of model predictions versus observations
    - `plot_parameter_uncertainty.R` , `plot_stylized_demand.R` , and `plot_main_results.R` to visualize outcomes

Depending on configuration settings in `common_definitions.R`, scripts may be run for selected regions or all 32 GCAM regions.

------------------------------------------------------------------------

## Manuscript figures

Figures for the manuscript are created by selecting subsets of results from the pdf reports generated by the plot\_\*.R scripts.
