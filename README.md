# Food Demand Uncertainty Analysis

This project analyzes uncertainty in projected food demand across global GCAM regions using an ensemble of demand function parameter combinations from an MCMC estimation routine. The analysis includes the projection of regional demand for staples and non-staples using the ambrosia food demand package, the identification of specific parameter sets that can span uncertainty in both demand and in the response of demand to price changes, and the generation of plots illustrating results.

## Contents

-   `R/init_packages.R`: Loads and installs required R packages.
-   `R/parameters_for_intervals_functions.R`: Contains helper functions for plotting demand projections.
-   `main_script.R` (this file): Reads processed demand data, summarizes iteration frequencies, generates tables and plots for regional projections, and exports results to PNG and PDF.

## Project Structure

```         
data/
  └── raw/                        # Region mappings and subdirectories
    └── mcmc_params/              # MCMC ensemble outputs
    └── obs_data/                 # Observational data
    └── final_rgcam_outputs/      # GCAM scenario output tables from rgcam
  └── processed/<mcmc_version>/   # Analysis results
      <scen_name>/<base_year>/
      <run_name>/<output_type/
      
scripts/                          # Main scripts

R/                                # Helper functions

output/
  └── figures/<run_name>/<scen>/  # Generated individual tables and plots
  └── reports/                    # Generated pdfs of multiple plots
```

## Key Features

-   **Sample Ensemble Visualization**: Plots a subset of the demand projection ensemble for each region.
-   **Max Iteration Identification**: Identifies the iterations most frequently associated with high and low demand for each region and globally.
-   **Comparative Line Plots**: Shows regional and global max iteration trajectories for each demand type (Qs, Qn, Qtot).
-   **PDF Output**: Generates multi-panel PDF with fixed row-column layout and dummy panels to preserve aspect ratio.
-   **Tables**: Summarizes frequencies and max iteration numbers for each region and demand type.

## Example Output

-   `demand_R2_ens_bc.png`: Single-region plot (region 2) comparing sample ensemble and max iteration trajectories.
-   `demand_all_regions_ens_bc.pdf`: Multipanel PDF of projections across multiple regions.
-   `table_max_frequencies_all_regions.png`: Table of most frequent iterations by region and scenario.
-   `table_max_iterations_all_regions.png`: Table of iteration numbers at max frequency.

## Running the Code

1.  **Dependencies**\
    Ensure R packages are installed. These are automatically handled via `init_packages.R`.

2.  **Run main script**\
    Execute the script with or without a region filter:

    ``` bash
    Rscript main_script.R            # Runs all regions
    Rscript main_script.R 5          # Runs only region 5
    ```

3.  **Interactive mode**\
    If running in RStudio or another interactive session, the tables will be saved as `.png` files. Otherwise, `.rds` files are saved.

## Inputs

-   **Region Ensemble Files**: Files named `demand_R<region>_ens_bc.RDS`, containing demand simulations for each region.
-   **Max Frequency Table**: `max_iter_frequencies_ens_bc.RDS`, summarizing the most frequent iterations.
-   **Region Names**: `GCAM_region_ID_mapping.Rdata`, mapping region numbers to names.

## Outputs

-   PDF and PNG files stored in `output/figures/<procdata_dir>/<scen>/`.

## Scripts Overview

| Script | Purpose |
|----------------------------------|--------------------------------------|
| `init_packages.R` | Load or install required packages. |
| `parameters_for_intervals_functions.R` | Defines helper functions for sampling and plotting. |
| `main_script.R` | Orchestrates data input, summary, and plot generation. |

## To Do / Fill In Later

-   [ ] **Model Description**: Overview of the demand model and its structure.
-   [ ] **Parameter Estimation**: Description of how the MCMC estimation is performed.
-   [ ] **Assumptions**: Assumptions underlying the scenarios and parameter sets.
-   [ ] **Data Sources**: Description and citations for input data sources.
-   [ ] **Uncertainty Interpretation**: Guidance on how to interpret the ensemble results.
-   [ ] **Validation**: Any model validation steps conducted.
-   [ ] **Future Work**: Planned extensions or applications.

## License

*TBD*

## Contact

For questions about this project, contact [Your Name or Institution].
