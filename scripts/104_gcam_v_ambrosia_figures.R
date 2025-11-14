# Combines GCAM and ambrosia results for specific scenarios and saves them to files;
# creates scatter plots comparing various results between the two scenarios and saves
# them as pdfs

# Load libraries
library(tidyverse)
library(patchwork)

# source files
source("R/gcam_amb_comparison_plot_functions.R")
source("R/min_calorie_adjustment.R")

# define sub-directories of data/processed to use
procdata_dir <- "update9_cnstrlam_agg32FE_24jan25"
procdata_subdir_ens_bc <- "ens_bc_20251009_221027"
procdata_subdir_RefML_MLparams <- "MLparams_bc_20251028_092955"
procdata_subdir_RefML_HDparams <- "HDparams_bc_20251028_093125"
procdata_subdir_RefML_LDparams <- "LDparams_bc_20251028_093042"
procdata_subdir_RefHD_HDparams <- "HDparams_bc_20251028_093223"
procdata_subdir_RefLD_LDparams <- "LDparams_bc_20251028_093306"
gcam_results_dir <- "results_gcam"

# define regions to run over
reg_list = c(1:29, 31, 32) # skip Taiwan (region 30), no future projections

# define minimum food demand level and allocation threshold
# Qs_floor <- 0.6
# Qn_floor <- 0.01
# alloc_ratio <- 1.1

# load ML and HD/LD parameter data including iteration #s
load(file.path("data", "processed", procdata_dir, "params_ML_intervals_global.RData"))
iter_table_HDLD <- read.csv(file.path("data", "processed", procdata_dir, "Ref_ML_gcam",
                                      procdata_subdir_ens_bc, "iter_table_HDLD_Qtot.csv"))

# identify iterations associated with ML and global HD and LD parameters
iter_ML <- params_ML_intervals_global[params_ML_intervals_global$measure == "ML", "iteration"]
iter_HD <- iter_table_HDLD[iter_table_HDLD$ID == 33, "HD_Qtot"]
iter_LD <- iter_table_HDLD[iter_table_HDLD$ID == 33, "LD_Qtot"]


# indicate whether to read in new GCAM and ambrosia results, combine and save
# them, or to read in results that have already been combined and saved
# also indicate whether to generate plots
combine_new_results <- TRUE
make_plots <- TRUE

if(combine_new_results) {
  
  # GCAM results; read from file produced by the 101 script
  
  # Ref ML
  gcamoutput_Ref_ML <- 
    readRDS(file.path("data", "processed", procdata_dir, gcam_results_dir,
                      "gcamoutput_Ref_ML.RDS"))
  
  # Ref HD
  gcamoutput_Ref_HD <- 
    readRDS(file.path("data", "processed", procdata_dir, gcam_results_dir,
                      "gcamoutput_Ref_HD.RDS"))
  
  # Ref LD
  gcamoutput_Ref_LD <-
    readRDS(file.path("data", "processed", procdata_dir, gcam_results_dir,
                      "gcamoutput_Ref_LD.RDS"))
  
  # modify gcam results by replacing bias terms with results sent separately by Kanishka; 
  # done Oct 17 2025 since most recent gcam results did not include correct bias
  # terms
  gcam_bias_ML <- read_csv("H:/My Drive/R projects/food_demand/food_demand_uncertainty/notes/pc_bias_tableML.csv")
  gcam_bias_HD <- read_csv("H:/My Drive/R projects/food_demand/food_demand_uncertainty/notes/pc_bias_tableHD_Qtot.csv")
  
  gcamoutput_Ref_ML <- merge(gcamoutput_Ref_ML, gcam_bias_ML, by = "region") %>%
    mutate(RBs = bias_adder_staples,
           RBn = bias_adder_nonstaples) %>%
    select(-bias_adder_staples, -bias_adder_nonstaples)
  gcamoutput_Ref_HD <- merge(gcamoutput_Ref_HD, gcam_bias_ML, by = "region") %>%
    mutate(RBs = bias_adder_staples,
           RBn = bias_adder_nonstaples) %>%
    select(-bias_adder_staples, -bias_adder_nonstaples)
  
  # ambrosia results
  # for Ref_ML, select from the bias-corrected ensemble 
  # for Ref_HD/LD, for now read from individual files produced by calculate_demand.R
  
  # Ref ML, with MDparams, HDparams, LDparams
  # amboutput_Ref_ML <- map_dfr(reg_list, function(r) {
  #   readRDS(file.path("data", "processed", procdata_dir, "Ref_ML_gcam",
  #                     paste0("demand_R", r, "_ens_bc_6jul.RDS"))) %>%
  #     filter(iteration %in% c(iter_ML, iter_HD, iter_LD))
  #   })
  # amboutput_Ref_ML_MLparams <- amboutput_Ref_ML %>% filter(iteration == iter_ML) 
  # amboutput_Ref_ML_HDparams <- amboutput_Ref_ML %>% filter(iteration == iter_HD)
  # amboutput_Ref_ML_LDparams <- amboutput_Ref_ML %>% filter(iteration == iter_LD)
  # rm(amboutput_Ref_ML)
  amboutput_Ref_ML_MLparams <- map_dfr(reg_list, function(r) {
    readRDS(file.path("data", "processed", procdata_dir, "Ref_ML_gcam", 
                      procdata_subdir_RefML_MLparams,
                      paste0("demand_R", r, "_MLparams_bc.RDS")))
  })
  amboutput_Ref_ML_HDparams <- map_dfr(reg_list, function(r) {
    readRDS(file.path("data", "processed", procdata_dir, "Ref_ML_gcam", 
                      procdata_subdir_RefML_HDparams,
                      paste0("demand_R", r, "_HDparams_bc.RDS")))
  })
  amboutput_Ref_ML_LDparams <- map_dfr(reg_list, function(r) {
    readRDS(file.path("data", "processed", procdata_dir, "Ref_ML_gcam", 
                      procdata_subdir_RefML_LDparams,
                      paste0("demand_R", r, "_LDparams_bc.RDS")))
  })
  
  if(TRUE) {
    
  # Ref HD
  amboutput_Ref_HD_HDparams <- map_dfr(reg_list, function(r) {
    readRDS(file.path("data", "processed", procdata_dir, "Ref_HD_gcam", 
                      procdata_subdir_RefHD_HDparams,
                      paste0("demand_R", r, "_HDparams_bc.RDS")))})
  
  # Ref LD
  amboutput_Ref_LD_LDparams <- map_dfr(reg_list, function(r) {
    readRDS(file.path("data", "processed", procdata_dir, "Ref_LD_gcam", 
                      procdata_subdir_RefLD_LDparams,
                      paste0("demand_R", r, "_LDparams_bc.RDS")))})
  
  }
  
  # merge gcam and ambrosia results into single df

  # Ref ML
  gcam_amb_output_Ref_ML <- 
    inner_join(gcamoutput_Ref_ML, amboutput_Ref_ML_MLparams,
               by = c("GCAM_region_ID","region","gcam-consumer","year"),
               suffix = c("_gcam","_amb"))
  
  if(TRUE) {
    
  # Ref HD
  gcam_amb_output_Ref_HD <- 
    inner_join(gcamoutput_Ref_HD, amboutput_Ref_HD_HDparams,
               by = c("GCAM_region_ID","region","gcam-consumer","year"),
               suffix = c("_gcam","_amb"))
  # Ref LD
  gcam_amb_output_Ref_LD <-
    inner_join(gcamoutput_Ref_LD, amboutput_Ref_LD_LDparams,
               by = c("GCAM_region_ID","region","gcam-consumer","year"),
               suffix = c("_gcam","_amb"))
  
  # Ref ML with HD params
  gcam_amb_output_Ref_ML_HDparams <-
    inner_join(gcamoutput_Ref_HD, amboutput_Ref_ML_HDparams,
               by = c("GCAM_region_ID","region","gcam-consumer","year"),
               suffix = c("_gcam","_amb"))
  
  # Ref ML with HD params test
  # replace GCAM Ref_HD with ambrosia Ref_HD to see if we get the same result
  gcam_amb_output_Ref_ML_HDparams_test <-
    inner_join(amboutput_Ref_HD_HDparams, amboutput_Ref_ML_HDparams,
               by = c("GCAM_region_ID","region","gcam-consumer","year"),
               suffix = c("_gcam","_amb"))
  
  # Ref ML with LD params
  gcam_amb_output_Ref_ML_LDparams <-
    inner_join(gcamoutput_Ref_LD, amboutput_Ref_ML_LDparams,
               by = c("GCAM_region_ID","region","gcam-consumer","year"),
               suffix = c("_gcam","_amb"))

  # Ref ML with LD params test
  # replace GCAM Ref_LD with ambrosia Ref_LD to see if we get the same result
  gcam_amb_output_Ref_ML_LDparams_test <-
    inner_join(amboutput_Ref_LD_LDparams, amboutput_Ref_ML_LDparams,
               by = c("GCAM_region_ID","region","gcam-consumer","year"),
               suffix = c("_gcam","_amb"))
  
  }
 
  # save results
  saveRDS(gcam_amb_output_Ref_ML,
          file.path("data", "processed", procdata_dir, "Ref_ML_gcam", 
                    procdata_subdir_RefML_MLparams,
                    "gcam_amb_output_Ref_ML_MLparams.RDS"))

  if(TRUE) {
    
  saveRDS(gcam_amb_output_Ref_HD,
          file.path("data", "processed", procdata_dir, "Ref_HD_gcam", 
                    procdata_subdir_RefHD_HDparams,
                    "gcam_amb_output_Ref_HD_HDparams.RDS"))
  saveRDS(gcam_amb_output_Ref_LD,
          file.path("data", "processed", procdata_dir, "Ref_LD_gcam", 
                    procdata_subdir_RefLD_LDparams,
                    "gcam_amb_output_Ref_LD_LDparams.RDS"))
  saveRDS(gcam_amb_output_Ref_ML_HDparams,
          file.path("data", "processed", procdata_dir, "Ref_ML_gcam", 
                    procdata_subdir_RefML_HDparams,
                    "gcam_amb_output_Ref_ML_HDparams.RDS"))
  saveRDS(gcam_amb_output_Ref_ML_HDparams_test,
          file.path("data", "processed", procdata_dir, "Ref_ML_gcam", 
                    procdata_subdir_RefML_HDparams,
                    "gcam_amb_output_Ref_ML_HDparams_test.RDS"))
  saveRDS(gcam_amb_output_Ref_ML_LDparams,
          file.path("data", "processed", procdata_dir, "Ref_ML_gcam", 
                    procdata_subdir_RefML_LDparams,
                    "gcam_amb_output_Ref_ML_LDparams.RDS"))
  saveRDS(gcam_amb_output_Ref_ML_LDparams_test,
          file.path("data", "processed", procdata_dir, "Ref_ML_gcam", 
                    procdata_subdir_RefML_LDparams,
                    "gcam_amb_output_Ref_ML_LDparams_test.RDS"))
  
  }

  } else {
    
  # load combined gcam/ambrosia results that have already been calculated and
  # saved in the code below
  gcam_amb_output_Ref_ML <- 
    readRDS(file.path("data", "processed", procdata_dir, "Ref_ML_gcam", 
                      procdata_subdir_RefML_MLparams,
                      "gcam_amb_output_Ref_ML_MLparams.RDS"))
  gcam_amb_output_Ref_HD <- 
    readRDS(file.path("data", "processed", procdata_dir, "Ref_HD_gcam",
                      procdata_subdir_RefHD_HDparams,
                      "gcam_amb_output_Ref_HD_HDparams.RDS"))
  gcam_amb_output_Ref_LD <-
    readRDS(file.path("data", "processed", procdata_dir, "Ref_LD_gcam", 
                      procdata_subdir_RefLD_LDparams,
                      "gcam_amb_output_Ref_LD_LDparams.RDS"))
  gcam_amb_output_Ref_ML_HDparams <- 
    readRDS(file.path("data", "processed", procdata_dir, "Ref_ML_gcam", 
                      procdata_subdir_RefML_HDparams,
                      "gcam_amb_output_Ref_ML_HDparams.RDS"))
  gcam_amb_output_Ref_ML_HDparams_test <- 
    readRDS(file.path("data", "processed", procdata_dir,  "Ref_ML_gcam", 
                      procdata_subdir_RefML_HDparams,
                     "gcam_amb_output_Ref_ML_HDparams_test.RDS"))
  gcam_amb_output_Ref_ML_LDparams <- 
    readRDS(file.path("data", "processed", procdata_dir, "Ref_ML_gcam", 
                      procdata_subdir_RefML_LDparams,
                      "gcam_amb_output_Ref_ML_LDparams.RDS"))
  gcam_amb_output_Ref_ML_LDparams_test <- 
    readRDS(file.path("data", "processed", procdata_dir, "Ref_ML_gcam", 
                      procdata_subdir_RefML_LDparams,
                      "gcam_amb_output_Ref_ML_LDparams_test.RDS"))
  }

# plot comparison of results for decile demand, regional demand, and decile bias
# adders; saved to pdf in output/reports

if(make_plots) {

# Ref_ML
plot_gcam_amb_comparison(
  df = gcam_amb_output_Ref_ML,
  base_year = 2015,
  end_year = 2100,
  output_subdir = procdata_subdir_RefML_MLparams,
  output_file = "gcam_amb_comparison_Ref_ML_MLparams.pdf")

if(TRUE) {
  
# Ref_HD
plot_gcam_amb_comparison(
  df = gcam_amb_output_Ref_HD,
  base_year = 2015,
  end_year = 2100,
  output_subdir = procdata_subdir_RefML_MLparams,
  output_file = "gcam_amb_comparison_Ref_HD_HDparams.pdf")
# Ref_LD
plot_gcam_amb_comparison(
  df = gcam_amb_output_Ref_LD,
  base_year = 2015,
  end_year = 2100,
  output_subdir = procdata_subdir_RefML_MLparams,
  output_file = "gcam_amb_comparison_Ref_LD_LDparams.pdf")
# Ref_ML_HDparams
plot_gcam_amb_comparison(
  df = gcam_amb_output_Ref_ML_HDparams,
  base_year = 2015,
  end_year = 2100,
  output_subdir = procdata_subdir_RefML_MLparams,
  output_file = "gcam_amb_comparison_Ref_ML_HDparams.pdf")
# Ref_ML_HDparams_test
plot_gcam_amb_comparison(
  df = gcam_amb_output_Ref_ML_HDparams_test,
  base_year = 2015,
  end_year = 2100,
  output_subdir = procdata_subdir_RefML_MLparams,
  output_file = "gcam_amb_comparison_Ref_ML_HDparams_test.pdf")
# Ref_ML_LDparams
plot_gcam_amb_comparison(
  df = gcam_amb_output_Ref_ML_LDparams,
  base_year = 2015,
  end_year = 2100,
  output_subdir = procdata_subdir_RefML_MLparams,
  output_file = "gcam_amb_comparison_Ref_ML_LDparams.pdf")
# Ref_ML_LDparams_test
plot_gcam_amb_comparison(
  df = gcam_amb_output_Ref_ML_LDparams,
  base_year = 2015,
  end_year = 2100,
  output_subdir = procdata_subdir_RefML_MLparams,
  output_file = "gcam_amb_comparison_Ref_ML_LDparams_test.pdf")

# plot comparison of results between two GCAM scenarios for decile demand, 
# regional demand, and prices; saved to pdf in output/reports

plot_gcam_scenario_comparison(
  df1 = gcamoutput_Ref_ML,
  df2 = gcamoutput_Ref_HD,
  base_year = 2015,
  end_year = 2100,
  output_file = "gcam_comparison_RefML_RefHD.pdf",
  group_filter_num = 1  # Use group 1 for region-level and price-level data
)

plot_gcam_scenario_comparison(
  df1 = gcamoutput_Ref_ML,
  df2 = gcamoutput_Ref_LD,
  base_year = 2015,
  end_year = 2100,
  output_file = "gcam_comparison_RefML_RefLD.pdf",
  group_filter_num = 1  # Use group 1 for region-level and price-level data
)
}

}

# manually do various kinds of checking of results

if (FALSE) {
  
  # identify outliers in Ref_ML results
  
  # identify rows with NAs in GCAM demand
  # note that Kanishka suggests these are likely cases where the GCAM result was
  # zero but it wrote out NA to avoid divide by zero errors; this is consistent with 
  # the fact that the ambrosia results in these cases are zero or v close to zero
  gcam_amb_output_Ref_ML %>% filter(is.na(Qs_gcam) | is.na(Qn_gcam))-> gcam_amb_output_Ref_ML_NAs
  gcam_amb_output_Ref_HD %>% filter(is.na(Qs_gcam) | is.na(Qn_gcam)) -> gcam_amb_output_Ref_HD_NAs
  gcam_amb_output_Ref_LD %>% filter(is.na(Qs_gcam) | is.na(Qn_gcam)) -> gcam_amb_output_Ref_LD_NAs
  gcam_amb_output_Ref_ML_HDparams %>% filter(is.na(Qs_gcam) | is.na(Qn_gcam)) -> gcam_amb_output_Ref_ML_HDparams_NAs
  gcam_amb_output_Ref_ML_LDparams %>% filter(is.na(Qs_gcam) | is.na(Qn_gcam)) -> gcam_amb_output_Ref_ML_LDparams_NAs
  
  # save to send to Kanishka
  saveRDS(gcam_amb_output_Ref_ML_HDparams,
          file=paste("data/processed",procdata_dir,
                     "Ref_ML_gcam/gcam_amb_output_Ref_ML_HDparams.RDS",sep="/"))
  
  gcam_amb_output_Ref_ML %>% filter(year == 2015, Qs_amb < 0.6) -> tmp
  gcam_amb_output_Ref_ML %>% filter(year == 2015, Qn_amb < 0.1) -> tmpQn
  gcam_amb_output_Ref_ML %>% filter(year == 2015, RBs_gcam > 0.4,
                                    RBs_amb < 0.3) -> tmp2
  gcam_amb_output_Ref_ML %>% filter(year == 2015, region == "Africa_Western") -> tmp3
  gcam_amb_output_Ref_ML %>% filter(year == 2015, abs(RBs_amb) - abs(RBs_gcam) > 0.05) -> tmp4
  
  gcam_amb_output_Ref_ML_HDparams %>% filter(year == 2100, 
                                             `gcam-consumer` == "FoodDemand_Group5") %>%
    select(region, `gcam-consumer`, year, Y.region_gcam, Y.region_amb, Y_gcam, Y_amb,
           Ps_gcam, Ps_amb, Pn_gcam, Pn_amb) -> tmp
  
  gcam_amb_output_Ref_ML_HDparams %>% filter(year == 2015) %>%
    select(region, `gcam-consumer`, year, RBs_gcam, RBs_amb, RBn_gcam, RBn_amb) -> tmp

    gcam_amb_output_Ref_ML_HDparams %>% filter(year == 2015,RBs_gcam < -0.4 &
                                               RBs_gcam > -0.5) -> tmp
  gcam_amb_output_Ref_ML_HDparams %>% filter(year == 2015,RBn_amb < -1) -> tmp
  gcam_amb_output_Ref_ML_HDparams %>% filter(year == 2015,RBs_amb < -0.7) -> tmp2
  gcam_amb_output_Ref_ML_HDparams %>% 
    filter(Qn.region_amb/Qn.region_gcam > 1.1, Qn.region_gcam > 2,
           `gcam-consumer` == "FoodDemand_Group1") %>%
    arrange(region,year) -> tmp3
  gcam_amb_output_Ref_ML_HDparams %>% 
    filter(Qs.region_gcam > 1.1, Qs.region_amb < 0.8,
           `gcam-consumer` == "FoodDemand_Group1") %>%
    arrange(region,year) -> tmp4
  
  # check absolute values of base year demand
  gcam_amb_output_Ref_ML %>% 
    filter(year == 2015, 
           `gcam-consumer` == "FoodDemand_Group1") %>%
    select(region, year, Qs.region_gcam, Qs.region_amb, 
           Qn.region_gcam, Qn.region_amb) -> tmp
  # get current bias corrected results to check their base year values
  amb_ens_bc_Ref_ML <- bind_rows(
    readRDS(
    paste0("data/processed/", procdata_dir, "/Ref_ML_gcam/demand_R1_ens_bc_10jun.RDS")
    ),
    readRDS(
      paste0("data/processed/", procdata_dir, "/Ref_ML_gcam/demand_R2_ens_bc_10jun.RDS")
    )) %>%
    filter(year == 2015, `gcam-consumer` == "FoodDemand_Group1") %>%
    select(region, year, Qs.region, Qn.region) %>%
    group_by(region,)
  
  
  # identify outliers and cases of good fit
  gcam_amb_output_Ref_ML %>% filter(Qs_amb/Qs_gcam < 0.8) -> tmp
  gcam_amb_output_Ref_ML %>% filter(Qn_amb/Qn_gcam < 0.8) -> tmp2
  gcam_amb_output_Ref_ML %>% filter(abs(Qs_amb/Qs_gcam -1) < 0.005) -> tmp3
  gcam_amb_output_Ref_ML %>% filter(abs(Qn_amb/Qn_gcam -1) < 0.005) -> tmp4
  
  gcam_amb_output_Ref_ML %>% filter(RBs_amb/RBs_gcam < 0.90 & year == 2015) -> tmp
  gcam_amb_output_Ref_ML %>% filter((region == "Africa_Eastern" | 
                                       region == "Africa_Southern") & 
                                      year == 2015) -> tmp2
  
  gcam_amb_output_Ref_ML %>% filter(Qs_amb/Qs_gcam < 0.95 & year >= 2015)
  gcam_amb_output_Ref_ML %>% filter(Qs_amb/Qs_gcam > 1.05 & year >= 2015)
  
  gcam_amb_output_Ref_ML %>% filter(Qn_amb/Qn_gcam < 0.9 & year >= 2015)
  gcam_amb_output_Ref_ML %>% filter(Qn_amb/Qn_gcam > 1.05 & year >= 2015)
  
  gcam_amb_output_Ref_ML %>% filter((Qn_amb/Qn_gcam < 0.95 | 
                                       Qn_amb/Qn_gcam > 1.05) & year < 2015)
  gcam_amb_output_Ref_ML %>% filter(Qn_amb/Qn_gcam > 1.05 & year >= 2015)
  
  tmp <- gcam_amb_output_Ref_HD %>% filter(is.na(Qs_gcam) | is.na(Qn_gcam))
  
  tmp2 <- gcam_amb_output_Ref_HD %>% filter(!is.na(Qs_gcam) & !is.na(Qn_gcam),
                                            Qs_amb < 0 | Qn_amb < 0,
                                            year >= 2015)
  
  gcam_amb_output_Ref_HD %>% filter(!is.na(Qs_gcam) & !is.na(Qn_gcam),
                                    Qs_amb < 0)
  
  tmpLD <- gcam_amb_output_Ref_LD %>% filter(is.na(Qs_gcam) | is.na(Qn_gcam))
  
  
  gcam_amb_output_Ref_HD %>% filter(Qs_amb < 0.7 & year > 2015)
  gcam_amb_output_Ref_HD %>% filter(Qn_amb < 0.1 & year > 2015)
  gcam_amb_output_Ref_HD %>% filter(Qs_gcam > 1.63 & Qs_gcam < 1.7 & year > 2015) -> tmp
  gcam_amb_output_Ref_HD %>% filter(Qs.region_amb < 1.45 & Qs.region_gcam > 1.45 & year > 2015) -> tmp2
  
  gcam_amb_output_Ref_LD %>% filter(Qs.region_amb/Qs.region_gcam > 1.05 | 
                                      Qs.region_amb/Qs.region_gcam < 0.95) -> tmp
  
  
  gcam_amb_output_Ref_HD %>% filter(alpha.s + alpha.n > 0.5, year > 1975)
  gcam_amb_output_Ref_HD %>% filter(alpha.s + alpha.n > 0.5)
  gcam_amb_output_Ref_HD %>% filter(alpha.n > 0.5)
  gcam_amb_output_Ref_HD %>% filter(region == "India",
                                    `gcam-consumer` == "FoodDemand_Group1")
  
  gcam_amb_output_Ref_HD %>% 
    filter(Qs_amb * 365 * Ps + Qn_amb * 365 * Pn > Y, year > 1975)
  
  gcam_amb_output_Ref_HD2 %>% 
    filter(Qn_amb < -5, year > 1990)
  
  
  
  # identify extreme outlier in ambrosia bias-corrected Qs; it's S Africa in 1990, 
  # food group 1; Qs_amb = ~ -444.9, RBs = -445.3; so this implies that 
  # pre-bias-corrected Qs_amb is ~ 0.4; there must be a mistake in either Qs_gcam,
  # or in the calculation of the bias correction; but Qs_gcam is 0.508 so that
  # seems to be ok, it must be a problem with RBs
  gcam_amb_output_Ref_ML %>% filter(Qs_amb < -100)
  # check pre-bias-corrected S Africa; it is Qs = 0.37 as expected
  gcam_amb_output_Ref_ML %>% filter(region == "South Africa",Y == 0.0546762)
  # just to be sure, check observations for S Africa; regional average Qs is 
  # 1.57, with Y of 8.08, so looks ok; must be a mistake in calculating the RBs
  load(paste(analysis_dir,input_path,"obs_data.Rdata",sep="/"))
  obs_data %>% filter(GCAM_region_ID == 24,year == 1990)
  
}


