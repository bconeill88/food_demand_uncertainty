# plots and analysis of ambrosia v gcam results

# Load libraries
library(tidyverse)
library(patchwork)

# source files
source("R/gcam_amb_comparison_plot_functions.R")

# define sub-directories of data/processed to use
procdata_dir <- "update9_cnstrlam_agg32FE_24jan25"
gcam_results_dir <- "results_gcam"

# define regions to run over
reg_list = c(1:32)

# indicate whether to read in new GCAM and ambrosia results, combine and save
# them, or to read in results that have already been combined and saved
combine_new_results <- FALSE

if(combine_new_results) {
  
  # GCAM results; read from file produced by the 101 script
  
  # Ref ML
  gcamoutput_Ref_ML <- 
    readRDS(paste("data/processed",procdata_dir,gcam_results_dir,
                  "gcamoutput_Ref_ML.RDS",sep="/"))
  
  # Ref HD
  gcamoutput_Ref_HD <- 
    readRDS(paste("data/processed",procdata_dir,gcam_results_dir,
                  "gcamoutput_Ref_HD.RDS",sep="/"))
  
  # # Ref LD
  # gcamoutput_Ref_LD <- 
  #   readRDS(paste("data/processed",procdata_dir,gcam_results_dir,
  #                 "gcamoutput_Ref_LD.RDS",sep="/"))
  
  # ambrosia results; read from file produced by the 03 script (via food.dmnd.wrapper())
  
  # Ref ML
  amboutput_Ref_ML_MLparams <- map_dfr(reg_list, function(r) {
    readRDS(paste0("data/processed/",procdata_dir,
                   "/Ref_ML_gcam/demand_R",r,"_MLparams.RDS"))})
  
  # Ref HD
  amboutput_Ref_HD_HDparams <- map_dfr(reg_list, function(r) {
    readRDS(paste0("data/processed/",procdata_dir,
                   "/Ref_HD_gcam/demand_R",r,"_HDparams.RDS"))})
  
  # # Ref LD
  # amboutput_Ref_LD_LDparams <- map_dfr(reg_list, function(r) {
  #   readRDS(paste0("data/processed/",procdata_dir,
  #                  "/Ref_LD_gcam/demand_R",r,"_LDparams.RDS"))
  
  # Ref ML with HD params
  amboutput_Ref_ML_HDparams <- map_dfr(reg_list, function(r) {
    readRDS(paste0("data/processed/",procdata_dir,
                   "/Ref_ML_gcam/demand_R",r,"_HDparams.RDS"))})
  
  # merge gcam and ambrosia results into single df
  
  # Ref ML
  gcam_amb_output_Ref_ML <- 
    inner_join(gcamoutput_Ref_ML,amboutput_Ref_ML_MLparams,
               by = c("GCAM_region_ID","region","gcam-consumer","year","Pn","Ps",
                      "Y"),
               suffix = c("_gcam","_amb"))
  # Ref HD
  gcam_amb_output_Ref_HD <- 
    inner_join(gcamoutput_Ref_HD,amboutput_Ref_HD_HDparams,
               by = c("GCAM_region_ID","region","gcam-consumer","year","Pn","Ps",
                      "Y"),
               suffix = c("_gcam","_amb"))
  # # Ref LD
  # gcam_amb_output_Ref_LD <- 
  #   inner_join(gcamoutput_Ref_LD,amboutput_Ref_LD_LDparams,
  #              by = c("GCAM_region_ID","region","gcam-consumer","year","Pn","Ps",
  #                     "Y","RBs","RBn"),
  #              suffix = c("_gcam","_amb"))
  
  # Ref ML with HD params
  # note: prices will differ, so don't join by them
  gcam_amb_output_Ref_ML_HDparams <-
    inner_join(gcamoutput_Ref_HD,amboutput_Ref_ML_HDparams,
               by = c("GCAM_region_ID","region","gcam-consumer","year","Y"),
               suffix = c("_gcam","_amb"))
  
  # save results
  saveRDS(gcam_amb_output_Ref_ML,
          file=paste("data/processed",procdata_dir,
                     "Ref_ML_gcam/gcam_amb_output_Ref_ML.RDS",sep="/"))
  saveRDS(gcam_amb_output_Ref_HD,
          file=paste("data/processed",procdata_dir,
                     "Ref_HD_gcam/gcam_amb_output_Ref_HD.RDS",sep="/"))
  # saveRDS(gcam_amb_output_Ref_LD,
  #      file=paste("data/processed",procdata_dir,
  #                 "Ref_LD_gcam/gcam_amb_output_Ref_LD.RDS",sep="/"))
  saveRDS(gcam_amb_output_Ref_ML_HDparams,
          file=paste("data/processed",procdata_dir,
                     "Ref_ML_gcam/gcam_amb_output_Ref_ML_HDparams.RDS",sep="/"))
} else {
    
  # load combined gcam/ambrosia results that have already been calculated and
  # saved in the code below
  gcam_amb_output_Ref_ML <- 
    readRDS(paste("data/processed",procdata_dir,
                  "Ref_ML_gcam/gcam_amb_output_Ref_ML.RDS",sep="/"))
  gcam_amb_output_Ref_HD <- 
    readRDS(paste("data/processed",procdata_dir,
                  "Ref_HD_gcam/gcam_amb_output_Ref_HD.RDS",sep="/"))
  # gcam_amb_output_Ref_LD <- 
  #   readRDS(paste("data/processed",procdata_dir,
  #                 "Ref_LD_gcam/gcam_amb_output_Ref_LD.RDS",sep="/"))
  gcam_amb_output_Ref_ML_HDparams <- 
    readRDS(paste("data/processed",procdata_dir,
                  "Ref_ML_gcam/gcam_amb_output_Ref_ML_HDparams.RDS",sep="/"))
}

# plot comparison of results for decile demand, regional demand, and decile bias
# adders; saved to pdf in output/reports

# Ref_ML
plot_gcam_amb_comparison(
  df = gcam_amb_output_Ref_ML,
  base_year = 2015,
  end_year = 2100,
  output_file = "gcam_amb_comparison_Ref_ML.pdf")
# Ref_HD
plot_gcam_amb_comparison(
  df = gcam_amb_output_Ref_HD,
  base_year = 2015,
  end_year = 2100,
  output_file = "gcam_amb_comparison_Ref_HD.pdf")
# # Ref_LD
# plot_gcam_amb_comparison(
#   df = gcam_amb_output_Ref_LD,
#   base_year = 2015,
#   end_year = 2100,
#   output_file = "gcam_amb_comparison_Ref_LD.pdf")
# Ref_ML_HDparams
plot_gcam_amb_comparison(
  df = gcam_amb_output_Ref_ML_HDparams,
  base_year = 2015,
  end_year = 2100,
  output_file = "gcam_amb_comparison_Ref_ML_HDparams.pdf")

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

# manually do various kinds of checking of results

if (FALSE) {
  
  # identify outliers in Ref_ML results
  
  # identify rows with NAs in GCAM demand
  # note that Kanishka suggests these are likely cases where the GCAM result was
  # zero but it wrote out NA to avoid divide by zero errors; this is consistent with 
  # the fact that the ambrosia results in these cases are zero or v close to zero
  gcam_amb_output_Ref_ML %>% filter(is.na(Qs_gcam) | is.na(Qn_gcam))
  gcam_amb_output_Ref_HD %>% filter(is.na(Qs_gcam) | is.na(Qn_gcam)) -> gcam_amb_output_Ref_HD_NAs
  gcam_amb_output_Ref_ML_HDparams %>% filter(is.na(Qs_gcam) | is.na(Qn_gcam))
  
  # save to send to Kanishka
  saveRDS(gcam_amb_output_Ref_HD_NAs,
          file=paste("data/processed",procdata_dir,
                     "Ref_HD_gcam/gcam_amb_output_Ref_HD_NAs.RDS",sep="/"))
  
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
  
  
  gcam_amb_output_Ref_HD %>% filter(Qn_amb < -5 & year > 1975)
  gcam_amb_output_Ref_LD %>% filter(Qs_gcam > 6 & year > 1975)
  
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


