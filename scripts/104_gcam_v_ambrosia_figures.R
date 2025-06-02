# plots and analysis of ambrosia v gcam results

# Load libraries
library(tidyverse)

# define sub-directories of data/processed to use
procdata_dir <- "update9_cnstrlam_agg32FE_24jan25"
gcam_results_dir <- "results_gcam"

# define regions to run over
reg_list = c(1:32)

# load combined gcam/ambrosia results that have already been calculated and saved
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

# GCAM results; read, rename, add shares, add regional mean consumption
# note: need to convert all this to a function and apply to each gcam 
# output file; or better, maybe do it all in the 101 script; only need
# renaming here for the purpose of comparison to ambrosia

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

# ambrosia results; by region and concatenate

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
# note: keep bias adders from both to compare

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

# check ambrosia Ref_ML_HDparams bias terms against gcam data system
gcam_bias <- read.csv("data/raw/pc_regional_bias.csv") %>%
  select(region, year, gcam.consumer, input, partial_difference) %>%
  pivot_wider(names_from = input, values_from = partial_difference) %>%
  group_by(region, year) %>%
  summarise(across(everything(), first), .groups = "drop") %>%
  select(-gcam.consumer) %>%
  rename(RBs_datasys = FoodDemand_Staples, RBn_datasys = FoodDemand_NonStaples) %>%
  mutate(RBs_datasys = RBs_datasys * 10^-3, RBn_datasys = RBn_datasys * 10^-3)
ambrosia_bias <- amboutput_Ref_ML_HDparams %>%
  group_by(region, year) %>%
  summarise(across(everything(), first), .groups = "drop") %>%
  select(region,year,RBs,RBn) %>%
  rename(RBs_amb = RBs, RBn_amb = RBn)
bias_gcam_v_amb <- left_join(gcam_bias,ambrosia_bias,
                             by = c("region","year"))

bias_gcam_v_amb %>% filter(year > 1975, abs(RBs_datasys - RBs_amb) > 0.05)
bias_gcam_v_amb %>% filter(year > 1975, abs(RBn_datasys - RBn_amb) > 0.1)
# RBs scatter plot
bias_gcam_v_amb %>% 
  filter(year > 1975) %>%
  ggplot() +
  geom_point(aes(x=RBs_datasys,y=RBs_amb)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Ref_ML_HDparams, RBs, 1975 < year <= 2015")
ggsave("bias_HDparams_RBs_amb_vs_datasys_hist.png",
       path = paste("output/figures",procdata_dir,"Ref_ML_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# RBn scatter plot
bias_gcam_v_amb %>% 
  filter(year > 1975) %>%
  ggplot() +
  geom_point(aes(x=RBn_datasys,y=RBn_amb)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Ref_ML_HDparams, RBn, 1975 < year <= 2015")
ggsave("bias_HDparams_RBn_amb_vs_datasys_hist.png",
       path = paste("output/figures",procdata_dir,"Ref_ML_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# Ref_ML plots

# Qs scatter plot - base year
gcam_amb_output_Ref_ML %>% 
  filter(year == 2015) %>%
  ggplot() +
  geom_point(aes(x=Qs_gcam,y=Qs_amb)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Ref_ML, Qs, year == 2015")
ggsave("demand_Qs_amb_vs_gcam_baseyr.png",
       path = paste("output/figures",procdata_dir,"Ref_ML_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# Qn scatter plot - base year
gcam_amb_output_Ref_ML %>% 
  filter(year == 2015) %>%
  ggplot() +
  geom_point(aes(x=Qn_gcam,y=Qn_amb)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Ref_ML, Qn, year == 2015")
ggsave("demand_Qn_amb_vs_gcam_baseyr.png",
       path = paste("output/figures",procdata_dir,"Ref_ML_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# Qs scatter plot - projected
gcam_amb_output_Ref_ML %>% 
  filter(year >=2015) %>%
  ggplot() +
  geom_point(aes(x=Qs_gcam,y=Qs_amb)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Ref_ML, Qs, year >= 2015")
ggsave("demand_Qs_amb_vs_gcam_proj.png",
       path = paste("output/figures",procdata_dir,"Ref_ML_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# Qn scatter plot - projected
gcam_amb_output_Ref_ML %>% 
  filter(year >= 2015) %>%
  ggplot() +
  geom_point(aes(x=Qn_gcam,y=Qn_amb)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Ref_ML, Qn, year >= 2015")
ggsave("demand_Qn_amb_vs_gcam_proj.png",
       path = paste("output/figures",procdata_dir,"Ref_ML_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# RBs scatter plot - historical
gcam_amb_output_Ref_ML %>% 
  filter(year <= 2015) %>%
  ggplot() +
  geom_point(aes(x=RBs_gcam,y=RBs_amb)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Ref_ML, RBs, year <= 2015")
ggsave("bias_RBs_amb_vs_gcam_hist.png",
       path = paste("output/figures",procdata_dir,"Ref_ML_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# RBn scatter plot - historical
gcam_amb_output_Ref_ML %>% 
  filter(year <= 2015) %>%
  ggplot() +
  geom_point(aes(x=RBn_gcam,y=RBn_amb)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Ref_ML, RBn, year <= 2015")
ggsave("bias_RBn_amb_vs_gcam_hist.png",
       path = paste("output/figures",procdata_dir,"Ref_ML_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# RBs scatter plot - projected
gcam_amb_output_Ref_ML %>% 
  filter(year >= 2015) %>%
  ggplot() +
  geom_point(aes(x=RBs_gcam,y=RBs_amb)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Ref_ML, RBs, year >= 2015")
ggsave("bias_RBs_amb_vs_gcam_proj.png",
       path = paste("output/figures",procdata_dir,"Ref_ML_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# RBn scatter plot - projected
gcam_amb_output_Ref_ML %>% 
  filter(year >= 2015) %>%
  ggplot() +
  geom_point(aes(x=RBn_gcam,y=RBn_amb)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Ref_ML, RBn, year >= 2015")
ggsave("bias_RBn_amb_vs_gcam_proj.png",
       path = paste("output/figures",procdata_dir,"Ref_ML_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# Ref_HD plots

# Qs scatter plot - base year
gcam_amb_output_Ref_HD %>% 
  filter(year == 2015) %>%
  ggplot() +
  geom_point(aes(x=Qs_gcam,y=Qs_amb)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Ref_HD, Qs, year == 2015")
ggsave("demand_Qs_amb_vs_gcam_baseyr.png",
       path = paste("output/figures",procdata_dir,"Ref_HD_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# Qn scatter plot - base year
gcam_amb_output_Ref_HD %>% 
  filter(year == 2015) %>%
  ggplot() +
  geom_point(aes(x=Qn_gcam,y=Qn_amb)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Ref_HD, Qn, year == 2015")
ggsave("demand_Qn_amb_vs_gcam_baseyr.png",
       path = paste("output/figures",procdata_dir,"Ref_HD_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# Qs scatter plot - projected
gcam_amb_output_Ref_HD %>%
  filter(year >= 2015, !is.na(Qs_gcam)) %>%
  ggplot() +
  geom_point(aes(x=Qs_gcam,y=Qs_amb)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Ref_HD, Qs, year >= 2015")
ggsave("demand_Qs_amb_vs_gcam_proj.png",
       path = paste("output/figures",procdata_dir,"Ref_HD_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# Qn scatter plot - projected
gcam_amb_output_Ref_HD %>%
  filter(year >= 2015, !is.na(Qn_gcam)) %>%
  ggplot() +
  geom_point(aes(x=Qn_gcam,y=Qn_amb)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Ref_HD, Qn, year >= 2015")
ggsave("demand_Qn_amb_vs_gcam_proj.png",
       path = paste("output/figures",procdata_dir,"Ref_HD_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# RBs scatter plot - historical
gcam_amb_output_Ref_HD %>% 
  filter(year <= 2015) %>%
  ggplot() +
  geom_point(aes(x=RBs_gcam,y=RBs_amb)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Ref_HD, RBs, year <= 2015")
ggsave("bias_RBs_amb_vs_gcam_hist.png",
       path = paste("output/figures",procdata_dir,"Ref_HD_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# RBn scatter plot - historical
gcam_amb_output_Ref_HD %>% 
  filter(year <= 2015) %>%
  ggplot() +
  geom_point(aes(x=RBn_gcam,y=RBn_amb)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Ref_HD, RBn, year <= 2015")
ggsave("bias_RBn_amb_vs_gcam_hist.png",
       path = paste("output/figures",procdata_dir,"Ref_HD_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# RBs scatter plot - projected
gcam_amb_output_Ref_HD %>% 
  filter(year >= 2015) %>%
  ggplot() +
  geom_point(aes(x=RBs_gcam,y=RBs_amb)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Ref_HD, RBs, year >= 2015")
ggsave("bias_RBs_amb_vs_gcam_proj.png",
       path = paste("output/figures",procdata_dir,"Ref_HD_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# RBn scatter plot - projected
gcam_amb_output_Ref_HD %>% 
  filter(year >= 2015) %>%
  ggplot() +
  geom_point(aes(x=RBn_gcam,y=RBn_amb)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Ref_HD, RBn, year >= 2015")
ggsave("bias_RBn_amb_vs_gcam_proj.png",
       path = paste("output/figures",procdata_dir,"Ref_HD_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# Ref_LD plots

# Qs scatter plot - remove single extreme outlier point
gcam_amb_output_Ref_LD %>%
  filter(year > 1975, !is.na(Qs_gcam), Qs_amb > -100) %>%
  ggplot() +
  geom_point(aes(x=Qs_gcam,y=Qs_amb)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Ref_LD, Qs, year > 1975")
ggsave("demand_Qs_amb_vs_gcam.png",
       path = paste("output/figures",procdata_dir,"Ref_LD_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# Qn scatter plot
gcam_amb_output_Ref_LD %>%
  filter(year > 1975, !is.na(Qn_gcam)) %>%
  ggplot() +
  geom_point(aes(x=Qn_gcam,y=Qn_amb)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Ref_LD, Qn, year > 1975")
ggsave("demand_Qn_amb_vs_gcam.png",
       path = paste("output/figures",procdata_dir,"Ref_LD_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# Ref_ML with HD params plots

# Qs.region scatter plot - historical
gcam_amb_output_Ref_ML_HDparams %>%
  filter(year <= 2015, !is.na(Qs_gcam)) %>%
  ggplot() +
  geom_point(aes(x=Qs.region_gcam,y=Qs.region_amb)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Ref_ML_HDparams, Qs.region, year <= 2015")
ggsave("demand_HDparams_Qsregion_amb_vs_gcam_hist.png",
       path = paste("output/figures",procdata_dir,"Ref_ML_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# Qn.region scatter plot - historical
gcam_amb_output_Ref_ML_HDparams %>%
  filter(year <= 2015, !is.na(Qn_gcam)) %>%
  ggplot() +
  geom_point(aes(x=Qn.region_gcam,y=Qn.region_amb)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Ref_ML_HDparams, Qn.region, year <= 2015")
ggsave("demand_HDparams_Qnregion_amb_vs_gcam_hist.png",
       path = paste("output/figures",procdata_dir,"Ref_ML_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# Qs.region scatter plot - base year
gcam_amb_output_Ref_ML_HDparams %>%
  filter(year == 2015, !is.na(Qs_gcam)) %>%
  ggplot() +
  geom_point(aes(x=Qs.region_gcam,y=Qs.region_amb)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Ref_ML_HDparams, Qs.region, year == 2015")
ggsave("demand_HDparams_Qsregion_amb_vs_gcam_baseyr.png",
       path = paste("output/figures",procdata_dir,"Ref_ML_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# Qn.region scatter plot - base year
gcam_amb_output_Ref_ML_HDparams %>%
  filter(year == 2015, !is.na(Qn_gcam)) %>%
  ggplot() +
  geom_point(aes(x=Qn.region_gcam,y=Qn.region_amb)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Ref_ML_HDparams, Qn.region, year == 2015")
ggsave("demand_HDparams_Qnregion_amb_vs_gcam_baseyr.png",
       path = paste("output/figures",procdata_dir,"Ref_ML_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# Qs.region scatter plot - projected
gcam_amb_output_Ref_ML_HDparams %>%
  filter(year >= 2015, !is.na(Qs_gcam)) %>%
  ggplot() +
  geom_point(aes(x=Qs.region_gcam,y=Qs.region_amb)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Ref_ML_HDparams, Qs.region, year >= 2015")
ggsave("demand_HDparams_Qsregion_amb_vs_gcam_proj.png",
       path = paste("output/figures",procdata_dir,"Ref_ML_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# Qn.region scatter plot - base year
gcam_amb_output_Ref_ML_HDparams %>%
  filter(year >= 2015, !is.na(Qn_gcam)) %>%
  ggplot() +
  geom_point(aes(x=Qn.region_gcam,y=Qn.region_amb)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Ref_ML_HDparams, Qn.region, year >= 2015")
ggsave("demand_HDparams_Qnregion_amb_vs_gcam_proj.png",
       path = paste("output/figures",procdata_dir,"Ref_ML_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# Qs scatter plot - projected
gcam_amb_output_Ref_ML_HDparams %>%
  filter(year >= 2015, !is.na(Qs_gcam)) %>%
  ggplot() +
  geom_point(aes(x=Qs_gcam,y=Qs_amb)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Ref_ML_HDparams, Qs, year >= 2015")
ggsave("demand_HDparams_Qs_amb_vs_gcam_proj.png",
       path = paste("output/figures",procdata_dir,"Ref_ML_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# Qn scatter plot - projected
gcam_amb_output_Ref_ML_HDparams %>%
  filter(year >= 2015, !is.na(Qn_gcam)) %>%
  ggplot() +
  geom_point(aes(x=Qn_gcam,y=Qn_amb)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Ref_ML_HDparams, Qn, year >= 2015")
ggsave("demand_HDparams_Qn_amb_vs_gcam_proj.png",
       path = paste("output/figures",procdata_dir,"Ref_ML_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# RBs scatter plot - historical
gcam_amb_output_Ref_ML_HDparams %>% 
  filter(year <= 2015) %>%
  ggplot() +
  geom_point(aes(x=RBs_gcam,y=RBs_amb)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Ref_ML_HDparams, RBs, year <= 2015")
ggsave("bias_HDparams_RBs_amb_vs_gcam_hist.png",
       path = paste("output/figures",procdata_dir,"Ref_ML_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# RBn scatter plot - historical
gcam_amb_output_Ref_ML_HDparams %>% 
  filter(year <= 2015) %>%
  ggplot() +
  geom_point(aes(x=RBn_gcam,y=RBn_amb)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Ref_ML_HDparams, RBn, year <= 2015")
ggsave("bias_HDparams_RBn_amb_vs_gcam_hist.png",
       path = paste("output/figures",procdata_dir,"Ref_ML_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# RBs scatter plot - base year
gcam_amb_output_Ref_ML_HDparams %>% 
  filter(year == 2015) %>%
  ggplot() +
  geom_point(aes(x=RBs_gcam,y=RBs_amb)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Ref_ML_HDparams, RBs, year == 2015")
ggsave("bias_HDparams_RBs_amb_vs_gcam_baseyr.png",
       path = paste("output/figures",procdata_dir,"Ref_ML_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# RBn scatter plot - base year
gcam_amb_output_Ref_ML_HDparams %>% 
  filter(year == 2015) %>%
  ggplot() +
  geom_point(aes(x=RBn_gcam,y=RBn_amb)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Ref_ML_HDparams, RBn, year == 2015")
ggsave("bias_HDparams_RBn_amb_vs_gcam_baseyr.png",
       path = paste("output/figures",procdata_dir,"Ref_ML_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# RBs scatter plot - projected
gcam_amb_output_Ref_ML_HDparams %>% 
  filter(year >= 2015) %>%
  ggplot() +
  geom_point(aes(x=RBs_gcam,y=RBs_amb)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Ref_ML_HDparams, RBs, year >= 2015")
ggsave("bias_HDparams_RBs_amb_vs_gcam_proj.png",
       path = paste("output/figures",procdata_dir,"Ref_ML_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# RBn scatter plot - projected
gcam_amb_output_Ref_ML_HDparams %>% 
  filter(year >= 2015) %>%
  ggplot() +
  geom_point(aes(x=RBn_gcam,y=RBn_amb)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Ref_ML_HDparams, RBn, year >= 2015")
ggsave("bias_HDparams_RBn_amb_vs_gcam_proj.png",
       path = paste("output/figures",procdata_dir,"Ref_ML_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# GCAM Ref_ML vs Ref_HD plots

# Ps scatter plot - historical
df <- data.frame(Ps_ML = gcamoutput_Ref_ML %>%
                   filter(`gcam-consumer` == "FoodDemand_Group1") %>% 
                   filter(year <= 2015) %>%
                   pull("Ps"), 
                 Ps_HD = gcamoutput_Ref_HD  %>%
                   filter(`gcam-consumer` == "FoodDemand_Group1") %>% 
                   filter(year <= 2015) %>%
                   pull("Ps"))
ggplot(df, aes(x = Ps_ML, y = Ps_HD)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "GCAM Ps, ML vs HD scenario, year <= 2015")
ggsave("Ps_HD_vs_ML_hist.png",
       path = paste("output/figures/results_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# Pn scatter plot - historical
df <- data.frame(Pn_ML = gcamoutput_Ref_ML %>%
                   filter(`gcam-consumer` == "FoodDemand_Group1") %>% 
                   filter(year <= 2015) %>%
                   pull("Pn"), 
                 Pn_HD = gcamoutput_Ref_HD  %>%
                   filter(`gcam-consumer` == "FoodDemand_Group1") %>% 
                   filter(year <= 2015) %>%
                   pull("Pn"))
ggplot(df, aes(x = Pn_ML, y = Pn_HD)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "GCAM Pn, ML vs HD scenario, year <= 2015")
ggsave("Pn_HD_vs_ML_hist.png",
       path = paste("output/figures/results_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# Ps scatter plot - projected
df <- data.frame(Ps_ML = gcamoutput_Ref_ML %>%
                   filter(`gcam-consumer` == "FoodDemand_Group1") %>% 
                   filter(year >= 2015) %>%
                   pull("Ps"), 
                 Ps_HD = gcamoutput_Ref_HD  %>%
                   filter(`gcam-consumer` == "FoodDemand_Group1") %>% 
                   filter(year >= 2015) %>%
                   pull("Ps"))
ggplot(df, aes(x = Ps_ML, y = Ps_HD)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "GCAM Ps, ML vs HD scenario, year >= 2015")
ggsave("Ps_HD_vs_ML_proj.png",
       path = paste("output/figures/results_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# Pn scatter plot - projected
df <- data.frame(Pn_ML = gcamoutput_Ref_ML %>%
                   filter(`gcam-consumer` == "FoodDemand_Group1") %>% 
                   filter(year >= 2015) %>%
                   pull("Pn"), 
                 Pn_HD = gcamoutput_Ref_HD  %>%
                   filter(`gcam-consumer` == "FoodDemand_Group1") %>% 
                   filter(year >= 2015) %>%
                   pull("Pn"))
ggplot(df, aes(x = Pn_ML, y = Pn_HD)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "GCAM Pn, ML vs HD scenario, year >= 2015")
ggsave("Pn_HD_vs_ML_proj.png",
       path = paste("output/figures/results_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# Qs.region scatter plot - historical
# filter on one consumer group to avoid redundancy
df <- data.frame(Qs.region_ML = gcamoutput_Ref_ML %>% 
                   filter(`gcam-consumer` == "FoodDemand_Group1") %>% 
                   filter(year <= 2015) %>%
                   pull("Qs.region"), 
                 Qs.region_HD = gcamoutput_Ref_HD %>% 
                   filter(`gcam-consumer` == "FoodDemand_Group1") %>% 
                   filter(year <= 2015) %>%
                   pull("Qs.region"))
ggplot(df, aes(x = Qs.region_ML, y = Qs.region_HD)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "GCAM Qs.region, ML vs HD scenario, year <= 2015")
ggsave("Qs.region_HD_vs_ML_hist.png",
       path = paste("output/figures/results_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# Qn.region scatter plot - historical
df <- data.frame(Qn.region_ML = gcamoutput_Ref_ML %>% 
                   filter(`gcam-consumer` == "FoodDemand_Group1") %>% 
                   filter(year <= 2015) %>%
                   pull("Qn.region"), 
                 Qn.region_HD = gcamoutput_Ref_HD %>% 
                   filter(`gcam-consumer` == "FoodDemand_Group1") %>% 
                   filter(year <= 2015) %>%
                   pull("Qn.region"))
ggplot(df, aes(x = Qn.region_ML, y = Qn.region_HD)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "GCAM Qn.region, ML vs HD scenario, year <= 2015")
ggsave("Qn.region_HD_vs_ML_hist.png",
       path = paste("output/figures/results_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# Qtot.region scatter plot - historical
df <- data.frame(Qtot.region_ML = gcamoutput_Ref_ML %>% 
                   filter(`gcam-consumer` == "FoodDemand_Group1") %>% 
                   filter(year <= 2015) %>%
                   pull("Qtot.region"), 
                 Qtot.region_HD = gcamoutput_Ref_HD %>% 
                   filter(`gcam-consumer` == "FoodDemand_Group1") %>% 
                   filter(year <= 2015) %>%
                   pull("Qtot.region"))
ggplot(df, aes(x = Qtot.region_ML, y = Qtot.region_HD)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "GCAM Qtot.region, ML vs HD scenario, year <= 2015")
ggsave("Qtot.region_HD_vs_ML_hist.png",
       path = paste("output/figures/results_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# Qs.region scatter plot - projected
# filter on one consumer group to avoid redundancy
df <- data.frame(Qs.region_ML = gcamoutput_Ref_ML %>% 
                   filter(`gcam-consumer` == "FoodDemand_Group1") %>% 
                   filter(year >= 2015) %>%
                   pull("Qs.region"), 
                 Qs.region_HD = gcamoutput_Ref_HD %>% 
                   filter(`gcam-consumer` == "FoodDemand_Group1") %>% 
                   filter(year >= 2015) %>%
                   pull("Qs.region"))
ggplot(df, aes(x = Qs.region_ML, y = Qs.region_HD)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "GCAM Qs.region, ML vs HD scenario, year >= 2015")
ggsave("Qs.region_HD_vs_ML_proj.png",
       path = paste("output/figures/results_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# Qn.region scatter plot - projected
df <- data.frame(Qn.region_ML = gcamoutput_Ref_ML %>% 
                   filter(`gcam-consumer` == "FoodDemand_Group1") %>% 
                   filter(year >= 2015) %>%
                   pull("Qn.region"), 
                 Qn.region_HD = gcamoutput_Ref_HD %>% 
                   filter(`gcam-consumer` == "FoodDemand_Group1") %>% 
                   filter(year >= 2015) %>%
                   pull("Qn.region"))
ggplot(df, aes(x = Qn.region_ML, y = Qn.region_HD)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "GCAM Qn.region, ML vs HD scenario, year >= 2015")
ggsave("Qn.region_HD_vs_ML_proj.png",
       path = paste("output/figures/results_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# Qtot.region scatter plot - projected
df <- data.frame(Qtot.region_ML = gcamoutput_Ref_ML %>% 
                   filter(`gcam-consumer` == "FoodDemand_Group1") %>% 
                   filter(year >= 2015) %>%
                   pull("Qtot.region"), 
                 Qtot.region_HD = gcamoutput_Ref_HD %>% 
                   filter(`gcam-consumer` == "FoodDemand_Group1") %>% 
                   filter(year >= 2015) %>%
                   pull("Qtot.region"))
ggplot(df, aes(x = Qtot.region_ML, y = Qtot.region_HD)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "GCAM Qtot.region, ML vs HD scenario, year >= 2015")
ggsave("Qtot.region_HD_vs_ML_proj.png",
       path = paste("output/figures/results_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# Qs scatter plot - base year
df <- data.frame(Qs_ML = gcamoutput_Ref_ML %>% 
                   filter(year == 2015) %>% 
                   pull("Qs"),
                 Qs_HD = gcamoutput_Ref_HD %>% 
                   filter(year == 2015) %>% 
                   pull("Qs"))
ggplot(df, aes(x = Qs_ML, y = Qs_HD)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "GCAM Qs, ML vs HD scenario, year == 2015")
ggsave("Qs_HD_vs_ML_baseyr.png",
       path = paste("output/figures/results_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# Qn scatter plot - base year
df <- data.frame(Qn_ML = gcamoutput_Ref_ML %>% 
                   filter(year == 2015) %>% 
                   pull("Qn"),
                 Qn_HD = gcamoutput_Ref_HD %>% 
                   filter(year == 2015) %>% 
                   pull("Qn"))
ggplot(df, aes(x = Qn_ML, y = Qn_HD)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "GCAM Qn, ML vs HD scenario, year == 2015")
ggsave("Qn_HD_vs_ML_baseyr.png",
       path = paste("output/figures/results_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# Qtot scatter plot - base year
df <- data.frame(Qtot_ML = gcamoutput_Ref_ML %>% 
                   filter(year == 2015) %>% 
                   pull("Qtot"),
                 Qtot_HD = gcamoutput_Ref_HD %>% 
                   filter(year == 2015) %>% 
                   pull("Qtot"))
ggplot(df, aes(x = Qtot_ML, y = Qtot_HD)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "GCAM Qtot, ML vs HD scenario, year == 2015")
ggsave("Qtot_HD_vs_ML_baseyr.png",
       path = paste("output/figures/results_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# Qs scatter plot - projected
df <- data.frame(Qs_ML = gcamoutput_Ref_ML %>% 
                   filter(year >= 2015) %>% 
                   pull("Qs"),
                 Qs_HD = gcamoutput_Ref_HD %>% 
                   filter(year >= 2015) %>% 
                   pull("Qs"))
ggplot(df, aes(x = Qs_ML, y = Qs_HD)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "GCAM Qs, ML vs HD scenario, year >= 2015")
ggsave("Qs_HD_vs_ML_proj.png",
       path = paste("output/figures/results_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# Qn scatter plot - projected
df <- data.frame(Qn_ML = gcamoutput_Ref_ML %>% 
                   filter(year >= 2015) %>% 
                   pull("Qn"),
                 Qn_HD = gcamoutput_Ref_HD %>% 
                   filter(year >= 2015) %>% 
                   pull("Qn"))
ggplot(df, aes(x = Qn_ML, y = Qn_HD)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "GCAM Qn, ML vs HD scenario, year >= 2015")
ggsave("Qn_HD_vs_ML_proj.png",
       path = paste("output/figures/results_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)

# Qtot scatter plot - projected
df <- data.frame(Qtot_ML = gcamoutput_Ref_ML %>% 
                   filter(year >= 2015) %>% 
                   pull("Qtot"),
                 Qtot_HD = gcamoutput_Ref_HD %>% 
                   filter(year >= 2015) %>% 
                   pull("Qtot"))
ggplot(df, aes(x = Qtot_ML, y = Qtot_HD)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "GCAM Qtot, ML vs HD scenario, year >= 2015")
ggsave("Qtot_HD_vs_ML_proj.png",
       path = paste("output/figures/results_gcam",sep="/"),
       width = 8, height = 6, dpi = 150)



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
