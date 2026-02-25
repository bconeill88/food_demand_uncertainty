# analyze distributions of parameter estimates for gcam food demand functions
# to do
# X plot prob dens of FE parameters, with ML and price hi/lo values
# - calculate and plot demand uncertainty for all regions for ML and independent
#   confidence interval (wait on addition of LL to demand output files)
# - plot uncertainty in differences in demand (move calculation of these results up in code)
# X calculate demand and elasticities for all samples, by region, without saving
#   parameter values (but including iteration)
# - plot uncertainty ranges for all of these, and for ML/hi/low values
# - identify income and scale joint uncertainty parameters
# X calculate and plot differences in demand due to price changes

# ultimately remove the FE model code from this file so there is no confusion
#   that master is the pic version

# make sure all plots have sizes specified so that their png versions are legible

# ------------------------------------------------------------------------------
# Initializations
# ------------------------------------------------------------------------------

#setwd("G:/My Drive/Work/Projects/R projects/food_demand/uncertainty")
setwd("H:/My Drive/R projects/food_demand/uncertainty")

# load packages
library(ambrosia)
library(dplyr)
library(ggplot2)
library(knitr)
library(tidyr)
#library(shiny)   # for runapp()

#vignette("ambrosia_vignette")
#runapp()

# input data path name, relative to uncertainty folder
input_raw_path <- "inputs/raw/"
input_derived_path <-  "inputs/derived/update9_cnstrlam_agg32FE_19dec24/" # "inputs/derived/"
# path to figures folder
fig_path <- "figures/update9_cnstrlam_agg32FE_19dec24/" # "figures/inputs/"

# ------------------------------------------------------------------------------
# Read in raw parameter estimate and observational data
# ------------------------------------------------------------------------------

# data file of parameter estimates that that underlie the Edmonds et al paper

# ambrosia_params_mc_orig9.dat
# they treat 9 parameters as free and two fixed as constants.
# from KN, 6 Feb 2024; 7M+ observations
param_data_orig9_all <- read.mc.data(
  paste(input_raw_path,"ambrosia_params_mc_orig9.dat",sep=""),
  varnames = namemc(nparam = 11))
# %>% sample_n(100000)

# data file of parameter estimates based on updated data relative to Edmonds et al
# but still treating 9 parameters as free and two as fixed
# in principle this should underlie values in GCAM 7 (and since v5.3); however
# those parameters were found with an optimization routine, not MCMC, and had
# parameter bounds

# ambrosia_params_26Mar24kbn_update9.dat
# from KN, 26 Mar 2024, 10k obs, with more flexible bounds on the lambda param;
# see readme.txt in /inputs/raw for description
# row 1 contains variable names, use it for column names then drop it
# drop iteration number column for consistency with other input datasets
# drop first 6150 observations as burn in period, based on parameter traces
param_data_update9_flexlam_all <- read.mc.data(
  paste(input_raw_path,"ambrosia_params_26Mar24kbn_update9.dat",sep=""),
  varnames = namemc(nparam = 11))
colnames(param_data_update9_flexlam_all) <- param_data_update9_flexlam_all[1,]
param_data_update9_flexlam_all <- param_data_update9_flexlam_all[-1,] %>%
  subset(select = -iteration_number) %>%
  mutate_if(is.character, as.numeric)
param_data_update9_flexlam <- param_data_update9_flexlam_all[-(1:6150),] %>%
  mutate(iteration = row_number())

# parameter_data_MCMC_23apr24.dat
# from KN, 23 Apr 2024, 11k obs, with more constrained bounds on the lambda param;
# see readme.txt in /inputs/raw for description
# drop first 3900 observations as burn in period, based on parameter traces
# and fact that max LL sample is iteration 3966
param_data_update9_cnstrlam_all <- read.mc.data(
  paste(input_raw_path,"parameter_data_MCMC_23apr24.dat",sep=""),
  varnames = namemc(nparam = 11))
param_data_update9_cnstrlam <- param_data_update9_cnstrlam_all[-(1:3900),] %>%
  mutate(iteration = row_number())

# parameter_values_MCMC_update11.csv
# data file of parameter estimates that are like ambrosia_params_26Mar24kbn_update9.dat
# but treat 11 parameters as free (not used in GCAM)
# from KN, 5 Feb 2024; 800 observations (sample of full set of results)
# also drop 'X' column to stay consistent with other files
param_data_update11 <- read.csv(paste(input_raw_path,"parameter_values_MCMC_update11.csv",
                                      sep="")) %>% select(-'X')

# 9_parameter_samples_ambrosia_unweighted.dat, staples_FE_parameters.dat
# from KN, 4 Oct 24, 170k obs, first 77k not included in staples_FE_parameters
# see readme.txt in /inputs/raw for description
# data files of 11 parameter estimates and fixed effect parameter estimates, respectively
# name or rename iteration column, drop samples up to 110k based on KN look at trace
# plots, order by iteration number; save full sample and random 10k subsample
# 11 parameter estimates
#param_data_update9_cnstrlam_agg32FE_params_all <-
#  read.mc.data(
#  paste(input_raw_path,"unweighted/9_parameter_samples_ambrosia_unweighted.dat",sep=""),
#  varnames = namemc(nparam = 11))
#colnames(param_data_update9_cnstrlam_agg32FE_params_all)[13] <- "iteration"
#param_data_update9_cnstrlam_agg32FE_params <-
#  param_data_update9_cnstrlam_agg32FE_params_all %>%
#  filter(iteration >= 110000) %>% arrange(iteration)
#param_data_update9_cnstrlam_agg32FE_params_10k <-
#  param_data_update9_cnstrlam_agg32FE_params %>%
#  slice_sample(n=10000) %>% arrange(iteration)
# FE estimates
#param_data_update9_cnstrlam_agg32FE_FEs_all <-
#  read.table(paste(input_raw_path,"unweighted/staples_FE_parameters.dat",sep=""),
#             header=TRUE,sep="") %>% rename(iteration = iteration_number)
#param_data_update9_cnstrlam_agg32FE_FEs <-
#  subset(param_data_update9_cnstrlam_agg32FE_FEs_all,
#         iteration %in% param_data_update9_cnstrlam_agg32FE_params$iteration)
#param_data_update9_cnstrlam_agg32FE_FEs_10k <-
#  subset(param_data_update9_cnstrlam_agg32FE_FEs_all,
#         iteration %in% param_data_update9_cnstrlam_agg32FE_params_10k$iteration)
# save files so that same random sample is used each time
#save(param_data_update9_cnstrlam_agg32FE_params,
#     file = paste0(input_derived_path,
#                   "param_data_update9_cnstrlam_agg32FE_params.RData"))
#save(param_data_update9_cnstrlam_agg32FE_FEs,
#    file = paste0(input_derived_path,
#                   "param_data_update9_cnstrlam_agg32FE_FEs.RData"))
#save(param_data_update9_cnstrlam_agg32FE_params_10k,
#     file = paste0(input_derived_path,
#                   "param_data_update9_cnstrlam_agg32FE_params_10k.RData"))
#save(param_data_update9_cnstrlam_agg32FE_FEs_10k,
#     file = paste0(input_derived_path,
#                   "param_data_update9_cnstrlam_agg32FE_FEs_10k.RData"))

# ambrosia_9_params_2ndDec24.dat, FE_params_2ndDec24.dat
# from KN, 2 Dec 24, ~72k obs, first 83.2k not included; iteration 121786 is ML
# see readme.txt in /inputs/raw for description
# data files of 11 parameter estimates and fixed effect parameter estimates, respectively
# name or rename iteration column, drop samples up to 121k based on KN look at trace
# plots, order by iteration number; save full sample and random 10k subsample
# 11 parameter estimates
#param_data_update9_cnstrlam_agg32FE_params_all <-
#  read.mc.data(
#  paste(input_raw_path,"unweighted/ambrosia_9_params_2ndDec24.dat",sep=""),
#  varnames = namemc(nparam = 11))
#colnames(param_data_update9_cnstrlam_agg32FE_params_all)[13] <- "iteration"
#param_data_update9_cnstrlam_agg32FE_params <-
#  param_data_update9_cnstrlam_agg32FE_params_all %>%
#  filter(iteration >= 121000) %>% arrange(iteration)
#param_data_update9_cnstrlam_agg32FE_params_10k <-
#  param_data_update9_cnstrlam_agg32FE_params %>%
#  slice_sample(n=10000) %>% arrange(iteration)
# FE estimates
#param_data_update9_cnstrlam_agg32FE_FEs_all <-
#  read.table(paste(input_raw_path,"unweighted/FE_params_2ndDec24.dat",sep=""),
#             header=TRUE,sep="") %>% rename(iteration = iteration_number)
#param_data_update9_cnstrlam_agg32FE_FEs <-
#  subset(param_data_update9_cnstrlam_agg32FE_FEs_all,
#         iteration %in% param_data_update9_cnstrlam_agg32FE_params$iteration)
#param_data_update9_cnstrlam_agg32FE_FEs_10k <-
#  subset(param_data_update9_cnstrlam_agg32FE_FEs_all,
#         iteration %in% param_data_update9_cnstrlam_agg32FE_params_10k$iteration)
# save files so that same random sample is used each time
#save(param_data_update9_cnstrlam_agg32FE_params,
#     file = paste0(input_derived_path,
#                   "update9_cnstrlam_agg32FE_dec24/param_data_update9_cnstrlam_agg32FE_params.RData"))
#save(param_data_update9_cnstrlam_agg32FE_FEs,
#    file = paste0(input_derived_path,
#                   "update9_cnstrlam_agg32FE_dec24/param_data_update9_cnstrlam_agg32FE_FEs.RData"))
#save(param_data_update9_cnstrlam_agg32FE_params_10k,
#     file = paste0(input_derived_path,
#                   "update9_cnstrlam_agg32FE_dec24/param_data_update9_cnstrlam_agg32FE_params_10k.RData"))
#save(param_data_update9_cnstrlam_agg32FE_FEs_10k,
#     file = paste0(input_derived_path,
#                   "update9_cnstrlam_agg32FE_dec24/param_data_update9_cnstrlam_agg32FE_FEs_10k.RData"))

# ambrosia_9_params_19Dec24.dat, FE_params_Dec1924.dat
# from KN, 19 Dec 24, ~68k obs, first 92743 not included; iteration 122956 is ML
# see readme.txt in /inputs/raw for description
# data files of 11 parameter estimates and fixed effect parameter estimates, respectively
# name or rename iteration column, drop samples up to 122900k based on BO look at trace
# plots, order by iteration number; save full sample and random 10k subsample
# 11 parameter estimates
param_data_update9_cnstrlam_agg32FE_params_all_old <-
  read.mc.data(
  paste(input_raw_path,"unweighted/ambrosia_9_params_19Dec24.dat",sep=""),
  varnames = namemc(nparam = 11))
colnames(param_data_update9_cnstrlam_agg32FE_params_all)[13] <- "iteration"
param_data_update9_cnstrlam_agg32FE_params <-
  param_data_update9_cnstrlam_agg32FE_params_all %>%
  filter(iteration >= 122900) %>% arrange(iteration)
param_data_update9_cnstrlam_agg32FE_params_10k <-
  param_data_update9_cnstrlam_agg32FE_params %>%
  slice_sample(n=10000) %>% arrange(iteration)
# FE estimates
param_data_update9_cnstrlam_agg32FE_FEs_all <-
  read.table(paste(input_raw_path,"unweighted/FE_paramsDec1924.dat",sep=""),
             header=TRUE,sep="") %>% rename(iteration = iteration_number)
param_data_update9_cnstrlam_agg32FE_FEs <-
  subset(param_data_update9_cnstrlam_agg32FE_FEs_all,
         iteration %in% param_data_update9_cnstrlam_agg32FE_params$iteration)
param_data_update9_cnstrlam_agg32FE_FEs_10k <-
  subset(param_data_update9_cnstrlam_agg32FE_FEs_all,
         iteration %in% param_data_update9_cnstrlam_agg32FE_params_10k$iteration)
# save files so that same random sample is used each time
save(param_data_update9_cnstrlam_agg32FE_params,
     file = paste0(input_derived_path,
                   "param_data_update9_cnstrlam_agg32FE_params.RData"))
save(param_data_update9_cnstrlam_agg32FE_FEs,
    file = paste0(input_derived_path,
                   "param_data_update9_cnstrlam_agg32FE_FEs.RData"))
save(param_data_update9_cnstrlam_agg32FE_params_10k,
     file = paste0(input_derived_path,
                   "param_data_update9_cnstrlam_agg32FE_params_10k.RData"))
save(param_data_update9_cnstrlam_agg32FE_FEs_10k,
     file = paste0(input_derived_path,
                   "param_data_update9_cnstrlam_agg32FE_FEs_10k.RData"))

# ambrosia_params_Jan_2025.dat, FE_paramsJan25.dat
# from KN, 13 Jan 25, ~88k obs, first 92000 not included; iteration 171522 is ML
# see readme.txt in /inputs/raw for description (extends 19dec24 results by ~20k)
# data files of 11 parameter estimates and fixed effect parameter estimates, respectively
# name or rename iteration column, drop samples up to 122900k based on BO look at trace
# plots, order by iteration number;
# 11 parameter estimates
param_data_update9_cnstrlam_agg32FE_params_all <-
  read.mc.data(
    paste(input_raw_path,"unweighted/ambrosia_params_Jan_2025.dat",sep=""),
    varnames = namemc(nparam = 11))
colnames(param_data_update9_cnstrlam_agg32FE_params_all)[13] <- "iteration"
# remove first row which repeats column headings
param_data_update9_cnstrlam_agg32FE_params <-
  param_data_update9_cnstrlam_agg32FE_params_all[-1,]
# convert all columns from character to numeric
i <- c(1:ncol(param_data_update9_cnstrlam_agg32FE_params))
param_data_update9_cnstrlam_agg32FE_params[, i] <-
  apply(param_data_update9_cnstrlam_agg32FE_params[, i], 2,
        function(x) as.numeric(unlist(x)))

# take first set of estimates (same as 19dec24 version)
param_data_update9_cnstrlam_agg32FE_params_1 <-
  param_data_update9_cnstrlam_agg32FE_params %>%
  filter(iteration >= 122900 & iteration <= 160965) %>% arrange(iteration)
# take second set of estimates (same as 19dec24 version)
param_data_update9_cnstrlam_agg32FE_params_2 <-
  param_data_update9_cnstrlam_agg32FE_params %>%
  filter(iteration >= 171000) %>% arrange(iteration)

# FE estimates
param_data_update9_cnstrlam_agg32FE_FEs_all <-
  read.table(paste(input_raw_path,"unweighted/FE_paramsJan25.dat",sep=""),
             header=TRUE,sep="") %>% rename(iteration = iteration_number)
param_data_update9_cnstrlam_agg32FE_FEs_1 <-
  subset(param_data_update9_cnstrlam_agg32FE_FEs_all,
         iteration %in% param_data_update9_cnstrlam_agg32FE_params_1$iteration)
param_data_update9_cnstrlam_agg32FE_FEs_2 <-
  subset(param_data_update9_cnstrlam_agg32FE_FEs_all,
         iteration %in% param_data_update9_cnstrlam_agg32FE_params_2$iteration)


# save files so that same random sample is used each time
save(param_data_update9_cnstrlam_agg32FE_params,
     file = paste0(input_derived_path,
                   "param_data_update9_cnstrlam_agg32FE_params.RData"))
save(param_data_update9_cnstrlam_agg32FE_FEs,
     file = paste0(input_derived_path,
                   "param_data_update9_cnstrlam_agg32FE_FEs.RData"))

# read parameter values from gcam input csv's; 9 params, tack on the two constants
# transform the nonstaples income elasticity parameter (nu1) and the staples max income
# term (k_s) to be consistent with ambrosia expectations (as per KN 2/6/24)
staples_data <- read.csv(paste(input_raw_path,"A_demand_food_staples.csv",sep=""),skip=7)
nonstaples_data <- read.csv(paste(input_raw_path,"A_demand_food_nonstaples.csv",sep=""),skip=7)
nonstaples_data$income.elasticity <- nonstaples_data$income.elasticity/2.0
staples_data$income.max.term <- exp(staples_data$income.max.term)
gcam_psscl <- 100 # these two constants are from Edmonds et al paper
gcam_pnscl <- 20
# combine and put in the order expected by ambrosia
params_gcam <- c(staples_data$scale.param,nonstaples_data$scale.param,
                 staples_data$self.price.elasticity,staples_data$cross.price.elasticity,
                 nonstaples_data$self.price.elasticity,nonstaples_data$income.elasticity,
                 staples_data$income.elasticity,staples_data$income.max.term,
                 staples_data$price.received,gcam_psscl,gcam_pnscl)

# read in country-level observations; saved sample of 500 (out of 4k+) so the
# same sample can always be retrieved
#Obs_Data <- read.csv(paste(input_raw_path,"Processed_Data_for_MC.csv",sep=""))
#Obs_Data_sample <- sample_n(Obs_Data,500)
#save(Obs_Data_sample,file = "obs_data_sample_500.RData")
load(paste0(input_derived_path,"obs_data_sample_500.RData"))

# read in GCAM region-level observations; save so that it is available as Rdata
#obs_data_gcam32 <- read.csv(paste(input_raw_path,
#                                  "unweighted/Processed_group_data.csv",sep=""))
#save(obs_data_gcam32,file = paste0(input_derived_path,"obs_data_gcam32.RData"))
# observations updated to include region 15 (Europe Non-EU)
obs_data_gcam32 <- read.csv(paste(input_raw_path,
                                  "unweighted/Processed_group_data_19Dec24.csv",sep=""))
save(obs_data_gcam32,file = paste0(input_derived_path,"obs_data_gcam32.RData"))

# ------------------------------------------------------------------------------
# Plot parameter traces from raw data for MCMC approaches
# ------------------------------------------------------------------------------

# used for deciding on number of iterations to drop (above) from raw data

# function for plotting traces for 9 variables
make_trace_plots <- function(param_data) {
  param_colnames <- all_of(colnames(param_data))[1:9]
  param_data_long <- gather(param_data,key="parameter",value="value",param_colnames)
  g_trace <- ggplot(param_data_long,aes(x=iteration,y=value)) +
    geom_point(size=0.3) +
    facet_wrap(~parameter, ncol=3, scales = "free")
  plot(g_trace)
}

# function for plotting traces for fixed effects
make_trace_plots_FE <- function(param_data) {
  g_trace <- ggplot(param_data,aes(x=iteration,y=staples_FE)) +
    geom_point(size=0.3) +
    facet_wrap(~region, ncol=4, scales = "free")
  plot(g_trace)
}

# orig9 dataset
# take first N samples to plot, for 9 variables and adding iteration column
param_data_orig9[1:10000,1:9] %>% mutate(iteration = row_number()) %>%
  make_trace_plots()

# update9 data
param_data_update9_flexlam_all %>% mutate(iteration = row_number()) %>%
  make_trace_plots()
param_data_update9_flexlam %>% mutate(iteration = row_number()) %>%
  make_trace_plots()
param_data_update9_cnstrlam_all %>% mutate(iteration = row_number()) %>%
  make_trace_plots()
param_data_update9_cnstrlam %>% mutate(iteration = row_number()) %>%
  make_trace_plots()


# ------------------------------------------------------------------------------
# Calculate parameter uncertainty values: ML, independent and joint uncertainty
# intervals
# ------------------------------------------------------------------------------

#    ---------------------------------------------------------------------------
#    Functions for calculating ML and independent or joint intervals
#    ---------------------------------------------------------------------------

# function for calculating ML and 95% independent confidence intervals
# use method from Edmonds et al: take the range of all variables after dropping
# samples below the 5th percentile of likelihood scores
# input is a raw parameter data file
make_df_MLintervals <- function(param_data) {
  params <- data.frame(matrix(ncol = ncol(param_data), nrow = 3))
  rownames(params) <- c("ML","Lo indep","Hi indep")
  colnames(params) <- colnames(param_data)
  # get the maximum likelihood parameter values
  params['ML',] <- param_data[which.max(param_data$LL),]
  # calculate confidence interval and add to dataframe
  quant_LL <- quantile(param_data$LL,probs = 0.05)
  quants <- param_data[param_data$LL > quant_LL,] %>% apply(2,range)
  params['Lo indep',] <- quants[1,]
  params['Hi indep',] <- quants[2,]
  return(params)
}

# function to find parameters representing joint price parameter combinations
# assumed to lead to the lowest and highest demand in response to a price
# increase ("lo" - lowest demand and jointly most negative parameters; "hi" -
# highest demand and jointly most positive parameters); produces a single
# dataframe with two rows
# seems like this function should be generalizable to work for joint income and
# joint scale parameters too
get_sens_params_price <- function(param_data) {

  # drop outliers by only using samples in 95% interval for likelihood scores
  quant_LL <- quantile(param_data$LL,probs = 0.05)
  param_data_95 <- param_data[param_data$LL > quant_LL,]

  # calculate all deciles of each variable
  get_quantiles <- function(sample) {
    quantile(sample,probs=seq(0.1, 0.9, 0.1))
  }
  quantiles_data <- apply(param_data_95,2,get_quantiles)

  # identify high and low price sensitivity values for xss, xnn, xcross

  # initialize quantiles to search over and parameter results
  quantlist = c("10%","20%","30%","40%","50%","60%","70%","80%","90%")
  i <- 1
  params_joint_cond1 <- params_joint_cond2 <-
    matrix(ncol = 0, nrow = 0) %>% data.frame()

  while (nrow(params_joint_cond1) < 1 && i < 10) {

    # identify samples that have all price elasticity params below a given percentile
    # within their marginal distributions
    quant <- quantlist[i]
    params_joint_cond1 <- param_data_95[
      param_data_95$xi.ss < quantiles_data[quant,"xi.ss"] &
        param_data_95$xi.nn < quantiles_data[quant,"xi.nn"] &
        param_data_95$xi.cross < quantiles_data[quant,"xi.cross"],]
    # select sample with lowest likelihood score
    params_joint_cond1 <- params_joint_cond1[
      params_joint_cond1$LL == min(params_joint_cond1$LL),] %>%
      filter(!duplicated(LL))

    i <- i+1
  }
  # name the row and add the quantile used to derive parameters to last column
  rownames(params_joint_cond1) <- "lo" # params leading to lowest demand
  params_joint_cond1 <- mutate(params_joint_cond1,quant = quantlist[i-1])

  while (nrow(params_joint_cond2) < 1 && i < 10) {

    # identify samples that have all price elasticity params above a given percentile
    # within their marginal distributions
    quant <- quantlist[10-i]
    params_joint_cond2 <- param_data_95[
      param_data_95$xi.ss > quantiles_data[quant,"xi.ss"] &
        param_data_95$xi.nn > quantiles_data[quant,"xi.nn"] &
        param_data_95$xi.cross > quantiles_data[quant,"xi.cross"],]
    # select sample with lowest likelihood score
    params_joint_cond2 <- params_joint_cond2[
      params_joint_cond2$LL == min(params_joint_cond2$LL),] %>%
      filter(!duplicated(LL))

    i <- i+1
  }
  # name the row and add the quantile used to derive parameters to last column
  rownames(params_joint_cond2) <- "hi" # params leading to highest demand
  params_joint_cond2 <- mutate(params_joint_cond2,quant = quantlist[10-(i-1)])

  # return a single dataframe with the two sets of parameters as rows
  params_joint <- rbind(params_joint_cond1,params_joint_cond2)
  return(params_joint)
}

# function to find parameters representing joint income parameter combinations
# assumed to lead to the lowest and highest demand in response to an income
# increase ("lo" - lowest demand and lowest parameters; "hi" - highest demand
# and highest parameters); produces a single dataframe with two rows
get_sens_params_inc <- function(param_data) {

  # drop outliers by only using samples in 95% interval for likelihood scores
  quant_LL <- quantile(param_data$LL,probs = 0.05)
  param_data_95 <- param_data[param_data$LL > quant_LL,]

  # calculate all deciles of each variable
  get_quantiles <- function(sample) {
    quantile(sample,probs=seq(0.1, 0.9, 0.1))
  }
  quantiles_data <- apply(param_data_95,2,get_quantiles)

  # identify high and low price sensitivity values for ks, lambda, eps1n

  # initialize quantiles to search over and parameter results
  quantlist = c("10%","20%","30%","40%","50%","60%","70%","80%","90%")
  i <- 1
  params_joint_cond1 <- params_joint_cond2 <-
    matrix(ncol = 0, nrow = 0) %>% data.frame()

  while (nrow(params_joint_cond1) < 1 && i < 10) {

    # identify samples that have all income parameters below a given percentile
    # within their marginal distributions
    quant <- quantlist[i]
    params_joint_cond1 <- param_data_95[
      param_data_95$ks < quantiles_data[quant,"ks"] &
        param_data_95$lambda < quantiles_data[quant,"lambda"] &
        param_data_95$eps1n < quantiles_data[quant,"eps1n"],]
    # select sample with lowest likelihood score
    params_joint_cond1 <- params_joint_cond1[
      params_joint_cond1$LL == min(params_joint_cond1$LL),] %>%
      filter(!duplicated(LL))

    i <- i+1
  }
  # name the row and add the quantile used to derive parameters to last column
  rownames(params_joint_cond1) <- "lo" # params leading to lowest demand
  params_joint_cond1 <- mutate(params_joint_cond1,quant = quantlist[i-1])

  while (nrow(params_joint_cond2) < 1 && i < 10) {

    # identify samples that have all income parameters above a given percentile
    # within their marginal distributions
    quant <- quantlist[10-i]
    params_joint_cond2 <- param_data_95[
      param_data_95$ks > quantiles_data[quant,"ks"] &
        param_data_95$lambda > quantiles_data[quant,"lambda"] &
        param_data_95$eps1n > quantiles_data[quant,"eps1n"],]
    # select sample with lowest likelihood score
    params_joint_cond2 <- params_joint_cond2[
      params_joint_cond2$LL == min(params_joint_cond2$LL),] %>%
      filter(!duplicated(LL))

    i <- i+1
  }
  # name the row and add the quantile used to derive parameters to last column
  rownames(params_joint_cond2) <- "hi" # params leading to highest demand
  params_joint_cond2 <- mutate(params_joint_cond2,quant = quantlist[10-(i-1)])

  # return a single dataframe with the two sets of parameters as rows
  params_joint <- rbind(params_joint_cond1,params_joint_cond2)
  return(params_joint)
}

# function to find parameters representing joint scale parameter combinations
# assumed to lead to the lowest and highest demand in response to an income
# increase ("lo" - lowest demand and lowest parameters; "hi" - highest demand
# and highest parameters); produces a single dataframe with two rows
get_sens_params_scale <- function(param_data) {

  # drop outliers by only using samples in 95% interval for likelihood scores
  quant_LL <- quantile(param_data$LL,probs = 0.05)
  param_data_95 <- param_data[param_data$LL > quant_LL,]

  # calculate all deciles of each variable
  get_quantiles <- function(sample) {
    quantile(sample,probs=seq(0.1, 0.9, 0.1))
  }
  quantiles_data <- apply(param_data_95,2,get_quantiles)

  # identify high and low price sensitivity values for ks, lambda, eps1n

  # initialize quantiles to search over and parameter results
  quantlist = c("10%","20%","30%","40%","50%","60%","70%","80%","90%")
  i <- 1
  params_joint_cond1 <- params_joint_cond2 <-
    matrix(ncol = 0, nrow = 0) %>% data.frame()

  while (nrow(params_joint_cond1) < 1 && i < 10) {

    # identify samples that have all income parameters below a given percentile
    # within their marginal distributions
    quant <- quantlist[i]
    params_joint_cond1 <- param_data_95[
      param_data_95$As < quantiles_data[quant,"As"] &
        param_data_95$An < quantiles_data[quant,"An"],]
    # select sample with lowest likelihood score
    params_joint_cond1 <- params_joint_cond1[
      params_joint_cond1$LL == min(params_joint_cond1$LL),] %>%
      filter(!duplicated(LL))

    i <- i+1
  }
  # name the row and add the quantile used to derive parameters to last column
  rownames(params_joint_cond1) <- "lo" # params leading to lowest demand
  params_joint_cond1 <- mutate(params_joint_cond1,quant = quantlist[i-1])

  while (nrow(params_joint_cond2) < 1 && i < 10) {

    # identify samples that have all income parameters above a given percentile
    # within their marginal distributions
    quant <- quantlist[10-i]
    params_joint_cond2 <- param_data_95[
      param_data_95$As > quantiles_data[quant,"As"] &
        param_data_95$An > quantiles_data[quant,"An"],]
    # select sample with lowest likelihood score
    params_joint_cond2 <- params_joint_cond2[
      params_joint_cond2$LL == min(params_joint_cond2$LL),] %>%
      filter(!duplicated(LL))

    i <- i+1
  }
  # name the row and add the quantile used to derive parameters to last column
  rownames(params_joint_cond2) <- "hi" # params leading to highest demand
  params_joint_cond2 <- mutate(params_joint_cond2,quant = quantlist[10-(i-1)])

  # return a single dataframe with the two sets of parameters as rows
  params_joint <- rbind(params_joint_cond1,params_joint_cond2)
  return(params_joint)
}

#    ---------------------------------------------------------------------------
#    Calculate ML and independent confidence intervals
#    SHOULD CHANGE VARIABLE NAMES TO PARAMS_ML95_* TO BE CLEARER WHAT THESE ARE
#    ---------------------------------------------------------------------------

# calculate max likelihood and 95% independent confidence intervals from
# databases of parameter samples for other sources

# record max likelihood and 95% confidence intervals directly from the Edmonds
# et al paper; ML values are the same as the default values in ambrosia.
# same as for GCAM parameters, transform the nonstaples income elasticity parameter
# (nu1) and the staples max income term (k_s) to be consistent with ambrosia
# expectations (as per KN 2/6/24)
params_edmonds <- data.frame(matrix(ncol = 12, nrow = 3))
rownames(params_edmonds) <- c("ML","Lo","Hi")
colnames(params_edmonds) <- c("As", "An", "xi.ss", "xi.cross", "xi.nn", "eps1n",
                              "lambda", "ks", "Pm", "psscl", "pnscl","LL")
params_edmonds['ML',] <-
  c(1.28, 1.14, -0.19, 0.21, -0.33, 0.98/2, 0.10, exp(2.77), 5.06, 100, 20,NA)
params_edmonds['Lo',] <-
  c(1.22, 0.76, -0.30, 0.05, -0.51, 0.87/2, 0.07, exp(2.01), 1.94, 100, 20,NA)
params_edmonds['Hi',] <-
  c(1.50, 1.23, -0.05, 0.31, -0.03, 1.34/2, 0.20, exp(2.99), 5.91, 100, 20,NA)

# orig_9 data
params_orig9 <- make_df_MLintervals(param_data_orig9_all)

# update9 data
params_update9_flexlam <- make_df_MLintervals(param_data_update9_flexlam)
params_update9_cnstrlam <- make_df_MLintervals(param_data_update9_cnstrlam)
save(params_update9_cnstrlam,
     file=paste0(input_derived_path,"params_update9_cnstrlam.Rdata"))

# update11 data
params_update11 <- make_df_MLintervals(param_data_update11)

# update9 cnstrlam data with FE model

# parameter ML and independent intervals
params_ML95_update9_cnstrlam_agg32FE_params <-
  make_df_MLintervals(param_data_update9_cnstrlam_agg32FE_params_2)
params_ML95_update9_cnstrlam_agg32FE_FEs <-
  split(param_data_update9_cnstrlam_agg32FE_FEs_2,
        f=param_data_update9_cnstrlam_agg32FE_FEs_2$region) %>%
  lapply(make_df_MLintervals)
save(params_ML95_update9_cnstrlam_agg32FE_params,
     file=paste0(input_derived_path,
                 "params_ML95_update9_cnstrlam_agg32FE_params_2.Rdata"))
save(params_ML95_update9_cnstrlam_agg32FE_FEs,
     file=paste0(input_derived_path,
                 "params_ML95_update9_cnstrlam_agg32FE_FEs_2.Rdata"))

load(paste0(input_derived_path,"params_ML95_update9_cnstrlam_agg32FE_params_1.Rdata"))
assign("res1",params_ML95_update9_cnstrlam_agg32FE_params)
load(paste0(input_derived_path,"params_ML95_update9_cnstrlam_agg32FE_params_2.Rdata"))
assign("res2",params_ML95_update9_cnstrlam_agg32FE_params)

plot(factor(colnames(res1)[1:9]),
     (res2['ML',c(1:9)]-res1['ML',c(1:9)])/res1['ML',c(1:9)],xlab = "global parameter",
     ylab = "fractional difference")


#    ---------------------------------------------------------------------------
#    Calculate joint parameter intervals
#    ---------------------------------------------------------------------------

# joint price sensitivity parameters
params_joint_price_update9_flexlam <-
  get_sens_params_price(param_data_update9_flexlam)
params_joint_price_update9_cnstrlam <-
  get_sens_params_price(param_data_update9_cnstrlam)
save(params_joint_price_update9_cnstrlam,
     file=paste0(input_derived_path,"params_joint_price_update9_cnstrlam.Rdata"))

# save for sending to Joo
#write.csv(rbind(
#  subset(params_update9_cnstrlam["ML",],select = -c(LL,iteration)),
#  subset(params_joint_price_update9_cnstrlam["lo",],select = -c(LL,iteration,quant))),
# file = "params_joint_price_update9_cnstrlam_17may24.csv")


#       ------------------------------------------------------------------------------
#       ... CONTINUED ... NOT YET UPDATED
#       ------------------------------------------------------------------------------

# left off here, have not modified for use with update9 beyond this point


# identify high and low income sensitivity values for ks, lambda, nu

# identify samples that have all income elasticity parameters above a given percentile
# within their marginal distributions (this will produce highest income sensitivity)
quant = "80%"
sample_joint_inc_hi <- param_data_orig9_sample_95[
  param_data_orig9_sample_95$ks > quantiles_orig9[quant,"ks"] &
    param_data_orig9_sample_95$lambda > quantiles_orig9[quant,"lambda"] &
    param_data_orig9_sample_95$eps1n > quantiles_orig9[quant,"eps1n"],]
# select sample with lowest likelihood score to use as our high sensitivity case
sample_joint_inc_hi <- sample_joint_inc_hi[
  sample_joint_inc_hi$LL == min(sample_joint_inc_hi$LL),] %>%
  filter(!duplicated(LL)) %>% select(1:11)

# identify samples that have all income elasticity parameters below a given percentile
# within their marginal distributions (this will produce lowest income sensitivity)
quant = "20%"
sample_joint_inc_lo <- param_data_orig9_sample_95[
  param_data_orig9_sample_95$ks < quantiles_orig9[quant,"ks"] &
    param_data_orig9_sample_95$lambda < quantiles_orig9[quant,"lambda"] &
    param_data_orig9_sample_95$eps1n < quantiles_orig9[quant,"eps1n"],]
# select sample with lowest likelihood score to use as our high sensitivity case
sample_joint_inc_lo <- sample_joint_inc_lo[
  sample_joint_inc_lo$LL == min(sample_joint_inc_lo$LL),] %>%
  filter(!duplicated(LL)) %>% select(1:11)

# identify high and low scale sensitivity values for As, An

# identify samples that have all scale parameters above a given percentile
# within their marginal distributions (this will produce highest scale sensitivity)
quant = "90%"
sample_joint_scale_hi <- param_data_orig9_sample_95[
  param_data_orig9_sample_95$As > quantiles_orig9[quant,"As"] &
    param_data_orig9_sample_95$An > quantiles_orig9[quant,"An"],]
# select sample with lowest likelihood score to use as our high sensitivity case
sample_joint_scale_hi <- sample_joint_scale_hi[
  sample_joint_scale_hi$LL == min(sample_joint_scale_hi$LL),] %>%
  filter(!duplicated(LL)) %>% select(1:11)

# identify samples that have all scale parameters below a given percentile
# within their marginal distributions (this will produce lowest scale sensitivity)
quant = "20%"
sample_joint_scale_lo <- param_data_orig9_sample_95[
  param_data_orig9_sample_95$As < quantiles_orig9[quant,"As"] &
    param_data_orig9_sample_95$An < quantiles_orig9[quant,"An"],]
# select sample with lowest likelihood score to use as our high sensitivity case
sample_joint_scale_lo <- sample_joint_scale_lo[
  sample_joint_scale_lo$LL == min(sample_joint_scale_lo$LL),] %>%
  filter(!duplicated(LL)) %>% select(1:11)

# identify high/low price and income sensitivity values for xss, xnn, xcross, ks, lambda, nu

# identify samples that have all price parameters below a given percentile and
# all income parameters above a given percentile within their marginal distributions
# (this will produce highest price/income sensitivity)
quantlo <- "30%"
quanthi <- "70%"
sample_joint_priceinc_hi <- param_data_orig9_sample_95[
  param_data_orig9_sample_95$xi.ss < quantiles_orig9[quantlo,"xi.ss"] &
    param_data_orig9_sample_95$xi.nn < quantiles_orig9[quantlo,"xi.nn"] &
    param_data_orig9_sample_95$xi.cross < quantiles_orig9[quantlo,"xi.cross"] &
    param_data_orig9_sample_95$ks > quantiles_orig9[quanthi,"ks"] &
    param_data_orig9_sample_95$lambda > quantiles_orig9[quanthi,"lambda"] &
    param_data_orig9_sample_95$eps1n > quantiles_orig9[quanthi,"eps1n"],]
# select sample with lowest likelihood score to use as our high sensitivity case
sample_joint_priceinc_hi <- sample_joint_priceinc_hi[
  sample_joint_priceinc_hi$LL == min(sample_joint_priceinc_hi$LL),] %>%
  filter(!duplicated(LL)) %>% select(1:11)

# identify samples that have all price parameters above a given percentile and
# all income parameters below a given percentile within their marginal distributions
# (this will produce lowest price/income sensitivity)
quantlo <- "40%"
quanthi <- "60%"
sample_joint_priceinc_lo <- param_data_orig9_sample_95[
  param_data_orig9_sample_95$xi.ss > quantiles_orig9[quanthi,"xi.ss"] &
    param_data_orig9_sample_95$xi.nn > quantiles_orig9[quanthi,"xi.nn"] &
    param_data_orig9_sample_95$xi.cross > quantiles_orig9[quanthi,"xi.cross"] &
    param_data_orig9_sample_95$ks < quantiles_orig9[quantlo,"ks"] &
    param_data_orig9_sample_95$lambda < quantiles_orig9[quantlo,"lambda"] &
    param_data_orig9_sample_95$eps1n < quantiles_orig9[quantlo,"eps1n"],]
# select sample with lowest likelihood score to use as our high sensitivity case
sample_joint_priceinc_lo <- sample_joint_priceinc_lo[
  sample_joint_priceinc_lo$LL == min(sample_joint_priceinc_lo$LL),] %>%
  filter(!duplicated(LL)) %>% select(1:11)

# identify high/low price, income and scale sensitivity values for xss, xnn, xcross, ks, lambda, nu, As, An

# identify samples that have all price parameters below a given percentile and
# all income and scale parameters above a given percentile within their marginal distributions
# (this will produce highest price/income/scale sensitivity)
quantlo <- "50%"
quanthi <- "50%"
sample_joint_priceincscale_hi <- param_data_orig9_sample_95[
  param_data_orig9_sample_95$xi.ss < quantiles_orig9[quantlo,"xi.ss"] &
    param_data_orig9_sample_95$xi.nn < quantiles_orig9[quantlo,"xi.nn"] &
    param_data_orig9_sample_95$xi.cross < quantiles_orig9[quantlo,"xi.cross"] &
    param_data_orig9_sample_95$ks > quantiles_orig9[quanthi,"ks"] &
    param_data_orig9_sample_95$lambda > quantiles_orig9[quanthi,"lambda"] &
    param_data_orig9_sample_95$eps1n > quantiles_orig9[quanthi,"eps1n"] &
    param_data_orig9_sample_95$As > quantiles_orig9[quanthi,"As"] &
    param_data_orig9_sample_95$An > quantiles_orig9[quanthi,"An"],]
# select sample with lowest likelihood score to use as our high sensitivity case
sample_joint_priceincscale_hi <- sample_joint_priceincscale_hi[
  sample_joint_priceincscale_hi$LL == min(sample_joint_priceincscale_hi$LL),] %>%
  filter(!duplicated(LL)) %>% select(1:11)

# identify samples that have all price parameters above a given percentile and
# all income and scale parameters below a given percentile within their marginal distributions
# (this will produce lowest price/income/scale sensitivity)
quantlo <- "50%"
quanthi <- "50%"
sample_joint_priceincscale_lo <- param_data_orig9_sample_95[
  param_data_orig9_sample_95$xi.ss > quantiles_orig9[quanthi,"xi.ss"] &
    param_data_orig9_sample_95$xi.nn > quantiles_orig9[quanthi,"xi.nn"] &
    param_data_orig9_sample_95$xi.cross > quantiles_orig9[quanthi,"xi.cross"] &
    param_data_orig9_sample_95$ks < quantiles_orig9[quantlo,"ks"] &
    param_data_orig9_sample_95$lambda < quantiles_orig9[quantlo,"lambda"] &
    param_data_orig9_sample_95$eps1n < quantiles_orig9[quantlo,"eps1n"] &
    param_data_orig9_sample_95$As < quantiles_orig9[quantlo,"As"] &
    param_data_orig9_sample_95$An < quantiles_orig9[quantlo,"An"],]
# select sample with lowest likelihood score to use as our high sensitivity case
sample_joint_priceincscale_lo <- sample_joint_priceincscale_lo[
  sample_joint_priceincscale_lo$LL == min(sample_joint_priceincscale_lo$LL),] %>%
  filter(!duplicated(LL)) %>% select(1:11)

param_colnames <- colnames(param_data_update9_sample_95)[1:12]

# make table to compare high/low price sensitivity case to intervals for each variable
# flexible lambda case
sample_df <- data.frame(
  Var_Name = colnames(params_joint_price_update9_flexlam)[1:12],
  low_sens = as.vector(t(params_joint_price_update9_flexlam["lo",1:12])),
  high_sens = as.vector(t(params_joint_price_update9_flexlam["hi",1:12])),
  update9_ml = as.vector(t(params_update9_flexlam['ML',])),
  update9_lo = as.vector(t(params_update9_flexlam['Lo',])),
  update9_hi = as.vector(t(params_update9_flexlam['Hi',])))
#kable(sample_df,format = "simple")

# make table to compare high/low price sensitivity case to intervals for each variable
# constrained lambda case
sample_df <- data.frame(
  Var_Name = colnames(params_joint_price_update9_cnstrlam)[1:12],
  low_sens = as.vector(t(params_joint_price_update9_cnstrlam["lo",1:12])),
  high_sens = as.vector(t(params_joint_price_update9_cnstrlam["hi",1:12])),
  update9_ml = as.vector(t(params_update9_cnstrlam['ML',])),
  update9_lo = as.vector(t(params_update9_cnstrlam['Lo',])),
  update9_hi = as.vector(t(params_update9_cnstrlam['Hi',])))
#kable(sample_df,format = "simple")

# make table to compare high/low income sensitivity case to intervals for each variable
sample_df <- data.frame(
  Var_Name = param_colnames,
  low_sens = as.vector(t(sample_joint_inc_lo[1,1:11])),
  high_sens = as.vector(t(sample_joint_inc_hi[1,1:11])),
  orig9_ml = as.vector(t(params_orig9['ML',])),
  orig9_lo = as.vector(t(params_orig9['Lo',])),
  orig9_hi = as.vector(t(params_orig9['Hi',])))
#kable(sample_df,format = "simple")

# make table to compare high/low scale sensitivity case to intervals for each variable
sample_df <- data.frame(
  Var_Name = param_colnames,
  low_sens = as.vector(t(sample_joint_scale_lo[1,1:11])),
  high_sens = as.vector(t(sample_joint_scale_hi[1,1:11])),
  orig9_ml = as.vector(t(params_orig9['ML',])),
  orig9_lo = as.vector(t(params_orig9['Lo',])),
  orig9_hi = as.vector(t(params_orig9['Hi',])))
#kable(sample_df,format = "simple")

# make table to compare high/low price and income sensitivity case to intervals for each variable
sample_df <- data.frame(
  Var_Name = param_colnames,
  low_sens = as.vector(t(sample_joint_priceinc_lo[1,1:11])),
  high_sens = as.vector(t(sample_joint_priceinc_hi[1,1:11])),
  orig9_ml = as.vector(t(params_orig9['ML',])),
  orig9_lo = as.vector(t(params_orig9['Lo',])),
  orig9_hi = as.vector(t(params_orig9['Hi',])))
#kable(sample_df,format = "simple")

# make table to compare high/low price, income and scale sensitivity case to intervals for each variable
sample_df <- data.frame(
  Var_Name = param_colnames,
  low_sens = as.vector(t(sample_joint_priceincscale_lo[1,1:11])),
  high_sens = as.vector(t(sample_joint_priceincscale_hi[1,1:11])),
  orig9_ml = as.vector(t(params_orig9['ML',])),
  orig9_lo = as.vector(t(params_orig9['Lo',])),
  orig9_hi = as.vector(t(params_orig9['Hi',])))
#kable(sample_df,format = "simple")




# ------------------------------------------------------------------------------
# Calculate demand and elasticities for sub-sample, with uncertainty values
# ------------------------------------------------------------------------------

#   ----------------------------------------------------------------------------
#   Functions for calculating elasticities and food demand
#   -----------------------------------------------------------------------

# function to calculate income elasticities for staples and non-staples from
# a list of incomes and a parameter structure (result of a call to vec2param())
calc_income_elast <- function(income,param_structure) {

  #calculate all income elasticities
  eta.s <- param_structure$yfunc[[1]](Y=income,FALSE)
  eta.n <- param_structure$yfunc[[2]](Y=income,FALSE)
  # create and return results in dataframe
  inc_elast.df <- data.frame(eta.s,eta.n)
}

# function to calculate price elasticities from a food_demand dataframe (result
# of a call to Food.Demand()) and parameter structure (result of a call
# to vec2param()). income elasticities must already have been calculated and be
# part of food_demand
calc_price_elast <- function(food_demand,param_structure) {

  #calculate all price elasticities
  elasticities <- calc1eps(food_demand$alpha.s, food_demand$alpha.n,
                           food_demand$eta.s, food_demand$eta.n,
                           param_structure$xi)
  # extract the different elasticities; each one follows another
  len <- nrow(food_demand)
  elast.ss <- elasticities[1:len]
  elast.ns <- elasticities[(len+1):(2*len)]
  elast.sn <- elasticities[(2*len+1):(3*len)]
  elast.nn <- elasticities[(3*len+1):(4*len)]
  # create and return results in dataframe
  elast.df <- data.frame(elast.ss,elast.nn,elast.sn,elast.ns)
}

# function to calculate food demand (using food.dmnd()) without fixed effects
# given a dataframe of price and income data, and a single row of a parameter data
# file (one iteration); returns a dataframe combining price/income data, parameters
# (unless this is commented out), demand (including Qtot), both income elasticities,
# and four price elasticities
food.dmnd.plus <- function(princdata,paramdata) {

  # get parameter structure needed for food.dmnd() and calculate food demand
  param_structure <- vec2param(as.vector(t(paramdata[1:11])))
  demand <- food.dmnd(princdata$Ps,princdata$Pn,princdata$Y,params = param_structure)
  # add total demand
  demand$Qtot <- demand$Qs + demand$Qn
  # calculate income elasticities and add to results
  inc_elast <- calc_income_elast(princdata$Y,param_structure)
  demand <- cbind(demand,inc_elast)
  # calculate price elasticities, needs budget shares and income elasticities in 'demand'
  price_elast <- calc_price_elast(demand,param_structure)
  demand <- cbind(demand,price_elast)
  # package price/income data, parameters, and demand/elasticities and return
  output <- cbind(Y=princdata$Y,Ps=princdata$Ps,Pn=princdata$Pn,
# comment this out to keep parameter values out of the output to save space
#                  do.call("rbind", replicate(nrow(princdata), paramdata, simplify = FALSE)),
                  demand)
  return(output)
}

# function for calculating regional food demand and elasticities with the FE model,
# given "global" food demand and elasticities (ie demand with no fixed effects)
# for a given set of parameter samples and price/income assumptions, parameters
# for each sample, and the base of a file name for saving the results (region
# number is appended within the function)
food.dmnd.plus.FE.regions <- function(paramdata,FEdata,demand_glob,filename) {
  # make sure paramdata, FEdata, and demand_glob are all sorted by iteration number so
  # they will be combined correctly
  paramdata <- paramdata[order(paramdata$iteration),]
  FEdata <- FEdata[order(FEdata$region,FEdata$iteration),]
  demand_glob <- demand_glob[order(demand_glob$iteration),]

  # loop over regions
  for (r in 1:max(FEdata$GCAM_region_ID)) {
    print(paste0("region ",r))
#    if(r==15) next # skip, region 15 FE data is missing

    # FE data for the region
    FEdata_reg <- FEdata[FEdata$GCAM_region_ID == r,]

    # group global demand/elasticities data by iteration
    demand_glob_byiter <- split(demand_glob, demand_glob$iteration)
    # initialize list to hold regional results
    demand_reg <- demand_glob_byiter
    # loop over iterations (should be able to do this with mapply)
    for(i in 1:nrow(FEdata_reg)) {
      # add regional fixed effect to global demand, and region name/ID
      demand_reg[[i]] <- demand_glob_byiter[[i]] %>%
        mutate(Qs = Qs + FEdata_reg[i,'staples_FE'],
               Qtot = Qtot + FEdata_reg[i,'staples_FE'],
               GCAM_region_ID = FEdata_reg[i,'GCAM_region_ID'],
               region = FEdata_reg[i,'region']) %>%
        # update budget shares
        mutate(alpha.s = Qs*Ps/Y, alpha.n = Qn*Pn/Y) %>%
        mutate(alpha.m = 1-alpha.s-alpha.n)
      # update price elasticities based on updated budget shares
      param_structure <- vec2param(as.vector(t(paramdata[i,1:11])))
      price_elast <- calc_price_elast(demand_reg[[i]],param_structure)
      demand_reg[[i]] <- mutate(demand_reg[[i]],
                                elast.ss = price_elast$elast.ss,
                                elast.nn = price_elast$elast.nn,
                                elast.sn = price_elast$elast.sn,
                                elast.ns = price_elast$elast.ns)
    }
    # recombine over iterations
    demand_reg <- bind_rows(demand_reg)
    # save regional results
    save(demand_reg,file = paste0(input_derived_path,filename,"_R",r,".Rdata"))
  }
}

#   ----------------------------------------------------------------------------
#   Calculate all demand outputs and save for different price regimes
#   and different subsets of parameter uncertainties
#   ----------------------------------------------------------------------------

# define different income and price inputs
incprice_def <- data.frame(Y=c(seq(0.1,5, by=0.1),seq(5.5,30,by=0.5),
                               seq(35,100,by=5))) %>%  mutate(Ps=0.1,Pn=0.2)
incprice_2xPsPn <- data.frame(Y=c(seq(0.1,5, by=0.1),seq(5.5,30,by=0.5),
                                  seq(35,100,by=5))) %>%  mutate(Ps=2*0.1,Pn=2*0.2)
incprice_2xPs <- data.frame(Y=c(seq(0.1,5, by=0.1),seq(5.5,30,by=0.5),
                                  seq(35,100,by=5))) %>%  mutate(Ps=2*0.1,Pn=0.2)
incprice_2xPn <- data.frame(Y=c(seq(0.1,5, by=0.1),seq(5.5,30,by=0.5),
                                  seq(35,100,by=5))) %>%  mutate(Ps=0.1,Pn=2*0.2)
incprice_PsPnHi <- data.frame(Y=c(seq(0.1,5, by=0.1),seq(5.5,30,by=0.5),
                                seq(35,100,by=5))) %>%  mutate(Ps=2.0,Pn=2.0)

# price/income ranges for E Africa and India, in case needed again
# Approximate the values for E Africa from GCAM ref case, 2015-2050 (MER)
#incprice_EAf <- incprice_data_EAf_highPsPn <- data.frame(Y=seq(0.5,2, by=0.1))
# GCAM ref case and high price variant (Ps x 3, Pn x 1.5)
#incprice_EAf <- incprice_EAf %>% mutate(Ps=0.05,Pn=0.32)
#incprice_EAf_highPsPn <- incprice_EAf %>% mutate(Ps=3*Ps,Pn=1.5*Pn)

# Approximate the values for India from GCAM ref case, 2015-2050 (MER)
#incprice_Ind <- incprice_Ind_highPsPn <- data.frame(Y=seq(1,5, by=0.1))
# GCAM ref case and high price variant (Ps x 3, Pn x 1.5)
#incprice_Ind <- incprice_Ind %>% mutate(Ps=0.03,Pn=0.13)
#incprice_Ind_highPsPn <- incprice_Ind %>% mutate(Ps=3*Ps,Pn=1.5*Pn)

# calculate demand and elasticities, return together with all inputs, save results
# in each case for default and doubled prices

# for full ranges of all parameters
all_update9_cnstrlam <- apply(
  param_data_update9_cnstrlam,1,food.dmnd.plus,
  princdata=incprice_def) %>% bind_rows()
save(all_update9_cnstrlam,file=paste0(input_derived_path,"all_update9_cnstrlam.Rdata"))
all_update9_cnstrlam_2xPsPn <- apply(
  param_data_update9_cnstrlam,1,food.dmnd.plus,
  princdata=incprice_2xPsPn) %>% bind_rows()
save(all_update9_cnstrlam_2xPsPn,file=paste0(input_derived_path,"all_update9_cnstrlam_2xPsPn.Rdata"))
all_update9_cnstrlam_2xPs <- apply(
  param_data_update9_cnstrlam,1,food.dmnd.plus,
  princdata=incprice_2xPs) %>% bind_rows()
save(all_update9_cnstrlam_2xPs,file=paste0(input_derived_path,"all_update9_cnstrlam_2xPs.Rdata"))
all_update9_cnstrlam_2xPn <- apply(
  param_data_update9_cnstrlam,1,food.dmnd.plus,
  princdata=incprice_2xPn) %>% bind_rows()
save(all_update9_cnstrlam_2xPn,file=paste0(input_derived_path,"all_update9_cnstrlam_2xPn.Rdata"))
all_update9_cnstrlam_PsPnHi <- apply(
  param_data_update9_cnstrlam,1,food.dmnd.plus,
  princdata=incprice_PsPnHi) %>% bind_rows()
# for some reason this would not save to the usual location; this was a workaround
save(all_update9_cnstrlam_PsPnHi,file="C:\\Users\\onei736\\Documents\\all_update9_cnstrlam_PsPnHi.Rdata")



#   ----------------------------------------------------------------------------
#   Calculate differences in outputs and save, for different price regimes and different
#   subsets of parameter uncertainties
#   ----------------------------------------------------------------------------

# function to subtract all elements of two dataframes of elasticities and food
# demand (df1 - df2), keeping columns that should not be subtracted (Y, iteration)
subtract_dfs <- function(df1,df2) {
  cbind(
    subset(df2, select = c(Y:iteration)),
    subset(df1, select = -c(Y:iteration)) - subset(df2, select = -c(Y:iteration)))
}

# calculate differences in demands and elasticities
# full range of uncertainty - double both prices
all_diffs_update9_cnstrlam_2xPsPn <-
  subtract_dfs(all_update9_cnstrlam_2xPsPn,all_update9_cnstrlam)
save(all_diffs_update9_cnstrlam_2xPsPn,
     file=paste0(input_derived_path,"all_diffs_update9_cnstrlam_2xPsPn.Rdata"))
# full range of uncertainty - double Ps only
all_diffs_update9_cnstrlam_2xPs <-
  subtract_dfs(all_update9_cnstrlam_2xPs,all_update9_cnstrlam)
save(all_diffs_update9_cnstrlam_2xPs,
     file=paste0(input_derived_path,"all_diffs_update9_cnstrlam_2xPs.Rdata"))
# full range of uncertainty - double Pn only
all_diffs_update9_cnstrlam_2xPn <-
  subtract_dfs(all_update9_cnstrlam_2xPn,all_update9_cnstrlam)
save(all_diffs_update9_cnstrlam_2xPn,
     file=paste0(input_derived_path,"all_diffs_update9_cnstrlam_2xPn.Rdata"))
# price parameter uncertainty only
all_diffs_update9_cnstrlam_MLprice_2xPsPn <-
  subtract_dfs(all_update9_cnstrlam_MLprice_2xPsPn,all_update9_cnstrlam_MLprice)
save(all_diffs_update9_cnstrlam_MLprice_2xPsPn,
     file=paste0(input_derived_path,"all_diffs_update9_cnstrlam_MLprice_2xPsPn.Rdata"))
# income parameter uncertainty only
all_diffs_update9_cnstrlam_MLinc_2xPsPn <-
  subtract_dfs(all_update9_cnstrlam_MLinc_2xPsPn,all_update9_cnstrlam_MLinc)
save(all_diffs_update9_cnstrlam_MLinc_2xPsPn,
     file=paste0(input_derived_path,"all_diffs_update9_cnstrlam_MLinc_2xPsPn.Rdata"))
# scale parameter uncertainty only
all_diffs_update9_cnstrlam_MLscale_2xPsPn <-
  subtract_dfs(all_update9_cnstrlam_MLscale_2xPsPn,all_update9_cnstrlam_MLscale)
save(all_diffs_update9_cnstrlam_MLscale_2xPsPn,
     file=paste0(input_derived_path,"all_diffs_update9_cnstrlam_MLscale_2xPsPn.Rdata"))
# Pm parameter uncertainty only
all_diffs_update9_cnstrlam_MLPm_2xPsPn <-
  subtract_dfs(all_update9_cnstrlam_MLPm_2xPsPn,all_update9_cnstrlam_MLPm)
save(all_diffs_update9_cnstrlam_MLPm_2xPsPn,
     file=paste0(input_derived_path,"all_diffs_update9_cnstrlam_MLPm_2xPsPn.Rdata"))

# ------------------------------------------------------------------------------
# Read in files of derived data: all inputs and outputs (update9 cnstrlam only)
# ------------------------------------------------------------------------------

# update9 cnstrlam data
# absolute demand
load(paste0(input_derived_path,"all_update9_cnstrlam.Rdata"))
load(paste0(input_derived_path,"all_update9_cnstrlam_2xPsPn.Rdata"))
load(paste0(input_derived_path,"all_update9_cnstrlam_2xPs.Rdata"))
load(paste0(input_derived_path,"all_update9_cnstrlam_2xPn.Rdata"))
load(paste0(input_derived_path,"all_update9_cnstrlam_MLprice.Rdata"))
load(paste0(input_derived_path,"all_update9_cnstrlam_MLprice_2xPsPn.Rdata"))
load(paste0(input_derived_path,"all_update9_cnstrlam_MLinc.Rdata"))
load(paste0(input_derived_path,"all_update9_cnstrlam_MLinc_2xPsPn.Rdata"))
load(paste0(input_derived_path,"all_update9_cnstrlam_MLscale.Rdata"))
load(paste0(input_derived_path,"all_update9_cnstrlam_MLscale_2xPsPn.Rdata"))
load(paste0(input_derived_path,"all_update9_cnstrlam_MLPm.Rdata"))
load(paste0(input_derived_path,"all_update9_cnstrlam_MLPm_2xPsPn.Rdata"))

# differences in demand
load(paste0(input_derived_path,"all_diffs_update9_cnstrlam_2xPsPn.Rdata"))
load(paste0(input_derived_path,"all_diffs_update9_cnstrlam_2xPs.Rdata"))
load(paste0(input_derived_path,"all_diffs_update9_cnstrlam_2xPn.Rdata"))
load(paste0(input_derived_path,"all_diffs_update9_cnstrlam_MLprice_2xPsPn.Rdata"))
load(paste0(input_derived_path,"all_diffs_update9_cnstrlam_MLinc_2xPsPn.Rdata"))
load(paste0(input_derived_path,"all_diffs_update9_cnstrlam_MLscale_2xPsPn.Rdata"))
load(paste0(input_derived_path,"all_diffs_update9_cnstrlam_MLPm_2xPsPn.Rdata"))

# sample of observational data
load(paste0(input_derived_path,"obs_data_sample_500.RData"))

# parameters representing ML + confidence intervals, and hi/lo price sensitivity
load(paste0(input_derived_path,"params_update9_cnstrlam.Rdata"))
load(paste0(input_derived_path,"params_joint_price_update9_cnstrlam.Rdata"))

# files for FE model

# raw data, and a smaller sample of it
load(paste0(input_derived_path,
            "param_data_update9_cnstrlam_agg32FE_params.RData"))
load(paste0(input_derived_path,
            "param_data_update9_cnstrlam_agg32FE_FEs.RData"))
load(paste0(input_derived_path,
            "param_data_update9_cnstrlam_agg32FE_params_10k.RData"))
load(paste0(input_derived_path,
            "param_data_update9_cnstrlam_agg32FE_FEs_10k.RData"))

# demand -- global (common to all regions)
load(paste0(input_derived_path,"demand_update9_cnstrlam_agg32FE_noFE.RData"))
load(paste0(input_derived_path,"demand_update9_cnstrlam_agg32FE_noFE_2xPsPn.RData"))
assign("demand_noFE_2xPsPn",demand_noFE)
load(paste0(input_derived_path,"demand_update9_cnstrlam_agg32FE_noFE_2xPs.RData"))
assign("demand_noFE_2xPs",demand_noFE)
load(paste0(input_derived_path,"demand_update9_cnstrlam_agg32FE_noFE_2xPn.RData"))
assign("demand_noFE_2xPn",demand_noFE)
load(paste0(input_derived_path,"demand_update9_cnstrlam_agg32FE_noFE_PsPnHi.RData"))
assign("demand_noFE_PsPnHi",demand_noFE)

# demand -- regional
# default prices
reglist <- c(9,15) # make this 1:32 to read in all regions
for (i in 1:length(reglist)) {
  reg <- reglist[i]
#  if (i==15) next # this region is missing
  load(paste0(input_derived_path,"demand_update9_cnstrlam_agg32FE_R",reg,".RData"))
  assign(paste0("demand_update9_cnstrlam_agg32FE_R",reg),demand_reg)
}
# 2xPsPn
reglist <- c(9,15) # make this 1:32 to read in all regions
for (i in 1:length(reglist)) {
  reg <- reglist[i]
  #  if (i==15) next # this region is missing
  load(paste0(input_derived_path,"demand_update9_cnstrlam_agg32FE_2xPsPn_R",reg,".RData"))
  assign(paste0("demand_update9_cnstrlam_agg32FE_2xPsPn_R",reg),demand_reg)
}
# 2xPs
for (i in 1:32) {
  if (i==15) next # this region is missing
  load(paste0(input_derived_path,"demand_update9_cnstrlam_agg32FE_2xPs_R",i,".RData"))
  assign(paste0("demand_update9_cnstrlam_agg32FE_2xPs_R",i),demand_reg)
}
# 2xPn
for (i in 1:32) {
  if (i==15) next # this region is missing
  load(paste0(input_derived_path,"demand_update9_cnstrlam_agg32FE_2xPn_R",i,".RData"))
  assign(paste0("demand_update9_cnstrlam_agg32FE_2xPn_R",i),demand_reg)
}
# PsPnHi
for (i in 1:32) {
  if (i==15) next # this region is missing
  load(paste0(input_derived_path,"demand_update9_cnstrlam_agg32FE_PsPnHi_R",i,".RData"))
  assign(paste0("demand_update9_cnstrlam_agg32FE_PsPnHi_R",i),demand_reg)
}

# differences in demand
# 2xPsPn
reglist <- c(9,15) # make this 1:32 to read in all regions
for (i in 1:length(reglist)) {
  reg <- reglist[i]
  #  if (i==15) next # this region is missing
  load(paste0(input_derived_path,"all_diffs_update9_cnstrlam_agg32FE_2xPsPn_R",reg,".RData"))
  assign(paste0("all_diffs_update9_cnstrlam_agg32FE_2xPsPn_R",reg),all_diffs)
}
# 2xPs
reglist <- c(9,15) # make this 1:32 to read in all regions
for (i in 1:length(reglist)) {
  reg <- reglist[i]
  #  if (i==15) next # this region is missing
  load(paste0(input_derived_path,"all_diffs_update9_cnstrlam_agg32FE_2xPs_R",reg,".RData"))
  assign(paste0("all_diffs_update9_cnstrlam_agg32FE_2xPs_R",reg),all_diffs)
}
# 2xPn
reglist <- c(9,15) # make this 1:32 to read in all regions
for (i in 1:length(reglist)) {
  reg <- reglist[i]
  #  if (i==15) next # this region is missing
  load(paste0(input_derived_path,"all_diffs_update9_cnstrlam_agg32FE_2xPn_R",reg,".RData"))
  assign(paste0("all_diffs_update9_cnstrlam_agg32FE_2xPn_R",reg),all_diffs)
}

# observational data
load(paste0(input_derived_path,"obs_data_gcam32.RData"))

# parameters representing ML + confidence intervals, and hi/lo price sensitivity
load(paste0(input_derived_path,"params_ML95_update9_cnstrlam_agg32FE_params.Rdata"))
load(paste0(input_derived_path,"params_ML95_update9_cnstrlam_agg32FE_FEs.Rdata"))
load(paste0(input_derived_path,"params_joint_price_update9_cnstrlam_agg32FE_params.Rdata"))
load(paste0(input_derived_path,"params_joint_price_update9_cnstrlam_agg32FE_FEs.Rdata"))
load(paste0(input_derived_path,"params_joint_inc_update9_cnstrlam_agg32FE_params.Rdata"))
load(paste0(input_derived_path,"params_joint_inc_update9_cnstrlam_agg32FE_FEs.Rdata"))
load(paste0(input_derived_path,"params_joint_scale_update9_cnstrlam_agg32FE_params.Rdata"))
load(paste0(input_derived_path,"params_joint_scale_update9_cnstrlam_agg32FE_FEs.Rdata"))

# ------------------------------------------------------------------------------
# Create tables of parameter values
# ------------------------------------------------------------------------------

# ML parameters and independent parameter ranges
params_df <- data.frame(
  Var_Name = colnames(params_update9_cnstrlam),
  ML = as.vector(t(params_update9_cnstrlam['ML',])),
  Indep_lo = as.vector(t(params_update9_cnstrlam['Lo indep',])),
  Indep_hi = as.vector(t(params_update9_cnstrlam['Hi indep',])))
kable(params_df,format = "simple")

# Hi/lo price response params, ML parameters and independent parameter ranges
# remove last (14th) column from the joint parameter df to be same length as the other
params_df <- data.frame(
  Var_Name = colnames(params_update9_cnstrlam),
  Lo_price_response = as.vector(t(params_joint_price_update9_cnstrlam['lo',-14])),
  Hi_price_response = as.vector(t(params_joint_price_update9_cnstrlam['hi',-14])),
  ML = as.vector(t(params_update9_cnstrlam['ML',])),
  Indep_lo = as.vector(t(params_update9_cnstrlam['Lo indep',])),
  Indep_hi = as.vector(t(params_update9_cnstrlam['Hi indep',])))
kable(params_df,format = "simple")

# Hi/lo price response params, ML parameters and independent parameter ranges -- FE model
# remove last (14th) column from the joint parameter df to be same length as the other
params_df <- data.frame(
  Var_Name = colnames(params_ML95_update9_cnstrlam_agg32FE_params),
  Lo_price_response = as.vector(t(params_joint_price_update9_cnstrlam_agg32FE_params['lo',-14])),
  Hi_price_response = as.vector(t(params_joint_price_update9_cnstrlam_agg32FE_params['hi',-14])),
  ML = as.vector(t(params_ML95_update9_cnstrlam_agg32FE_params['ML',])),
  Indep_lo = as.vector(t(params_ML95_update9_cnstrlam_agg32FE_params['Lo indep',])),
  Indep_hi = as.vector(t(params_ML95_update9_cnstrlam_agg32FE_params['Hi indep',])))
kable(params_df,format = "simple")

# table comparing the best estimates
params_df <- data.frame(
  Var_Name = colnames(params_edmonds),
  edmonds_ml = as.vector(t(params_edmonds['ML',])),
  orig9_ml = as.vector(t(params_orig9['ML',])),
  gcam = append(params_gcam,NA),
  update9_ml = as.vector(t(params_update9_cnstrlam['ML',])),
  update11_ml = as.vector(t(params_update11['ML',])))
#kable(params_df,format = "simple")

# set up a table comparing the confidence intervals
quants_df <- data.frame(
  Var_Name = colnames(params_edmonds),
  edmonds_lo = as.vector(t(params_edmonds['Lo',])),
  edmonds_hi = as.vector(t(params_edmonds['Hi',])),
  orig9_lo = as.vector(t(params_orig9['Lo',])),
  orig9_hi = as.vector(t(params_orig9['Hi',])),
  update9_lo = as.vector(t(params_update9_cnstrlam['Lo',])),
  update9_hi = as.vector(t(params_update9_cnstrlam['Hi',])))
#kable(quants_df,format = "simple")

# set up a table comparing best estimates and confidence intervals
quants_df <- data.frame(
  Var_Name = colnames(params_orig),
  orig9_ml = as.vector(t(params_orig9['ML',])),
  orig9_lo = as.vector(t(params_orig9['Lo',])),
  orig9_hi = as.vector(t(params_orig9['Hi',])),
  update9_ml = as.vector(t(params_update9_cnstrlam['ML',])),
  update9_lo = as.vector(t(params_update9_cnstrlam['Lo',])),
  update9_hi = as.vector(t(params_update9_cnstrlam['Hi',])))
#kable(quants_df,format = "simple")

# ------------------------------------------------------------------------------
# Plot specific scenarios of demand and elasticities
# ------------------------------------------------------------------------------

#    ---------------------------------------------------------------------------
#    Functions
#    ---------------------------------------------------------------------------

# function for making demand plot for Qs, Qn, Qtot for three datasets (e.g., ML
# hi/lo sensitivity parameters)
make_demand3_plot <- function(df1,df2,df3,title1text,title2text) {
  thickness <- 1
  g <- ggplot()+
    geom_line(data=df1,aes(x= Y,y= Qs,color= "Staples"),
              linewidth = thickness)+
    geom_line(data=df1,aes(x= Y,y= Qn,color= "Non Staples"),
              linewidth = thickness)+
    geom_line(data=df1,aes(x= Y,y= Qtot,color= "Total"),
              linewidth = thickness)+

    geom_line(data=df2,aes(x= Y,y= Qs,color= "Staples"),
              linetype = "dashed",linewidth = thickness)+
    geom_line(data=df2,aes(x= Y,y= Qn,color= "Non Staples"),
              linetype = "dashed",linewidth = thickness)+
    geom_line(data=df2,aes(x= Y,y= Qtot,color= "Total"),
              linetype = "dashed",linewidth = thickness)+

    geom_line(data=df3,aes(x= Y,y= Qs,color= "Staples"),
              linetype = "dotted",linewidth = thickness)+
    geom_line(data=df3,aes(x= Y,y= Qn,color= "Non Staples"),
              linetype = "dotted",linewidth = thickness)+
    geom_line(data=df3,aes(x= Y,y= Qtot,color= "Total"),
              linetype = "dotted",linewidth = thickness)+

#    scale_colour_manual(name = "Demand", values = c("red","green","blue")) +
    scale_colour_manual(name = "Demand", values = c("red","gray","green","black","blue")) +
    xlim(0,100) +
    ylim(-1,6) +
    labs(title = title1text,subtitle = title2text) +
    xlab("GDP per capita in thousand USD" )+
    ylab("Thousand calories") +
    coord_fixed(100/6)
}

# function for making plot of price elasticities for three sets of parameters
# (e.g., ML, hi/lo sensitivity parameters)
make_priceelast3_plot <- function(label1,df1,label2,df2,label3,df3,title1text,
                                  title2text) {

  # gather elasticities for faceting
  elastcolnames <- c("elast.ss","elast.nn","elast.sn","elast.ns")
  df1_long <- gather(df1,key="parameter", value="value",elastcolnames)
  df2_long <- gather(df2,key="parameter", value="value",elastcolnames)
  df3_long <- gather(df3,key="parameter", value="value",elastcolnames)

  g <- ggplot()+
    geom_line(data=df1_long,aes(x= Y,y= value,linetype= label1))+
    geom_line(data=df2_long,aes(x= Y,y= value,linetype= label2))+
    geom_line(data=df3_long,aes(x= Y,y= value,linetype= label3))+

    scale_linetype_manual(name = "Case", values = c("dotted","dashed","solid")) +

    facet_wrap(~parameter, ncol=2, scales = "free") +

    xlim(0,30) +
    ylim(-0.5,0.5) +
    labs(title = title1text, subtitle = title2text) +
    xlab("GDP per capita in thousand USD" )+
    ylab("Elasticity")
}

# function for making plot of income elasticities for three sets of parameters
# (e.g., ML, hi/lo sensitivity parameters)
make_incelast3_plot <- function(label1,df1,label2,df2,label3,df3,title1text,
                                title2text) {

  # gather elasticities for faceting
  inccolnames <- c("eta.s","eta.n")
  df1_long <- gather(df1,key="parameter", value="value",inccolnames)
  df2_long <- gather(df2,key="parameter", value="value",inccolnames)
  df3_long <- gather(df3,key="parameter", value="value",inccolnames)

  g <- ggplot()+
    geom_line(data=df1_long,aes(x= Y,y= value,linetype = label1))+
    geom_line(data=df2_long,aes(x= Y,y= value,linetype = label2))+
    geom_line(data=df3_long,aes(x= Y,y= value,linetype = label3))+

    scale_linetype_manual(name = "Case", values = c("dotted","dashed","solid")) +

    facet_wrap(~parameter, ncol=2, scales = "free") +

    xlim(0,30) +
    ylim(-0.35,1.0) +
    labs(title = title1text,subtitle = title2text) +
    xlab("GDP per capita in thousand USD" )+
    ylab("Elasticity")
}

#    ---------------------------------------------------------------------------
#    Plots
#    ---------------------------------------------------------------------------

# update9 flexlam data

# plot baseline demand functions (all default prices)
make_demand3_plot(
  Food_Demand_update9_flexlam,
  Food_Demand_update9_flexlam_hisens_price,
  Food_Demand_update9_flexlam_losens_price,
  "ML and Hi/Lo price sensitivity parameters, update9 flexible lambda",
  "Default prices in all cases") %>%
  plot()

# plot price elasticities
make_priceelast3_plot(
  "ML",Food_Demand_update9_flexlam,
  "Hi sens",Food_Demand_update9_flexlam_hisens_price,
  "Lo sens",Food_Demand_update9_flexlam_losens_price,
  "ML and Hi/Lo price elasticties, update9 flexible lambda") %>%
  plot()

# plot income elasticities
make_incelast3_plot(
  "ML",Food_Demand_update9_flexlam,
  "Hi sens",Food_Demand_update9_flexlam_hisens_price,
  "Lo sens",Food_Demand_update9_flexlam_losens_price,
  "ML and Hi/Lo income elasticties, update9 flexible lambda") %>%
  plot()

# update9 cnstrlam data

# get all data for 3 cases
ML_data <- all_update9_cnstrlam[all_update9_cnstrlam$iteration ==
                                  params_update9_cnstrlam['ML','iteration'],]
hi_joint_price_data <-
  all_update9_cnstrlam[all_update9_cnstrlam$iteration ==
                         params_joint_price_update9_cnstrlam['hi','iteration'],]
lo_joint_price_data <-
  all_update9_cnstrlam[all_update9_cnstrlam$iteration ==
                         params_joint_price_update9_cnstrlam['lo','iteration'],]

# plot baseline demand functions (all default prices)
make_demand3_plot(ML_data,lo_joint_price_data,hi_joint_price_data,
                  "Food demand, ML and Hi/Lo price responses",
                  "Default prices in all cases, update9 constrained lambda") %>%
  plot()
ggsave("demand_update9_cnstrlam_hl_sens_price.png",path = fig_path)

# plot price elasticities
make_priceelast3_plot(
  "ML",ML_data,
  "Lo dem",lo_joint_price_data,
  "Hi dem",hi_joint_price_data,
  "Price elasticities, ML and Hi/Lo price responses",
  "Update9 constrained lambda") %>%
  plot()
ggsave("elasticity_price_update9_cnstrlam_hl_sens_price.png",path = fig_path)

# plot income elasticities
make_incelast3_plot(
  "ML",ML_data,
  "Lo dem",lo_joint_price_data,
  "Hi dem",hi_joint_price_data,
  "Income elasticities, ML and Hi/Lo price responses",
  "Update9 constrained lambda") %>%
  plot()
ggsave("elasticity_inc_update9_cnstrlam_hl_sens_price.png",path = fig_path)

# FE model
# we don't have demand calculated for all parameter samples, only 10k subset,
# so ML or hi/lo sets may not be available already; therefore identify from full
# sample here

# joint price uncertainty, plotted for default prices
ML_data <-
  demand_update9_cnstrlam_agg32FE_R6[demand_update9_cnstrlam_agg32FE_R6$iteration ==
    params_ML95_update9_cnstrlam_agg32FE_params['ML','iteration'],]
hi_joint_price_data <-
  demand_update9_cnstrlam_agg32FE_R6[demand_update9_cnstrlam_agg32FE_R6$iteration ==
                                       params_ML95_update9_cnstrlam_agg32FE_params['hi','iteration'],]
lo_joint_price_data <-
  demand_update9_cnstrlam_agg32FE_R6[demand_update9_cnstrlam_agg32FE_R6$iteration ==
                                       params_ML95_update9_cnstrlam_agg32FE_params['lo','iteration'],]
make_demand3_plot(ML_data,lo_joint_price_data,hi_joint_price_data,
                  "Food demand, ML and Hi/Lo price responses",
                  "Default prices in all cases, update9 constrained lambda agg32FE") %>%
  plot()
ggsave("demand_update9_cnstrlam_hl_sens_price.png",path = fig_path)

params_ML95_update9_cnstrlam_agg32FE_params['ML','iteration']

param_data_update9_cnstrlam_agg32FE_params


# ------------------------------------------------------------------------------
# Plot specific scenarios of differences in demand
# ------------------------------------------------------------------------------

# function for making plot of demand differences, faceted for Qs, Qn, Qtot,
# between one set of three datasets and a second set of three datasets (e.g.,
# differences due to different price assumptions for three different parameter sets)
make_demand3_diffs_plot <- function(label1,df1,label2,df2,label3,df3,
                                    title1text,title2text) {

  # gather demand differences for faceting
  demandcolnames <- c("Qs","Qn","Qtot")
  df1_long <- gather(df1,key="parameter", value="value",demandcolnames)
  df2_long <- gather(df2,key="parameter", value="value",demandcolnames)
  df3_long <- gather(df3,key="parameter", value="value",demandcolnames)

  thickness <- 1
  g <- ggplot()+
    geom_line(data=df1_long,aes(x= Y,y= value,linetype= label1),
              linewidth = thickness)+
    geom_line(data=df2_long,aes(x= Y,y= value,linetype= label2),
              linewidth = thickness)+
    geom_line(data=df3_long,aes(x= Y,y= value,linetype= label3),
              linewidth = thickness)+

    scale_linetype_manual(name = "Case", values = c("dotted","dashed","solid")) +

    facet_wrap(~parameter, ncol=3, scales = "free") +

    xlim(0,30) +
    ylim(-2.5,1) +
    labs(title = title1text,subtitle = title2text) +
    xlab("GDP per capita in thousand USD" )+
    ylab("Thousand calories")
}

# update9 cnstrlam data

# demand differences, doubling both prices
# get all data for 3 cases
ML_data <-
  all_diffs_update9_cnstrlam_2xPsPn[all_diffs_update9_cnstrlam_2xPsPn$iteration ==
                                      params_update9_cnstrlam['ML','iteration'],]
hi_joint_price_data <-
  all_diffs_update9_cnstrlam_2xPsPn[all_diffs_update9_cnstrlam_2xPsPn$iteration ==
                                      params_joint_price_update9_cnstrlam['hi','iteration'],]
lo_joint_price_data <-
  all_diffs_update9_cnstrlam_2xPsPn[all_diffs_update9_cnstrlam_2xPsPn$iteration ==
                                      params_joint_price_update9_cnstrlam['lo','iteration'],]
# plot
make_demand3_diffs_plot(
  "ML",ML_data,
  "Lo dem",lo_joint_price_data,
  "Hi dem",hi_joint_price_data,
  "Demand differences, ML and Hi/Lo price sensitivity",
  "Doubling Ps & Pn, update9 constrained lambda") %>%
  plot()
ggsave("demand_diffs_update9_cnstrlam_hl_sens_price_2xPsPn.png",path = fig_path)

# demand differences, doubling Ps
# get all data for 3 cases
ML_data <-
  all_diffs_update9_cnstrlam_2xPs[all_diffs_update9_cnstrlam_2xPs$iteration ==
                                      params_update9_cnstrlam['ML','iteration'],]
hi_joint_price_data <-
  all_diffs_update9_cnstrlam_2xPs[all_diffs_update9_cnstrlam_2xPs$iteration ==
                                      params_joint_price_update9_cnstrlam['hi','iteration'],]
lo_joint_price_data <-
  all_diffs_update9_cnstrlam_2xPs[all_diffs_update9_cnstrlam_2xPs$iteration ==
                                      params_joint_price_update9_cnstrlam['lo','iteration'],]
# plot
make_demand3_diffs_plot(
  "ML",ML_data,
  "Lo dem",lo_joint_price_data,
  "Hi dem",hi_joint_price_data,
  "Demand differences, ML and Hi/Lo price sensitivity",
  "Doubling Ps only, update9 constrained lambda") %>%
  plot()
ggsave("demand_diffs_update9_cnstrlam_hl_sens_price_2xPs.png",path = fig_path)

# demand differences, doubling Pn
# get all data for 3 cases
ML_data <-
  all_diffs_update9_cnstrlam_2xPn[all_diffs_update9_cnstrlam_2xPn$iteration ==
                                    params_update9_cnstrlam['ML','iteration'],]
hi_joint_price_data <-
  all_diffs_update9_cnstrlam_2xPn[all_diffs_update9_cnstrlam_2xPn$iteration ==
                                    params_joint_price_update9_cnstrlam['hi','iteration'],]
lo_joint_price_data <-
  all_diffs_update9_cnstrlam_2xPn[all_diffs_update9_cnstrlam_2xPn$iteration ==
                                      params_joint_price_update9_cnstrlam['lo','iteration'],]
# plot
make_demand3_diffs_plot(
  "ML",ML_data,
  "Lo dem",lo_joint_price_data,
  "Hi dem",hi_joint_price_data,
  "Demand differences, ML and Hi/Lo price sensitivity",
  "Doubling Pn only, update9 constrained lambda") %>%
  plot()
ggsave("demand_diffs_update9_cnstrlam_hl_sens_price_2xPn.png",path = fig_path)

# ...the rest needs to be updated ...

# double price of staples only
make_demand3_diffs_plot(
  "ML",Food_Demand_update9_cnstrlam,
  Food_Demand_update9_cnstrlam_highPs,
  "Hi sens",Food_Demand_update9_cnstrlam_hisens_price,
  Food_Demand_update9_cnstrlam_hisens_price_highPs,
  "Lo sens",Food_Demand_update9_cnstrlam_losens_price,
  Food_Demand_update9_cnstrlam_losens_price_highPs,
  "ML and Hi/Lo price sensitivity parameters, update9 constrained lambda",
  "Demand difference due to doubling price of Ps") %>%
  plot()
# double price of non staples only
make_demand3_diffs_plot(
  "ML",Food_Demand_update9_cnstrlam,
  Food_Demand_update9_cnstrlam_highPn,
  "Hi sens",Food_Demand_update9_cnstrlam_hisens_price,
  Food_Demand_update9_cnstrlam_hisens_price_highPn,
  "Lo sens",Food_Demand_update9_cnstrlam_losens_price,
  Food_Demand_update9_cnstrlam_losens_price_highPn,
  "ML and Hi/Lo price sensitivity parameters, update9 constrained lambda",
  "Demand difference due to doubling price of Pn") %>%
  plot()

# update9 flexlam data

# plot differences in demand between price variants and default prices
make_demand3_diffs_plot(
  "ML",Food_Demand_update9_flexlam,
  Food_Demand_update9_flexlam_highPsPn,
  "Hi sens",Food_Demand_update9_flexlam_hisens_price,
  Food_Demand_update9_flexlam_hisens_price_highPsPn,
  "Lo sens",Food_Demand_update9_flexlam_losens_price,
  Food_Demand_update9_flexlam_losens_price_highPsPn,
  "ML and Hi/Lo price sensitivity parameters, update9 flexible lambda",
  "Demand difference due to doubling prices of Ps, Pn") %>%
  plot()
# double price of staples only
make_demand3_diffs_plot(
  "ML",Food_Demand_update9_flexlam,
  Food_Demand_update9_flexlam_highPs,
  "Hi sens",Food_Demand_update9_flexlam_hisens_price,
  Food_Demand_update9_flexlam_hisens_price_highPs,
  "Lo sens",Food_Demand_update9_flexlam_losens_price,
  Food_Demand_update9_flexlam_losens_price_highPs,
  "ML and Hi/Lo price sensitivity parameters, update9 constrained lambda",
  "Demand difference due to doubling price of Ps") %>%
  plot()
# double price of non staples only
make_demand3_diffs_plot(
  "ML",Food_Demand_update9_flexlam,
  Food_Demand_update9_flexlam_highPn,
  "Hi sens",Food_Demand_update9_flexlam_hisens_price,
  Food_Demand_update9_flexlam_hisens_price_highPn,
  "Lo sens",Food_Demand_update9_flexlam_losens_price,
  Food_Demand_update9_flexlam_losens_price_highPn,
  "ML and Hi/Lo price sensitivity parameters, update9 constrained lambda",
  "Demand difference due to doubling price of Pn") %>%
  plot()

# ------------------------------------------------------------------------------
# Plot values of fixed effects across regions
# ------------------------------------------------------------------------------

# plot ML values of fixed effects
FE_ML <- data.frame()
for (i in 1:32) {
#  if (i == 15) next
  FE_ML <- rbind(FE_ML,
                 c(as.numeric(params_ML95_update9_cnstrlam_agg32FE_FEs[[i]]['ML','GCAM_region_ID']),
                   as.numeric(params_ML95_update9_cnstrlam_agg32FE_FEs[[i]]['ML','staples_FE'])))
}
plot(x=FE_ML[,1],y=FE_ML[,2],xlab = "GCAM region",ylab = "Fixed Effect, ML")

# ------------------------------------------------------------------------------
# Plot mean and quantiles of demand and elasticities for a dataset
# ------------------------------------------------------------------------------

# this should be changed to do the calculation of the mean and quantiles outside
# the function because now it is done in several different places, so all would
# need to be edited to make one change

# function to calculate and plot mean and 90% confidence interval of Qs, Qn, Qtot
# for a given food demand dataset by calling the make_demand3_plot() function
make_meanquant_demand_plot <- function(demand_data,title1text,title2text) {
  mean_by_inc <- demand_data %>% group_by(Y) %>%
    summarize(across(-iteration, mean))
  q95_by_inc <- demand_data %>% group_by(Y) %>%
    summarize(across(-iteration, ~quantile(., probs = 0.95, na.rm = TRUE)))
  q5_by_inc <- demand_data %>% group_by(Y) %>%
    summarize(across(-iteration, ~quantile(., probs = 0.05, na.rm = TRUE)))
  make_demand3_plot(mean_by_inc,q5_by_inc,q95_by_inc,title1text,title2text)
}

# same function as above but adding observations to the plot
make_meanquant_demand_plot_w_obs <- function(demand_data,obs_data,title1text,title2text) {
  mean_by_inc <- demand_data %>% group_by(Y) %>%
    summarize(across(-iteration, mean))
  q95_by_inc <- demand_data %>% group_by(Y) %>%
    summarize(across(-iteration, ~quantile(., probs = 0.95, na.rm = TRUE)))
  q5_by_inc <- demand_data %>% group_by(Y) %>%
    summarize(across(-iteration, ~quantile(., probs = 0.05, na.rm = TRUE)))
  make_demand3_plot(mean_by_inc,q5_by_inc,q95_by_inc,title1text,title2text) +
    geom_point(data=obs_data,aes(x=Y,y=Qs,color="Staples obs.")) +
    geom_point(data=obs_data,aes(x=Y,y=Qn,color="Non staples obs."))
}


# function to calculate and plot mean and 90% confidence interval of price
# elasticity parameters for a given food demand dataset by calling the
# make_priceelast3_plot() function
make_meanquant_prelast_plot <- function(demand_data,title1text,title2text) {
  mean_by_inc <- demand_data %>% group_by(Y) %>%
    summarize(across(-iteration, mean))
  q95_by_inc <- demand_data %>% group_by(Y) %>%
    summarize(across(-iteration, ~quantile(., probs = 0.95, na.rm = TRUE)))
  q5_by_inc <- demand_data %>% group_by(Y) %>%
    summarize(across(-iteration, ~quantile(., probs = 0.05, na.rm = TRUE)))
  make_priceelast3_plot("Mean",mean_by_inc,"95%",q95_by_inc,"5%",q5_by_inc,
                        title1text,title2text)
}

# function to calculate and plot mean and 90% confidence interval of income
# elasticity parameters for a given food demand dataset by calling the
# make_incelast3_plot() function
make_meanquant_incelast_plot <- function(demand_data,title1text,title2text) {
  mean_by_inc <- demand_data %>% group_by(Y) %>%
    summarize(across(-iteration, mean))
  q95_by_inc <- demand_data %>% group_by(Y) %>%
    summarize(across(-iteration, ~quantile(., probs = 0.95, na.rm = TRUE)))
  q5_by_inc <- demand_data %>% group_by(Y) %>%
    summarize(across(-iteration, ~quantile(., probs = 0.05, na.rm = TRUE)))
  make_incelast3_plot("Mean",mean_by_inc,"95%",q95_by_inc,"5%",q5_by_inc,
                      title1text,title2text)
}

# plot and save food demand uncertainty ranges
# full uncertainty
make_meanquant_demand_plot(all_update9_cnstrlam,
                           "Demand uncertainty, mean and 90% interval",
                           "Full uncertainty range, update9 constrained lambda") %>% plot()
ggsave("demand_update9_cnstrlam_meanquant_full.png",path = fig_path)
# uncertainty due to price parameters
make_meanquant_demand_plot(all_update9_cnstrlam_MLprice,
                           "Demand uncertainty, mean and 90% interval",
                           "Uncertainty due to price parameters, update9 constrained lambda") %>% plot()
ggsave("demand_update9_cnstrlam_meanquant_price.png",path = fig_path)
# uncertainty due to income parameters
make_meanquant_demand_plot(all_update9_cnstrlam_MLinc,
                           "Demand uncertainty, mean and 90% interval",
                           "Uncertainty due to income parameters, update9 constrained lambda") %>% plot()
ggsave("demand_update9_cnstrlam_meanquant_inc.png",path = fig_path)
# uncertainty due to scale parameters
make_meanquant_demand_plot(all_update9_cnstrlam_MLscale,
                           "Demand uncertainty, mean and 90% interval",
                           "Uncertainty due to scale parameters, update9 constrained lambda") %>% plot()
ggsave("demand_update9_cnstrlam_meanquant_scale.png",path = fig_path)
# full uncertainty with high prices for both Ps and Pn
make_meanquant_demand_plot(all_update9_cnstrlam_PsPnHi,
                           "Demand uncertainty, mean and 90% interval",
                           "Full uncertainty range, Ps=Pn=2, update9 constrained lambda") %>% plot()
# for some reason error was generated with usual path name, this was a workaround
ggsave("demand_update9_cnstrlam_meanquant_PsPnHi.png",path = "C:\\Users\\onei736\\Documents")

# plot and save price elasticity full uncertainty ranges
make_meanquant_prelast_plot(all_update9_cnstrlam,
                            "Price elasticity uncertainty, mean and 90% interval",
                            "Full uncertainty range, update9 constrained lambda") %>% plot()
ggsave("elasticity_price_update9_cnstrlam_meanquant_full.png",path = fig_path)

# plot and save income elasticity full uncertainty ranges
make_meanquant_incelast_plot(all_update9_cnstrlam,
                             "Income elasticity uncertainty, mean and 90% interval",
                             "Full uncertainty range, update9 constrained lambda") %>% plot()
ggsave("elasticity_inc_update9_cnstrlam_meanquant_full.png",path = fig_path)

# FE model

# plot and save food demand uncertainty ranges
# full uncertainty
make_meanquant_demand_plot(demand_update9_cnstrlam_agg32FE_R9 %>% select(-region),
                           "Demand uncertainty, mean and 90% interval, CAC",
                           "Full uncertainty range, update9 constrained lambda, agg32FE") %>% plot()
ggsave("demand_update9_cnstrlam_meanquant_full_R9.png",path = fig_path)
make_meanquant_demand_plot(demand_update9_cnstrlam_agg32FE_R15 %>% select(-region),
                           "Demand uncertainty, mean and 90% interval, Eur Non-EU",
                           "Full uncertainty range, update9 constrained lambda, agg32FE") %>% plot()
ggsave("demand_update9_cnstrlam_meanquant_full_R15.png",path = fig_path)

# full uncertainty with observations
obs_data_R9 <- obs_data_gcam32 %>% filter(GCAM_region_ID == 9)
make_meanquant_demand_plot_w_obs(demand_update9_cnstrlam_agg32FE_R9 %>% select(-region),
                                 obs_data_R9,
                           "Demand uncertainty, mean and 90% interval, CAC",
                           "Full uncertainty range, update9 constrained lambda, agg32FE") %>% plot()
ggsave("demand_update9_cnstrlam_meanquant_full_wobs_R9_limx.png",path = fig_path)
obs_data_R15 <- obs_data_gcam32 %>% filter(GCAM_region_ID == 15)
make_meanquant_demand_plot_w_obs(demand_update9_cnstrlam_agg32FE_R15 %>% select(-region),
                           obs_data_R15,
                           "Demand uncertainty, mean and 90% interval, Eur Non-EU",
                           "Full uncertainty range, update9 constrained lambda, agg32FE") %>% plot()
ggsave("demand_update9_cnstrlam_meanquant_full_wobs_R15.png",path = fig_path,
       width = 6, height = 4.5, dpi = 150)

# plot and save price elasticity full uncertainty ranges
make_meanquant_prelast_plot(demand_update9_cnstrlam_agg32FE_R9 %>% select(-region),
                            "Price elasticity uncertainty, mean and 90% interval, CAC",
                            "Full uncertainty range, update9 constrained lambda, agg32FE") %>% plot()
ggsave("elasticity_price_update9_cnstrlam_meanquant_full_R9.png",path = fig_path)
make_meanquant_prelast_plot(demand_update9_cnstrlam_agg32FE_R15 %>% select(-region),
                            "Price elasticity uncertainty, mean and 90% interval, Eur Non-EU",
                            "Full uncertainty range, update9 constrained lambda, agg32FE") %>% plot()
ggsave("elasticity_price_update9_cnstrlam_meanquant_full_R15.png",path = fig_path)

# plot and save income elasticity full uncertainty ranges
make_meanquant_incelast_plot(demand_update9_cnstrlam_agg32FE_R9 %>% select(-region),
                             "Income elasticity uncertainty, mean and 90% interval, CAC",
                             "Full uncertainty range, update9 constrained lambda, agg32FE") %>% plot()
ggsave("elasticity_inc_update9_cnstrlam_meanquant_full_R9.png",path = fig_path)
make_meanquant_incelast_plot(demand_update9_cnstrlam_agg32FE_R15 %>% select(-region),
                             "Income elasticity uncertainty, mean and 90% interval, Eur Non-EU",
                             "Full uncertainty range, update9 constrained lambda, agg32FE") %>% plot()
ggsave("elasticity_inc_update9_cnstrlam_meanquant_full_R15.png",path = fig_path)

# ------------------------------------------------------------------------------
# Plot mean and quantiles for differences in demand due to price changes, for
# full range and subsets of outcomes
# ------------------------------------------------------------------------------

# function for plotting mean and 90% confidence interval of Qs, Qn, and Qtot
# in black and faceted; used for plotting differences in demand
make_meanquant_demand_diffs_plot <- function(demand_data,title1text,title2text) {

  # calculate mean and quantiles, then gather each for faceting
  demandcolnames <- c("Qs","Qn","Qtot")
  mean_by_inc <- demand_data %>% group_by(Y) %>%
    summarize(across(-iteration, mean)) %>%
    gather(key="parameter", value="value",demandcolnames)
  q95_by_inc <- demand_data %>% group_by(Y) %>%
    summarize(across(-iteration, ~quantile(., probs = 0.95, na.rm = TRUE))) %>%
    gather(key="parameter", value="value",demandcolnames)
  q5_by_inc <- demand_data %>% group_by(Y) %>%
    summarize(across(-iteration, ~quantile(., probs = 0.05, na.rm = TRUE))) %>%
    gather(key="parameter", value="value",demandcolnames)

  thickness <- 1
  g <- ggplot()+
    geom_line(data=mean_by_inc,aes(x= Y,y= value,linetype = "Mean"),
              linewidth = thickness)+
    geom_line(data=q95_by_inc,aes(x= Y,y= value,linetype = "95%"),
              linewidth = thickness)+
    geom_line(data=q5_by_inc,aes(x= Y,y= value,linetype = "5%"),
              linewidth = thickness)+

    scale_linetype_manual(values = c("dashed", "dotted","solid")) +

    facet_wrap(~parameter, ncol=3, scales = "free") +

    xlim(0,30) +
    ylim(-1,1) +
    labs(title = title1text,subtitle = title2text) +
    xlab("GDP per capita in thousand USD" )+
    ylab("Thousand calories")
}

# plot differences in food demand
make_meanquant_demand_diffs_plot(
  all_diffs_update9_cnstrlam_2xPsPn,
  "Differences in demand, full uncertainty, Mean and 90% interval",
  "2 x Ps, Pn, update9 constrained lambda") %>%
  plot()
ggsave("demand_diffs_update9_cnstrlam_meanquant_full_2xPsPn.png",path = fig_path)
# price uncertainty
make_meanquant_demand_diffs_plot(
  all_diffs_update9_cnstrlam_MLprice_2xPsPn,
  "Differences in demand, price param uncertainty, Mean and 90% interval",
  "2 x Ps, Pn, update9 constrained lambda") %>%
  plot()
ggsave("demand_diffs_update9_cnstrlam_meanquant_price_2xPsPn.png",path = fig_path)
# income uncertainty
make_meanquant_demand_diffs_plot(
  all_diffs_update9_cnstrlam_MLinc_2xPsPn,
  "Differences in demand, income param uncertainty, Mean and 90% interval",
  "2 x Ps, Pn, update9 constrained lambda") %>%
  plot()
ggsave("demand_diffs_update9_cnstrlam_meanquant_inc_2xPsPn.png",path = fig_path)
# scale uncertainty
make_meanquant_demand_diffs_plot(
  all_diffs_update9_cnstrlam_MLscale_2xPsPn,
  "Differences in demand, scale param uncertainty, Mean and 90% interval",
  "2 x Ps, Pn, update9 constrained lambda") %>%
  plot()
ggsave("demand_diffs_update9_cnstrlam_meanquant_scale_2xPsPn.png",path = fig_path)
# Pm uncertainty
make_meanquant_demand_diffs_plot(
  all_diffs_update9_cnstrlam_MLPm_2xPsPn,
  "Differences in demand, Pm param uncertainty, Mean and 90% interval",
  "2 x Ps, Pn, update9 constrained lambda") %>%
  plot()
ggsave("demand_diffs_update9_cnstrlam_meanquant_Pm_2xPsPn.png",path = fig_path)

# for FE model

# plot differences in food demand
# 2xPsPn
make_meanquant_demand_diffs_plot(
  all_diffs_update9_cnstrlam_agg32FE_2xPsPn_R9 %>% select(-region),
  "Differences in demand, full uncertainty, Mean and 90% interval",
  "2 x Ps, Pn, update9 constrained lambda agg32FE, CAC") %>%
  plot()
ggsave("demand_diffs_update9_cnstrlam_aff32FE_meanquant_full_2xPsPn_R9.png",path = fig_path)
make_meanquant_demand_diffs_plot(
  all_diffs_update9_cnstrlam_agg32FE_2xPsPn_R15 %>% select(-region),
  "Differences in demand, full uncertainty, Mean and 90% interval",
  "2 x Ps, Pn, update9 constrained lambda agg32FE, Eur Non-EU") %>%
  plot()
ggsave("demand_diffs_update9_cnstrlam_aff32FE_meanquant_full_2xPsPn_R15.png",path = fig_path)
# 2xPs
make_meanquant_demand_diffs_plot(
  all_diffs_update9_cnstrlam_agg32FE_2xPs_R6 %>% select(-region),
  "Differences in demand, full uncertainty, Mean and 90% interval",
  "2 x Ps, update9 constrained lambda agg32FE, Australia_NZ") %>%
  plot()
ggsave("demand_diffs_update9_cnstrlam_aff32FE_meanquant_full_2xPs_R6_limx.png",path = fig_path)
make_meanquant_demand_diffs_plot(
  all_diffs_update9_cnstrlam_agg32FE_2xPs_R28 %>% select(-region),
  "Differences in demand, full uncertainty, Mean and 90% interval",
  "2 x Ps, update9 constrained lambda agg32FE, S Korea") %>%
  plot()
ggsave("demand_diffs_update9_cnstrlam_aff32FE_meanquant_full_2xPs_R28_limx.png",path = fig_path)
# 2xPn
make_meanquant_demand_diffs_plot(
  all_diffs_update9_cnstrlam_agg32FE_2xPn_R6 %>% select(-region),
  "Differences in demand, full uncertainty, Mean and 90% interval",
  "2 x Pn, update9 constrained lambda agg32FE, Australia_NZ") %>%
  plot()
ggsave("demand_diffs_update9_cnstrlam_aff32FE_meanquant_full_2xPn_R6_limx.png",path = fig_path)
make_meanquant_demand_diffs_plot(
  all_diffs_update9_cnstrlam_agg32FE_2xPn_R28 %>% select(-region),
  "Differences in demand, full uncertainty, Mean and 90% interval",
  "2 x Pn, update9 constrained lambda agg32FE, S Korea") %>%
  plot()
ggsave("demand_diffs_update9_cnstrlam_aff32FE_meanquant_full_2xPn_R28_limx.png",path = fig_path)


# ------------------------------------------------------------------------------
# Derive and plot distributions of high and low subsets of differences in Qtot
# in response to price doubling
# ------------------------------------------------------------------------------

# function for making plots of three density functions for each of nine parameters
# takes three dataframes (containing at least the values for parameters) and
# a list of legend labels for each; line types are sold (df1), dashed (df2), dotted (df3)
make_density3_plots <- function(df1,df2,df3,labels,title1,title2) {

  # convert to long format for faceting, 9 variables
  param_colnames <- c("As", "An", "xi.ss", "xi.cross", "xi.nn", "eps1n",
                      "lambda", "ks", "Pm")
  df1_long <- gather(df1, key="parameter", value="value",c(param_colnames))
  df2_long <- gather(df2, key="parameter", value="value",c(param_colnames))
  df3_long <- gather(df3, key="parameter", value="value",c(param_colnames))

  # plot
  thickness <- 1
  g <- ggplot() +
    geom_density(data = df1_long, aes(x = value,linetype = labels[1]),
                 linewidth = thickness) +
    geom_density(data = df2_long, aes(x = value,linetype = labels[2]),
                 linewidth = thickness) +
    geom_density(data = df3_long, aes(x = value,linetype = labels[3]),
                 linewidth = thickness) +
    scale_linetype_manual(name = "Data", values = c("dashed","dotted","solid")) +
    facet_wrap(~parameter, ncol=3, scales = "free") +
    labs(title = title1, subtitle = title2)
  plot(g)
}

# get subsets of data for for high and low intervals of Qtot, for a given
# level of income
inc_level <- 10  # thousands of dollars of income
q_lo <- 0.10     # max quantile of low interval
q_hi <- 0.90     # min quantil of high interval
data_by_inc <- all_diffs_update9_cnstrlam_2xPsPn[
  all_diffs_update9_cnstrlam_2xPsPn$Y == inc_level,]
# low demand difference range
qlo_by_inc <- quantile(data_by_inc$Qtot,probs = q_lo, na.rm = TRUE)
LD_data <- data_by_inc[data_by_inc$Qtot < qlo_by_inc,]
# high demand difference range
qhi_by_inc <- quantile(data_by_inc$Qtot,probs = q_hi, na.rm = TRUE)
HD_data <- data_by_inc[data_by_inc$Qtot > qhi_by_inc,]

# plot densities of all parameters for full ranges and hi/lo intervals
make_density3_plots(
  data_by_inc,LD_data,HD_data,c("Full","10%-","90%+"),
  "Parameter probability densities for differences in demand at $10k income",
  "Full range and high/low intervals, update9 constrained lambda")
ggsave("probdens_params_full_intervals_update9_cnstrlam.png",path = fig_path)

#    ---------------------------------------------------------------------------
#    Calculate parameter values for hi and lo demand cases based on medians of
#    sub-distributions of data
#    ---------------------------------------------------------------------------

params_lodiff <-
  summarize_at(LD_data,c("As", "An", "xi.ss", "xi.cross", "xi.nn", "eps1n",
                         "lambda", "ks", "Pm", "psscl", "pnscl"),median)
rownames(params_lodiff) <- "lodiff"
params_hidiff <-
  summarize_at(HD_data,c("As", "An", "xi.ss", "xi.cross", "xi.nn", "eps1n",
                         "lambda", "ks", "Pm", "psscl", "pnscl"),median)
rownames(params_hidiff) <- "hidiff"
params_joint_price_update9_cnstrlam <- bind_rows(params_joint_price_update9_cnstrlam,
                                     params_lodiff,params_hidiff)


# ------------------------------------------------------------------------------
# Compare parameter marginal densities (PARTIALLY UPDATED)
# ------------------------------------------------------------------------------

# code below here to be updated

# get random sample of orig9 data to match size of update9 dataset
param_data_orig9_sample <- sample_n(param_data_orig9,nrow(param_data_update9_cnstrlam))

# to trim the density plots, use only samples that are in the 95% LL interval
trim_density <- function(param_data) {
  quant_LL <- quantile(param_data$LL,probs = 0.05)
  param_data_95 <- param_data[param_data$LL > quant_LL,]
}
# orig9 data
param_data_orig9_sample_95 <- trim_density(param_data_orig9_sample)
# update9 data
param_data_update9_flexlam_95 <- trim_density(param_data_update9_flexlam)
param_data_update9_cnstrlam_95 <- trim_density(param_data_update9_cnstrlam)

# plot comparison of densities of two datasets of parameter values, plus
# vertical lines marking ML estimates and ranges, and gcam values
make_density_plots <- function(
    dataset1,dataset2,  # datasets of parameter samples
    params1,params2,    # ML/Hi/Lo parameter values for datasets
    label1,label2,      # lables for plot legend
    gcam_MLparams)      # ML/Hi/Lo parameter values for GCAM
  {

  # convert to long format for faceting, 9 variables
  param_colnames <- colnames(dataset1)[1:9]
  dataset1_long <- gather(dataset1[,1:9],key="parameter", value="value",c(param_colnames))
  dataset2_long <- gather(dataset2[,1:9], key="parameter", value="value",c(param_colnames))

  # define ML and range data to plot as vertical lines
  vline.d1.ml <- data.frame(d1_ML = as.vector(t(params1['ML',]))[1:9],
                               parameter=c(param_colnames))
  vline.d1.hi <- data.frame(d1_Hi = as.vector(t(params1['Hi',]))[1:9],
                               parameter=c(param_colnames))
  vline.d1.lo <- data.frame(d1_Lo = as.vector(t(params1['Lo',]))[1:9],
                               parameter=c(param_colnames))
  vline.d2.ml <- data.frame(d2_ML = as.vector(t(params2['ML',]))[1:9],
                                 parameter=c(param_colnames))
  vline.d2.hi <- data.frame(d2_Hi = as.vector(t(params2['Hi',]))[1:9],
                                 parameter=c(param_colnames))
  vline.d2.lo <- data.frame(d2_Lo = as.vector(t(params2['Lo',]))[1:9],
                                 parameter=c(param_colnames))
  vline.gcam <- data.frame(gcam_ML = gcam_MLparams[1:9],
                           parameter=c(param_colnames))

  # plot
  thickness <- 1
  g <- ggplot() +
    geom_density(data = dataset1_long, aes(x = value,color = label1),
                 linewidth = thickness) +
    geom_density(data = dataset2_long, aes(x = value,color = label2),
                 linewidth = thickness) +
    geom_vline(data = vline.d1.ml, aes(xintercept = d1_ML, color = label1),
               linewidth = thickness) +
    geom_vline(data = vline.d1.hi, aes(xintercept = d1_Hi, color = label1),
               linetype = "dotted",linewidth = thickness) +
    geom_vline(data = vline.d1.lo, aes(xintercept = d1_Lo, color = label1),
               linetype = "dotted",linewidth = thickness) +
    geom_vline(data = vline.d2.ml, aes(xintercept = d2_ML, color = label2),
               linewidth = thickness) +
    geom_vline(data = vline.d2.hi, aes(xintercept = d2_Hi, color = label2),
               linetype = "dotted",linewidth = thickness) +
    geom_vline(data = vline.d2.lo, aes(xintercept = d2_Lo, color = label2),
               linetype = "dotted",linewidth = thickness) +
    geom_vline(data = vline.gcam, aes(xintercept = gcam_ML, color = "gcam"),
               linewidth = thickness) +
    facet_wrap(~parameter, ncol=3, scales = "free")
  plot(g)
}

# orig9 vs update9_flexlam data
make_density_plots(
  param_data_orig9_sample_95,param_data_update9_flexlam_95,
  params_orig9,params_update9_flexlam,
  "orig9","update9_flexlam",params_gcam)

# orig9 vs update9_cnstrlam data
make_density_plots(
  param_data_orig9_sample_95,param_data_update9_cnstrlam_95,
  params_orig9,params_update9_cnstrlam,
  "orig9","update9_cnstrlam",params_gcam)

# update9_flexlam vs update9_cnstrlam data
make_density_plots(
  param_data_update9_flexlam_95,param_data_update9_cnstrlam_95,
  params_update9_flexlam,params_update9_cnstrlam,
  "update9_flexlam9","update9_cnstrlam",params_gcam)

# plot likelihood vs parameter values for all 9 parameters
param_data_update9_long <- gather(param_data_update9, key="parameter", value="value",
                                  c(param_colnames))
g <- ggplot(param_data_update9_long,aes(x=value, y=LL)) +
  geom_point() +
  facet_wrap(~parameter, ncol=3, scales = "free")
#plot(g)

# ------------------------------------------------------------------------------
# Analyze parameter joint distributions (FOR THE ORIG9 DATA; TO BE UPDATED)
# Find high/low sensitivity cases
# ------------------------------------------------------------------------------

# sample the orig9 data first to make it easier to work with
# note that because it is a random sample, the results (high/low sens parameters)
# will differ slightly each time this is run; a sample of 100k always produces
# high/low parameter sets that are above/below at least the 80/20 percentile
param_data_orig9_sample <- sample_n(param_data_orig9,100000)

# use only samples that are in the 95% interval for likelihood scores to ensure
# that we don't select a highly unlikely outlier
quant_LL_orig9_sample <- quantile(param_data_orig9_sample$LL,probs = 0.05)
param_data_orig9_sample_95 <- param_data_orig9_sample[param_data_orig9_sample$LL >
                                          quant_LL_orig9_sample,]

# calculate all deciles of each variable
get_quantiles <- function(sample) {
  quantile(sample,probs=seq(0.1, 0.9, 0.1))
}
quantiles_orig9 <- apply(param_data_orig9_sample_95,2,get_quantiles)

# here I should write a function to take a list of variables and find the joint
# high/low values across all of them, using the deciles; here is some code that works
# that could contribute to that function, but too time-consuming to complete now
#param_data <- param_data_orig9_sample_95
#vars <- c("xi.ss","xi.nn")
#param_data[
#  param_data[,vars[[1]]] < quantiles_orig9[1,vars[[1]]] &
#    param_data[,vars[[2]]] < quantiles_orig9[1,vars[[2]]],]

# identify high and low price sensitivity values for xss, xnn, xcross

# identify samples that have all price elasticities below a given percentile
# within their marginal distributions (this will produce highest price sensitivity)
quant <- "10%"
sample_joint_price_hi <- param_data_orig9_sample_95[
  param_data_orig9_sample_95$xi.ss < quantiles_orig9[quant,"xi.ss"] &
    param_data_orig9_sample_95$xi.nn < quantiles_orig9[quant,"xi.nn"] &
    param_data_orig9_sample_95$xi.cross < quantiles_orig9[quant,"xi.cross"],]
# select sample with lowest likelihood score to use as our high sensitivity case
sample_joint_price_hi <- sample_joint_price_hi[
  sample_joint_price_hi$LL == min(sample_joint_price_hi$LL),] %>%
  filter(!duplicated(LL)) %>% select(1:11)

# identify samples that have all price elasticities above a given percentile
# within their marginal distributions (this will produce lowest price sensitivity)
quant <- "80%"
sample_joint_price_lo <- param_data_orig9_sample_95[
  param_data_orig9_sample_95$xi.ss > quantiles_orig9[quant,"xi.ss"] &
    param_data_orig9_sample_95$xi.nn > quantiles_orig9[quant,"xi.nn"] &
    param_data_orig9_sample_95$xi.cross > quantiles_orig9[quant,"xi.cross"],]
# select sample with lowest likelihood score to use as our high sensitivity case
sample_joint_price_lo <- sample_joint_price_lo[
  sample_joint_price_lo$LL == min(sample_joint_price_lo$LL),] %>%
  filter(!duplicated(LL)) %>% select(1:11)

# identify high and low income sensitivity values for ks, lambda, nu

# identify samples that have all income elasticity parameters above a given percentile
# within their marginal distributions (this will produce highest income sensitivity)
quant = "80%"
sample_joint_inc_hi <- param_data_orig9_sample_95[
  param_data_orig9_sample_95$ks > quantiles_orig9[quant,"ks"] &
    param_data_orig9_sample_95$lambda > quantiles_orig9[quant,"lambda"] &
    param_data_orig9_sample_95$eps1n > quantiles_orig9[quant,"eps1n"],]
# select sample with lowest likelihood score to use as our high sensitivity case
sample_joint_inc_hi <- sample_joint_inc_hi[
  sample_joint_inc_hi$LL == min(sample_joint_inc_hi$LL),] %>%
  filter(!duplicated(LL)) %>% select(1:11)

# identify samples that have all income elasticity parameters below a given percentile
# within their marginal distributions (this will produce lowest income sensitivity)
quant = "20%"
sample_joint_inc_lo <- param_data_orig9_sample_95[
  param_data_orig9_sample_95$ks < quantiles_orig9[quant,"ks"] &
    param_data_orig9_sample_95$lambda < quantiles_orig9[quant,"lambda"] &
    param_data_orig9_sample_95$eps1n < quantiles_orig9[quant,"eps1n"],]
# select sample with lowest likelihood score to use as our high sensitivity case
sample_joint_inc_lo <- sample_joint_inc_lo[
  sample_joint_inc_lo$LL == min(sample_joint_inc_lo$LL),] %>%
  filter(!duplicated(LL)) %>% select(1:11)

# identify high and low scale sensitivity values for As, An

# identify samples that have all scale parameters above a given percentile
# within their marginal distributions (this will produce highest scale sensitivity)
quant = "90%"
sample_joint_scale_hi <- param_data_orig9_sample_95[
  param_data_orig9_sample_95$As > quantiles_orig9[quant,"As"] &
    param_data_orig9_sample_95$An > quantiles_orig9[quant,"An"],]
# select sample with lowest likelihood score to use as our high sensitivity case
sample_joint_scale_hi <- sample_joint_scale_hi[
  sample_joint_scale_hi$LL == min(sample_joint_scale_hi$LL),] %>%
  filter(!duplicated(LL)) %>% select(1:11)

# identify samples that have all scale parameters below a given percentile
# within their marginal distributions (this will produce lowest scale sensitivity)
quant = "20%"
sample_joint_scale_lo <- param_data_orig9_sample_95[
  param_data_orig9_sample_95$As < quantiles_orig9[quant,"As"] &
    param_data_orig9_sample_95$An < quantiles_orig9[quant,"An"],]
# select sample with lowest likelihood score to use as our high sensitivity case
sample_joint_scale_lo <- sample_joint_scale_lo[
  sample_joint_scale_lo$LL == min(sample_joint_scale_lo$LL),] %>%
  filter(!duplicated(LL)) %>% select(1:11)

# identify high/low price and income sensitivity values for xss, xnn, xcross, ks, lambda, nu

# identify samples that have all price parameters below a given percentile and
# all income parameters above a given percentile within their marginal distributions
# (this will produce highest price/income sensitivity)
quantlo <- "30%"
quanthi <- "70%"
sample_joint_priceinc_hi <- param_data_orig9_sample_95[
  param_data_orig9_sample_95$xi.ss < quantiles_orig9[quantlo,"xi.ss"] &
    param_data_orig9_sample_95$xi.nn < quantiles_orig9[quantlo,"xi.nn"] &
    param_data_orig9_sample_95$xi.cross < quantiles_orig9[quantlo,"xi.cross"] &
    param_data_orig9_sample_95$ks > quantiles_orig9[quanthi,"ks"] &
    param_data_orig9_sample_95$lambda > quantiles_orig9[quanthi,"lambda"] &
    param_data_orig9_sample_95$eps1n > quantiles_orig9[quanthi,"eps1n"],]
# select sample with lowest likelihood score to use as our high sensitivity case
sample_joint_priceinc_hi <- sample_joint_priceinc_hi[
  sample_joint_priceinc_hi$LL == min(sample_joint_priceinc_hi$LL),] %>%
  filter(!duplicated(LL)) %>% select(1:11)

# identify samples that have all price parameters above a given percentile and
# all income parameters below a given percentile within their marginal distributions
# (this will produce lowest price/income sensitivity)
quantlo <- "40%"
quanthi <- "60%"
sample_joint_priceinc_lo <- param_data_orig9_sample_95[
  param_data_orig9_sample_95$xi.ss > quantiles_orig9[quanthi,"xi.ss"] &
    param_data_orig9_sample_95$xi.nn > quantiles_orig9[quanthi,"xi.nn"] &
    param_data_orig9_sample_95$xi.cross > quantiles_orig9[quanthi,"xi.cross"] &
    param_data_orig9_sample_95$ks < quantiles_orig9[quantlo,"ks"] &
    param_data_orig9_sample_95$lambda < quantiles_orig9[quantlo,"lambda"] &
    param_data_orig9_sample_95$eps1n < quantiles_orig9[quantlo,"eps1n"],]
# select sample with lowest likelihood score to use as our high sensitivity case
sample_joint_priceinc_lo <- sample_joint_priceinc_lo[
  sample_joint_priceinc_lo$LL == min(sample_joint_priceinc_lo$LL),] %>%
  filter(!duplicated(LL)) %>% select(1:11)

# identify high/low price, income and scale sensitivity values for xss, xnn, xcross, ks, lambda, nu, As, An

# identify samples that have all price parameters below a given percentile and
# all income and scale parameters above a given percentile within their marginal distributions
# (this will produce highest price/income/scale sensitivity)
quantlo <- "50%"
quanthi <- "50%"
sample_joint_priceincscale_hi <- param_data_orig9_sample_95[
  param_data_orig9_sample_95$xi.ss < quantiles_orig9[quantlo,"xi.ss"] &
    param_data_orig9_sample_95$xi.nn < quantiles_orig9[quantlo,"xi.nn"] &
    param_data_orig9_sample_95$xi.cross < quantiles_orig9[quantlo,"xi.cross"] &
    param_data_orig9_sample_95$ks > quantiles_orig9[quanthi,"ks"] &
    param_data_orig9_sample_95$lambda > quantiles_orig9[quanthi,"lambda"] &
    param_data_orig9_sample_95$eps1n > quantiles_orig9[quanthi,"eps1n"] &
    param_data_orig9_sample_95$As > quantiles_orig9[quanthi,"As"] &
    param_data_orig9_sample_95$An > quantiles_orig9[quanthi,"An"],]
# select sample with lowest likelihood score to use as our high sensitivity case
sample_joint_priceincscale_hi <- sample_joint_priceincscale_hi[
  sample_joint_priceincscale_hi$LL == min(sample_joint_priceincscale_hi$LL),] %>%
  filter(!duplicated(LL)) %>% select(1:11)

# identify samples that have all price parameters above a given percentile and
# all income and scale parameters below a given percentile within their marginal distributions
# (this will produce lowest price/income/scale sensitivity)
quantlo <- "50%"
quanthi <- "50%"
sample_joint_priceincscale_lo <- param_data_orig9_sample_95[
  param_data_orig9_sample_95$xi.ss > quantiles_orig9[quanthi,"xi.ss"] &
    param_data_orig9_sample_95$xi.nn > quantiles_orig9[quanthi,"xi.nn"] &
    param_data_orig9_sample_95$xi.cross > quantiles_orig9[quanthi,"xi.cross"] &
    param_data_orig9_sample_95$ks < quantiles_orig9[quantlo,"ks"] &
    param_data_orig9_sample_95$lambda < quantiles_orig9[quantlo,"lambda"] &
    param_data_orig9_sample_95$eps1n < quantiles_orig9[quantlo,"eps1n"] &
    param_data_orig9_sample_95$As < quantiles_orig9[quantlo,"As"] &
    param_data_orig9_sample_95$An < quantiles_orig9[quantlo,"An"],]
# select sample with lowest likelihood score to use as our high sensitivity case
sample_joint_priceincscale_lo <- sample_joint_priceincscale_lo[
  sample_joint_priceincscale_lo$LL == min(sample_joint_priceincscale_lo$LL),] %>%
  filter(!duplicated(LL)) %>% select(1:11)

# make table to compare high/low price sensitivity case to intervals for each variable
sample_df <- data.frame(
  Var_Name = param_colnames,
  low_sens = as.vector(t(sample_joint_price_lo[1,1:11])),
  high_sens = as.vector(t(sample_joint_price_hi[1,1:11])),
  orig9_ml = as.vector(t(params_orig9['ML',])),
  orig9_lo = as.vector(t(params_orig9['Lo',])),
  orig9_hi = as.vector(t(params_orig9['Hi',])))
#kable(sample_df,format = "simple")

# make table to compare high/low income sensitivity case to intervals for each variable
sample_df <- data.frame(
  Var_Name = param_colnames,
  low_sens = as.vector(t(sample_joint_inc_lo[1,1:11])),
  high_sens = as.vector(t(sample_joint_inc_hi[1,1:11])),
  orig9_ml = as.vector(t(params_orig9['ML',])),
  orig9_lo = as.vector(t(params_orig9['Lo',])),
  orig9_hi = as.vector(t(params_orig9['Hi',])))
#kable(sample_df,format = "simple")

# make table to compare high/low scale sensitivity case to intervals for each variable
sample_df <- data.frame(
  Var_Name = param_colnames,
  low_sens = as.vector(t(sample_joint_scale_lo[1,1:11])),
  high_sens = as.vector(t(sample_joint_scale_hi[1,1:11])),
  orig9_ml = as.vector(t(params_orig9['ML',])),
  orig9_lo = as.vector(t(params_orig9['Lo',])),
  orig9_hi = as.vector(t(params_orig9['Hi',])))
#kable(sample_df,format = "simple")

# make table to compare high/low price and income sensitivity case to intervals for each variable
sample_df <- data.frame(
  Var_Name = param_colnames,
  low_sens = as.vector(t(sample_joint_priceinc_lo[1,1:11])),
  high_sens = as.vector(t(sample_joint_priceinc_hi[1,1:11])),
  orig9_ml = as.vector(t(params_orig9['ML',])),
  orig9_lo = as.vector(t(params_orig9['Lo',])),
  orig9_hi = as.vector(t(params_orig9['Hi',])))
#kable(sample_df,format = "simple")

# make table to compare high/low price, income and scale sensitivity case to intervals for each variable
sample_df <- data.frame(
  Var_Name = param_colnames,
  low_sens = as.vector(t(sample_joint_priceincscale_lo[1,1:11])),
  high_sens = as.vector(t(sample_joint_priceincscale_hi[1,1:11])),
  orig9_ml = as.vector(t(params_orig9['ML',])),
  orig9_lo = as.vector(t(params_orig9['Lo',])),
  orig9_hi = as.vector(t(params_orig9['Hi',])))
#kable(sample_df,format = "simple")

# -------------------------------------------------------------------------------
# 2d scatter and contour plots -- TO BE UPDATED
# -------------------------------------------------------------------------------

# create scatter plot of the trimmed sample to illustrate the kind of samples
# we are looking for
g_ssvnn_scatter <- ggplot(param_data_orig9_sample_95,
                          aes(x=param_data_orig9_sample_95$xi.ss,
                              y=param_data_orig9_sample_95$xi.nn)) +
  geom_point() +
  xlab("Staples Price Elasticity") +
  ylab("Nonstaples Pric Elasticity")
#plot(g_ssvnn_scatter)

#plot(param_data_orig9_sample_95$xi.ss,param_data_orig9_sample_95$xi.nn,
#     xlim=c(-0.3,-0.05),ylim=c(-0.5,-0.05))
#identify(param_data_orig9_sample_95$xi.ss,param_data_orig9_sample_95$xi.nn)

# make 2d contour plots to see how parameters are jointly distributed
# also compare to location of max likelihood point for full set of params
# and to the high/low sensitivity cases
g_ssvnn <- ggplot(param_data_orig9_sample,aes(x=param_data_orig9_sample$xi.ss,
                                              y=param_data_orig9_sample$xi.nn)) +
  geom_density2d() +
  geom_point(aes(x=xi_ss, y=xi_nn,color = 'ML'),size=3) +
  geom_point(aes(x=sample_joint_price_hi$xi.ss, y=sample_joint_price_hi$xi.nn,
                 color = 'high sens'),size=3) +
  geom_point(aes(x=sample_joint_price_lo$xi.ss, y=sample_joint_price_lo$xi.nn,
                 color = 'low sens'),size=3) +
  xlab("Staples price elasticity parameter") +
  ylab("Nonstaples price elasticity parameter")

g_ssvcross <- ggplot(param_data_orig9_sample,aes(x=param_data_orig9_sample$xi.ss,
                                                 y=param_data_orig9_sample$xi.cross)) +
  geom_density2d() +
  geom_point(aes(x=xi_ss, y=xi_cross,color = 'ML'),size=3) +
  geom_point(aes(x=sample_joint_price_hi$xi.ss, y=sample_joint_price_hi$xi.cross,
                 color = 'high sens'),size=3) +
  geom_point(aes(x=sample_joint_price_lo$xi.ss, y=sample_joint_price_lo$xi.cross,
                 color = 'low sens'),size=3) +
  xlab("Staples price elasticity parameter") +
  ylab("Cross price elasticity parameter")

g_nnvcross <- ggplot(param_data_orig9_sample,aes(x=param_data_orig9_sample$xi.nn,
                                                 y=param_data_orig9_sample$xi.cross)) +
  geom_density2d() +
  geom_point(aes(x=xi_nn, y=xi_cross,color = 'ML'),size=3) +
  geom_point(aes(x=sample_joint_price_hi$xi.nn, y=sample_joint_price_hi$xi.cross,
                 color = 'high sens'),size=3) +
  geom_point(aes(x=sample_joint_price_lo$xi.nn, y=sample_joint_price_lo$xi.cross,
                 color = 'low sens'),size=3) +
  xlab("Nonstaples price elasticity parameter") +
  ylab("Cross price elasticity parameter")

#plot(g_ssvnn)
#plot(g_ssvcross)
#plot(g_nnvcross)

g_ksvlambda <- ggplot(param_data_orig9_sample,aes(x=param_data_orig9_sample$lambda,
                                                 y=param_data_orig9_sample$ks)) +
  geom_density2d(bins=10) +
  geom_point(aes(x=lambda_s, y=k_s,color = 'ML'),size=3) +
  geom_point(aes(x=sample_joint_inc_hi$lambda, y=sample_joint_inc_hi$ks,
                 color = 'high sens'),size=3) +
  geom_point(aes(x=sample_joint_inc_lo$lambda, y=sample_joint_inc_lo$ks,
                 color = 'low sens'),size=3) +
  xlab("Staples income elasticity parameter (lambda)") +
  ylab("Staples max demand parameter (ks)")

g_ksvnu <- ggplot(param_data_orig9_sample,aes(x=param_data_orig9_sample$eps1n,
                                                  y=param_data_orig9_sample$ks)) +
  geom_density2d(bins=10) +
  geom_point(aes(x=nu1_n, y=k_s,color = 'ML'),size=3) +
  geom_point(aes(x=sample_joint_inc_hi$eps1n, y=sample_joint_inc_hi$ks,
                 color = 'high sens'),size=3) +
  geom_point(aes(x=sample_joint_inc_lo$eps1n, y=sample_joint_inc_lo$ks,
                 color = 'low sens'),size=3) +
  xlab("Nonstaples income elasticity parameter (nu)") +
  ylab("Staples max demand parameter (ks)")

g_lambdavnu <- ggplot(param_data_orig9_sample,aes(x=param_data_orig9_sample$eps1n,
                                              y=param_data_orig9_sample$lambda)) +
  geom_density2d(bins=10) +
  geom_point(aes(x=nu1_n, y=lambda_s,color = 'ML'),size=3) +
  geom_point(aes(x=sample_joint_inc_hi$eps1n, y=sample_joint_inc_hi$lambda,
                 color = 'high sens'),size=3) +
  geom_point(aes(x=sample_joint_inc_lo$eps1n, y=sample_joint_inc_lo$lambda,
                 color = 'low sens'),size=3) +
  xlab("Nonstaples income elasticity parameter (nu)") +
  ylab("Staples income elasticity parameter (lambda)")

#plot(g_ksvlambda)
#plot(g_ksvnu)
#plot(g_lambdavnu)

g_AsvAn <- ggplot(param_data_orig9_sample,aes(x=param_data_orig9_sample$An,
                                                  y=param_data_orig9_sample$As)) +
  geom_density2d(bins=10) +
  geom_point(aes(x=A_n, y=A_s,color = 'ML'),size=3) +
  geom_point(aes(x=sample_joint_scale_lo$An, y=sample_joint_scale_lo$As,
                 color = 'low sens'),size=3) +
  geom_point(aes(x=sample_joint_scale_hi$An, y=sample_joint_scale_hi$As,
                 color = 'high sens'),size=3) +
  xlab("Nonstaples scale parameter (An)") +
  ylab("Staples scale parameter (As)")

#plot(g_AsvAn)

# ------------------------------------------------------------------------------
# Compare model to observational data
# ------------------------------------------------------------------------------

#    ---------------------------------------------------------------------------
#    Scatter plots and plots vs income
#    ---------------------------------------------------------------------------

# demand with parameters from update9_cnstrlam, prices/income from observational data
demand_fromobs_update9_cnstrlam_ML <-
  food.dmnd.plus(Obs_Data_sample,params_update9_cnstrlam['ML',])
demand_fromobs_update9_cnstrlam_hi <-
  food.dmnd.plus(Obs_Data_sample,params_joint_price_update9_cnstrlam['hi',])
demand_fromobs_update9_cnstrlam_lo <-
  food.dmnd.plus(Obs_Data_sample,params_joint_price_update9_cnstrlam['lo',])
# country level observations, modified for Qtot
Obs_Data_sample <- Obs_Data_sample %>% mutate(Qtot = Qn + Qs)

# for the Fixed Effects model: function for calculating demand from observed prices
# and income, one set of parameter values (with varying FEs across regions). produces dataframe with observed prices/income, 11 parameter values,
# FE value, regional ID, modeled demand, observed demand, modeled elasticities
get_demand_fromobs <- function(obsdata,paramdata,FEdata) {
  result <- data.frame()
  # loop over each region, to get FEs for that region
  regions <- unique(obs_data_gcam32$GCAM_region_ID)
  for(i in 1:length(regions)) {
    # get fixed effect for this region, this parameter set
    regionID <- regions[i]
    iter <- paramdata[['iteration']]
    FEval <- filter(FEdata,iteration==iter,GCAM_region_ID==regionID)[['staples_FE']]
    # calculate demand
    region_dmnd <- food.dmnd.plus(
      # observations for region i (save for later use)
      tmp<-filter(obsdata,GCAM_region_ID==regionID),
      # parameters for demand function
      paramdata) %>%
      # identify demand results as modeled values
      rename(Qs_mod = Qs,Qn_mod = Qn,Qm_mod = Qm,Qtot_mod = Qtot) %>%
      mutate(
        # add fixed effect for region i
        Qs_mod = Qs_mod + FEval,
        Qtot_mod = Qs_mod + Qn_mod,
        # add regional information
        staples_FE = FEval,
        GCAM_region_ID = regionID,
        # add observed values
        Qs_obs = tmp$Qs,
        Qn_obs = tmp$Qn,
        Qtot_obs = Qs_obs + Qn_obs)
    result <- rbind(result, region_dmnd)
  }
  return(result)
}

# demand with ML parameters from agg32FE data, prices/income from observations
demand_fromobs_update9_cnstrlam_agg32FE_ML <-
  get_demand_fromobs(obs_data_gcam32,
                     params_ML95_update9_cnstrlam_agg32FE_params['ML',],
                     param_data_update9_cnstrlam_agg32FE_FEs)
# demand with high price sensitivity parameters
demand_fromobs_update9_cnstrlam_agg32FE_hi_price <-
  get_demand_fromobs(obs_data_gcam32,
                     params_joint_price_update9_cnstrlam_agg32FE_params['hi',],
                     param_data_update9_cnstrlam_agg32FE_FEs)
# demand with low price sensitivity parameters
demand_fromobs_update9_cnstrlam_agg32FE_lo_price <-
  get_demand_fromobs(obs_data_gcam32,
                     params_joint_price_update9_cnstrlam_agg32FE_params['lo',],
                     param_data_update9_cnstrlam_agg32FE_FEs)
# demand with high income sensitivity parameters
demand_fromobs_update9_cnstrlam_agg32FE_hi_inc <-
  get_demand_fromobs(obs_data_gcam32,
                     params_joint_inc_update9_cnstrlam_agg32FE_params['hi',],
                     param_data_update9_cnstrlam_agg32FE_FEs)
# demand with low income sensitivity parameters
demand_fromobs_update9_cnstrlam_agg32FE_lo_inc <-
  get_demand_fromobs(obs_data_gcam32,
                     params_joint_inc_update9_cnstrlam_agg32FE_params['lo',],
                     param_data_update9_cnstrlam_agg32FE_FEs)
# demand with high scale sensitivity parameters
demand_fromobs_update9_cnstrlam_agg32FE_hi_scale <-
  get_demand_fromobs(obs_data_gcam32,
                     params_joint_scale_update9_cnstrlam_agg32FE_params['hi',],
                     param_data_update9_cnstrlam_agg32FE_FEs)
# demand with low scale sensitivity parameters
demand_fromobs_update9_cnstrlam_agg32FE_lo_scale <-
  get_demand_fromobs(obs_data_gcam32,
                     params_joint_scale_update9_cnstrlam_agg32FE_params['lo',],
                     param_data_update9_cnstrlam_agg32FE_FEs)

demand_fromobs_update9_cnstrlam_agg32FE_hi_scale[
  demand_fromobs_update9_cnstrlam_agg32FE_hi_scale$Qs_mod > 2.5 &
    demand_fromobs_update9_cnstrlam_agg32FE_hi_scale$Y > 2,]


# function for plotting three scatter plots, model v observed, for Qs, Qn, Qtot
make_obsvmodel_scatter <- function(obsdata,modeldata,title1,xmax,ymax,ratio) {
  list(data.frame(x=obsdata$Qs,y=modeldata$Qs,dataset="Qs"),
       data.frame(x=obsdata$Qn,y=modeldata$Qn,dataset="Qn"),
       data.frame(x=obsdata$Qtot,y=modeldata$Qtot,dataset="Qtot")) %>%
    bind_rows() %>%
    ggplot(aes(x,y)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    facet_wrap(~dataset) +
    labs(title = title1) +
    xlim(0,xmax) +
    ylim(0,ymax) +
    xlab("Demand (observations)") +
    ylab("Demand (model)") +
    coord_fixed(ratio)
}

# function for plotting three scatter plots, model v observed as fn of income,
# for Qs, Qn, Qtot
make_obsvmodel_inc_plot <- function(obsdata,modeldata,title1,xmax,ymax,ratio) {
  list(data.frame(x=obsdata$Y,y1=obsdata$Qs,y2=modeldata$Qs,dataset="Qs"),
       data.frame(x=obsdata$Y,y1=obsdata$Qn,y2=modeldata$Qn,dataset="Qn"),
       data.frame(x=obsdata$Y,y1=obsdata$Qtot,y2=modeldata$Qtot,dataset="Qtot")) %>%
    bind_rows() %>%
    ggplot() +
    geom_point(aes(x,y1)) +
    geom_point(aes(x,y2),color="orange") +
    facet_wrap(~dataset) +
    labs(title = title1) +
    xlim(0,xmax) +
    ylim(0,ymax) +
    xlab("Income") +
    ylab("Demand") +
    coord_fixed(ratio)
}

# plot and save scatterplots
# ML params: xy limits = 4, 4, coord fixed = 1
g_ml <- make_obsvmodel_scatter(
  Obs_Data_sample,demand_fromobs_update9_cnstrlam_ML,
  "Model vs Observations, update9 cnstrlam ML parameters",
  4,4,4/4)
plot(g_ml)
ggsave(g_ml,file=paste0(fig_path,"scatter_demand_update9_cnstrlam_ML.png"))
g_hi <- make_obsvmodel_scatter(
  Obs_Data_sample,demand_fromobs_update9_cnstrlam_hi,
  "Model vs Observations, update9 cnstrlam hi parameters",
  4,11,4/11)
plot(g_hi)
ggsave(g_hi,file=paste0(fig_path,"scatter_demand_update9_cnstrlam_hi.png"))
g_lo <- make_obsvmodel_scatter(
  Obs_Data_sample,demand_fromobs_update9_cnstrlam_lo,
  "Model vs Observations, update9 cnstrlam lo parameters",
  4,7,4/7)
plot(g_lo)
ggsave(g_lo,file=paste0(fig_path,"scatter_demand_update9_cnstrlam_lo.png"))

# plot and save model v observation as function of income
g_ml <- make_obsvmodel_inc_plot(
  Obs_Data_sample,demand_fromobs_update9_cnstrlam_ML,
  "Model vs Observations, update9 cnstrlam ML parameters",
  120,4,120/4)
plot(g_ml)
ggsave(g_ml,file=paste0(fig_path,"scatter_demand-income_update9_cnstrlam_ML.png"))
g_hi <- make_obsvmodel_inc_plot(
  Obs_Data_sample,demand_fromobs_update9_cnstrlam_hi,
  "Model vs Observations, update9 cnstrlam hi parameters",
  120,11,120/11)
plot(g_hi)
ggsave(g_hi,file=paste0(fig_path,"scatter_demand-income_update9_cnstrlam_hi.png"))
g_lo <- make_obsvmodel_inc_plot(
  Obs_Data_sample,demand_fromobs_update9_cnstrlam_lo,
  "Model vs Observations, update9 cnstrlam lo parameters",
  120,7,120/7)
plot(g_lo)
ggsave(g_lo,file=paste0(fig_path,"scatter_demand-income_update9_cnstrlam_lo.png"))

# for FE model

# scatter plots
# ML params
g_ml_FE <- make_obsvmodel_scatter(
  rename(demand_fromobs_update9_cnstrlam_agg32FE_ML,
         Qs = Qs_obs,Qn = Qn_obs,Qtot = Qtot_obs),
  rename(demand_fromobs_update9_cnstrlam_agg32FE_ML,
         Qs = Qs_mod,Qn = Qn_mod,Qtot = Qtot_mod),
  "Model vs Observations, update9 cnstrlam agg32FE ML parameters",
  4,6,4/6)
plot(g_ml_FE)
ggsave(g_ml_FE,file=paste0(fig_path,"scatter_demand_update9_cnstrlam_agg32FE_ML.png"),
       width = 8, height = 6.5, dpi = 150)
# hi joint price params
g_hi_FE <- make_obsvmodel_scatter(
  rename(demand_fromobs_update9_cnstrlam_agg32FE_hi_price,
         Qs = Qs_obs,Qn = Qn_obs,Qtot = Qtot_obs),
  rename(demand_fromobs_update9_cnstrlam_agg32FE_hi_price,
         Qs = Qs_mod,Qn = Qn_mod,Qtot = Qtot_mod),
  "Model vs Observations, update9 cnstrlam agg32FE hi price parameters",
  4,6,4/6)
plot(g_hi_FE)
ggsave(g_hi_FE,file=paste0(fig_path,"scatter_demand_update9_cnstrlam_agg32FE_hi_price.png"),
       width = 8, height = 6.5, dpi = 150)
# lo joint price params
g_lo_FE <- make_obsvmodel_scatter(
  rename(demand_fromobs_update9_cnstrlam_agg32FE_lo_price,
         Qs = Qs_obs,Qn = Qn_obs,Qtot = Qtot_obs),
  rename(demand_fromobs_update9_cnstrlam_agg32FE_lo_price,
         Qs = Qs_mod,Qn = Qn_mod,Qtot = Qtot_mod),
  "Model vs Observations, update9 cnstrlam agg32FE lo price parameters",
  4,6,4/6)
plot(g_lo_FE)
ggsave(g_lo_FE,file=paste0(fig_path,"scatter_demand_update9_cnstrlam_agg32FE_lo_price.png"),
       width = 8, height = 6.5, dpi = 150)
# hi joint income params
g_hi_FE <- make_obsvmodel_scatter(
  rename(demand_fromobs_update9_cnstrlam_agg32FE_hi_inc,
         Qs = Qs_obs,Qn = Qn_obs,Qtot = Qtot_obs),
  rename(demand_fromobs_update9_cnstrlam_agg32FE_hi_inc,
         Qs = Qs_mod,Qn = Qn_mod,Qtot = Qtot_mod),
  "Model vs Observations, update9 cnstrlam agg32FE hi income parameters",
  4,6,4/6)
plot(g_hi_FE)
ggsave(g_hi_FE,file=paste0(fig_path,"scatter_demand_update9_cnstrlam_agg32FE_hi_inc.png"),
       width = 8, height = 6.5, dpi = 150)
# lo joint income params
g_lo_FE <- make_obsvmodel_scatter(
  rename(demand_fromobs_update9_cnstrlam_agg32FE_lo_inc,
         Qs = Qs_obs,Qn = Qn_obs,Qtot = Qtot_obs),
  rename(demand_fromobs_update9_cnstrlam_agg32FE_lo_inc,
         Qs = Qs_mod,Qn = Qn_mod,Qtot = Qtot_mod),
  "Model vs Observations, update9 cnstrlam agg32FE lo income parameters",
  4,6,4/6)
plot(g_lo_FE)
ggsave(g_lo_FE,file=paste0(fig_path,"scatter_demand_update9_cnstrlam_agg32FE_lo_inc.png"),
       width = 8, height = 6.5, dpi = 150)
# hi joint scale params
g_hi_FE <- make_obsvmodel_scatter(
  rename(demand_fromobs_update9_cnstrlam_agg32FE_hi_scale,
         Qs = Qs_obs,Qn = Qn_obs,Qtot = Qtot_obs),
  rename(demand_fromobs_update9_cnstrlam_agg32FE_hi_scale,
         Qs = Qs_mod,Qn = Qn_mod,Qtot = Qtot_mod),
  "Model vs Observations, update9 cnstrlam agg32FE hi scale parameters",
  4,6,4/6)
plot(g_hi_FE)
ggsave(g_hi_FE,file=paste0(fig_path,"scatter_demand_update9_cnstrlam_agg32FE_hi_scale.png"),
       width = 8, height = 6.5, dpi = 150)
# lo joint scale params
g_lo_FE <- make_obsvmodel_scatter(
  rename(demand_fromobs_update9_cnstrlam_agg32FE_lo_scale,
         Qs = Qs_obs,Qn = Qn_obs,Qtot = Qtot_obs),
  rename(demand_fromobs_update9_cnstrlam_agg32FE_lo_scale,
         Qs = Qs_mod,Qn = Qn_mod,Qtot = Qtot_mod),
  "Model vs Observations, update9 cnstrlam agg32FE lo scale parameters",
  4,6,4/6)
plot(g_lo_FE)
ggsave(g_lo_FE,file=paste0(fig_path,"scatter_demand_update9_cnstrlam_agg32FE_lo_scale.png"),
       width = 8, height = 6.5, dpi = 150)


# model v observation as function of income
# ML params
g_ml_FE <- make_obsvmodel_inc_plot(
  rename(demand_fromobs_update9_cnstrlam_agg32FE_ML,
         Qs = Qs_obs,Qn = Qn_obs,Qtot = Qtot_obs),
  rename(demand_fromobs_update9_cnstrlam_agg32FE_ML,
         Qs = Qs_mod,Qn = Qn_mod,Qtot = Qtot_mod),
  "Model vs Observations, update9 cnstrlam agg32FE ML parameters",
  55,6,55/6)
plot(g_ml_FE)
ggsave(g_ml_FE,file=paste0(fig_path,"scatter_demand-income_update9_cnstrlam_agg32FE_ML.png"),
       width = 8, height = 6.5, dpi = 150)
# hi joint price params
g_hi_FE <- make_obsvmodel_inc_plot(
  rename(demand_fromobs_update9_cnstrlam_agg32FE_hi_price,
         Qs = Qs_obs,Qn = Qn_obs,Qtot = Qtot_obs),
  rename(demand_fromobs_update9_cnstrlam_agg32FE_hi_price,
         Qs = Qs_mod,Qn = Qn_mod,Qtot = Qtot_mod),
  "Model vs Observations, update9 cnstrlam agg32FE hi price parameters",
  55,6,55/6)
plot(g_hi_FE)
ggsave(g_hi_FE,file=paste0(fig_path,"scatter_demand-income_update9_cnstrlam_agg32FE_hi_price.png"),
       width = 8, height = 6.5, dpi = 150)
# lo joint price params
g_lo_FE <- make_obsvmodel_inc_plot(
  rename(demand_fromobs_update9_cnstrlam_agg32FE_lo_price,
         Qs = Qs_obs,Qn = Qn_obs,Qtot = Qtot_obs),
  rename(demand_fromobs_update9_cnstrlam_agg32FE_lo_price,
         Qs = Qs_mod,Qn = Qn_mod,Qtot = Qtot_mod),
  "Model vs Observations, update9 cnstrlam agg32FE lo price parameters",
  55,6,55/6)
plot(g_lo_FE)
ggsave(g_lo_FE,file=paste0(fig_path,"scatter_demand-income_update9_cnstrlam_agg32FE_lo_price.png"),
       width = 8, height = 6.5, dpi = 150)
# hi joint income params
g_hi_FE <- make_obsvmodel_inc_plot(
  rename(demand_fromobs_update9_cnstrlam_agg32FE_hi_inc,
         Qs = Qs_obs,Qn = Qn_obs,Qtot = Qtot_obs),
  rename(demand_fromobs_update9_cnstrlam_agg32FE_hi_inc,
         Qs = Qs_mod,Qn = Qn_mod,Qtot = Qtot_mod),
  "Model vs Observations, update9 cnstrlam agg32FE hi income parameters",
  55,6,55/6)
plot(g_hi_FE)
ggsave(g_hi_FE,file=paste0(fig_path,"scatter_demand-income_update9_cnstrlam_agg32FE_hi_inc.png"),
       width = 8, height = 6.5, dpi = 150)
# lo joint income params
g_lo_FE <- make_obsvmodel_inc_plot(
  rename(demand_fromobs_update9_cnstrlam_agg32FE_lo_inc,
         Qs = Qs_obs,Qn = Qn_obs,Qtot = Qtot_obs),
  rename(demand_fromobs_update9_cnstrlam_agg32FE_lo_inc,
         Qs = Qs_mod,Qn = Qn_mod,Qtot = Qtot_mod),
  "Model vs Observations, update9 cnstrlam agg32FE lo income parameters",
  55,6,55/6)
plot(g_lo_FE)
ggsave(g_lo_FE,file=paste0(fig_path,"scatter_demand-income_update9_cnstrlam_agg32FE_lo_inc.png"),
       width = 8, height = 6.5, dpi = 150)
# hi joint scale params
g_hi_FE <- make_obsvmodel_inc_plot(
  rename(demand_fromobs_update9_cnstrlam_agg32FE_hi_scale,
         Qs = Qs_obs,Qn = Qn_obs,Qtot = Qtot_obs),
  rename(demand_fromobs_update9_cnstrlam_agg32FE_hi_scale,
         Qs = Qs_mod,Qn = Qn_mod,Qtot = Qtot_mod),
  "Model vs Observations, update9 cnstrlam agg32FE hi scale parameters",
  55,6,55/6)
plot(g_hi_FE)
ggsave(g_hi_FE,file=paste0(fig_path,"scatter_demand-income_update9_cnstrlam_agg32FE_hi_scale.png"),
       width = 8, height = 6.5, dpi = 150)
# lo joint scale params
g_lo_FE <- make_obsvmodel_inc_plot(
  rename(demand_fromobs_update9_cnstrlam_agg32FE_lo_scale,
         Qs = Qs_obs,Qn = Qn_obs,Qtot = Qtot_obs),
  rename(demand_fromobs_update9_cnstrlam_agg32FE_lo_scale,
         Qs = Qs_mod,Qn = Qn_mod,Qtot = Qtot_mod),
  "Model vs Observations, update9 cnstrlam agg32FE lo scale parameters",
  55,6,55/6)
plot(g_lo_FE)
ggsave(g_lo_FE,file=paste0(fig_path,"scatter_demand-income_update9_cnstrlam_agg32FE_lo_scale.png"),
       width = 8, height = 6.5, dpi = 150)

demand_fromobs_update9_cnstrlam_agg32FE_lo[
  demand_fromobs_update9_cnstrlam_agg32FE_lo$Qs_mod > 2.5,]
demand_fromobs_update9_cnstrlam_agg32FE_hi_price[
  demand_fromobs_update9_cnstrlam_agg32FE_hi_price$Qs_mod > 2.3 &
    demand_fromobs_update9_cnstrlam_agg32FE_hi_price$Y > 1,]

#       ------------------------------------------------------------------------
#       CONTINUED ... NOT YET UPDATED ...
#       -----------------------------------------------------------------------

# not updated past here ...

# scatter plot of observations vs predictions, gcam parameters
obsvgcam_data <-
  data.frame(Qtot_model=Food_Demand_fromobs_gcam$Total_Demand,
             Qtot_obs=Obs_Data_sample$Total_Demand)
g_obsvgcam_scatter <- ggplot(obsvgcam_data,
                             aes(x=Qtot_obs,y=Qtot_model)) +
  geom_point() +
  labs(title = "Model vs Observations, GCAM parameters") +
  xlab("Total Demand (observations)") +
  ylab("Total Demand (model)")
plot(g_obsvgcam_scatter)

obsvgcam_data <- data.frame(Qs_model=Food_Demand_fromobs_gcam$Qs,
                            Qs_obs=Obs_Data_sample$Qs)
g_obsvgcam_scatter <- ggplot(obsvgcam_data,
                             aes(x=Qs_obs,y=Qs_model)) +
  geom_point() +
  labs(title = "Model vs Observations, GCAM parameters") +
  xlab("Staples Demand (observations)") +
  ylab("Staples Demand (model)")
plot(g_obsvgcam_scatter)

obsvgcam_data <- data.frame(Qn_model=Food_Demand_fromobs_gcam$Qn,
                            Qn_obs=Obs_Data_sample$Qn)
g_obsvgcam_scatter <- ggplot(obsvgcam_data,
                             aes(x=Qn_obs,y=Qn_model)) +
  geom_point() +
  labs(title = "Model vs Observations, GCAM ML parameters") +
  xlab("Non-Staples Demand (observations)") +
  ylab("Non-Staples Demand (model)")
plot(g_obsvgcam_scatter)

# scatter plot of observations vs predictions as fn of income, update9 ML parameters
g_obsvupdate9_scatter <- ggplot() +
  geom_point(data=Obs_Data_sample,aes(x=Y,y=Total_Demand,color="Observed")) +
  geom_point(data=Food_Demand_fromobs_update9_ML,aes(x=Y,y=Total_Demand,color="Modeled")) +
  labs(title = "Model vs Observations, update9 ML parameters") +
  xlab("Income") +
  ylab("Total Demand")
plot(g_obsvupdate9_scatter)

g_obsvupdate9_scatter <- ggplot() +
  geom_point(data=Obs_Data_sample,aes(x=Y,y=Qs,color="Observed")) +
  geom_point(data=Food_Demand_fromobs_update9_ML,aes(x=Y,y=Qs,color="Modeled")) +
  labs(title = "Model vs Observations, update9 ML parameters") +
  xlab("Income") +
  ylab("Staples Demand")
plot(g_obsvupdate9_scatter)

g_obsvupdate9_scatter <- ggplot() +
  geom_point(data=Obs_Data_sample,aes(x=Y,y=Qn,color="Observed")) +
  geom_point(data=Food_Demand_fromobs_update9_ML,aes(x=Y,y=Qn,color="Modeled")) +
  labs(title = "Model vs Observations, update9 ML parameters") +
  xlab("Income") +
  ylab("Non-Staples Demand")
plot(g_obsvupdate9_scatter)

# scatter plot of observations vs predictions as fn of income, GCAM parameters
g_obsvgcam_scatter <- ggplot() +
  geom_point(data=Obs_Data_sample,aes(x=Y,y=Total_Demand,color="Observed")) +
  geom_point(data=Food_Demand_fromobs_gcam,aes(x=Y,y=Total_Demand,color="Modeled")) +
  labs(title = "Model vs Observations, GCAM parameters") +
  xlab("Income") +
  ylab("Total Demand")
plot(g_obsvgcam_scatter)

g_obsvgcam_scatter <- ggplot() +
  geom_point(data=Obs_Data_sample,aes(x=Y,y=Qs,color="Observed")) +
  geom_point(data=Food_Demand_fromobs_gcam,aes(x=Y,y=Qs,color="Modeled")) +
  labs(title = "Model vs Observations, GCAM parameters") +
  xlab("Income") +
  ylab("Staples Demand")
plot(g_obsvgcam_scatter)

g_obsvgcam_scatter <- ggplot() +
  geom_point(data=Obs_Data_sample,aes(x=Y,y=Qn,color="Observed")) +
  geom_point(data=Food_Demand_fromobs_gcam,aes(x=Y,y=Qn,color="Modeled")) +
  labs(title = "Model vs Observations, GCAM parameters") +
  xlab("Income") +
  ylab("Non-Staples Demand")
plot(g_obsvgcam_scatter)

g_incomeelast_s_obs <- ggplot() +
  geom_line(data=Food_Demand_fromobs_gcam,aes(x=Y,y=eta.s,color="GCAM")) +
  geom_line(data=Food_Demand_fromobs_update9_ML,aes(x=Y,y=eta.s,color="update9 ML")) +
  labs(title = "Income elasticity (staples), GCAM v update9 ML parameters") +
  xlab("Income") +
  ylab("Elasticity")
plot(g_incomeelast_s_obs)

#    ---------------------------------------------------------------------------
#    Identify inputs causing near-zero predicted demand
#    ---------------------------------------------------------------------------

# get subset of data with near zero Qs or Qn; inspection shows they are the same subsets
zeroQs_data <- demand_fromobs_update9_cnstrlam_ML %>% filter(Qs < 0.1)
zeroQn_data <- demand_fromobs_update9_cnstrlam_ML %>% filter(Qn < 0.1)

# rename the full dataset for convenience
all_data <- demand_fromobs_update9_cnstrlam_ML

# plot Pn vs Ps for full data set and subset with near-zero Qs, Qn
g_PnPs <- ggplot() +
  geom_point(aes(all_data$Ps,all_data$Pn)) +
  geom_point(aes(zeroQs_data$Ps,zeroQs_data$Pn),color="orange") +
  xlim(0,6.5) +
  ylim(0,8) +
  labs(title = "Pn v Ps for all obs and near-zero model demand") +
  xlab("Price, staples") +
  ylab("Price, non-staples") +
  coord_fixed(6.5/8)
plot(g_PnPs)
ggsave(g_PnPs,file=paste0(fig_path,"PnvPs_obs_zerodemand_update9_cnstrlam_ML.png"))

# plot Pn vs Income for full data set and subset with near-zero Qs, Qn
g_PnInc <- ggplot() +
  geom_point(aes(all_data$Y,all_data$Pn)) +
  geom_point(aes(zeroQs_data$Y,zeroQs_data$Pn),color="orange") +
  xlim(0,120) +
  ylim(0,8) +
  labs(title = "Pn v income for all obs and near-zero model demand") +
  xlab("Income per capita") +
  ylab("Price, non-staples") +
  coord_fixed(120/8)
plot(g_PnInc)
ggsave(g_PnInc,file=paste0(fig_path,"PnvInc_obs_zerodemand_update9_cnstrlam_ML.png"))

# ------------------------------------------------------------------------------
# Plots for other parameter data sets -- TO BE UPDATED
# ------------------------------------------------------------------------------

#     --------------------------------------------------------------------------
#     Plots for Edmonds et al parameters
#     --------------------------------------------------------------------------

# high price and (independent) uncertainty intervals (high side) relative to
# Edmonds et al parameters and prices
g <- ggplot()+
  geom_line(data=Food_Demand_edmonds,aes(x= Y,y= Qs,color= "Staple Demand"),
            linewidth = thickness)+
  geom_line(data=Food_Demand_edmonds,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linewidth = thickness)+
  geom_line(data=Food_Demand_edmonds,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linewidth = thickness)+

  geom_line(data=Food_Demand_edmonds_highPsPn,aes(x= Y,y= Qs,color= "Staple Demand"),
            linetype = "dashed",linewidth = thickness)+
  geom_line(data=Food_Demand_edmonds_highPsPn,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linetype = "dashed",linewidth = thickness)+
  geom_line(data=Food_Demand_edmonds_highPsPn,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linetype = "dashed",linewidth = thickness)+

  geom_line(data=Food_Demand_edmonds_HighPelast,aes(x= Y,y= Qs,color= "Staple Demand"),
            linetype = "dotted",linewidth = thickness)+
  geom_line(data=Food_Demand_edmonds_HighPelast,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linetype = "dotted",linewidth = thickness)+
  geom_line(data=Food_Demand_edmonds_HighPelast,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linetype = "dotted",linewidth = thickness)+

  geom_line(data=Food_Demand_edmonds_HighPxelast,aes(x= Y,y= Qs,color= "Staple Demand"),
            linetype = "dotdash",linewidth = thickness)+
  geom_line(data=Food_Demand_edmonds_HighPxelast,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linetype = "dotdash",linewidth = thickness)+
  geom_line(data=Food_Demand_edmonds_HighPxelast,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linetype = "dotdash",linewidth = thickness)+

  xlab("GDP per capita in thousand USD" )+
  ylab("Thousand calories")

# high and low Kappa cases relative to the Edmonds et al parameters
g_k_s <- ggplot()+
  geom_line(data=Food_Demand_edmonds,aes(x= Y,y= Qs,color= "Staple Demand"),
            linewidth = thickness)+
  geom_line(data=Food_Demand_edmonds,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linewidth = thickness)+
  geom_line(data=Food_Demand_edmonds,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linewidth = thickness)+

  geom_line(data=Food_Demand_edmonds_LowK,aes(x= Y,y= Qs,color= "Staple Demand"),
            linetype = "dashed",linewidth = thickness)+
  geom_line(data=Food_Demand_edmonds_LowK,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linetype = "dashed",linewidth = thickness)+
  geom_line(data=Food_Demand_edmonds_LowK,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linetype = "dashed",linewidth = thickness)+

  geom_line(data=Food_Demand_edmonds_HighK,aes(x= Y,y= Qs,color= "Staple Demand"),
            linetype = "dotted",linewidth = thickness)+
  geom_line(data=Food_Demand_edmonds_HighK,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linetype = "dotted",linewidth = thickness)+
  geom_line(data=Food_Demand_edmonds_HighK,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linetype = "dotted",linewidth = thickness)+

  xlim(0,2.5) +
  labs(title = "High and low cases for k_s, Edmonds et al parameters",
       subtitle = "95% confidence interval (independent)") +
  xlab("GDP per capita in thousand USD" )+
  ylab("Thousand calories")

# high and low income sensitivity cases relative to the Edmonds et al parameters
g_sens_inc <- ggplot()+
  geom_line(data=Food_Demand_edmonds,aes(x= Y,y= Qs,color= "Staple Demand"),
            linewidth = thickness)+
  geom_line(data=Food_Demand_edmonds,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linewidth = thickness)+
  geom_line(data=Food_Demand_edmonds,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linewidth = thickness)+

  geom_line(data=Food_Demand_edmonds_HighYelast,aes(x= Y,y= Qs,color= "Staple Demand"),
            linetype = "dashed",linewidth = thickness)+
  geom_line(data=Food_Demand_edmonds_HighYelast,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linetype = "dashed",linewidth = thickness)+
  geom_line(data=Food_Demand_edmonds_HighYelast,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linetype = "dashed",linewidth = thickness)+

  geom_line(data=Food_Demand_edmonds_LowYelast,aes(x= Y,y= Qs,color= "Staple Demand"),
            linetype = "dotted",linewidth = thickness)+
  geom_line(data=Food_Demand_edmonds_LowYelast,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linetype = "dotted",linewidth = thickness)+
  geom_line(data=Food_Demand_edmonds_LowYelast,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linetype = "dotted",linewidth = thickness)+

  labs(title = "High and low income elasticities, Edmonds et al parameters") +
  xlab("GDP per capita in thousand USD" )+
  ylab("Thousand calories")

#     --------------------------------------------------------------------------
#     Plots for orig9 parameters
#     --------------------------------------------------------------------------

# high and low joint price sensitivity cases relative to the Edmonds et al parameters
g_sens_price <- ggplot()+
  geom_line(data=Food_Demand_edmonds,aes(x= Y,y= Qs,color= "Staple Demand"),
            linewidth = thickness)+
  geom_line(data=Food_Demand_edmonds,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linewidth = thickness)+
  geom_line(data=Food_Demand_edmonds,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linewidth = thickness)+

  geom_line(data=Food_Demand_orig9_hisens_price,aes(x= Y,y= Qs,color= "Staple Demand"),
            linetype = "dashed",linewidth = thickness)+
  geom_line(data=Food_Demand_orig9_hisens_price,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linetype = "dashed",linewidth = thickness)+
  geom_line(data=Food_Demand_orig9_hisens_price,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linetype = "dashed",linewidth = thickness)+

  geom_line(data=Food_Demand_orig9_losens_price,aes(x= Y,y= Qs,color= "Staple Demand"),
            linetype = "dotted",linewidth = thickness)+
  geom_line(data=Food_Demand_orig9_losens_price,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linetype = "dotted",linewidth = thickness)+
  geom_line(data=Food_Demand_orig9_losens_price,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linetype = "dotted",linewidth = thickness)+

  labs(title = "High and low joint price senstivity cases, Edmonds et al parameters") +
  xlab("GDP per capita in thousand USD" )+
  ylab("Thousand calories")

# high and low joint income sensitivity cases relative to the Edmonds et al parameters
g_sens_inc <- ggplot()+
  geom_line(data=Food_Demand_edmonds,aes(x= Y,y= Qs,color= "Staple Demand"),
            linewidth = thickness)+
  geom_line(data=Food_Demand_edmonds,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linewidth = thickness)+
  geom_line(data=Food_Demand_edmonds,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linewidth = thickness)+

  geom_line(data=Food_Demand_orig9_hisens_inc,aes(x= Y,y= Qs,color= "Staple Demand"),
            linetype = "dashed",linewidth = thickness)+
  geom_line(data=Food_Demand_orig9_hisens_inc,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linetype = "dashed",linewidth = thickness)+
  geom_line(data=Food_Demand_orig9_hisens_inc,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linetype = "dashed",linewidth = thickness)+

  geom_line(data=Food_Demand_orig9_losens_inc,aes(x= Y,y= Qs,color= "Staple Demand"),
            linetype = "dotted",linewidth = thickness)+
  geom_line(data=Food_Demand_orig9_losens_inc,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linetype = "dotted",linewidth = thickness)+
  geom_line(data=Food_Demand_orig9_losens_inc,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linetype = "dotted",linewidth = thickness)+

#  xlim(0,2) +
  labs(title = "High and low joint income sensitivity cases, Edmonds et al parameters") +
  xlab("GDP per capita in thousand USD" )+
  ylab("Thousand calories")

# high and low joint scale sensitivity cases relative to the Edmonds et al parameters
g_sens_scale <- ggplot()+
  geom_line(data=Food_Demand_edmonds,aes(x= Y,y= Qs,color= "Staple Demand"),
            linewidth = thickness)+
  geom_line(data=Food_Demand_edmonds,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linewidth = thickness)+
  geom_line(data=Food_Demand_edmonds,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linewidth = thickness)+

  geom_line(data=Food_Demand_orig9_hisens_scale,aes(x= Y,y= Qs,color= "Staple Demand"),
            linetype = "dashed",linewidth = thickness)+
  geom_line(data=Food_Demand_orig9_hisens_scale,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linetype = "dashed",linewidth = thickness)+
  geom_line(data=Food_Demand_orig9_hisens_scale,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linetype = "dashed",linewidth = thickness)+

  geom_line(data=Food_Demand_orig9_losens_scale,aes(x= Y,y= Qs,color= "Staple Demand"),
            linetype = "dotted",linewidth = thickness)+
  geom_line(data=Food_Demand_orig9_losens_scale,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linetype = "dotted",linewidth = thickness)+
  geom_line(data=Food_Demand_orig9_losens_scale,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linetype = "dotted",linewidth = thickness)+

  labs(title = "High and low joint scale sensitivity cases, Edmonds et al parameters") +
  xlab("GDP per capita in thousand USD" )+
  ylab("Thousand calories")

# high and low joint price and income sensitivity cases relative to the Edmonds et al parameters
g_sens_priceinc <- ggplot()+
  geom_line(data=Food_Demand_edmonds,aes(x= Y,y= Qs,color= "Staple Demand"),
            linewidth = thickness)+
  geom_line(data=Food_Demand_edmonds,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linewidth = thickness)+
  geom_line(data=Food_Demand_edmonds,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linewidth = thickness)+

  geom_line(data=Food_Demand_orig9_hisens_priceinc,aes(x= Y,y= Qs,color= "Staple Demand"),
            linetype = "dashed",linewidth = thickness)+
  geom_line(data=Food_Demand_orig9_hisens_priceinc,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linetype = "dashed",linewidth = thickness)+
  geom_line(data=Food_Demand_orig9_hisens_priceinc,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linetype = "dashed",linewidth = thickness)+

  geom_line(data=Food_Demand_orig9_losens_priceinc,aes(x= Y,y= Qs,color= "Staple Demand"),
            linetype = "dotted",linewidth = thickness)+
  geom_line(data=Food_Demand_orig9_losens_priceinc,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linetype = "dotted",linewidth = thickness)+
  geom_line(data=Food_Demand_orig9_losens_priceinc,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linetype = "dotted",linewidth = thickness)+

  labs(title = "High and low joint price and income senstivity cases, Edmonds et al parameters") +
  xlab("GDP per capita in thousand USD" )+
  ylab("Thousand calories")

# high and low joint price, income and scale sensitivity cases relative to the Edmonds et al parameters
g_sens_priceincscale <- ggplot()+
  geom_line(data=Food_Demand_edmonds,aes(x= Y,y= Qs,color= "Staple Demand"),
            linewidth = thickness)+
  geom_line(data=Food_Demand_edmonds,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linewidth = thickness)+
  geom_line(data=Food_Demand_edmonds,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linewidth = thickness)+

  geom_line(data=Food_Demand_orig9_hisens_priceincscale,aes(x= Y,y= Qs,color= "Staple Demand"),
            linetype = "dashed",linewidth = thickness)+
  geom_line(data=Food_Demand_orig9_hisens_priceincscale,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linetype = "dashed",linewidth = thickness)+
  geom_line(data=Food_Demand_orig9_hisens_priceincscale,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linetype = "dashed",linewidth = thickness)+

  geom_line(data=Food_Demand_orig9_losens_priceincscale,aes(x= Y,y= Qs,color= "Staple Demand"),
            linetype = "dotted",linewidth = thickness)+
  geom_line(data=Food_Demand_orig9_losens_priceincscale,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linetype = "dotted",linewidth = thickness)+
  geom_line(data=Food_Demand_orig9_losens_priceincscale,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linetype = "dotted",linewidth = thickness)+

  labs(title = "High and low joint price, income and scale senstivity cases, Edmonds et al parameters") +
  xlab("GDP per capita in thousand USD" )+
  ylab("Thousand calories")

#     --------------------------------------------------------------------------
#     Plots for GCAM parameters
#     --------------------------------------------------------------------------

# high price result relative to the GCAM default parameters and prices
g_gcam <- ggplot()+
  geom_line(data=Food_Demand_gcam,aes(x= Y,y= Qs,color= "Staple Demand"),
            linewidth = thickness)+
  geom_line(data=Food_Demand_gcam,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linewidth = thickness)+
  geom_line(data=Food_Demand_gcam,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linewidth = thickness)+
  #  geom_line(data=Food_Demand_gcam,aes(x= Y,y= Qm,color= "Mat Demand"),linewidth = thickness)+
  geom_line(data=Food_Demand_gcam_highPsPn,aes(x= Y,y= Qs,color= "Staple Demand"),
            linetype = "dashed",linewidth = thickness)+
  geom_line(data=Food_Demand_gcam_highPsPn,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linetype = "dashed",linewidth = thickness)+
  geom_line(data=Food_Demand_gcam_highPsPn,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linetype = "dashed",linewidth = thickness)+
  xlab("GDP per capita in thousand USD" )+
  ylab("Thousand calories")

# E Africa: high price result relative to the GCAM default parameters and prices
g_gcam_EAf <- ggplot()+
  geom_line(data=Food_Demand_gcam_EAf,aes(x= Y,y= Qs,color= "Staple Demand"),
            linewidth = thickness)+
  geom_line(data=Food_Demand_gcam_EAf,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linewidth = thickness)+
  geom_line(data=Food_Demand_gcam_EAf,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linewidth = thickness)+
  geom_line(data=Food_Demand_gcam_EAf_highPsPn,aes(x= Y,y= Qs,color= "Staple Demand"),
            linetype = "dashed",linewidth = thickness)+
  geom_line(data=Food_Demand_gcam_EAf_highPsPn,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linetype = "dashed",linewidth = thickness)+
  geom_line(data=Food_Demand_gcam_EAf_highPsPn,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linetype = "dashed",linewidth = thickness)+
  labs(title = "East Africa, GCAM parameters",
       subtitle = "Income and prices representative of reference case, 2015-2050") +
  xlab("GDP per capita in thousand USD" )+
  ylab("Thousand calories")

# India: high price result relative to the GCAM default parameters and prices
g_gcam_Ind <- ggplot()+
  geom_line(data=Food_Demand_gcam_Ind,aes(x= Y,y= Qs,color= "Staple Demand"),
            linewidth = thickness)+
  geom_line(data=Food_Demand_gcam_Ind,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linewidth = thickness)+
  geom_line(data=Food_Demand_gcam_Ind,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linewidth = thickness)+
  geom_line(data=Food_Demand_gcam_Ind_highPsPn,aes(x= Y,y= Qs,color= "Staple Demand"),
            linetype = "dashed",linewidth = thickness)+
  geom_line(data=Food_Demand_gcam_Ind_highPsPn,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linetype = "dashed",linewidth = thickness)+
  geom_line(data=Food_Demand_gcam_Ind_highPsPn,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linetype = "dashed",linewidth = thickness)+
  labs(title = "India, GCAM parameters",
       subtitle = "Income and prices representative of reference case, 2015-2050") +
  xlab("GDP per capita in thousand USD" )+
  ylab("Thousand calories")

#     --------------------------------------------------------------------------
#     Plots for update9 parameters
#     --------------------------------------------------------------------------

# old plots from here down, may need to be updated...

# price sensitivity relative to the update9 ML parameters
g_sens_price <- ggplot()+
  geom_line(data=Food_Demand_update9,aes(x= Y,y= Qs,color= "Staple Demand"),
            linewidth = 1.3)+
  geom_line(data=Food_Demand_update9,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linewidth = 1.3)+
  geom_line(data=Food_Demand_update9,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linewidth = 1.3)+

  geom_line(data=Food_Demand_update9_highPsPn,aes(x= Y,y= Qs,color= "Staple Demand"),
            linewidth = 0.5)+
  geom_line(data=Food_Demand_update9_highPsPn,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linewidth = 0.5)+
  geom_line(data=Food_Demand_update9_highPsPn,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linewidth = 0.5)+

  labs(title = "Price sensitivity to update9 ML parameters") +
  xlab("GDP per capita in thousand USD" )+
  ylab("Thousand calories")

# price sensitivity relative to the update9 Hi parameters
g_sens_price <- ggplot()+
  geom_line(data=Food_Demand_update9_hisens_price,aes(x= Y,y= Qs,color= "Staple Demand"),
            linetype = "dashed",linewidth = 1.3)+
  geom_line(data=Food_Demand_update9_hisens_price,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linetype = "dashed",linewidth = 1.3)+
  geom_line(data=Food_Demand_update9_hisens_price,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linetype = "dashed",linewidth = 1.3)+

  geom_line(data=Food_Demand_update9_hisens_price_highPsPn,aes(x= Y,y= Qs,color= "Staple Demand"),
            linetype = "dashed",linewidth = 0.5)+
  geom_line(data=Food_Demand_update9_hisens_price_highPsPn,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linetype = "dashed",linewidth = 0.5)+
  geom_line(data=Food_Demand_update9_hisens_price_highPsPn,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linetype = "dashed",linewidth = 0.5)+

  labs(title = "Price sensitivity to update9 high sensitivity (price) parameters") +
  xlab("GDP per capita in thousand USD" )+
  ylab("Thousand calories")

# price sensitivity relative to the update9 Lo parameters
g_sens_price <- ggplot()+
  geom_line(data=Food_Demand_update9_losens_price,aes(x= Y,y= Qs,color= "Staple Demand"),
            linetype = "dotted",linewidth = 1.3)+
  geom_line(data=Food_Demand_update9_losens_price,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linetype = "dotted",linewidth = 1.3)+
  geom_line(data=Food_Demand_update9_losens_price,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linetype = "dotted",linewidth = 1.3)+

  geom_line(data=Food_Demand_update9_losens_price_highPsPn,aes(x= Y,y= Qs,color= "Staple Demand"),
            linetype = "dotted",linewidth = 0.8)+
  geom_line(data=Food_Demand_update9_losens_price_highPsPn,aes(x= Y,y= Qn,color= "Non Staple Demand"),
            linetype = "dotted",linewidth = 0.8)+
  geom_line(data=Food_Demand_update9_losens_price_highPsPn,aes(x= Y,y= Total_Demand,color= "Total Demand"),
            linetype = "dotted",linewidth = 0.8)+

  labs(title = "Price sensitivity to update9 low sensitivity (price) parameters") +
  xlab("GDP per capita in thousand USD" )+
  ylab("Thousand calories")

# ------------------------------------------------------------------------------

