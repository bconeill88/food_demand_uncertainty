# Calculate results for 2017 parameter estimation (Edmonds et al) --------------
# ----


# TO DO:
# source functions
# redo path names to be consistent with project structure
# change file format to rds for saved files
# define region list


# record max likelihood and 95% (independent) confidence intervals directly from
# the Edmonds et al 2017 paper; ML values are the same as the default values in ambrosia.
# same as for 2021 parameters, transform the nonstaples income elasticity parameter
# (nu1) and the staples max income term (k_s) to be consistent with ambrosia
# expectations (as per KN 2/6/24)
params_2017 <- data.frame(matrix(ncol = 12, nrow = 3))
rownames(params_2017) <- c("ML","Lo","Hi")
colnames(params_2017) <- c("As", "An", "xi.ss", "xi.cross", "xi.nn", "eps1n",
                           "lambda", "ks", "Pm", "psscl", "pnscl","LL")
params_2017['ML',] <-
  c(1.28, 1.14, -0.19, 0.21, -0.33, 0.98/2, 0.10, exp(2.77), 5.06, 100, 20,NA)
params_2017['Lo',] <-
  c(1.22, 0.76, -0.30, 0.05, -0.51, 0.87/2, 0.07, exp(2.01), 1.94, 100, 20,NA)
params_2017['Hi',] <-
  c(1.50, 1.23, -0.05, 0.31, -0.03, 1.34/2, 0.20, exp(2.99), 5.91, 100, 20,NA)

# save parameters to input directory

save(params_2017,file=paste0("inputs/derived/params_2017.RData"))

# calculate and save demand from observations; use iteration = 1 for 2017 ML params

# global and FE params for 2017 ML
params_2017_global <- mutate(params_2017['ML',], measure = "ML2017", iteration=1) %>% 
  relocate(measure)
params_2017_FE <- data.frame(measure = "ML2017",GCAM_region_ID = c(1:32),
                             staples_FE = 0,LL = NA, region = NA, iteration = 1)

# load obs data
load(paste(analysis_dir,input_path,"obs_data.RData",sep="/"))

# calculate demand based on obs
measures <- c("ML2017")
food.dmnd.obs(params_2017_global,params_2017_FE,obs_data,measures[1],reg_list)

# clean up
rm(params_2017_global,params_2017_FE,obs_data)



# Calculate results for 2021 parameter estimation (Narayan and Waldhoff) -------
# ----



# read parameter values from gcam input csv's; 9 params, tack on the two constants
# transform the nonstaples income elasticity parameter (nu1) and the staples max income
# term (k_s) to be consistent with ambrosia expectations (as per KN 2/6/24)
staples_data <- read.csv(paste("inputs/raw/A_demand_food_staples.csv",sep=""),skip=7)
nonstaples_data <- read.csv(paste("inputs/raw/A_demand_food_nonstaples.csv",sep=""),skip=7)
nonstaples_data$income.elasticity <- nonstaples_data$income.elasticity/2.0
staples_data$income.max.term <- exp(staples_data$income.max.term)
gcam_psscl <- 100 # these two constants are from Edmonds et al paper
gcam_pnscl <- 20
# combine and put in the order expected by ambrosia
params_2021 <- data.frame(matrix(NA, nrow = 1, ncol = 11))
rownames(params_2021) <- c("ML")
colnames(params_2021) <- c("As", "An", "xi.ss", "xi.cross", "xi.nn", "eps1n",
                           "lambda", "ks", "Pm", "psscl", "pnscl")
params_2021[1,] <- c(staples_data$scale.param,nonstaples_data$scale.param,
                     staples_data$self.price.elasticity,staples_data$cross.price.elasticity,
                     nonstaples_data$self.price.elasticity,nonstaples_data$income.elasticity,
                     staples_data$income.elasticity,staples_data$income.max.term,
                     staples_data$price.received,gcam_psscl,gcam_pnscl)

# save parameters to input directory

save(params_2021,file=paste0("inputs/derived/params_2021.RData"))

# calculate and save demand from observations; use iteration = 2 for 2021 ML params

# global and FE params for 2017 ML
params_2021_global <- mutate(params_2021['ML',], measure = "ML2021", iteration=2) %>% 
  relocate(measure)
params_2021_FE <- data.frame(measure = "ML2021",GCAM_region_ID = c(1:32),
                             staples_FE = 0,LL = NA, region = NA, iteration = 2)

# load obs data
load(paste(analysis_dir,input_path,"obs_data.RData",sep="/"))

# calculate demand based on obs
measures <- c("ML2021")
food.dmnd.obs(params_2021_global,params_2021_FE,obs_data,measures[1],reg_list)

# clean up
rm(params_2021_global,params_2021_FE,obs_data)


