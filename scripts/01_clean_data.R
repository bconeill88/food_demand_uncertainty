# read in raw data, clean, save 

# TO DO:
# load required packages
# source functions
# redo path names to be consistent with project structure
# define data file names
# define start and stop iteration numbers
# define sub-sample size
# change file format to rds for saved files

# details of cleaning depend on the raw files, so separate code for each set
if(grepl("update9_cnstrlam_agg32FE_13jan25",analysis_dir)) {
  
  # global parameter data
  param_data_global_raw <-
    read.mc.data(paste(input_raw_path,param_data_global_file,sep="/"),
                 varnames = namemc(nparam = 11))
  # name this unnamed column
  colnames(param_data_global_raw)[13] <- "iteration"
  # remove first row which repeats column headings
  param_data_global_clean <- param_data_global_raw[-1,]
  # convert all columns from character to numeric
  i <- c(1:ncol(param_data_global_clean))
  param_data_global_clean[, i] <- apply(param_data_global_clean[, i], 2,
                                        function(x) as.numeric(unlist(x)))
  # keep samples only after burn in
  param_data_global_clean <- param_data_global_clean %>%
    filter(iteration >= iter_start,iteration <= iter_end) %>% arrange(iteration)
  # create sub-sample for analysis of distributions
  param_data_global_clean_sub <- param_data_global_clean %>%
    slice_sample(n=subsample) %>% arrange(iteration)
  
  # FE parameter data
  param_data_FE_raw <-
    read.table(paste(input_raw_path,param_data_FE_file,sep="/"),
               header=TRUE,sep="") %>% rename(iteration = iteration_number)
  # select same iterations as in global parameter data
  param_data_FE_clean <-
    subset(param_data_FE_raw,iteration %in% param_data_global_clean$iteration)
  param_data_FE_clean_sub <-
    subset(param_data_FE_raw,iteration %in% param_data_global_clean_sub$iteration)
  
  # observational data for 32 GCAM regions
  obs_data <- read.csv(paste(input_raw_path,obs_data_file,sep="/"))
  
} else if(grepl("update9_cnstrlam_agg32FE_24jan25",analysis_dir)) {
  
  # global parameter data
  param_data_global_raw <-
    read.mc.data(paste(input_raw_path,param_data_global_file,sep="/"),
                 varnames = namemc(nparam = 11))
  # name this unnamed column
  colnames(param_data_global_raw)[13] <- "iteration"
  # remove last column which is not needed
  param_data_global_raw[14] <- NULL
  # convert all columns from character to numeric
  param_data_global_clean <- param_data_global_raw
  i <- c(1:ncol(param_data_global_clean))
  param_data_global_clean[, i] <- apply(param_data_global_clean[, i], 2,
                                        function(x) as.numeric(unlist(x)))
  # keep samples only after burn in
  param_data_global_clean <- param_data_global_clean %>%
    filter(iteration >= iter_start,iteration <= iter_end) %>% arrange(iteration)
  # create sub-sample for analysis of distributions
  param_data_global_clean_sub <- param_data_global_clean %>%
    slice_sample(n=subsample) %>% arrange(iteration)
  
  # FE parameter data
  param_data_FE_raw <-
    read.table(paste(input_raw_path,param_data_FE_file,sep="/"),
               header=TRUE,sep="") %>% rename(iteration = iteration_number)
  # select same iterations as in global parameter data
  param_data_FE_clean <-
    subset(param_data_FE_raw,iteration %in% param_data_global_clean$iteration)
  param_data_FE_clean_sub <-
    subset(param_data_FE_raw,iteration %in% param_data_global_clean_sub$iteration)
  
  # observational data for 32 GCAM regions
  obs_data <- read.csv(paste(input_raw_path,obs_data_file,sep="/"))
}

# save cleaned files
save(param_data_global_clean,
     file = paste(analysis_dir,input_path,"param_data_global_clean.RData",sep="/"))
save(param_data_global_clean_sub,
     file = paste(analysis_dir,input_path,"param_data_global_clean_sub.RData",sep="/"))
save(param_data_FE_clean,
     file = paste(analysis_dir,input_path,"param_data_FE_clean.RData",sep="/"))
save(param_data_FE_clean_sub,
     file = paste(analysis_dir,input_path,"param_data_FE_clean_sub.RData",sep="/"))
save(obs_data,
     file = paste(analysis_dir,input_path,"obs_data.RData",sep="/"))

# clean up
rm(param_data_global_raw,param_data_FE_raw)
rm(param_data_global_clean,param_data_global_clean_sub,param_data_FE_clean,
   param_data_FE_clean_sub,obs_data)

print("saved cleaned data")



