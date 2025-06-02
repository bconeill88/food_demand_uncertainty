# calculate and save demand and elasticities from observations -----------------
# ADD CALC OF OBS FROM HD/LD PARAMS --------------------------------------------
# ----


# TO DO:
# source functions
# redo path names to be consistent with project structure
# change file format to rds for saved files
# define region list


# load needed files
load(paste(analysis_dir,results_path,"params_ML_intervals_global.RData",sep="/"))
load(paste(analysis_dir,results_path,"params_ML_intervals_FE.RData",sep="/"))
load(paste(analysis_dir,input_path,"obs_data.RData",sep="/"))

# calculate demand from obs
measures <- c("ML","LPR","HPR","LIR","HIR","LSR","HSR")
# calculate
for(m in 1:length(measures)) {
  food.dmnd.obs(params_ML_intervals_global,params_ML_intervals_FE,obs_data,
                measures[m],reg_list)
}

# clean up
rm(params_ML_intervals_global,params_ML_intervals_FE,obs_data)

print("saved demand for observations")

