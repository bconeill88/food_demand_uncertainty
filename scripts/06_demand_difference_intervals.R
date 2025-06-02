# identify and save uncertainty intervals for differences in demand ------------
#   and elasticities -------------------------------------------------------------
# ----

# TO DO:
# source functions
# redo path names to be consistent with project structure
# change file format to rds for saved files
# define scenario list

# calculate demand difference uncertainty intervals - independent, and differences
# associated with joint uncertainty intervals
# load needed files
load(paste(analysis_dir,results_path,"params_ML_intervals_global.RData",sep="/"))
lapply(scen_list_diffs,function(x)
  make_demand_intervals(confinterval,x,scen_base_diffs,reg_list,params_ML_intervals_global)) %>%
  invisible()
rm(params_ML_intervals_global)

print("saved intervals for differences in demand")
