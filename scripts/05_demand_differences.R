
# calculate and save differences in demand and elasticities for subsample ------
#   and intervals ----------------------------------------------------------------
# ----

# TO DO:
# source functions
# redo path names to be consistent with project structure
# change file format to rds for saved files
# define scenario list

# calculate difference in demand and elasticities between two different demand
# scenarios, for regions defined by number in reg_list
lapply(scen_list_diffs, function(x) subtract_demand_dfs(x,scen_base_diffs,reg_list)) %>%
  invisible()

print("saved differences in demand")



