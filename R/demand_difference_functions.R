# calculate and save differences in demand and elasticities for ----------------
#   subsample and intervals ------------------------------------------------------
# ----

# function to subtract all elements of two income/price scenarios of food demand and
# elasticities (scen1 - scen2) for regions defined in "regions", keeping columns
# that should not be subtracted (Y, iteration, etc.)
subtract_demand_dfs <- function(scen1,scen2,regions) {
  
  for (r in 1:length(regions)) {
    
    # load regional demand for two scenarios to subtract
    load(paste0(analysis_dir,"/",results_path,"/",scen1,"/","demand_R",
                regions[r],".RData"))
    assign("df1",demand_reg)
    load(paste0(analysis_dir,"/",results_path,"/",scen2,"/","demand_R",
                regions[r],".RData"))
    assign("df2",demand_reg)
    
    # subtract
    diffs_reg <- cbind(
      subset(df2, select = c(Y:Pn,LL:region)),
      subset(df1, select = -c(Y:Pn,LL:region)) - subset(df2, select = -c(Y:Pn,LL:region)))
    
    # save in scen1 directory
    save(diffs_reg,
         file=paste0(analysis_dir,"/",results_path,"/",scen1,"/","demand_diffs_",
                     scen1,"_",scen2,"_R",regions[r],".RData"))
  }
}
