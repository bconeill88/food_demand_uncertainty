print("figure generation for update9_cnstrlam_agg32FE_24jan25")
cat("\n")


# Initializations ------------------------------------------------------------------
# ----

# generate figures for food demand uncertainty analysis

# load packages
library(tidyverse)

# load functions
#source("R/demand_functions.R")

# subdirectories of data/processed to use
procdata_dir <- "update9_cnstrlam_agg32FE_24jan25"

# define list of scenarios to run
scen_list_demand <- list(c("Ref_ML_gcam_ens_bc"))

# define regions to run over
# get command line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  reg_list <- c(as.numeric(args[1]))
} else {
  reg_list <- c(1:2)
}


# create figures directory if necessary
if(!dir.exists(paste(analysis_dir,"figures",sep="/"))) {
  dir.create(paste(analysis_dir,"figures",sep="/"))
}

# Parameter traces ------------------------------------------------------------------
# UPDATE THIS
# ----

# function for plotting traces for 9 variables
make_trace_plots <- function(param_data) {
  param_colnames <- select(param_data,c('As':'Pm')) %>% colnames()
  param_data_long <- gather(param_data,key="parameter",value="value",param_colnames)
  g_trace <- ggplot(param_data_long,aes(x=iteration,y=value)) +
    geom_point(size=0.3) +
    facet_wrap(~parameter, ncol=3, scales = "free")
#  plot(g_trace)
}

# function for plotting traces for fixed effects
make_trace_plots_FE <- function(param_data) {
  g_trace <- ggplot(param_data,aes(x=iteration,y=staples_FE)) +
    geom_point(size=0.3) +
    facet_wrap(~region, ncol=4, scales = "free")
#  plot(g_trace)
}

# plot traces
load(paste(analysis_dir,input_path,"param_data_global_clean_10k.RData",sep="/"))
load(paste(analysis_dir,input_path,"param_data_FE_clean_10k.RData",sep="/"))
# global parameters plot
param_data_global_clean_10k %>% make_trace_plots()
ggsave("param_trace_global.png",path = paste(analysis_dir,fig_path,sep="/"),
       width = 6, height = 4.5, dpi = 150)
# FE parameters plot, first set of regions
param_data_FE_clean_10k %>% filter(GCAM_region_ID <= 16) %>% make_trace_plots_FE()
ggsave("param_trace_FE_1.png",path = paste(analysis_dir,fig_path,sep="/"),
       width = 8, height = 5, dpi = 150)
# FE parameters plot, second set of regions
param_data_FE_clean_10k %>% filter(GCAM_region_ID > 16) %>% make_trace_plots_FE()
ggsave("param_trace_FE_2.png",path = paste(analysis_dir,fig_path,sep="/"),
       width = 8, height = 5, dpi = 150)
# clean up
rm(param_data_global_clean_10k,param_data_FE_clean_10k)


# Parameter marginal densities -------------------------------------------------
# ----

# plot densities of 9 global parameters along with ML estimates: current, 2017
# params, and 2021 params
make_density_plots_global <- function(
    paramdata,  # df of parameter samples, for density plot
    MLparams,    # df containing ML parameter values, for vertical line
    edmondsparams,
    gcamparams,
    title,subtitle)
{
  # convert to long format for faceting, 9 variables
  param_colnames <- subset(paramdata,select=As:Pm) %>% colnames() 
  paramdata_long <- 
    gather(paramdata[,1:9],key="parameter", value="value",c(param_colnames))
  # define ML to plot as vertical lines
  vline.p1.ml <- data.frame(
    p1_ML = as.vector(
      t(MLparams[MLparams$measure == "ML",] %>% subset(select=As:Pm))),
    parameter = c(param_colnames))
  vline.p2.ml <- data.frame(
    p2_ML = as.vector(
      t(edmondsparams["ML",] %>% subset(select=As:Pm))),
    parameter = c(param_colnames))
  vline.p3.ml <- data.frame(
    p3_ML = as.vector(
      t(gcamparams["ML",] %>% subset(select=As:Pm))),
    parameter = c(param_colnames))

  # plot
  thickness <- 1
  colors = c("2025" = "red","2021" = "blue","2017" = "green")
  g <- ggplot() +
    geom_density(data = paramdata_long, aes(x = value),
                 linewidth = thickness, color = "black") +
    geom_vline(data = vline.p1.ml, aes(xintercept = p1_ML, color = "2025"),
               linewidth = thickness) +
    geom_vline(data = vline.p2.ml, aes(xintercept = p2_ML, color = "2017"),
                 linewidth = thickness) +
    geom_vline(data = vline.p3.ml, aes(xintercept = p3_ML, color = "2021"),
                 linewidth = thickness) +
      facet_wrap(~parameter, ncol=3, scales = "free") +
      scale_color_manual(name="Estimate",values=colors) +
    theme(strip.text = element_text(size = 15)) +
    ggtitle(title,subtitle=subtitle)
  plot(g)
  ggsave(file=paste(analysis_dir,fig_path,"probdens_params_global_withML.png",sep="/"),
         width = 8, height = 6, dpi = 150)
}

# plot densities of FE parameters (16 regions) along with ML estimates
make_density_plots_FE <- function(
    paramdata,  # df of parameter samples, for density plot
    MLparams,    # df containing ML parameter values, for vertical line
    title,subtitle)      # plot title
{
  # define ML to plot as vertical lines
  vline.p1.ml <- data.frame(
    p1_ML = as.vector(MLparams[MLparams$measure == "ML","staples_FE"]),
    region = as.vector(MLparams[MLparams$measure == "ML","region"]))
  # plot
  thickness <- 1
  g <- ggplot() +
    geom_density(data = paramdata, aes(x = staples_FE),
                 linewidth = thickness, color = "black") +
    geom_vline(data = vline.p1.ml, aes(xintercept = p1_ML),
               linewidth = thickness, color = "red") +
    facet_wrap(~region, ncol=4, scales = "free") +
    theme(strip.text = element_text(size = 15)) +
    ggtitle(title,subtitle=subtitle)
  plot(g)
  ggsave(file=paste(analysis_dir,fig_path,"probdens_params_FE_withML.png",sep="/"),
         width = 8, height = 6, dpi = 150)
}

# load needed files
load(paste(analysis_dir,input_path,"param_data_global_clean_sub.Rdata",sep="/"))
load(paste(analysis_dir,results_path,"params_ML_intervals_global.Rdata",sep="/"))
load(paste0("inputs/derived/params_2017.Rdata"))
load(paste0("inputs/derived/params_2021.Rdata"))

# plot global parameters
make_density_plots_global(
  param_data_global_clean_sub,
  params_ML_intervals_global,
  params_2017,
  params_2021,
  "Global parameter densities and Max Likelihood estimates",
  "")

# load needed files
load(paste(analysis_dir,input_path,"param_data_FE_clean_sub.Rdata",sep="/"))
load(paste(analysis_dir,results_path,"params_ML_intervals_FE.Rdata",sep="/"))

# plot FE parameters
make_density_plots_FE(
  param_data_FE_clean_sub %>% filter(GCAM_region_ID <=16),
  params_ML_intervals_FE %>% filter(GCAM_region_ID <=16),
  "FE parameter densities and Max Likelihood estimates",
  "")

# clean up
rm(param_data_FE_clean_sub,param_data_global_clean_sub,params_ML_intervals_FE,
   params_ML_intervals_global)

# Values of fixed effects -------------------------------------------------
# ----

# function for plotting FE parameter values across regions, faceted for each case
# in the parameter data file passed as an argument
make_FE_values_plot <- function(paramdata,title1) {
  ggplot(paramdata,aes(x = GCAM_region_ID, y = staples_FE)) +
    geom_point() +
    facet_wrap(~factor(measure, c("ML", "HI", "LI","HPR","LPR",
                                  "HIR","LIR","HSR","LSR"))) +
    theme(strip.text = element_text(size = 15)) +
    labs(title = title1) +
    xlab("GCAM region") +
    ylab("Fixed Effect")
  ggsave(file=paste(analysis_dir,fig_path,"params_FE_values.png",sep="/"),
         width = 8, height = 6, dpi = 150)
}

# load needed files
load(paste(analysis_dir,results_path,"params_ML_intervals_FE.Rdata",sep="/"))

make_FE_values_plot(params_ML_intervals_FE,"Regional FE parameter values")

# clean up
rm(params_ML_intervals_FE)


# Model comparison to observations ---------------------------------------------
# ADD PLOTTING OF FITS FOR PARAMETER SETS BEYOND ML, INCLUDING 2017 AND 2021
# PARAMS; CUSTOMIZE PLOT TITLES AND FILE NAMES PLOTS ARE SAVED TO
# ----

# function for plotting three scatter plots, model v observed, for Qs, Qn, Qtot
make_obsvmodel_scatter <- function(demand_data,title1,xmax,ymax,ratio) {
  list(data.frame(x=demand_data$Qs_obs,y=demand_data$Qs_mod,dataset="Qs"),
       data.frame(x=demand_data$Qn_obs,y=demand_data$Qn_mod,dataset="Qn"),
       data.frame(x=demand_data$Qtot_obs,y=demand_data$Qtot_mod,dataset="Qtot")) %>%
    bind_rows() %>%
    ggplot(aes(x,y)) +
    geom_point(size=1.0) +
    geom_abline(intercept = 0, slope = 1) +
    facet_wrap(~dataset) +
    theme(strip.text = element_text(size = 15)) +
    labs(title = title1) +
    xlim(0,xmax) +
    ylim(0,ymax) +
    xlab("Demand (observations)") +
    ylab("Demand (model)") +
    coord_fixed(ratio)
  ggsave(file=paste(analysis_dir,fig_path,"demand_obs_GCAM.png",sep="/"),
         width = 8, height = 8, dpi = 150)
}

# function for plotting three scatter plots, model v observed as fn of income,
# for Qs, Qn, Qtot
make_obsvmodel_inc_plot <- function(demand_data,title1,xmax,ymax,ratio) {
  colors <- c("Modeled" = "orange","Observed" = "black")
  list(data.frame(x=demand_data$Y,y1=demand_data$Qs_obs,
                  y2=demand_data$Qs_mod,dataset="Qs"),
       data.frame(x=demand_data$Y,y1=demand_data$Qn_obs,
                  y2=demand_data$Qn_mod,dataset="Qn"),
       data.frame(x=demand_data$Y,y1=demand_data$Qtot_obs,
                  y2=demand_data$Qtot_mod,dataset="Qtot")) %>%
    bind_rows() %>%
    ggplot() +
    geom_point(aes(x,y1,color="Observed"),size=1.0) +
    geom_point(aes(x,y2,color="Modeled"),size=1.0) +
    scale_color_manual(name=element_blank(),values=colors) +
    facet_wrap(~dataset) +
    theme(legend.position = "left",strip.text = element_text(size = 15,),
          legend.text = element_text(size=12)) +
    labs(title = title1) +
    xlim(0,xmax) +
    ylim(0,ymax) +
    xlab("Income") +
    ylab("Demand") +
    coord_fixed(ratio)
  ggsave(file=paste(analysis_dir,fig_path,"demand_obs_inc_ML.png",sep="/"),
         width = 8, height = 8, dpi = 150)
}

# load file of modeled and observed results
load(paste(analysis_dir,results_path,"Obs/demand_obs_ML.Rdata",sep="/"))

# plot and save scatterplots
# ML params: xy limits = 4, 4, coord fixed = 1
make_obsvmodel_scatter(demand_obs,"Model vs Observations, ML parameters",
                       4,4,4/4) # xlim, ylim, ratio
make_obsvmodel_inc_plot(demand_obs,"Model vs Observations, ML parameters",
                        NA,4,max((demand_obs$Y)/4)) # xlim, ylim, ratio

load(paste(analysis_dir,results_path,"Obs/demand_obs_GCAM.Rdata",sep="/"))
make_obsvmodel_scatter(demand_obs,"Model vs Observations, GCAM parameters",
                       4,4,4/4) # xlim, ylim, ratio

# clean up
rm(demand_obs)

# Demand uncertainty ranges ----------------------------------------------------
# ADD PLOT OF ALL REGIONS; MAKE MORE FLEXIBLE
# ----

# function for making demand plot for Qs, Qn, Qtot for three datasets (e.g., ML,
# HI, LI parameters) for a single region
make_demand3_plot <- function(df1,df2,df3,title1text,title2text) {
  thickness <- 1
  colors <- c("Staples" = "green","Non-staples" = "red","Total" = "blue")
  lines <- c("ML" = "solid","90% CI" = "dotted")
  g <- ggplot()+
    geom_line(data=df1,aes(x= Y,y= Qs,color= "Staples",linetype = "ML"),
              linewidth = thickness)+
    geom_line(data=df1,aes(x= Y,y= Qn,color= "Non-staples",linetype = "ML"),
              linewidth = thickness)+
    geom_line(data=df1,aes(x= Y,y= Qtot,color= "Total",linetype = "ML"),
              linewidth = thickness)+
    
    geom_line(data=df2,aes(x= Y,y= Qs,color= "Staples",linetype ="90% CI"),
              linewidth = thickness)+
    geom_line(data=df2,aes(x= Y,y= Qn,color= "Non-staples",linetype ="90% CI"),
              linewidth = thickness)+
    geom_line(data=df2,aes(x= Y,y= Qtot,color= "Total",linetype ="90% CI"),
              linewidth = thickness)+
    
    geom_line(data=df3,aes(x= Y,y= Qs,color= "Staples",linetype ="90% CI"),
              linewidth = thickness)+
    geom_line(data=df3,aes(x= Y,y= Qn,color= "Non-staples",linetype ="90% CI"),
              linewidth = thickness)+
    geom_line(data=df3,aes(x= Y,y= Qtot,color= "Total",linetype ="90% CI"),
              linewidth = thickness)+
    
    scale_colour_manual(name = "Demand", values = colors) +
    scale_linetype_manual(name = element_blank(), values = lines) +
    xlim(0,100) +
    ylim(-1,7) +
    labs(title = title1text,subtitle = title2text) +
    xlab("GDP per capita (thousand USD)" )+
    ylab("Calories (thousand/cap/day)") +
    coord_fixed(100/8)
  ggsave(g,file=paste0(analysis_dir,"/",fig_path,"/demand_MLHILI_R",reg_n,".png"),
         width = 6, height = 6, dpi = 150)
}

# plot ML and independent CI for demand for two regions with largest/smallest
# FE values: Non-EU Europe (or S Korea is 2nd) and Aus_NZ

# price scenario
scen <- "Pdef"
# region
reg_n <- 6 # 6 = Aus/NZ, 28 = S Korea, 15 = Non EU Europe
reg_name <- "Australia-New Zealand"
reg_n <- 15
reg_name <- "Non-EU Europe"
reg_n <- 28
reg_name <- "S Korea"

# load needed files
load(paste0(analysis_dir,"/",results_path,"/",scen,"/demand_intervals_R",reg_n,".Rdata"))

# plot
make_demand3_plot(intervals[intervals$measure == "ML",],
                  intervals[intervals$measure == "HI",],
                  intervals[intervals$measure == "LI",],
                  paste0("Food demand, ML and 90% CI, ",reg_name),
                  "Default prices") 


# function for making demand plot for Qs, Qn, Qtot for five datasets (e.g., ML,
# HI, LI parameters, compared to HD, LD parameters) for a single region
make_demand5_plot <- function(df1,df2,df3,df4,df5,title1text,title2text,region) {
  thickness <- 1
  colors <- c("Staples" = "green","Non-staples" = "red","Total" = "blue")
  lines <- c("ML" = "solid","90% CI" = "dotted","HD LD" = "dashed")
  g <- ggplot()+
    geom_line(data=df1,aes(x= Y,y= Qs,color= "Staples",linetype = "ML"),
              linewidth = thickness)+
    geom_line(data=df1,aes(x= Y,y= Qn,color= "Non-staples",linetype = "ML"),
              linewidth = thickness)+
#    geom_line(data=df1,aes(x= Y,y= Qtot,color= "Total",linetype = "ML"),
#              linewidth = thickness)+
    
    geom_line(data=df2,aes(x= Y,y= Qs,color= "Staples",linetype ="90% CI"),
              linewidth = thickness)+
    geom_line(data=df2,aes(x= Y,y= Qn,color= "Non-staples",linetype ="90% CI"),
              linewidth = thickness)+
    #    geom_line(data=df2,aes(x= Y,y= Qtot,color= "Total",linetype ="90% CI"),
    #              linewidth = thickness)+
    
    geom_line(data=df3,aes(x= Y,y= Qs,color= "Staples",linetype ="90% CI"),
              linewidth = thickness)+
    geom_line(data=df3,aes(x= Y,y= Qn,color= "Non-staples",linetype ="90% CI"),
              linewidth = thickness)+
    #    geom_line(data=df3,aes(x= Y,y= Qtot,color= "Total",linetype ="90% CI"),
    #              linewidth = thickness)+
    
    geom_line(data=df4,aes(x= Y,y= Qs,color= "Staples",linetype ="HD LD"),
              linewidth = thickness)+
    geom_line(data=df4,aes(x= Y,y= Qn,color= "Non-staples",linetype ="HD LD"),
              linewidth = thickness)+
    #    geom_line(data=df4,aes(x= Y,y= Qtot,color= "Total",linetype ="HD LD"),
    #              linewidth = thickness)+

    geom_line(data=df5,aes(x= Y,y= Qs,color= "Staples",linetype ="HD LD"),
              linewidth = thickness)+
    geom_line(data=df5,aes(x= Y,y= Qn,color= "Non-staples",linetype ="HD LD"),
              linewidth = thickness)+
    #    geom_line(data=df5,aes(x= Y,y= Qtot,color= "Total",linetype ="HD LD"),
    #              linewidth = thickness)+
    
    scale_colour_manual(name = "Demand", values = colors) +
    scale_linetype_manual(name = element_blank(), values = lines) +
    xlim(0,5) +
    ylim(-1,4) +
    labs(title = title1text,subtitle = title2text) +
    xlab("GDP per capita (thousand USD)" )+
    ylab("Calories (thousand/cap/day)") +
    coord_fixed(5/5)
  ggsave(g,file=paste0(analysis_dir,"/",fig_path,"/demand_MLHILIHDLD_R",region,"_zoom.png"),
         width = 6, height = 6, dpi = 150)
}

# plot ML and independent CI for demand for two regions with largest/smallest
# FE values: Non-EU Europe (or S Korea is 2nd) and Aus_NZ

# price scenario
scen <- "Pdef"
# region
reg_n <- 6 # 6 = Aus/NZ, 28 = S Korea, 15 = Non EU Europe
reg_name <- "Australia-New Zealand"
reg_n <- 15
reg_name <- "Non-EU Europe"
reg_n <- 28
reg_name <- "S Korea"
reg_n <- 2
reg_name <- "Eastern Africa"
reg_n <- 11
reg_name <- "China"

# load needed files
load(paste0(analysis_dir,"/",results_path,"/",scen,"/demand_intervals_R",reg_n,".Rdata"))
load(paste(analysis_dir,results_path,scen,"/demand_freq_HD_Qtot.Rdata",sep="/"))
assign("demand_HD",demand_spcase)
load(paste(analysis_dir,results_path,scen,"/demand_freq_LD_Qtot.Rdata",sep="/"))
assign("demand_LD",demand_spcase)

# plot
make_demand5_plot(intervals[intervals$measure == "ML",],
                  intervals[intervals$measure == "HI",],
                  intervals[intervals$measure == "LI",],
                  demand_HD[demand_HD$GCAM_region_ID == reg_n,],
                  demand_LD[demand_HD$GCAM_region_ID == reg_n,],
                  paste0("Food demand, ML, 90% CI, and HD LD ",reg_name),
                  "Default prices",
                  reg_n) 



# Differences in demand --------------------------------------------------------
# STILL WORKING ON THIS
# ----


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


# function for making plot of demand differences for a single region, faceted for
# Qs, Qn, Qtot,
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

# function for making plot of ML and hi/lo interval of demand (Qs, Qn, or Qtot)
# for up to 16 regions for results from two different analyses, so they can be compared;
make_demand_compare_plot <- function(analysis1,scen1,analysis2,scen2,regions) {

  # load demand interval results for both analyses for required regions
  intervals1 <- lapply(regions,function(x) {
    load(paste0(analysis1,"/",results_path,"/",scen1,"/","demand_intervals_R",x,".RData"))
    intervals
    }) %>% bind_rows()
  intervals2 <- lapply(regions,function(x) {
    load(paste0(analysis2,"/",results_path,"/",scen2,"/","demand_intervals_R",x,".RData"))
    intervals
  }) %>% bind_rows()

  # datasets for three curves for first set of results
  df1_mid <- intervals1[intervals1$measure == "ML",]
  df1_hi <- intervals1[intervals1$measure == "Hi_indep",]
  df1_lo <- intervals1[intervals1$measure == "Lo_indep",]

  # datasets for three curves for second set of results
  df2_mid <- intervals2[intervals2$measure == "ML",]
  df2_hi <- intervals2[intervals2$measure == "Hi_indep",]
  df2_lo <- intervals2[intervals2$measure == "Lo_indep",]

  thickness <- 1
  g_trace <- ggplot() +

    # results from analysis 1
    geom_line(data=df1_mid,aes(x= Y,y= Qs,color=scen1,linetype= "ML"),
              linewidth = thickness)+
    geom_line(data=df1_hi,aes(x= Y,y= Qs,color=scen1,linetype= "Hi indep"),
              linewidth = thickness)+
    geom_line(data=df1_lo,aes(x= Y,y= Qs,color=scen1,linetype= "Lo indep"),
              linewidth = thickness)+

    # results from analysis 2
    geom_line(data=df2_mid,aes(x= Y,y= Qs,color=scen2,linetype= "ML"),
              linewidth = thickness)+
    geom_line(data=df2_hi,aes(x= Y,y= Qs,color=scen2,linetype= "Hi indep"),
              linewidth = thickness)+
    geom_line(data=df2_lo,aes(x= Y,y= Qs,color=scen2,linetype= "Lo indep"),
              linewidth = thickness)+

    scale_linetype_manual(name = "Measure", values = c("dashed","dotted","solid")) +
    scale_colour_manual(name = "Scenario", values = c("black","red")) +

    facet_wrap(~region, ncol=4, scales = "free")
  plot(g_trace)
}

  param_colnames <- select(param_data,c('As':'Pm')) %>% colnames()
  param_data_long <- gather(param_data,key="parameter",value="value",param_colnames)
  g_trace <- ggplot(param_data_long,aes(x=iteration,y=value)) +
    geom_point(size=0.3) +
    facet_wrap(~parameter, ncol=3, scales = "free")
  #  plot(g_trace)

reg_list <- c(1:16)
make_demand_compare_plot(analysis_dir,"Pdef",analysis_dir,"2xPsPn",reg_list)


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


var <- Ps
intervals$var
intervals$as.name(var)
parse(text = var)
intervals$parse(text = var)


# HD/LD vs uncertainty intervals ----------------------
# CLEAN THIS UP
# ----

# function for making plot of ML and hi/lo interval of demand (Qs, Qn, or Qtot)
# for up to 16 regions for results from two different analyses, so they can be compared;

# make food type an argument to this function
# understand why distribution is skewed high relative to ML
# understand why LD seems not as good a match as HD, maybe it is due
# to high income being undersampled?


make_demand_compare_plot2 <- function(analysis,scen,regions,
                                      fd_type_interval,fd_type_plot) {

  # load demand interval results for required regions
  intervals_main <- lapply(regions,function(x) {
    load(paste0(analysis,"/",results_path,"/",scen,"/demand_intervals_R",x,".RData"))
    intervals
  }) %>% bind_rows()
  # load HD/LD demand interval results and keep required regions
  load(paste0(analysis,"/",results_path,"/",scen,"/demand_freq_HD_",fd_type_interval,".RData"))
  intervals_HD <- demand_spcase %>% filter(GCAM_region_ID %in% regions)
  load(paste0(analysis,"/",results_path,"/",scen,"/demand_freq_LD_",fd_type_interval,".RData"))
  intervals_LD <- demand_spcase %>% filter(GCAM_region_ID %in% regions)
  # datasets for three curves for first set of results
  df1_mid <- intervals_main[intervals_main$measure == "ML",]
  df1_hi <- intervals_main[intervals_main$measure == "HI",]
  df1_lo <- intervals_main[intervals_main$measure == "LI",]

  thickness <- 1
  g <- ggplot() +

    # results from main intervals
    geom_line(data=df1_mid,aes(x= Y,y= eval(parse(text=fd_type_plot)),color=scen,linetype= "ML"),
              linewidth = thickness)+
    geom_line(data=df1_hi,aes(x= Y,y= eval(parse(text=fd_type_plot)),color=scen,linetype= "90% CI"),
              linewidth = thickness)+
    geom_line(data=df1_lo,aes(x= Y,y= eval(parse(text=fd_type_plot)),color=scen,linetype= "90% CI"),
              linewidth = thickness)+

    # results from special case
    geom_line(data=intervals_HD,aes(x= Y,y= eval(parse(text=fd_type_plot)),color=scen,linetype= "HD"),
              linewidth = thickness)+
    geom_line(data=intervals_LD,aes(x= Y,y= eval(parse(text=fd_type_plot)),color=scen,linetype= "LD"),
              linewidth = thickness)+

    scale_linetype_manual(name = "Measure",
                          values = c("dashed","dotted","dotted","solid")) +
    scale_colour_manual(name = "Scenario",
                        values = c("black","red")) +
    xlab("GDP per capita") +
    ylab("Calories (thous/cap/day)") +
    xlim(0,10) +
    facet_wrap(~region, ncol=4, scales = "free")
#    scale_x_continuous(trans = "log10")

  plot(g)
}

# plot demand intervals and HD/LD demand for each region, in two plots
reg_list <- c(1:16)
make_demand_compare_plot2(analysis_dir,"Pdef",reg_list,"Qtot","Qn")
reg_list <- c(1:16)
make_demand_compare_plot2(analysis_dir,"Pdef",reg_list,"QsQn","Qs")

# check density at a given income level
load(paste0(analysis_dir,"/",results_path,"/Pdef/demand_R1.RData"))
demand_dist <- demand_reg[as.numeric(demand_reg$Y) == 1.2,]
g <- ggplot(demand_dist, aes(x = Qtot)) +
  geom_density()
plot(g)


# ambrosia time series samples -------------------------------------------------
# ----

# plot sample of bias-corrected ensemble of ambrosia demand time series for Qs 
# and Qn, both for a given decile and for the regional average

# define GCAM region ID, scenario, decile, and sample size
reg <- 1
scen <- "Ref_ML_gcam"
dec <- 1
nsammple <- 100

# get bias-corrected demand for the Ref_ML_gcam scenario for one region as example
demand_reg <- readRDS(paste0("data/processed/", procdata_dir,"/", scen, 
                             "/demand_R", reg, "_ens_bc.RDS"))

# get random sample of iterations for a single decile
demand_sample <- demand_reg %>%
  filter(`gcam-consumer` == paste0("FoodDemand_Group",dec)) %>%
  filter(iteration %in% (
    demand_reg %>%
      distinct(iteration) %>%
      slice_sample(n = nsample) %>%
      pull(iteration)
  ))

# plot this sample of time series

# Convert data to long format for faceting
df_long <- demand_sample %>%
  pivot_longer(cols = c(Qs, Qn, Qs.region, Qn.region), names_to = "Variable", 
               values_to = "Value") %>%
  # Ensure Qs variables appears on the left and Qn on the right of the plot
  mutate(Variable = factor(Variable, levels = c("Qs", "Qn", "Qs.region", "Qn.region")))

# Extract unique region and gcam-consumer values
region_str <- paste(unique(demand_sample$region), collapse = ", ")
consumer_str <- paste(unique(demand_sample$`gcam-consumer`), collapse = ", ")

# Create the faceted time series plot for the decile and the region
ggplot(df_long, aes(x = year, y = Value, group = iteration, color = as.factor(iteration))) +
  geom_line(alpha = 0.5) +
  facet_wrap(~Variable, scales = "free_y") +  
  labs(title = paste("Time Series of Qs and Qn by Iteration\nRegion:", region_str, 
                     "| GCAM Consumer:", consumer_str),
       x = "Years",
       y = "Value",
       color = "Iteration") +
  theme_minimal() +
  theme(legend.position = "none")  # Hide legend since there are many iterations
# save figure
ggsave(paste0("demand_R", reg, "_decile", dec, "_ens_bc.png"), 
       path = paste("output/figures", procdata_dir, scen, sep = "/"),
       bg = "white")



# DON'T KNOW WHAT THE REST OF THIS IS ------------------------------------------
# ----

load(paste0(analysis_dir,"/",results_path,"/Pdef/demand_HD_freq_Qtot.RData"))
assign("demand_HD_freq_Qtot",demand_spcase)
load(paste0(analysis_dir,"/",results_path,"/Pdef/demand_LD_freq_Qtot.RData"))
assign("demand_LD_freq_Qtot",demand_spcase)
rm(demand_spcase)

load(paste0(analysis_dir,"/",results_path,"/Pdef/demand_intervals_R1.RData"))




load(paste0(analysis_dir,"/",results_path,"/Pdef/params_HDLD_Qtot_freq_global.RData"))

load(paste0(analysis_dir,"/",results_path,"/Pdef/params_HDLD_Qtot_freq_FE.RData"))




make_density3_plots(
  param_data_global_clean_sub,param_global_lo_interval,param_global_hi_interval,
  c("Full","7.5-12.5%","92.5-97.5%"),
  "Parameter probability densities for differences in demand at $10k income",
  "Full range and high/low intervals, update9 constrained lambda")
ggsave("probdens_params_full_intervals_update9_cnstrlam.png",path = fig_path)


# function for making plots of three density functions for each of nine parameters
# takes three dataframes (containing at least the values for parameters) and
# a list of legend labels for each; line types are solid (df1), dashed (df2), dotted (df3)
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

labels <- c("HD","LD")
paramdata <- tmp
# function for making plots of parameter values over income for HD and LD cases for
# each region for each of nine parameters
# takes one dataframe containing the measure (e.g., HD, LD), parameter values and
# income; and
# a list of legend labels for each measure; line types are blue (measure1), red
# (measure2)
#make_HD_LD_global_param_plots <- function(paramdata,labels,title1,title2) {

# convert to long format for faceting, 9 variables
param_colnames <- c("As", "An", "xi.ss", "xi.cross", "xi.nn", "eps1n",
                    "lambda", "ks", "Pm")
paramdata_long <- gather(paramdata, key="parameter", value="value",c(param_colnames))
data1 <- paramdata_long[paramdata_long$GCAM_region_ID == 1 &
                          paramdata_long$measure == labels[1],]
data2 <- paramdata_long[paramdata_long$GCAM_region_ID == 1 &
                          paramdata_long$measure == labels[2],]
data3 <- paramdata_long[paramdata_long$GCAM_region_ID == 2 &
                          paramdata_long$measure == labels[1],]
data4 <- paramdata_long[paramdata_long$GCAM_region_ID == 2 &
                          paramdata_long$measure == labels[2],]

# plot
thickness <- 1
g <- ggplot() +
  geom_line(data = data1, aes(x = Y,y = value,color = labels[1]),
            linewidth = thickness) +
  geom_line(data = data2, aes(x = Y,y = value,color = labels[2]),
            linewidth = thickness) +
  geom_line(data = data3, aes(x = Y,y = value,color = labels[1]),
            linewidth = thickness) +
  geom_line(data = data4, aes(x = Y,y = value,color = labels[2]),
            linewidth = thickness) +

  scale_color_manual(name = "Measure", values = c("red","blue")) +
  facet_wrap(~parameter, ncol=3, scales = "free") +
  labs(title = title1, subtitle = title2)
plot(g)
