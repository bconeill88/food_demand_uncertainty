
# ------------------------------------------------------------------------------
# Initializations
# ------------------------------------------------------------------------------

setwd("G:/My Drive/Work/Projects/R projects/food_demand/ambrosia")

# load libraries
library(ambrosia)
library(dplyr)
library(ggplot2)

#vignette("ambrosia_vignette")

# input data path name, relative to ambrosia folder
input_raw_path <- "inputs/raw/"
input_derived_path <-  "inputs/derived/"
# path to figures folder
fig_path <- "figures/"

# ------------------------------------------------------------------------------
# Get raw and pre-existing original processed data
# ------------------------------------------------------------------------------

# load raw data; 6840 obs=; do not rename Q variables now, processing function
# expects original variable names
data("training_data")
training_data <- training_data %>%
#  rename(`Qn`=`ns_cal_pcap_day_thous`,`Qs`=`s_cal_pcap_day_thous`) %>%
  mutate(Qtot = ns_cal_pcap_day_thous + s_cal_pcap_day_thous)

# processed data used in original MCMC parameter estimation; 4191 obs
processed_data <- read.csv(paste(input_raw_path,"Processed_data_for_MC.csv",sep="")) %>%
  mutate(Qtot = Qs+Qn, sigQs = sqrt(sig2Qs), sigQn = sqrt(sig2Qn))

# ------------------------------------------------------------------------------
# Create and save new processed datasets
# ------------------------------------------------------------------------------

# create processed dataset based on raw data with no constraints
# 4191 obs
# note min_clusters constraint does not bind
fd_data_no_constr <- create.dataset.for.parameter.fit(
  data=training_data,
  min_clusters = 0,
  min_price_pd = 1e10, # max Pn constraint
  min_cal_fd = 0, # min Qtot constraint
  outdir=tempdir())
save(fd_data_no_constr,file=paste0(input_derived_path,"fd_data_no_constr.Rdata"))

# re-create original processed dataset and save it
# 4191 obs, so constraints exclude 39% of original observations
# note min_clusters constraint does not bind
fd_data_orig <- create.dataset.for.parameter.fit(
  data=training_data,
  min_clusters = 20,
  min_price_pd = 20, # max Pn constraint
  min_cal_fd = 1700, # min Qtot constraint
  outdir=tempdir())
save(fd_data_orig,file=paste0(input_derived_path,"fd_data_orig.Rdata"))

# create alternative dataset with no minimum Qtot constraint
# 5528 obs, so max Pn constraint excludes 19% of original data, min Qtot excludes 20%
fd_data_no_min_Qtot <- create.dataset.for.parameter.fit(
  data=training_data,
  min_clusters = 20,
  min_price_pd = 20,
  min_cal_fd = 0,
  outdir=tempdir())
save(fd_data_no_min_Qtot,file=paste0(input_derived_path,"fd_data_no_min_Qtot.Rdata"))

# create alternative dataset with no max Pn constraint
# 5528 obs, so max Pn constraint excludes 19% of orginal data
fd_data_no_max_Pn <- create.dataset.for.parameter.fit(
  data=training_data,
  min_clusters = 20,
  min_price_pd = 1e10,
  min_cal_fd = 1700,
  outdir=tempdir())
save(fd_data_no_max_Pn,file=paste0(input_derived_path,"fd_data_no_max_Pn.Rdata"))

# ------------------------------------------------------------------------------
# Read in saved datasets
# ------------------------------------------------------------------------------

# re-creation of original processed dataset
load(paste0(input_derived_path,"fd_data_orig.Rdata"))
fd_data_orig <- fd_data_clust20
fd_data_orig <- fd_data_orig %>% mutate(Qtot = Qs+Qn, sigQs = sqrt(sig2Qs),
                                              sigQn = sqrt(sig2Qn))
# alternative dataset with no minimum Qtot constraint
load(paste0(input_derived_path,"fd_data_no_min_Qtot.Rdata"))
fd_data_no_min_Qtot <- fd_data_no_min_cal
fd_data_no_min_Qtot <- fd_data_no_min_Qtot %>% mutate(Qtot = Qs+Qn, sigQs = sqrt(sig2Qs),
                                        sigQn = sqrt(sig2Qn))
# alternative dataset with no maximum price (Pn) constraint
load(paste0(input_derived_path,"fd_data_no_max_Pn.Rdata"))
fd_data_no_max_Pn <- fd_data_no_max_Pn %>% mutate(Qtot = Qs+Qn, sigQs = sqrt(sig2Qs),
                                                      sigQn = sqrt(sig2Qn))
# alternative dataset with no constraints at all
load(paste0(input_derived_path,"fd_data_no_constr.Rdata"))
fd_data_no_constr <- fd_data_no_constr %>% mutate(Qtot = Qs+Qn, sigQs = sqrt(sig2Qs),
                                                  sigQn = sqrt(sig2Qn))
# check median of the standard deviations of the cluster sizes
fd_data_no_constr %>% summarize(med_sigQs = sd(sigQs), med_sigQn = sd(sigQn))

# ------------------------------------------------------------------------------
# Compare original to re-created processed data to confirm they are the same
# ------------------------------------------------------------------------------

# training data distribution of total consumption (also checked sigma Q's)
g<-ggplot()+
  geom_histogram(data=processed_data,aes(x=Qtot),bins = 40)+
  xlim(0,NA) +
  xlab("Qtot")+
  ggtitle("Distribution of total demand, processed_data")
plot(g)
# processed data (pre-existing); confirms it is same results as fd_data_orig
g<-ggplot()+
  geom_histogram(data=fd_data_orig,aes(x=Qtot),bins = 40)+
  xlim(0,NA) +
  xlab("Qtot")+
  ggtitle("Distribution of total demand, fd_data_orig")
plot(g)

# ------------------------------------------------------------------------------
# Compare total demand of any two datasets
# ------------------------------------------------------------------------------

# function for plotting two histograms of demand data for Qs, Qn, Qtot
make_demand_hist_plot <-
  function(obsdata1,label1,obsdata2,label2,title1,xmax,ymax,ratio) {
  tempdata <- list(data.frame(x=obsdata1$Qs,dataset="obsdata1",var="Qs"),
                   data.frame(x=obsdata1$Qn,dataset="obsdata1",var="Qn"),
                   data.frame(x=obsdata1$Qtot,dataset="obsdata1",var="Qtot"),
                   data.frame(x=obsdata2$Qs,dataset="obsdata2",var="Qs"),
                   data.frame(x=obsdata2$Qn,dataset="obsdata2",var="Qn"),
                   data.frame(x=obsdata2$Qtot,dataset="obsdata2",var="Qtot")) %>%
    bind_rows()
  ggplot() +
    geom_histogram(data=tempdata[tempdata$dataset=="obsdata1",],
                   aes(x=x,color=label1),bins = 40,fill="transparent")+
    geom_histogram(data=tempdata[tempdata$dataset=="obsdata2",],
                   aes(x=x,color=label2),bins = 40,fill="transparent")+
    facet_wrap(~var) +
    scale_color_manual('Dataset',values=c("red","black")) +
    labs(title = title1) +
    xlim(0,xmax) +
    ylim(0,ymax) +
    xlab("Demand") +
    ggtitle(title1) +
    coord_fixed(ratio)
  }

# plot original processed data vs re-processed data to confirm they are the same
make_demand_hist_plot(processed_data,"processed",
                      fd_data_orig,"re-processed",
                      "Distribution of demand, original processed vs re-processed",
                      5,800,5/800)
ggsave("demand_hist_processed_v_reprocessed.png",path=fig_path)

# plot original processed data vs raw data
make_demand_hist_plot(training_data,"raw",
                      fd_data_orig,"original",
                      "Distribution of demand, original processed vs raw",
                      5,800,5/800)
ggsave("demand_hist_orig_v_raw.png",path=fig_path)

# plot original processed data vs data with no min Qtot constraint
make_demand_hist_plot(fd_data_no_min_Qtot,"no min Qtot",
                      fd_data_orig,"original",
                      "Distribution of demand, original vs no min Qtot",
                      5,800,5/800)
ggsave("demand_hist_orig_v_no_min_Qtot.png",path=fig_path)

# plot original processed data vs data with no max Pn constraint
make_demand_hist_plot(fd_data_no_max_Pn,"no max Pn",
                      fd_data_orig,"original",
                      "Distribution of demand, original vs no max Pn",
                      5,800,5/800)
ggsave("demand_hist_orig_v_no_max_Pn.png",path=fig_path)

# ------------------------------------------------------------------------------
# Plot distributions of sigma Qs and sigma Qn
# ------------------------------------------------------------------------------

# function for plotting two histograms of sigma values data for Qs, Qn
make_sigma_hist_plot <-
  function(obsdata1,label1,obsdata2,label2,title1,xmax,ymax,ratio) {
    tempdata <- list(data.frame(x=obsdata1$sigQs,dataset="obsdata1",var="Qs"),
                     data.frame(x=obsdata1$sigQn,dataset="obsdata1",var="Qn"),
                     data.frame(x=obsdata2$sigQs,dataset="obsdata2",var="Qs"),
                     data.frame(x=obsdata2$sigQn,dataset="obsdata2",var="Qn")) %>%
      bind_rows()
    ggplot() +
      geom_histogram(data=tempdata[tempdata$dataset=="obsdata1",],
                     aes(x=x,color=label1),bins = 40,fill="transparent")+
      geom_histogram(data=tempdata[tempdata$dataset=="obsdata2",],
                     aes(x=x,color=label2),bins = 40,fill="transparent")+
      facet_wrap(~var) +
      scale_color_manual('Dataset',values=c("red","black")) +
      labs(title = title1) +
      xlim(0,xmax) +
      ylim(0,ymax) +
      xlab("Sigma Q (thous calories)") +
      ggtitle(title1) +
      coord_fixed(ratio)
  }

# plot original processed data vs re-processed data to confirm they are the same
make_sigma_hist_plot(processed_data,"processed",
                      fd_data_orig,"re-processed",
                      "Distribution of sigma, original processed vs re-processed",
                      1,350,1/350)
ggsave("demand_sigma_hist_processed_v_reprocessed.png",path=fig_path)

# **************************** to run when raw data is processed **************
# plot original processed data vs raw data
make_sigma_hist_plot(fd_data_no_constr,"raw",
                      fd_data_orig,"original",
                      "Distribution of sigma, original processed vs raw",
                      1,350,1/350)
ggsave("demand_sigma_hist_orig_v_raw.png",path=fig_path)

# plot original processed data vs data with no min Qtot constraint
make_sigma_hist_plot(fd_data_no_min_Qtot,"no min Qtot",
                      fd_data_orig,"original",
                      "Distribution of sigma, original vs no min Qtot",
                     1,350,1/350)
ggsave("demand_sigma_orig_v_no_min_Qtot.png",path=fig_path)

# plot original processed data vs data with no max Pn constraint
make_sigma_hist_plot(fd_data_no_max_Pn,"no max Pn",
                      fd_data_orig,"original",
                      "Distribution of sigma, original vs no max Pn",
                      1,350,1/350)
ggsave("demand_sigma_orig_v_no_max_Pn.png",path=fig_path)

# ------------------------------------------------------------------------------
# Analyze number and size of clusters in original processed data
# ------------------------------------------------------------------------------

# find number of clusters; 418 in original processed data based on clusterID
# see my meeting notes, the correct way to do this is with clusterID
get_cluster_num <- function(cluster_df) { length(unique(cluster_df$clusterID)) }
datalist <- list(fd_data_orig,fd_data_no_min_Qtot,fd_data_no_max_Pn)
cluster_nums <- lapply(datalist,get_cluster_num)
# find size of each cluster for each dataset
get_cluster_size <- function(cluster_df) {table(cluster_df$clusterID)}
cluster_size <- lapply(datalist,get_cluster_size)

lapply(cluster_size,min)

tmp <- as.name("fd_data_orig")
tmp[1]

table_data <- data.frame(
  data=datalist,
  number=cluster_nums)

# clusters larger than 50
cluster_size %>% filter(.>50)

# distributions of cluster sizes smaller than 50
# staples
g<-ggplot()+
  geom_histogram(data=cluster_size,aes(x=Freq),bins = 40,fill="transparent",
                 color="black")+
  xlim(0,50) +
  xlab("Cluster size")+
  ggtitle("Distribution of cluster size, processed_data")
plot(g)
ggsave("cluster_size_processed_data.png",path=fig_path)

# ------------------------------------------------------------------------------
# Compare original processed data to data without minimum calorie constraint
# ------------------------------------------------------------------------------

# function for plotting three scatter plots, two datasets as fn of income,
# for Qs, Qn, Qtot
make_obsvinc_plot <- function(obsdata1,obsdata2,title1,xmax,ymax,ratio) {
  tempdata <- list(data.frame(x=obsdata1$Y,y=obsdata1$Qs,dataset="obsdata1",var="Qs"),
       data.frame(x=obsdata1$Y,y=obsdata1$Qn,dataset="obsdata1",var="Qn"),
       data.frame(x=obsdata1$Y,y=obsdata1$Qtot,dataset="obsdata1",var="Qtot"),
       data.frame(x=obsdata2$Y,y=obsdata2$Qs,dataset="obsdata2",var="Qs"),
       data.frame(x=obsdata2$Y,y=obsdata2$Qn,dataset="obsdata2",var="Qn"),
       data.frame(x=obsdata2$Y,y=obsdata2$Qtot,dataset="obsdata2",var="Qtot")) %>%
    bind_rows()
  ggplot() +
    geom_point(data=tempdata[tempdata$dataset=="obsdata2",],aes(x,y),
               color="orange") +
    geom_point(data=tempdata[tempdata$dataset=="obsdata1",],aes(x,y)) +
    facet_wrap(~var) +
    labs(title = title1) +
    xlim(0,xmax) +
    ylim(0,ymax) +
    xlab("Income") +
    ylab("Demand") +
    coord_fixed(ratio)
}

# plot full range of data
g <- make_obsvinc_plot(
  processed_data,fd_data_no_min_Qtot,
  "Observed demand v income, processed_data and fd_data_no_min_Qtot",
  120,4,120/4)
plot(g)
ggsave("scatter_demand-income_processed_v_no_min_Qtot.png",path=fig_path)

# focus on low income range
g <- make_obsvinc_plot(
  processed_data,fd_data_no_min_Qtot,
  "Observed demand v income, processed_data and fd_data_no_min_Qtot",
  5,4,5/4)
plot(g)
ggsave("scatter_demand-income_processed_v_no_min_Qtot_lowincome.png",path=fig_path)
