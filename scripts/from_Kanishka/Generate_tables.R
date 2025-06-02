library(dplyr)
library(tidyr)
library(rgcam)
library(ggplot2)
library(data.table)




#Add constants for scheme
scheme_basic <- theme_bw() +
  theme(legend.text = element_text(size = 15)) +
  theme(legend.title = element_text(size = 15)) +
  theme(axis.text = element_text(size = 18)) +
  theme(axis.title = element_text(size = 18, face = "bold")) +
  theme(plot.title = element_text(size = 15, face = "bold", vjust = 1)) +
  theme(plot.subtitle = element_text(size = 9, face = "bold", vjust = 1))+ 
  theme(strip.text = element_text(size = 6))+
  theme(strip.text.x = element_text(size = 12, face = "bold"))+
  theme(strip.text.y = element_text(size = 15, face = "bold"))+
  theme(legend.position = "right")+
  theme(legend.text = element_text(size = 12))+
  theme(legend.title = element_text(size = 12,color = "black",face="bold"))+
  theme(axis.text.x= element_text(angle = 90,hjust=1))+
  theme(legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))


hdr_path <-"/qfs/people/nara836/food_demand_mult_cons/food_demand_Waldhoffetal2024/Rgcam/scripts/"

#2. Path to databses
db_path<-"/qfs/people/nara836/food_demand_mult_cons/food_demand_Waldhoffetal2024/gcam-core/output"

#3.Project name and date 
project_name<-"BETO jet fuels"
date <- date()

#source( paste0(hdr_path,"diag_header.R"))	# configuration and helper functions
#source( paste0(hdr_path,"diag_grapher.R"))  # functions to generate figures
#source( paste0(hdr_path,"color_schemes.R" )) # some predefined color schemes
#source( paste0(hdr_path,"diag_util_functions.R")) # library of useful utility functions

args <- commandArgs(trailingOnly = TRUE)
job_name <- args[1]


sce_names <- c(job_name)


for(i in sce_names){
  
  assign(paste0("tables_",i),loadProject(paste0("tables_",i,".proj")))
  
}

QUERY_BATCH_FILE <- paste0("/qfs/people/nara836/food_demand_mult_cons/food_demand_Waldhoffetal2024/Rgcam/Verification_queries/Model_verification_queries_food_demand.xml")


for ( i in sce_names){
  print(paste0("Processing: ", i))
  branch_db_conn<-localDBConn(paste0(db_path),paste0("database_basexdbBase",i), validatedb = FALSE, migabble = FALSE)
  
  assign(paste0("tables_",i),addScenario(branch_db_conn, paste0("tables_",i,".proj"), "Reference", QUERY_BATCH_FILE))
  
  #For some reason, the paths create problems so, let's save the project files again
  saveProject(get(paste0("tables_",i)),paste0("tables_",i,".proj"))
  
  
}







