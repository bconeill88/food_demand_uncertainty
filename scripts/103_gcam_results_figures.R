
# need to add code to read in data, etc.
# this is for plotting results of GCAM scenario outcomes




# Plot demand uncertainty for single region, decile ----------------------------
# ----

# function for making demand plot for Qs, Qn, Qtot for three datasets (e.g., ML,
# HI, LI parameters) for a single region
make_demand3_plot <- function(df1,df2,df3,title1text,title2text) {
  thickness <- 1
  colors <- c("Staples" = "green","Non-staples" = "red","Total" = "blue")
  lines <- c("ML" = "solid","HD" = "dotted","LD" = "dashed")
  g <- ggplot()+
    geom_line(data=df1,aes(x= year,y= Qs,color= "Staples",linetype = "ML"),
              linewidth = thickness)+
    geom_line(data=df1,aes(x= year,y= Qn,color= "Non-staples",linetype = "ML"),
              linewidth = thickness)+
    geom_line(data=df1,aes(x= year,y= Qtot,color= "Total",linetype = "ML"),
              linewidth = thickness)+
    
    geom_line(data=df2,aes(x= year,y= Qs,color= "Staples",linetype ="HD"),
              linewidth = thickness)+
    geom_line(data=df2,aes(x= year,y= Qn,color= "Non-staples",linetype ="HD"),
              linewidth = thickness)+
    geom_line(data=df2,aes(x= year,y= Qtot,color= "Total",linetype ="HD"),
              linewidth = thickness)+
    
    geom_line(data=df3,aes(x= year,y= Qs,color= "Staples",linetype ="LD"),
              linewidth = thickness)+
    geom_line(data=df3,aes(x= year,y= Qn,color= "Non-staples",linetype ="LD"),
              linewidth = thickness)+
    geom_line(data=df3,aes(x= year,y= Qtot,color= "Total",linetype ="LD"),
              linewidth = thickness)+
    
    scale_colour_manual(name = "Demand", values = colors) +
    scale_linetype_manual(name = element_blank(), values = lines) +
    xlim(2015,2100) +
    ylim(0,5) +
    labs(title = title1text,subtitle = title2text) +
    xlab("Year" )+
    ylab("Calories (thousand/cap/day)") +
    coord_fixed(75/5)
}

# plot demand uncertainty for d1, Africa_Eastern
fdpc_d1_AfrE_ML <- prj_ML[['Reference']][['food demand per capita']] %>%
  filter(region == "Africa_Eastern",`gcam-consumer` == "FoodDemand_Group1") %>%
  spread(key=input,value=value) %>%
  rename(Qs="FoodDemand_Staples",Qn="FoodDemand_NonStaples") %>%
  mutate(Qtot=Qs+Qn)
fdpc_d1_AfrE_HD <- prj_HD[['Reference']][['food demand per capita']] %>%
  filter(region == "Africa_Eastern",`gcam-consumer` == "FoodDemand_Group1") %>%
  spread(key=input,value=value) %>%
  rename(Qs="FoodDemand_Staples",Qn="FoodDemand_NonStaples") %>%
  mutate(Qtot=Qs+Qn)
fdpc_d1_AfrE_LD <- prj_LD[['Reference']][['food demand per capita']] %>%
  filter(region == "Africa_Eastern",`gcam-consumer` == "FoodDemand_Group1") %>%
  spread(key=input,value=value) %>%
  rename(Qs="FoodDemand_Staples",Qn="FoodDemand_NonStaples") %>%
  mutate(Qtot=Qs+Qn)

make_demand3_plot(fdpc_d1_AfrE_ML,fdpc_d1_AfrE_HD,fdpc_d1_AfrE_LD,
                  "Food demand per capita","Africa Eastern, decile 1") %>% plot()

# plot demand uncertainty for d10, Africa_Eastern
fdpc_d10_AfrE_ML <- prj_ML[['Reference']][['food demand per capita']] %>%
  filter(region == "Africa_Eastern",`gcam-consumer` == "FoodDemand_Group10") %>%
  spread(key=input,value=value) %>%
  rename(Qs="FoodDemand_Staples",Qn="FoodDemand_NonStaples") %>%
  mutate(Qtot=Qs+Qn)
fdpc_d10_AfrE_HD <- prj_HD[['Reference']][['food demand per capita']] %>%
  filter(region == "Africa_Eastern",`gcam-consumer` == "FoodDemand_Group10") %>%
  spread(key=input,value=value) %>%
  rename(Qs="FoodDemand_Staples",Qn="FoodDemand_NonStaples") %>%
  mutate(Qtot=Qs+Qn)
fdpc_d10_AfrE_LD <- prj_LD[['Reference']][['food demand per capita']] %>%
  filter(region == "Africa_Eastern",`gcam-consumer` == "FoodDemand_Group10") %>%
  spread(key=input,value=value) %>%
  rename(Qs="FoodDemand_Staples",Qn="FoodDemand_NonStaples") %>%
  mutate(Qtot=Qs+Qn)

make_demand3_plot(fdpc_d10_AfrE_ML,fdpc_d10_AfrE_HD,fdpc_d10_AfrE_LD,
                  "Food demand per capita","Africa Eastern, decile 10") %>% plot()

# plot demand uncertainty for d1, USA
fdpc_d1_USA_ML <- prj_ML[['Reference']][['food demand per capita']] %>%
  filter(region == "USA",`gcam-consumer` == "FoodDemand_Group1") %>%
  spread(key=input,value=value) %>%
  rename(Qs="FoodDemand_Staples",Qn="FoodDemand_NonStaples") %>%
  mutate(Qtot=Qs+Qn)
fdpc_d1_USA_HD <- prj_HD[['Reference']][['food demand per capita']] %>%
  filter(region == "USA",`gcam-consumer` == "FoodDemand_Group1") %>%
  spread(key=input,value=value) %>%
  rename(Qs="FoodDemand_Staples",Qn="FoodDemand_NonStaples") %>%
  mutate(Qtot=Qs+Qn)
fdpc_d1_USA_LD <- prj_LD[['Reference']][['food demand per capita']] %>%
  filter(region == "USA",`gcam-consumer` == "FoodDemand_Group1") %>%
  spread(key=input,value=value) %>%
  rename(Qs="FoodDemand_Staples",Qn="FoodDemand_NonStaples") %>%
  mutate(Qtot=Qs+Qn)

make_demand3_plot(fdpc_d1_USA_ML,fdpc_d1_USA_HD,fdpc_d1_USA_LD,
                  "Food demand per capita","USA, decile 1") %>% plot()

# plot demand uncertainty for d10, USA
fdpc_d10_USA_ML <- prj_ML[['Reference']][['food demand per capita']] %>%
  filter(region == "USA",`gcam-consumer` == "FoodDemand_Group10") %>%
  spread(key=input,value=value) %>%
  rename(Qs="FoodDemand_Staples",Qn="FoodDemand_NonStaples") %>%
  mutate(Qtot=Qs+Qn)
fdpc_d10_USA_HD <- prj_HD[['Reference']][['food demand per capita']] %>%
  filter(region == "USA",`gcam-consumer` == "FoodDemand_Group10") %>%
  spread(key=input,value=value) %>%
  rename(Qs="FoodDemand_Staples",Qn="FoodDemand_NonStaples") %>%
  mutate(Qtot=Qs+Qn)
fdpc_d10_USA_LD <- prj_LD[['Reference']][['food demand per capita']] %>%
  filter(region == "USA",`gcam-consumer` == "FoodDemand_Group10") %>%
  spread(key=input,value=value) %>%
  rename(Qs="FoodDemand_Staples",Qn="FoodDemand_NonStaples") %>%
  mutate(Qtot=Qs+Qn)

make_demand3_plot(fdpc_d10_USA_ML,fdpc_d10_USA_HD,fdpc_d10_USA_LD,
                  "Food demand per capita","USA, decile 10") %>% plot()

# plot demand uncertainty for d1, China
fdpc_d1_China_ML <- prj_ML[['Reference']][['food demand per capita']] %>%
  filter(region == "China",`gcam-consumer` == "FoodDemand_Group1") %>%
  spread(key=input,value=value) %>%
  rename(Qs="FoodDemand_Staples",Qn="FoodDemand_NonStaples") %>%
  mutate(Qtot=Qs+Qn)
fdpc_d1_China_HD <- prj_HD[['Reference']][['food demand per capita']] %>%
  filter(region == "China",`gcam-consumer` == "FoodDemand_Group1") %>%
  spread(key=input,value=value) %>%
  rename(Qs="FoodDemand_Staples",Qn="FoodDemand_NonStaples") %>%
  mutate(Qtot=Qs+Qn)
fdpc_d1_China_LD <- prj_LD[['Reference']][['food demand per capita']] %>%
  filter(region == "China",`gcam-consumer` == "FoodDemand_Group1") %>%
  spread(key=input,value=value) %>%
  rename(Qs="FoodDemand_Staples",Qn="FoodDemand_NonStaples") %>%
  mutate(Qtot=Qs+Qn)

make_demand3_plot(fdpc_d1_China_ML,fdpc_d1_China_HD,fdpc_d1_China_LD,
                  "Food demand per capita","China, decile 1") %>% plot()
ggsave(file=paste0(fig_path,"/fdpc_d1_China_MLHDLD.png"),
       width = 6, height = 6, dpi = 150)

# plot demand uncertainty for d10, China
fdpc_d10_China_ML <- prj_ML[['Reference']][['food demand per capita']] %>%
  filter(region == "China",`gcam-consumer` == "FoodDemand_Group10") %>%
  spread(key=input,value=value) %>%
  rename(Qs="FoodDemand_Staples",Qn="FoodDemand_NonStaples") %>%
  mutate(Qtot=Qs+Qn)
fdpc_d10_China_HD <- prj_HD[['Reference']][['food demand per capita']] %>%
  filter(region == "China",`gcam-consumer` == "FoodDemand_Group10") %>%
  spread(key=input,value=value) %>%
  rename(Qs="FoodDemand_Staples",Qn="FoodDemand_NonStaples") %>%
  mutate(Qtot=Qs+Qn)
fdpc_d10_China_LD <- prj_LD[['Reference']][['food demand per capita']] %>%
  filter(region == "China",`gcam-consumer` == "FoodDemand_Group10") %>%
  spread(key=input,value=value) %>%
  rename(Qs="FoodDemand_Staples",Qn="FoodDemand_NonStaples") %>%
  mutate(Qtot=Qs+Qn)

make_demand3_plot(fdpc_d10_China_ML,fdpc_d10_China_HD,fdpc_d10_China_LD,
                  "Food demand per capita","China, decile 10") %>% plot()
ggsave(file=paste0(fig_path,"/fdpc_d10_China_MLHDLD.png"),
       width = 6, height = 6, dpi = 150)

# Plot demand comparisons between scenarios for single region, decile ----------
# ----

# function for making demand plot for Qs, Qn, Qtot for two datasets (e.g., Ref, 
# Ref-ML) for a single region
make_demand2_plot <- function(df1,df2,title1text,title2text) {
  thickness <- 1
  colors <- c("Staples" = "green","Non-staples" = "red","Total" = "blue")
  lines <- c("GCAM 7.1" = "solid","This analysis" = "dashed")
  g <- ggplot()+
    geom_line(data=df1,aes(x= year,y= Qs,color= "Staples",linetype = "GCAM 7.1"),
              linewidth = thickness)+
    geom_line(data=df1,aes(x= year,y= Qn,color= "Non-staples",linetype = "GCAM 7.1"),
              linewidth = thickness)+
    geom_line(data=df1,aes(x= year,y= Qtot,color= "Total",linetype = "GCAM 7.1"),
              linewidth = thickness)+
    
    geom_line(data=df2,aes(x= year,y= Qs,color= "Staples",linetype ="This analysis"),
              linewidth = thickness)+
    geom_line(data=df2,aes(x= year,y= Qn,color= "Non-staples",linetype ="This analysis"),
              linewidth = thickness)+
    geom_line(data=df2,aes(x= year,y= Qtot,color= "Total",linetype ="This analysis"),
              linewidth = thickness)+
    
    scale_colour_manual(name = "Demand", values = colors) +
    scale_linetype_manual(name = element_blank(), values = lines) +
    xlim(2015,2100) +
    ylim(0,5) +
    labs(title = title1text,subtitle = title2text) +
    xlab("Year" )+
    ylab("Calories (thousand/cap/day)") +
    coord_fixed(75/5)
}

#    Ref 2025 vs Ref 2021 ------------------------------------------------------
# ----

# plot demand comparison for d1, China
fdpc_d1_China_Ref <- prj_Ref[['Reference']][['food demand per capita']] %>%
  filter(region == "China",`gcam-consumer` == "FoodDemand_Group1") %>%
  spread(key=input,value=value) %>%
  rename(Qs="FoodDemand_Staples",Qn="FoodDemand_NonStaples") %>%
  mutate(Qtot=Qs+Qn)
fdpc_d1_China_ML <- prj_ML[['Reference']][['food demand per capita']] %>%
  filter(region == "China",`gcam-consumer` == "FoodDemand_Group1") %>%
  spread(key=input,value=value) %>%
  rename(Qs="FoodDemand_Staples",Qn="FoodDemand_NonStaples") %>%
  mutate(Qtot=Qs+Qn)

make_demand2_plot(fdpc_d1_China_Ref,fdpc_d1_China_ML,
                  "Food demand per capita","China, decile 1") %>% plot()
ggsave(file=paste0(fig_path,"/fdpc_d1_China_RefML.png"),
       width = 6, height = 6, dpi = 150)

# plot demand comparison for d10, China
fdpc_d10_China_Ref <- prj_Ref[['Reference']][['food demand per capita']] %>%
  filter(region == "China",`gcam-consumer` == "FoodDemand_Group10") %>%
  spread(key=input,value=value) %>%
  rename(Qs="FoodDemand_Staples",Qn="FoodDemand_NonStaples") %>%
  mutate(Qtot=Qs+Qn)
fdpc_d10_China_ML <- prj_ML[['Reference']][['food demand per capita']] %>%
  filter(region == "China",`gcam-consumer` == "FoodDemand_Group10") %>%
  spread(key=input,value=value) %>%
  rename(Qs="FoodDemand_Staples",Qn="FoodDemand_NonStaples") %>%
  mutate(Qtot=Qs+Qn)

make_demand2_plot(fdpc_d10_China_Ref,fdpc_d10_China_ML,
                  "Food demand per capita","China, decile 10") %>% plot()
ggsave(file=paste0(fig_path,"/fdpc_d10_China_RefML.png"),
       width = 6, height = 6, dpi = 150)

# plot demand comparison for d1, Africa_Eastern
fdpc_d1_AfrE_Ref <- prj_Ref[['Reference']][['food demand per capita']] %>%
  filter(region == "Africa_Eastern",`gcam-consumer` == "FoodDemand_Group1") %>%
  spread(key=input,value=value) %>%
  rename(Qs="FoodDemand_Staples",Qn="FoodDemand_NonStaples") %>%
  mutate(Qtot=Qs+Qn)
fdpc_d1_AfrE_ML <- prj_ML[['Reference']][['food demand per capita']] %>%
  filter(region == "Africa_Eastern",`gcam-consumer` == "FoodDemand_Group1") %>%
  spread(key=input,value=value) %>%
  rename(Qs="FoodDemand_Staples",Qn="FoodDemand_NonStaples") %>%
  mutate(Qtot=Qs+Qn)

make_demand2_plot(fdpc_d1_AfrE_Ref,fdpc_d1_AfrE_ML,
                  "Food demand per capita","Africa Eastern, decile 1") %>% plot()
ggsave(file=paste0(fig_path,"/fdpc_d1_AfrE_RefML.png"),
       width = 6, height = 6, dpi = 150)

# plot demand comparison for d10, Africa_Eastern
fdpc_d10_AfrE_Ref <- prj_Ref[['Reference']][['food demand per capita']] %>%
  filter(region == "Africa_Eastern",`gcam-consumer` == "FoodDemand_Group10") %>%
  spread(key=input,value=value) %>%
  rename(Qs="FoodDemand_Staples",Qn="FoodDemand_NonStaples") %>%
  mutate(Qtot=Qs+Qn)
fdpc_d10_AfrE_ML <- prj_ML[['Reference']][['food demand per capita']] %>%
  filter(region == "Africa_Eastern",`gcam-consumer` == "FoodDemand_Group10") %>%
  spread(key=input,value=value) %>%
  rename(Qs="FoodDemand_Staples",Qn="FoodDemand_NonStaples") %>%
  mutate(Qtot=Qs+Qn)

make_demand2_plot(fdpc_d10_AfrE_Ref,fdpc_d10_AfrE_ML,
                  "Food demand per capita","Africa Eastern, decile 10") %>% plot()
ggsave(file=paste0(fig_path,"/fdpc_d10_AfrE_RefML.png"),
       width = 6, height = 6, dpi = 150)

#    Ref 2025 vs High Price 2025 -----------------------------------------------
# ----

# plot demand comparison for d1, China
fdpc_d1_China_ML <- prj_ML[['Reference']][['food demand per capita']] %>%
  filter(region == "China",`gcam-consumer` == "FoodDemand_Group1") %>%
  spread(key=input,value=value) %>%
  rename(Qs="FoodDemand_Staples",Qn="FoodDemand_NonStaples") %>%
  mutate(Qtot=Qs+Qn)
fdpc_d1_China_MLHP <- prj_MLHP[['Reference']][['food demand per capita']] %>%
  filter(region == "China",`gcam-consumer` == "FoodDemand_Group1") %>%
  spread(key=input,value=value) %>%
  rename(Qs="FoodDemand_Staples",Qn="FoodDemand_NonStaples") %>%
  mutate(Qtot=Qs+Qn)

make_demand2_plot(fdpc_d1_China_ML,fdpc_d1_China_MLHP,
                  "Food demand per capita, Ref 2025 vs Ref 2025 HP, ML",
                  "China, decile 1") %>% plot()
ggsave(file=paste0(fig_path,"/fdpc_d1_China_RefML.png"),
       width = 6, height = 6, dpi = 150)

# plot demand comparison for d10, China
fdpc_d10_China_ML <- prj_ML[['Reference']][['food demand per capita']] %>%
  filter(region == "China",`gcam-consumer` == "FoodDemand_Group10") %>%
  spread(key=input,value=value) %>%
  rename(Qs="FoodDemand_Staples",Qn="FoodDemand_NonStaples") %>%
  mutate(Qtot=Qs+Qn)
fdpc_d10_China_MLHP <- prj_MLHP[['Reference']][['food demand per capita']] %>%
  filter(region == "China",`gcam-consumer` == "FoodDemand_Group10") %>%
  spread(key=input,value=value) %>%
  rename(Qs="FoodDemand_Staples",Qn="FoodDemand_NonStaples") %>%
  mutate(Qtot=Qs+Qn)

make_demand2_plot(fdpc_d10_China_ML,fdpc_d10_China_MLHP,
                  "Food demand per capita, Ref 2025 vs Ref 2025 HP, ML",
                  "China, decile 10") %>% plot()
ggsave(file=paste0(fig_path,"/fdpc_d10_China_ML_MLHP.png"),
       width = 6, height = 6, dpi = 150)


# Plot price comparisons across regions ----------------------------------------
# ----

# function for making price comparison plot for Ps, Pn for two datasets (e.g.,  
# Ref-ML, Ref-ML-High Price) for a single region
# make_price2_plot <- function(df1,df2,scen1,scen2,title1text,title2text) {
#   thickness <- 1
#   colors <- c("Staples" = "green","Non-staples" = "red")
# #  lines <- c(eval(parse(scen1)) = "solid",eval(parse(scen2)) = "dashed")
#   print(lines)
#   g <- ggplot()+
#     geom_line(data=df1,aes(x= year,y= Ps,color= "Staples",linetype = scen1),
#               linewidth = thickness)+
#     geom_line(data=df1,aes(x= year,y= Pn,color= "Non-staples",linetype = scen1),
#               linewidth = thickness)+
# 
#     geom_line(data=df2,aes(x= year,y= Ps,color= "Staples",linetype = scen2),
#               linewidth = thickness)+
#     geom_line(data=df2,aes(x= year,y= Pn,color= "Non-staples",linetype = scen2),
#               linewidth = thickness)+
# 
#     scale_colour_manual(name = "Price", values = colors) +
# #    scale_linetype_manual(name = element_blank(), values = lines) +
#     xlim(2015,2100) +
#     ylim(0,1) +
#     labs(title = title1text,subtitle = title2text) +
#     xlab("Year")+
#     ylab("Price (2005$/Mcal)") +
#     coord_fixed(75/1)
# }

# function for making price comparison plot for Ps, Pn for two datasets (e.g.,  
# Ref-ML, Ref-ML-High Price) for up to 16 regions
make_price2_plot <- function(df1,df2,scen1,scen2,title1text,title2text) {
  thickness <- 1
  colors <- c("Staples" = "green","Non-staples" = "red")
  #  lines <- c(eval(parse(scen1)) = "solid",eval(parse(scen2)) = "dashed")
  print(lines)
  g <- ggplot()+
    geom_line(data=df1,aes(x= year,y= Ps,color= "Staples",linetype = scen1),
              linewidth = thickness)+
    geom_line(data=df1,aes(x= year,y= Pn,color= "Non-staples",linetype = scen1),
              linewidth = thickness)+
    
    geom_line(data=df2,aes(x= year,y= Ps,color= "Staples",linetype = scen2),
              linewidth = thickness)+
    geom_line(data=df2,aes(x= year,y= Pn,color= "Non-staples",linetype = scen2),
              linewidth = thickness)+
    
    scale_colour_manual(name = "Price", values = colors) +
    facet_wrap(~region) +
    scale_linetype_manual(name = element_blank(), values = c("dashed","solid")) +
    xlim(2015,2100) +
    ylim(0,0.7) +
    labs(title = title1text,subtitle = title2text) +
    xlab("Year")+
    ylab("Price (2005$/Mcal)") +
    coord_fixed(75/0.9)
}

# plot first 16 regions
make_price2_plot(prices_RefML[prices_RefML$GCAM_region_ID <= 16,],
                 prices_MLHP[prices_MLHP$GCAM_region_ID <= 16,],
                 "Ref ML",
                 "ML High Price",
                 "Food prices, Ref 2025 vs High Price condition, ML",
                 "") %>% 
  plot()
ggsave(file=paste0(fig_path,"/prices_R1to16_ML_MLHP.png"),
       width = 8, height = 8, dpi = 150)
# plot next 16 regions
make_price2_plot(prices_RefML[prices_RefML$GCAM_region_ID > 16,],
                 prices_MLHP[prices_MLHP$GCAM_region_ID > 16,],
                 "Ref ML",
                 "ML High Price",
                 "Food prices, Ref 2025 vs High Price condition, ML",
                 "") %>% 
  plot()
ggsave(file=paste0(fig_path,"/prices_R17to32_ML_MLHP.png"),
       width = 8, height = 8, dpi = 150)