
#Assuming you have a df called "new_base_service" where the base.service is basically the (ambrosia demand + bias) in Pcal/year.  
new_base_service%>%
  left_join(population_income_groups%>%select(-subregional.population.share)%>%mutate(gcam.consumer = gsub("d", "FoodDemand_Group", gcam.consumer)), by = c("region", "year","gcam.consumer")) %>%
  #Min value for staples is 600 calories and min non-staple calories being 10 calories. 
  #totalPop is the total population in a decile. 
  mutate(min_value= ifelse(input=="FoodDemand_Staples",0.6*365*totalPop*1e-9*1000,0.01*365*totalPop*1e-9*1000),
         diff=base.service-min_value,
         diff=ifelse(diff >0,0,diff))->new_base_service_test

#Identify neg values
new_base_service_test%>% filter(diff <0)->neg_values

while (nrow(neg_values)>0){
  print("Seeing values for deciles below thresholds. Going to try re-allocation")
  
  
  
  
  
  new_base_service_test%>% 
    #If a decile has more than 10% of required minimum, its a good candidate for deduction
    mutate(candidate=ifelse(base.service>min_value*1.1,1,0))%>%
    group_by(region, year, input)%>%
    #Sum up the available candidates and the diff
    mutate(candidate=sum(candidate),
           diff= sum(diff))%>%
    ungroup()%>%  
    mutate(base.service=ifelse(base.service < min_value,base.service+(min_value- base.service),ifelse(base.service>min_value*1.1,base.service+(diff/candidate),base.service)))%>%
    mutate(diff=base.service-min_value,
           diff=ifelse(diff >0,0,diff))->new_base_service_test
  
  #Re-Identify neg values
  new_base_service_test%>% filter(diff <0)->neg_values 
  
  
}

#Test write the outputs. Mean calories should be same in old and new. Else we will get calibration errors in GCAM!
write.csv(new_base_service_test, "new_data.csv")
write.csv(new_base_service, "old_data.csv")

#Replace old with the new
new_base_service <- new_base_service_test 