# Function for getting price, income, demand and bias data from a GCAM run and 
# saving the results; takes as input a data file extracted from GCAM output in 
# rgcam, along with scenarioname to use when saving results file (to a directory 
# within data/processed given by data_dir and gcam_dir); also requires income shares by 
# region, decile, and time step, because per cap income by decile in GCAM output is 
# incorrect, so it needs to be calculated from regional per cap income and income 
# shares; it is calculated in the function as: 
# Ypc,dec = Ypc,reg * 10 * inc_share_dec
get_GCAM_results <- function(proj_data, inc_shares, scen, data_dir, gcam_dir) {
  
  # account for differences in rgcam output file structures
  if(length(proj_data) == 1) proj_data <- proj_data[[1]]
  
  # get data
  prices <- proj_data[['food demand prices']] %>%
    select(-c(nodeinput, Units, scenario)) %>%
    spread(key=input,value=value) %>%
    rename(Ps = FoodDemand_Staples,Pn = FoodDemand_NonStaples)
  
  # version if per cap income by decile from GCAM output were correct
  # income <- proj_data[['subregional income']] %>%
  #   select(-Units) %>%
  #   rename(Y = value)
  
  # alternative: calculate per cap income by decile from other variables
  income_reg <- proj_data[['GDP per capita PPP by region']] %>%
    select(-c(Units, account, scenario)) %>%
    rename(Y.region = value)
  pop <- proj_data[['subregional population']] %>%
    select(-c(Units, scenario)) %>%
    rename(Pop = value)
  # merge regional income, decile pop, and decile income shares into one df
  # use inner_join to only include rows in common; drops Taiwan data from inc_share_data
  income <- inner_join(pop,inc_shares,join_by(region, `gcam-consumer`, year)) %>%
    # use left_join so that regional income is repeated for each decile in a given
    # region and year
    left_join(income_reg, by = c("region", "year")) %>%
    # calculate per cap income by decile in k$/cap/yr
    mutate(Y = Y.region*10*income.share)
  
  demand <- proj_data[['food demand per capita']] %>%
    select(-c(Units, scenario, nodeinput)) %>%
    spread(key=input,value=value) %>%
    rename(Qs = FoodDemand_Staples,Qn = FoodDemand_NonStaples) %>%
    mutate(Qtot = Qs + Qn)
  
  reg_bias <- proj_data[['food demand regional bias']] %>%
    select(-c(Units, scenario, nodeinput)) %>%
    spread(key=input,value=value) %>%
    rename(RBs = FoodDemand_Staples,RBn = FoodDemand_NonStaples)
  
  cropland <- proj_data[['Aggregated Land Allocation']] %>%
    filter(`land-allocation` == "crops") %>%
    rename(cropland = value) %>%
    select(region, year, cropland)
  
  withdrawals <- proj_data[['water withdrawals by region']] %>%
    rename(withdrawals = value) %>%
    select(region, year, withdrawals)
  
  biomass <- proj_data[['purpose-grown biomass production']] %>%
    rename(bio_production = value) %>%
    select(region, year, bio_production)
  
  pasture <- proj_data[['Aggregated Land Allocation']] %>%
    filter(`land-allocation` == "pasture (grazed)") %>%
    rename(pasture = value) %>%
    select(region, year, pasture)
  
  forest <- proj_data[['Aggregated Land Allocation']] %>%
    filter(`land-allocation` == "Softwood_Forest" | 
             `land-allocation` == "Hardwood_Forest") %>%
    spread(key = `land-allocation`, value = value) %>%
    mutate(forest = Hardwood_Forest + Softwood_Forest) %>%
    select(region, year, forest)
  
  luc_emis <- proj_data[['LUC emissions by region']] %>%
    group_by(region, year) %>%
    summarize(emissions = sum(value)) %>%
    ungroup()
  
  # combine into a single df and add variables
  
  # Merge separate groups since they have different variables to merge on
  group1 <- list(prices, income, demand) %>% 
    reduce(full_join, by = c("region", "gcam-consumer", "year"))
  group2 <- list(cropland, withdrawals, biomass, pasture, forest, luc_emis) %>% 
    reduce(full_join, by = c("region", "year"))
  
  # Combine and continue; gcam-consumer will be NA for second group
  gcamoutput <- full_join(group1, group2, by = c("region", "year")) %>%
    merge(GCAM_region_ID_mapping) %>%
    relocate(GCAM_region_ID) %>%
    # add consumption shares
    mutate(alpha.s = Ps*Qs/Y, alpha.n = Pn*Qn/Y, alpha.m = 1 - alpha.s - alpha.n,
           alpha.t = alpha.s + alpha.n) %>%
    # add regional demand per cap
    group_by(region,year) %>%
    mutate(Qs.region = if_else(
      any(is.na(Qs) | is.na(Pop)),  # check if any NA exists
      NA_real_,  # return NA if any input is NA
      sum(Qs * Pop) / sum(Pop)  # otherwise, compute weighted mean
    )) %>%
    mutate(Qn.region = if_else(
      any(is.na(Qn) | is.na(Pop)),  # check if any NA exists
      NA_real_,  # return NA if any input is NA
      sum(Qn * Pop) / sum(Pop)  # otherwise, compute weighted mean
    )) %>%
    ungroup() %>%
    mutate(Qtot.region = Qs.region + Qn.region)
  
  # if desired, check results for NAs
  # message("NAs for ", scen, ":\n",
  #         paste(names(gcamoutput), colSums(is.na(gcamoutput)), sep = ": ", 
  #               collapse = "  "))

  # save results
  saveRDS(gcamoutput, 
          file.path("data", "processed", data_dir, gcam_dir, 
                    paste0("/gcamoutput_", scen, ".RDS")))
}