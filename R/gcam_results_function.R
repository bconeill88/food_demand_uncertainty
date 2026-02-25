# function for getting price, income, demand and bias data from a GCAM run and 
# saving the results; takes as input a data file extracted from GCAM output in 
# rgcam, along with scenarioname to use when saving results file (to a directory 
# within data/processed given by data_dir and gcam_dir); also requires income shares by 
# region, decile, and time step, because per cap income by decile in GCAM output is 
# incorrect, so it needs to be calculated from regional per cap income and income 
# shares; it is calculated in the function as follows: we want to calculate Ypc,dec 
# from Ypc,reg, Pdec, and inc_share_dec, so we start with
# Ypc,dec = Ydec / Pdec 
# Ypc,dec = Yreg * inc_share_dec / Pdec
# Ypc,dec = Ypc,reg * Preg * inc_share_dec / Pdec
# Ypc,dec = Ypc,reg * Pdec * 10 * inc_share_dec / Pdec
# Ypc,dec = Ypc,reg * 10 * inc_share_dec
# which is pretty obvious in the first place!
get_GCAM_results <- function(proj_data, inc_shares, scen, data_dir, gcam_dir) {
  
  # account for differences in rgcam output file structures
  if(length(proj_data) == 1) proj_data <- proj_data[[1]]
  
  # get data
  prices <- proj_data[['food demand prices']] %>%
    select(-c(nodeinput, Units, scenario)) %>%
    spread(key=input,value=value) %>%
    rename(Ps = FoodDemand_Staples,Pn = FoodDemand_NonStaples) %>%
    merge(GCAM_region_ID_mapping) %>%
    relocate(GCAM_region_ID)
  
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
  
  # combine into a single df and add variables; use inner_join just in case there
  # are some rows not in common across data types (leave them out)
  data_list <- list(prices,income,demand,reg_bias)
  gcamoutput <- data_list %>% 
    reduce(inner_join,by=c("region","gcam-consumer","year")) %>%
    # add material consumption
    mutate(Qm = Y - Qs - Qn) %>%
    # add consumption shares
    mutate(alpha.s = Qs/Y, alpha.n = Qn/Y, alpha.m = Qm/Y, alpha.t = alpha.s + alpha.n) %>%
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