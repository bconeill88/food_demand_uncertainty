
# function to calculate food demand (using food.dmnd() from ambrosia) without fixed effects
# given a dataframe of price and income data (possibly including year and decile
# information if the price and income paths are from GCAM), and a single row of a 
# parameter data file (one iteration); returns a dataframe combining price/income
# data, year/decile information (if present), parameters (unless this is commented
# out, which is the default case), demand (including Qtot), both income elasticities,
# four price elasticities, and log likelihood of the sample
food.dmnd.plus <- function(princdata,globalparams) {
  
  # get parameter structure needed for food.dmnd() and calculate food demand
  param_structure <- vec2param(as.vector(t(select(globalparams,c('As':'pnscl')))))
  demand <- food.dmnd(princdata$Ps,princdata$Pn,princdata$Y,params = param_structure)
  # add total demand
  demand$Qtot <- demand$Qs + demand$Qn
  # calculate income elasticities and add to results
  inc_elast <- calc_income_elast(princdata$Y,param_structure)
  demand <- cbind(demand,inc_elast)
  # calculate price elasticities, needs budget shares and income elasticities in 'demand'
  price_elast <- calc_price_elast(demand,param_structure)
  demand <- cbind(demand,price_elast)
  # package price/income data, parameters, demand/elasticities, likelihood and return
  output <- cbind(Y=princdata$Y,Ps=princdata$Ps,Pn=princdata$Pn,
                  # comment this out to keep parameter values out of the output to save space
                  #                  do.call("rbind", replicate(nrow(princdata), paramdata, simplify = FALSE)),
                  demand,LL = globalparams[['LL']])
  # package year and decile information with output if present
  if("gcam-consumer" %in% colnames(princdata)) {
    output <- cbind(`gcam-consumer`=princdata$`gcam-consumer`,output)
  }
  if("year" %in% colnames(princdata)) {
    output <- cbind(year=princdata$year,output)
  }
  return(output)
}


# function for calculating regional food demand (without bias correction) and 
# elasticities with the FE model, for a set of one or more parameter samples, 
# a single set of price/income assumptions, and a set of one or more regions;
# function calls food.dmnd.plus() to calculate "global" food demand and elasticities,
# then adds fixed effects; it tests whether income and prices are uniform across
# regions, and if so global demand is calculated only once and used for all regions,
# if not it is recalculated for each region; 
# saves regional results to results sub-directory indicated by "scen"; if file
# names need a special extension, it is indicated by the "case" argument; if 
# scen = "Obs" (calculating demand based on observed prices/income), results for
# the single region calculated are returned, not saved
food.dmnd.plus.FE.regions <- function(paramdata,FEdata,princdata,scen,case,regions) {
  
  print(paste0("  calculating demand for scenario ",scen))
  
  # set flag for whether income and price scenario is globally uniform or not
  
  # no regional information in price/income input file
  if(!any(c("region", "GCAM_region_ID") %in% colnames(princdata))) {
    
    # set flag
    globalprices <- TRUE
    
    # must be regional information, test whether it is uniform or not
  } else {
    
    # find how many unique values of income and prices there are across regions
    check_regions <- princdata %>%
      # group data for all regions together for each year and decile
      # this will include all iterations if there are more than one
      group_by(year, `gcam-consumer`) %>%
      # find unique values of three variables in each group; would have used
      # reframe(), but unix version of dplyr is too old
      summarise(Y = list(unique(Y)), Ps = list(unique(Ps)), Pn = list(unique(Pn)),
                .groups = "drop") %>%
      # find max number of unique values across three variables, for each row
      mutate(n = pmax(lengths(Y), lengths(Ps), lengths(Pn))) %>%
      # Keep only rows with more than one unique value for at least one variable
      filter(n > 1)
    
    # set flag
    globalprices <- nrow(check_regions) == 0
  }
  
  # loop over regions
  for (r in 1:length(regions)) {
    
    print(paste0("    starting region ",regions[r]))
    
    # calculate global demand
    
    # if prices /income are uniform across regions, only calculate global demand once
    if (globalprices & r == 1) {
      
      # calculate demand for all parameter iterations
      demand_glob <- lapply(seq_len(nrow(paramdata)),function(z) {
        
        # for each iteration, calculate demand for the price/income scenario;
        # this assumes princdata includes one time series for use by all regions
        princdata %>%
          food.dmnd.plus(paramdata[z,]) %>%
          mutate(iteration = as.numeric(paramdata[z,'iteration']))
      }) %>%
        bind_rows()
      
      # if prices or income vary across regions, calculate global demand for each region  
    } else if (!globalprices) {
      
      # extract regional income/price scenario
      princdata_region <- princdata %>%
        # ensure matching data types
        filter(as.numeric(GCAM_region_ID) == as.numeric(regions[r]))
      if (nrow(princdata_region) == 0) stop(paste("No princdata for region ", regions[r]))
      
      # calculate demand for all parameter iterations
      demand_glob <- lapply(seq_len(nrow(paramdata)),function(z) {
        
        # keep track of progress
        if(z %% 500 == 0) print(paste0("    reached iteration ",z,
                                       " for calculating global demand"))
        
        # for each iteration, calculate demand for the price/income scenario for
        # this region
        princdata_region %>%
          food.dmnd.plus(paramdata[z,]) %>%
          mutate(iteration = as.numeric(paramdata[z,'iteration']))
      }) %>%
        bind_rows()
    }
    
    #    print(paste0("      finished global demand for region ",regions[r]))
    
    # calculate regional demand
    
    # group global demand/elasticities results by iteration
    demand_glob_byiter <- split(demand_glob, demand_glob$iteration)
    
    # get FE data for the region and group by iteration
    FEdata_reg <- FEdata[FEdata$GCAM_region_ID == regions[r],]
    FEdata_byiter <- split(FEdata_reg, FEdata_reg$iteration)
    
    # initialize list to hold regional results for each iteration
    demand_reg <- demand_glob_byiter
    
    # loop over iterations (should be able to do this with mapply)
    for(j in 1:length(demand_glob_byiter)) {
      
      # get iteration number of j'th element of global demand results
      iteration <- as.numeric(names(demand_glob_byiter)[[j]])
      
      # Retrieve corresponding FEdata for the iteration
      FEdata_iter <- FEdata_byiter[[as.character(iteration)]]
      
      # Ensure FEdata_iter is not NULL or missing
      if (is.null(FEdata_iter) || nrow(FEdata_iter) == 0)
        stop(paste("Missing FE data for iteration ", iteration))
      
      # add regional fixed effect to global demand, and region name/ID
      demand_reg[[j]] <- demand_glob_byiter[[j]] %>%
        mutate(Qs = Qs + FEdata_iter$staples_FE,
               Qtot = Qs + Qn,
               GCAM_region_ID = unique(FEdata_iter$GCAM_region_ID),
               region = unique(FEdata_iter$region)) %>%
        # update budget shares
        mutate(alpha.s = Qs*Ps/Y, alpha.n = Qn*Pn/Y) %>%
        mutate(alpha.m = 1-alpha.s-alpha.n)
      
      # update price elasticities based on updated budget shares
      param_structure <- vec2param(as.vector(t(select(paramdata[j,],c('As':'pnscl')))))
      price_elast <- calc_price_elast(demand_reg[[j]],param_structure)
      demand_reg[[j]] <- mutate(demand_reg[[j]],
                                elast.ss = price_elast$elast.ss,
                                elast.nn = price_elast$elast.nn,
                                elast.sn = price_elast$elast.sn,
                                elast.ns = price_elast$elast.ns)
    }
    # recombine over iterations
    demand_reg <- bind_rows(demand_reg)
    
    #    print(paste0("      finished adding fixed effect to demand for region ",regions[r]))
    
    # if demand is calculated based on observations, return results since
    # the call to this function will be for only one region and results saved
    # in other code
    if(scen=="Obs") {
      return(demand_reg)
      # in all other cases save regional demand to results directory
    } else {
      pathname <- paste("data/processed",data_dir,scen,"results",sep="/")
      if(case == "") separator <- "" else separator <- "_"
      save(demand_reg,file = paste0(pathname,"/demand_R",regions[r],
                                    separator,case,".RData"))
    }
  } # end region loop
}

# function to bias correct ambrosia per capita demand results based on 
# corresponding GCAM simulation, enforcing non-negative demand and cap on total
# food budget share; non-negative constraint is enforced both before bias-
# correction (to match GCAM approach) and after (to avoid inducing negative 
# demand); cap on food share is achieved by reducing non-staples consumption first,
# since it will normally be a less efficient source of calories than staples;
# note ambrosia enforces this cap for un-bias-corrected demand already, but GCAM
# does it post-bias-correction so need to add it here for ambrosia as well
bias_correct_ambrosia_demand <- function(scen,case,regions,max_alphat) {
  
  # get gcam results and store in clearer variable name
  if(scen == "Ref_ML_gcam2") {
    gcam_file <- paste0("incpricedem_Ref_ML_gcam.RData")
  } else if(scen == "Ref_HD_gcam2") {
    gcam_file <- paste0("incpricedem_Ref_HD_gcam.RData")
  } else {
    gcam_file <- paste0("incpricedem_",scen,".RData")
  }
  load(paste("data/processed",data_dir,gcam_results_dir,gcam_file,sep="/"))
  incpricedem_gcam <- incpricedem
  
  for(r in regions) {
    
    # get ambrosia results and store in clearer variable name
    separator <- ifelse(case == "", "", "_")
    ambrosia_file <- paste0("demand_R",r,separator,case,".RData")
    load(paste("data/processed",data_dir,scen,"results",ambrosia_file,sep="/"))
    demand_reg_amb <- demand_reg
    
    # combine gcam and ambrosia dataframes, bias correct, enforce non-negative
    # demand, enforce cap on food budget share, and keep only needed columns
    keepcols <- c(colnames(demand_reg_amb),"RBs","RBn","alpha.t")
    demand_reg_amb <- 
      inner_join(demand_reg_amb,incpricedem_gcam,
                 by = c("year","gcam-consumer","GCAM_region_ID","region"),
                 # no extension for ambrosia columns so they keep original names
                 suffix = c("",".gcam")) %>%
      # replace negative demand with zero before bias correction, update shares
      mutate(Qs = pmax(Qs, 0), Qn = pmax(Qn, 0), Qtot = Qs + Qn) %>%
      mutate(alpha.s = Qs / Y, alpha.n = Qn / Y, alpha.m = 1 - alpha.s - alpha.n) %>%
      # bias correct ambrosia demand
      mutate(Qs = Qs + RBs,Qn = Qn + RBn,Qtot = Qs + Qn) %>%
      # replace negative demand with zero after bias correction, update shares
      mutate(Qs = pmax(Qs, 0), Qn = pmax(Qn, 0), Qtot = Qs + Qn) %>%
      mutate(alpha.s = Qs / Y, alpha.n = Qn / Y, alpha.m = 1 - alpha.s - alpha.n) %>%
      # # Compute total food budget share
      mutate(alpha.t = alpha.s + alpha.n) %>%
      # adjust consumption levels if they violate the food budget constraint
      mutate(
        alpha.n = case_when(
          alpha.t <= max_alphat ~ alpha.n,  # No change if within limit
          alpha.n >= (alpha.t - max_alphat) ~ alpha.n - (alpha.t - max_alphat),  # Reduce alpha.n first
          TRUE ~ 0  # If alpha.n isn't enough, set it to zero
        ),
        alpha.s = case_when(
          alpha.t <= max_alphat ~ alpha.s,  # No change if within limit
          alpha.n == 0 ~ max_alphat,  # Reduce alpha.s only if alpha.n is already zero
          TRUE ~ alpha.s  # Otherwise, keep original value
        ),
        alpha.t = alpha.s + alpha.n  # Recalculate total budget share
      ) %>%
      # Recalculate demand based on adjusted budget shares
      mutate(Qs = alpha.s * Y, Qn = alpha.n * Y, Qtot = Qs + Qn) %>%
      select(keepcols)  # Keep relevant columns
    
    # save bias-corrected results, with _bc added to file name
    save(demand_reg_amb,file = paste("data/processed",data_dir,scen,"results",
                                     str_replace(ambrosia_file,".RData","_bc.RData"),
                                     sep="/"))
  }
}
