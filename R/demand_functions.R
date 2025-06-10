# functions for calculating and saving demand and elasticities with ambrosia

# function to calculate income elasticities for staples and non-staples from
# a list of incomes and a parameter structure (result of a call to vec2param())
calc_income_elast <- function(income,param_structure) {
  
  #calculate all income elasticities
  eta.s <- param_structure$yfunc[[1]](Y=income,FALSE)
  eta.n <- param_structure$yfunc[[2]](Y=income,FALSE)
  # create and return results
  inc_elast.df <- data.frame(eta.s,eta.n)
}

# function to calculate price elasticities from a food_demand dataframe and 
# parameter structure (result of a call to vec2param()). income elasticities 
# must already have been calculated and be part of food_demand
calc_price_elast <- function(food_demand,param_structure) {
  
  #calculate all price elasticities
  elasticities <- calc1eps(food_demand$alpha.s, food_demand$alpha.n,
                           food_demand$eta.s, food_demand$eta.n,
                           param_structure$xi)
  # extract the different elasticities; each one follows another
  len <- nrow(food_demand)
  elast.ss <- elasticities[1:len]
  elast.ns <- elasticities[(len+1):(2*len)]
  elast.sn <- elasticities[(2*len+1):(3*len)]
  elast.nn <- elasticities[(3*len+1):(4*len)]
  # create and return results
  elast.df <- data.frame(elast.ss,elast.nn,elast.sn,elast.ns)
}

# function for calculating regional food demand and elasticities with the FE 
# model, for a set of one or more parameter samples, a single set of input 
# assumptions (prices, income), and a set of one or more region numbers;
# globalparams are the 9 demand function parameters, are specific to each iteration,
# but apply to all regions;
# regparams are the fixed effects and bias adders, are specific to each iteration,
# and to each region;
# inputdata are the prices and income, are common to all iterations, but are specific
# to each region;
# function calls food.dmnd() from ambrosia package, which calculates demand and 
# budget shares, accounting for fixed effects and regional bias, and which applies 
# a food budget constraint that limits the sum of staples and non-staples shares to 1,
# saves regional results to sub-directory of data/processed given by "output_dir"
# and "scen" using the file name demand_RX_case, where X is the region number and 
# case an extension added if "case" is not "" in the function call;
# if scen = "Obs" (calculating demand based on observed prices/income) or
# save_results = FALSE, results for the single region calculated are returned,
# not saved
food.dmnd.wrapper <- function(globalparams, regparams, inputdata, regions, 
                              output_dir, scen, case, save_result = TRUE) {
  
  message("  calculating demand for scenario ", scen)
  
  # if no regional parameters df, initialize to zero for each region and iteration
  if (is.null(regparams)) {
    regparams <- data.frame(
      GCAM_region_ID = rep(regions,nrow(globalparams)),
      iteration = rep(globalparams$iteration, each = length(regions)),
      staples_FE = rep(0, length(regions) * nrow(globalparams)),
      RBs = rep(0, length(regions) * nrow(globalparams)),
      RBn = rep(0, length(regions) * nrow(globalparams)),
      stringsAsFactors = FALSE)
  # if regional parameters df exists, initialize any that are missing
  } else {
    if (!"staples_FE" %in% names(regparams)) regparams$staples_FE <- 0
    if (!"RBs" %in% names(regparams)) regparams$RBs <- 0
    if (!"RBn" %in% names(regparams)) regparams$RBn <- 0
  }
  
  # initialize in case results will be returned rather than saved to files
  all_demand_results <- list()
  
  # Loop over regions
  for (r in seq_along(regions)) {
    
    region_id <- regions[r]
    message("    starting region ", region_id)
    
    # extract region-specific price/income data
    inputdata_region <- inputdata %>% filter(GCAM_region_ID == region_id)
    if (nrow(inputdata_region) == 0) stop("No inputdata for region ", region_id)
    
    # extract region-specific parameters for all iterations
    regparams_region <- regparams %>% filter(GCAM_region_ID == region_id)

    # split globalparams by iteration
    globalparams_byiter <- split(globalparams, globalparams$iteration)
    # remove names to ensure imap_dfr() works with numeric indices
    names(globalparams_byiter) <- NULL
    
    # calculate demand for all iterations
    demand_reg <- imap_dfr(globalparams_byiter, function(gparams, i) {
      
      # progress tracking
      if (i %% 500 == 0) message("    reached iteration ", i, " for calculating demand")
      
      # prepare parameter structure
      param_structure <- vec2param(as.vector(t(select(gparams, As:pnscl))))
      
      # extract regional parameter values for this iteration
      regparams_reg_iter <- regparams_region %>% 
        filter(iteration == gparams$iteration)
      
      # calculate demand for this iteration
      demand_reg_iter <- food.dmnd(inputdata_region$Ps, inputdata_region$Pn, 
                                   inputdata_region$Y, params = param_structure,
                                   NULL, # rgn argument, not needed
                                   regparams_reg_iter$staples_FE, 
                                   regparams_reg_iter$RBs, 
                                   regparams_reg_iter$RBn) %>%
        # add iteration number, likelihood value, bias adders
        mutate(iteration = gparams$iteration,
               LL = gparams$LL,
               RBs = regparams_reg_iter$RBs,
               RBn = regparams_reg_iter$RBn) %>%
        # add elasticities
        bind_cols(calc_income_elast(inputdata_region$Y, param_structure)) %>%
        # this must be done separately so that income elasticities are available
        bind_cols(calc_price_elast(., param_structure)) %>%
        # add input data
        bind_cols(select(inputdata_region, Y, Ps, Pn, Y.region))

      # include additional columns if present
      if ("gcam-consumer" %in% colnames(inputdata_region)) {
        demand_reg_iter <- bind_cols(select(inputdata_region, `gcam-consumer`), demand_reg_iter)
      }
      if ("year" %in% colnames(inputdata_region)) {
        demand_reg_iter <- bind_cols(select(inputdata_region, year), demand_reg_iter)
      }
      
      return(demand_reg_iter)
    })
    
    # finalize regional results 
    demand_reg <- demand_reg %>%
      # add total demand, total food budget share, and region name and number
      mutate(
        Qtot = Qs + Qn,
        alpha.t = alpha.s + alpha.n,
        GCAM_region_ID = unique(regparams_region$GCAM_region_ID),
        region = unique(regparams_region$region)
      ) %>%
      # add regional demand per cap -- unweighted average across deciles
      group_by(region,year,iteration) %>%
      mutate(Qs.region = if_else(
        is.na(Qs),  # check if any NA exists
        NA_real_,  # return NA if any input is NA
        mean(Qs)  # otherwise, compute mean
      )) %>%
      mutate(Qn.region = if_else(
        is.na(Qn),  # check if any NA exists
        NA_real_,  # return NA if any input is NA
        mean(Qn)  # otherwise, compute weighted mean
      )) %>%
      ungroup() %>%
      mutate(Qtot.region = Qs.region + Qn.region) %>%
      # order columns for readability
      select(GCAM_region_ID, region, year, `gcam-consumer`, 
             Y, Ps, Pn, 
             Qs, Qn, Qm, Qtot, 
             RBs, RBn, 
             Y.region, Qs.region, Qn.region, Qtot.region, 
             alpha.s, alpha.n, alpha.m, alpha.t,
             eta.s, eta.n, elast.ss, elast.nn, elast.sn, elast.ns,
             LL, iteration,
             everything())
    
    # conditionally save regional result
    if (save_result) {
      pathname <- file.path("data/processed", output_dir, scen)
      separator <- ifelse(case == "", "", "_")
      saveRDS(demand_reg, file = file.path(pathname, paste0("demand_R", region_id, separator, case, ".RDS")))
    }
    
    # conditionally store regional result in named list
    if (!save_result || scen == "Obs") {
      all_demand_results[[as.character(region_id)]] <- demand_reg
    }
    
  } # end region loop
  
  # return concatenated result if requested
  if (!save_result || scen == "Obs") {
    return(bind_rows(all_demand_results))
  }
}

# function to calculate food demand for an ensemble of parameter values, with each
# ensemble member bias corrected to the base year value of the total regional
# staples and non-staples demand of a reference scenario; takes as input global
# and regional parameter ensembles; income and price input data; a list of region
# numbers to do calculations for; the output directory; scenario name; case, which
# serves as an output file name tag; the base year, and whether results should
# be saved or returned
food.dmnd.ens_bc <- function(globalparams, regparams, inputdata, regions, 
                             output_dir, scen, case, baseyr, save_result = TRUE) {
  
  # get demand in base year for ensemble with zero bias terms
  regparams_noBias <- regparams %>% mutate(RBs = 0, RBn = 0)
  inputdata_baseyr <- inputdata %>% 
    filter(year == baseyr) %>% select(-c(RBs, RBn))
  demand_noBias <- food.dmnd.wrapper(
    globalparams = globalparams,
    regparams = regparams_noBias,
    inputdata = inputdata_baseyr,
    regions = regions,
    output_dir = output_dir,
    scen = scen,
    case = "noBias",       # provide case so progress message is informative
    save_result = FALSE)   # don't save result, return it
  
  # calculate bias terms for each iteration from difference between the zero 
  # bias scenarios and the single reference scenario in the base year
  
  # get single reference scenario results
  demand_ref <- map_dfr(regions, function(r) {
    readRDS(paste0("data/processed/", output_dir, "/", scen, "/demand_R", r,
                   "_MLparams.RDS"))
  })
  
  # calculate bias terms
  bias_terms <-
    inner_join(demand_ref, demand_noBias,
               by = c("GCAM_region_ID", "region", "gcam-consumer", "year"),
               suffix = c(".ref", ".noBias")) %>%
    mutate(RBs = Qs.region.ref - Qs.region.noBias,
           RBn = Qn.region.ref - Qn.region.noBias) %>%
    rename(iteration = iteration.noBias) %>%
    # keep only what we need for regional bias terms
    select(GCAM_region_ID, region, iteration, RBs, RBn) %>%
    distinct()
  
  # add bias terms to the regional parameter data
  regparams_wBias <- regparams %>%
    left_join(bias_terms, 
              by = c("GCAM_region_ID", "region", "iteration"))
  
  # calculate ambrosia demand with derived bias terms, for base year and projection
  inputdata_proj <- inputdata %>% 
    filter(year >= baseyr) %>% select(-c(RBs, RBn))
  food.dmnd.wrapper(
    globalparams = globalparams,
    regparams = regparams_wBias,
    inputdata = inputdata_proj,
    regions = regions,
    output_dir = output_dir,
    scen = scen,
    case = case,       
    save_result = TRUE)           # save results to files
}

# function for calculating demand from observed prices and income, global and
# regional parameter data, for a given measure (scen = ML, LPR, etc.) and
# set of regions; results file includes observed prices and income, observed
# demand, and modeled demand and elasticities; saved to Obs directory
food.dmnd.obs <- function(globalparamdata,FEdata,obsdata,scen,regions) {
  
  print(paste0("calculating demand from observations for scenario ",scen))
  
  demand_obs <- data.frame()
  # get global parameter data for the scenario
  scen_globaldata <- globalparamdata[globalparamdata$measure == scen,]
  for(r in 1:length(regions)) {
    # get observations for the region
    reg_obs <- obsdata[obsdata$GCAM_region_ID == regions[r],]
    # get FE parameter for the scenario and region
    reg_FEdata <- FEdata[FEdata$measure == scen & FEdata$GCAM_region_ID == regions[r],]
    # calculate demand for the region based on observations
    reg_dmnd <-
      food.dmnd.plus.FE.regions(scen_globaldata,reg_FEdata,reg_obs,"Obs","",regions[r]) %>%
      # rename as modeled results
      rename(Qs_mod = Qs,Qn_mod = Qn,Qm_mod = Qm,Qtot_mod = Qtot) %>%
      # add observed results to the df
      mutate(Qs_obs = reg_obs$Qs,Qn_obs = reg_obs$Qn,Qtot_obs = Qs_obs + Qn_obs)
    demand_obs <- bind_rows(demand_obs,reg_dmnd)
  }
  save(demand_obs,file = paste0("data/processed",data_dir,"/","results","/","Obs","/",
                                "demand_obs_",scen,".RData"))
}

# ------------------------------------------------------------------------------
# the remaining functions are no longer used (replaced by food.dmnd.wrapper())
# holding on to the code for now just in case
# ------------------------------------------------------------------------------

if(FALSE) {
  
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
      # # replace negative demand with zero after bias correction, update shares
      # mutate(Qs = pmax(Qs, 0), Qn = pmax(Qn, 0), Qtot = Qs + Qn) %>%
      # mutate(alpha.s = Qs / Y, alpha.n = Qn / Y, alpha.m = 1 - alpha.s - alpha.n) %>%
      # # Compute total food budget share
      mutate(alpha.t = alpha.s + alpha.n) %>%
      # # adjust consumption levels if they violate the food budget constraint
      # mutate(
      #   alpha.n = case_when(
      #     alpha.t <= max_alphat ~ alpha.n,  # No change if within limit
      #     alpha.n >= (alpha.t - max_alphat) ~ alpha.n - (alpha.t - max_alphat),  # Reduce alpha.n first
      #     TRUE ~ 0  # If alpha.n isn't enough, set it to zero
      #   ),
      #   alpha.s = case_when(
      #     alpha.t <= max_alphat ~ alpha.s,  # No change if within limit
      #     alpha.n == 0 ~ max_alphat,  # Reduce alpha.s only if alpha.n is already zero
      #     TRUE ~ alpha.s  # Otherwise, keep original value
      #   ),
      #   alpha.t = alpha.s + alpha.n  # Recalculate total budget share
      # ) %>%
      # # Recalculate demand based on adjusted budget shares
      # mutate(Qs = alpha.s * Y, Qn = alpha.n * Y, Qtot = Qs + Qn) %>%
      select(keepcols)  # Keep relevant columns
    
    # save bias-corrected results, with _bc added to file name
    save(demand_reg_amb,file = paste("data/processed",data_dir,scen,"results",
                                     str_replace(ambrosia_file,".RData","_bc.RData"),
                                     sep="/"))
  }
}

# function to bias correct an ensemble of ambrosia per capita demand results 
# to the base year value of a target ambrosia scenario. This does not bias-correct
# in the sense of matching to observations (necessarily), but rather so that all 
# members of the ensemble will start from the same value in the base year. The 
# target is taken to be a bias-corrected ambrosia scenario that uses a gcam 
# income/price scenario defined in "scen" and a set of parameters defined in 
# "case" ("bias-corrected" here means the ambrosia scenario applies the gcam 
# regional bias adders to its results in order to match the observations 
# calibrated to by gcam in the base year; so actual in practice the ensemble
# bias correction is in fact matching observations by extension). After the
# ensemble bias correction, constraints for non-negative demand and for maximum
# total food budget share are applied and demand adjusted if necessary.
bias_correct_ambrosia_ensemble <- function(scen,case,regions,bias_yr,max_alphat)  {
  
  # loop over regions
  for(r in regions) {
    
    # get bias-corrected ambrosia results for gcam income/price scenario; this is
    # target scenario to bias correct the ensemble to; store in clearer variable name
    separator <- ifelse(case == "", "", "_")
    ambrosia_file <- paste0("demand_R",r,separator,case,"_bc.RData")
    load(paste("data/processed",data_dir,scen,"results",ambrosia_file,sep="/"))
    demand_reg_target <- demand_reg
    
    # target demand in base year to bias correct to
    Qs_target <- demand_reg_target[year == bias_yr,'Qs']
    Qn_target <- demand_reg_target[year == bias_yr,'Qn']
    
    # get ambrosia ensemble results for the region; this is the ensemble to be
    # bias corrected; store in clearer variable name
    ambrosia_file <- paste0("demand_R",r,".RData")
    load(paste("data/processed",data_dir,scen,"results",ambrosia_file,sep="/"))
    demand_reg_ens <- demand_reg
    
    # bias correct each ensemble member
    demand_reg_ens_bc <- demand_reg_ens %>%
      group_by(iteration) %>%
      # calculate and apply ensemble bias correction for each iteration
      mutate(RBs_ens = Qs_target - Qs[year == bias_yr],
             RBn_ens = Qn_target - Qn[year == bias_yr],
             Qs = Qs + RBs_ens,
             Qn = Qn + RBn_ens) %>%
      ungroup %>%
      # replace negative demand with zero after bias correction, update shares
      mutate(Qs = pmax(Qs, 0), Qn = pmax(Qn, 0), Qtot = Qs + Qn) %>%
      mutate(alpha.s = Qs / Y, alpha.n = Qn / Y, alpha.m = 1 - alpha.s - alpha.n) %>%
      # Compute total food budget share
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
      mutate(Qs = alpha.s * Y, Qn = alpha.n * Y, Qtot = Qs + Qn)
  }
}

} # end if statement