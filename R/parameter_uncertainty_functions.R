# calculate and save parameter ML and uncertainty intervals --------------------
# ----

# function for identifying ML and calculating independent confidence intervals for
# global parameters or FE parameter data for a single region; use method from Edmonds et
# al: take the range of all variables after dropping samples below the 5th
# percentile of likelihood scores
make_param_MLintervals <- function(param_data,type,ci) {
  
  # get the maximum likelihood parameter values
  MLparams <- param_data[which.max(param_data$LL),] %>%
    mutate(measure = "ML")
  # drop least likely iterations based on confidence interval
  quant_LL <- quantile(param_data$LL,probs = (1-ci/100))
  param_data <- param_data[param_data$LL > quant_LL,]
  # get the max and min of confidence interval and assemble results
  if(type == "global") {
    # max and min of confidence interval
    maxparams <- param_data %>% summarize(across(everything(),max)) %>%
      mutate(measure = "HI")
    minparams <- param_data %>% summarize(across(everything(),min)) %>%
      mutate(measure = "LI")
    # assemble results
    params <- bind_rows(MLparams,minparams,maxparams) %>% relocate(measure)
  }
  if(type == "FE") {
    # max and min of confidence interval
    maxparams <- param_data %>% summarize(across(-region,max)) %>%
      mutate(measure = "HI")
    minparams <- param_data %>% summarize(across(-region,min)) %>%
      mutate(measure = "LI")
    # assemble results
    params <- bind_rows(MLparams,minparams,maxparams) %>%
      mutate(region = param_data$region[[1]]) %>%
      relocate(measure)
  }
  return(params)
}

# find high and low price sensitivity parameters (global parameters)

# function to find parameters representing joint price parameter combinations
# assumed to lead to the lowest and highest demand in response to a price
# increase ("lo" - lowest demand and jointly most negative parameters; "hi" -
# highest demand and jointly most positive parameters); produces a single
# dataframe with two rows
# seems like this function should be generalizable to work for joint income and
# joint scale parameters too
get_sens_params_price <- function(param_data,ci) {
  
  # drop outliers by only using samples in confidence interval for likelihood scores
  quant_LL <- quantile(param_data$LL,probs = (1-ci/100))
  param_data_interval <- param_data[param_data$LL > quant_LL,]
  
  # calculate all deciles of each variable
  get_quantiles <- function(sample) {
    quantile(sample,probs=seq(0.1, 0.9, 0.1))
  }
  quantiles_data <- apply(param_data_interval,2,get_quantiles)
  
  # identify high and low price sensitivity values for xss, xnn, xcross
  
  # initialize quantiles to search over and parameter results
  quantlist = c("10%","20%","30%","40%","50%","60%","70%","80%","90%")
  i <- 1
  params_joint_cond1 <- params_joint_cond2 <-
    matrix(ncol = 0, nrow = 0) %>% data.frame()
  
  while (nrow(params_joint_cond1) < 1 && i < 10) {
    
    # identify samples that have all price elasticity params below a given percentile
    # within their marginal distributions
    quant <- quantlist[i]
    params_joint_cond1 <- param_data_interval[
      param_data_interval$xi.ss < quantiles_data[quant,"xi.ss"] &
        param_data_interval$xi.nn < quantiles_data[quant,"xi.nn"] &
        param_data_interval$xi.cross < quantiles_data[quant,"xi.cross"],]
    i <- i+1
  }
  # select sample with lowest likelihood score in case there is more than one
  params_joint_cond1 <- params_joint_cond1[
    params_joint_cond1$LL == min(params_joint_cond1$LL),] %>%
    filter(!duplicated(LL))
  # add the measure name and the quantile used to derive parameters
  params_joint_cond1 <-
    mutate(measure = "LPR",params_joint_cond1,quant = quantlist[i-1]) %>%
    relocate(measure)
  
  while (nrow(params_joint_cond2) < 1 && i < 10) {
    
    # identify samples that have all price elasticity params above a given percentile
    # within their marginal distributions
    quant <- quantlist[10-i]
    params_joint_cond2 <- param_data_interval[
      param_data_interval$xi.ss > quantiles_data[quant,"xi.ss"] &
        param_data_interval$xi.nn > quantiles_data[quant,"xi.nn"] &
        param_data_interval$xi.cross > quantiles_data[quant,"xi.cross"],]
    i <- i+1
  }
  # select sample with lowest likelihood score in case there is more than one
  params_joint_cond2 <- params_joint_cond2[
    params_joint_cond2$LL == min(params_joint_cond2$LL),] %>%
    filter(!duplicated(LL))
  # add the measure name and the quantile used to derive parameters
  params_joint_cond2 <-
    mutate(measure = "HPR",params_joint_cond2,quant = quantlist[10-(i-1)]) %>%
    relocate(measure)
  
  # return a single dataframe with the two sets of parameters as rows
  params_joint <- rbind(params_joint_cond1,params_joint_cond2)
  return(params_joint)
}

# find high and low income sensitivity parameters (global parameters)

# function to find parameters representing joint income parameter combinations
# assumed to lead to the lowest and highest demand in response to an income
# increase ("lo" - lowest demand and lowest parameters; "hi" - highest demand
# and highest parameters); produces a single dataframe with two rows
get_sens_params_income <- function(param_data,ci) {
  
  # drop outliers by only using samples in confidence interval for likelihood scores
  quant_LL <- quantile(param_data$LL,probs = (1-ci/100))
  param_data_interval <- param_data[param_data$LL > quant_LL,]
  
  # calculate all deciles of each variable
  get_quantiles <- function(sample) {
    quantile(sample,probs=seq(0.1, 0.9, 0.1))
  }
  quantiles_data <- apply(param_data_interval,2,get_quantiles)
  
  # identify high and low price sensitivity values for ks, lambda, eps1n
  
  # initialize quantiles to search over and parameter results
  quantlist = c("10%","20%","30%","40%","50%","60%","70%","80%","90%")
  i <- 1
  params_joint_cond1 <- params_joint_cond2 <-
    matrix(ncol = 0, nrow = 0) %>% data.frame()
  
  while (nrow(params_joint_cond1) < 1 && i < 10) {
    
    # identify samples that have all income parameters below a given percentile
    # within their marginal distributions
    quant <- quantlist[i]
    params_joint_cond1 <- param_data_interval[
      param_data_interval$ks < quantiles_data[quant,"ks"] &
        param_data_interval$lambda < quantiles_data[quant,"lambda"] &
        param_data_interval$eps1n < quantiles_data[quant,"eps1n"],]
    i <- i+1
  }
  # select sample with lowest likelihood score in case there is more than one
  params_joint_cond1 <- params_joint_cond1[
    params_joint_cond1$LL == min(params_joint_cond1$LL),] %>%
    filter(!duplicated(LL))
  # add the measure name and the quantile used to derive parameters
  params_joint_cond1 <-
    mutate(measure = "LIR",params_joint_cond1,quant = quantlist[i-1]) %>%
    relocate(measure)
  
  while (nrow(params_joint_cond2) < 1 && i < 10) {
    
    # identify samples that have all income parameters above a given percentile
    # within their marginal distributions
    quant <- quantlist[10-i]
    params_joint_cond2 <- param_data_interval[
      param_data_interval$ks > quantiles_data[quant,"ks"] &
        param_data_interval$lambda > quantiles_data[quant,"lambda"] &
        param_data_interval$eps1n > quantiles_data[quant,"eps1n"],]
    i <- i+1
  }
  # select sample with lowest likelihood score in case there is more than one
  params_joint_cond2 <- params_joint_cond2[
    params_joint_cond2$LL == min(params_joint_cond2$LL),] %>%
    filter(!duplicated(LL))
  # add the measure name and the quantile used to derive parameters
  params_joint_cond2 <-
    mutate(measure = "HIR",params_joint_cond2,quant = quantlist[10-(i-1)]) %>%
    relocate(measure)
  
  # return a single dataframe with the two sets of parameters as rows
  params_joint <- rbind(params_joint_cond1,params_joint_cond2)
  return(params_joint)
}

# find high and low scale sensitivity parameters

# function to find parameters representing joint scale parameter combinations
# assumed to lead to the lowest and highest demand in response to an income
# increase ("lo" - lowest demand and lowest parameters; "hi" - highest demand
# and highest parameters); produces a single dataframe with two rows
get_sens_params_scale <- function(param_data,ci) {
  
  # drop outliers by only using samples in confidence interval for likelihood scores
  quant_LL <- quantile(param_data$LL,probs = (1-ci/100))
  param_data_interval <- param_data[param_data$LL > quant_LL,]
  
  # calculate all deciles of each variable
  get_quantiles <- function(sample) {
    quantile(sample,probs=seq(0.1, 0.9, 0.1))
  }
  quantiles_data <- apply(param_data_interval,2,get_quantiles)
  
  # identify high and low price sensitivity values for ks, lambda, eps1n
  
  # initialize quantiles to search over and parameter results
  quantlist = c("10%","20%","30%","40%","50%","60%","70%","80%","90%")
  i <- 1
  params_joint_cond1 <- params_joint_cond2 <-
    matrix(ncol = 0, nrow = 0) %>% data.frame()
  
  while (nrow(params_joint_cond1) < 1 && i < 10) {
    
    # identify samples that have all income parameters below a given percentile
    # within their marginal distributions
    quant <- quantlist[i]
    params_joint_cond1 <- param_data_interval[
      param_data_interval$As < quantiles_data[quant,"As"] &
        param_data_interval$An < quantiles_data[quant,"An"],]
    i <- i+1
  }
  # select sample with lowest likelihood score in case there is more than one
  params_joint_cond1 <- params_joint_cond1[
    params_joint_cond1$LL == min(params_joint_cond1$LL),] %>%
    filter(!duplicated(LL))
  # add the measure name and the quantile used to derive parameters
  params_joint_cond1 <-
    mutate(measure = "LSR",params_joint_cond1,quant = quantlist[i-1]) %>%
    relocate(measure)
  
  while (nrow(params_joint_cond2) < 1 && i < 10) {
    
    # identify samples that have all income parameters above a given percentile
    # within their marginal distributions
    quant <- quantlist[10-i]
    params_joint_cond2 <- param_data_interval[
      param_data_interval$As > quantiles_data[quant,"As"] &
        param_data_interval$An > quantiles_data[quant,"An"],]
    i <- i+1
  }
  # select sample with lowest likelihood score in case there is more than one
  params_joint_cond2 <- params_joint_cond2[
    params_joint_cond2$LL == min(params_joint_cond2$LL),] %>%
    filter(!duplicated(LL))
  # add the measure name and the quantile used to derive parameters
  params_joint_cond2 <-
    mutate(measure = "HSR",params_joint_cond2,quant = quantlist[10-(i-1)]) %>%
    relocate(measure)
  
  # return a single dataframe with the two sets of parameters as rows
  params_joint <- rbind(params_joint_cond1,params_joint_cond2)
  return(params_joint)
}
