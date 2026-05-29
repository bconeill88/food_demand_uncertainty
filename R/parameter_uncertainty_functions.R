# Helper functions to calculate and save parameter ML and uncertainty intervals 

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
