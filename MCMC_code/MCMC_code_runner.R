library(devtools)
library(dplyr)
library(tidyr)
#devtools::load_all(".")

#You will need the ambrosia package to run this MCMC code- https://github.com/JGCRI/ambrosia
# First function here won't write values. It just calculated the LL. The other writes the values
log_likelihood <- mc.setup("Processed_group_data.csv",trace_param = FALSE)
log_likelihood_write <- mc.setup("Processed_group_data.csv",trace_param = TRUE)

burn_in_period <- 5000
tuning_interval <- 100
target_acceptance_rate <- 0.2

# Initial parameter values. Based on 7M values calculated by RPL by previous MCMC
theta <- c(1.28,1.14,-0.19,0.21,-0.33,0.5,0.1,16,5.06)

#theta <- c(1.20922371,  1.13350526, -0.16938847,  0.13619865,-0.23429367,  0.48957960,  0.09771347, 15.99872346,4.96793544)


#Store FE_params

first_FE_params <- read.csv("Processed_group_data.csv") %>%
                   select(GCAM_region_ID, staples_FE) %>%
                   mutate(staples_FE=ifelse(GCAM_region_ID==30,0,staples_FE)) %>%
                   distinct()


FE_params<- first_FE_params

first_FE_params %>%
    mutate(iteration=0)->fe_params_for_write

write.table(fe_params_for_write %>% mutate(LL=-896), file = "FE_params.dat", row.names = FALSE, col.names = TRUE, sep = "\t")
# Number of iterations
n_iterations <- 1000000

#This will track the acceptance rate
j <-0

#This will track the sample acceptance rate
k <-0
accept_count <- 0
# Matrix to store samples
samples <- matrix(NA, nrow = n_iterations, ncol = 11)
sd=0.05


 #Save LL old since we will rewrite the data below 
log_likelihood(c(theta,100,20))->LL_old

t <- log_likelihood_write(c(theta,100,20))
# MCMC loop
for (i in 1:n_iterations) {
    

    # Propose new parameter values. Use a sd value of 0.29 which is the sqrt of median staple sigma2 which is 0.088
    theta_new <- rnorm(9, mean = theta, sd = sd)

    #Propose new fixed effect parameters for staples. Start with old and then update.
    FE_params_updated <- FE_params
    for (l in 1:nrow(FE_params_updated)){

        FE_params_updated$staples_FE[l] <- rnorm(1, mean = FE_params_updated$staples_FE[l], sd = sd)

    }

    #Need to re-write all observed data and recalc likelihood functions
     obs.data <- read.csv("Processed_group_data.csv") %>%
                select(-staples_FE) %>%
                left_join(FE_params_updated) %>%
        #select(GCAM_region_ID, staples_FE) %>%
        mutate(staples_FE=ifelse(GCAM_region_ID==30,0,staples_FE))

   
   write.csv(obs.data,"Processed_group_data.csv",row.names = F)
    log_likelihood <- mc.setup("Processed_group_data.csv",trace_param = FALSE)
    log_likelihood_write <- mc.setup("Processed_group_data.csv",trace_param = TRUE)


    LL_new <- log_likelihood(c(theta_new,100,20))
    # Calculate log likelihood for the proposed values
    log_ratio <- LL_new - LL_old

    # Accept or reject the proposed values
    if (log(runif(1)) < log_ratio) {
        theta <- theta_new
        #Update old LL to compare against
        log_likelihood(c(theta,100,20))->LL_old
        FE_params <- FE_params_updated
        print(theta_new)
        j=j+1
        print(paste0("Proposal acceptance rate is ", toString((j/i)*100), "%"))
        accept_count <- accept_count+1
    }


  # Calculate acceptance rate periodically and adjust SD during burn-in
    if (i %% tuning_interval == 0 && i <= burn_in_period) {
        current_acceptance_rate <- accept_count / tuning_interval

        if (current_acceptance_rate < target_acceptance_rate) {
            sd <- sd * 0.9  # Decrease SD to make proposals smaller
        } else if (current_acceptance_rate > target_acceptance_rate) {
            sd <- sd * 1.1  # Increase SD to make proposals larger
        }
        sd <- max(sd, 0.05)
         #Reset acceptance count for the next tuning interval
        accept_count <- 0
        
    }

    # Now calculate the LL and see if we should sccept or reject it as a valid sample. Use an LL of -4000 as the cutoff
   t <- LL_new
    if(t > -10000){

  log_likelihood_write(c(theta_new,100,20))
        write.table(FE_params_updated %>% mutate(iteration_number=i,
                                                 LL=t), file = "FE_params.dat", row.names = FALSE, col.names = FALSE, sep = "\t", append = TRUE)


  k=k+1
  print(paste0("Sample acceptance rate is ", toString((k/i)*100), "%"))
  print(paste0("Tuning: Adjusted SD to ", sd, " at iteration ", i))


}




}

