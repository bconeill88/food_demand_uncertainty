library(devtools)
library(dplyr)
library(tidyr)
devtools::load_all(".")

# First function calculates the LL without writing values, the second writes the values
log_likelihood <- mc.setup("Processed_group_data.csv", trace_param = FALSE)
log_likelihood_write <- mc.setup("Processed_group_data.csv", trace_param = TRUE)

# Initial parameter values
theta <- c(1.28, 1.14, -0.19, 0.21, -0.33, 0.5, 0.1, 16, 5.06)

# Store FE_params
first_FE_params <- read.csv("Processed_group_data.csv") %>%
  select(GCAM_region_ID, staples_FE) %>%
  mutate(staples_FE = ifelse(GCAM_region_ID == 30, 0, staples_FE)) %>%
  distinct()

FE_params <- first_FE_params

# Write initial FE params
fe_params_for_write <- first_FE_params %>%
  mutate(iteration = 0)

write.table(fe_params_for_write %>% mutate(LL = -896), file = "FE_params.dat", row.names = FALSE, col.names = TRUE, sep = "\t")

# Number of iterations
n_iterations <- 1000000

# Matrix to store samples
samples <- matrix(NA, nrow = n_iterations, ncol = 11)

# Initial likelihood calculation
LL_old <- log_likelihood_write(c(theta, 100, 20))

# MCMC loop with Metropolis-Hastings inside Gibbs sampling
for (i in 1:n_iterations) {

    # Step 1: Gibbs sampling for theta parameters with Metropolis-Hastings for each
    for (j in 1:length(theta)) {
        
        
        # Propose a new value for theta_j
        theta_proposed <- theta
        #Update 1 parameter at a time
        theta_proposed[j] <- rnorm(1, mean = theta[j], sd = 0.078)  # Adjust sd as necessary
        
        # Calculate log likelihood for the proposed theta
        LL_new <- log_likelihood(c(theta_proposed, 100, 20))
        
        # Calculate the Metropolis-Hastings acceptance ratio
        acceptance_ratio <- exp(LL_new - LL_old)
        
        # Accept or reject the proposed value based on acceptance ratio
        if (runif(1) < acceptance_ratio) {
            theta[j] <- theta_proposed[j]  # Accept the new value
            LL_old <- LL_new  # Update likelihood to the new one
        }
    }

    # Step 2: Gibbs sampling for FE_params with Metropolis-Hastings
    FE_params_updated <- FE_params
    for (l in 1:nrow(FE_params_updated)) {
        # Propose a new value for staples_FE
        FE_params_proposed <- FE_params_updated
        FE_params_proposed$staples_FE[l] <- rnorm(1, mean = FE_params_updated$staples_FE[l], sd = 0.078)  # Adjust sd as necessary
        
        # Update observed data with the proposed FE parameters
        obs.data <- read.csv("Processed_group_data.csv") %>%
          select(-staples_FE) %>%
          left_join(FE_params_proposed) %>%
          mutate(staples_FE = ifelse(GCAM_region_ID == 30, 0, staples_FE))
        
       # The likelihood function is set up to accept a csv file as an input. We need to write a test file and set up a new likelihood function that points to this test file.
       # If we succeed in finding a better value, we will update the actual file.
        write.csv(obs.data, "Processed_group_data_test.csv", row.names = F)
        
        # Recalculate log likelihood with the proposed FE_params
        log_likelihood_proposed <- mc.setup("Processed_group_data_test.csv", trace_param = FALSE)
        
        # We use theta proposed since we want to cycle through the FE parameters after cycling through the original 9. If we have succeeded in getting a better theta this will have been updated anyway.
        LL_new <- log_likelihood_proposed(c(theta_proposed, 100, 20))
        
        # Metropolis-Hastings acceptance ratio for FE_params
        acceptance_ratio_FE <- exp(LL_new - LL_old)
        
        # Accept or reject the new FE_params
        if (runif(1) < acceptance_ratio_FE) {
            FE_params_updated$staples_FE[l] <- FE_params_proposed$staples_FE[l]  # Accept the new value
            FE_params$staples_FE[l] <- FE_params_proposed$staples_FE[l]  # Update FE_params
            LL_old <- LL_new  # Update likelihood
        }
    }

    # Step 3: Write the updated parameters and likelihood to file
    if (LL_new > -10000) {
        obs.data <- read.csv("Processed_group_data.csv") %>%
          select(-staples_FE) %>%
          left_join(FE_params_proposed) %>%
          mutate(staples_FE = ifelse(GCAM_region_ID == 30, 0, staples_FE))
     
        write.csv(obs.data, "Processed_group_data.csv", row.names = F)
        
#Technically we don't need to create another write function. Trying to be careful here.
        log_likelihood_for_sample_write <- mc.setup("Processed_group_data.csv", trace_param = TRUE)   
        l<-log_likelihood_for_sample_write(c(theta_proposed, 100, 20))
        
           write.table(FE_params_proposed %>%
                      mutate(iteration_number = i, LL = l),
                    file = "FE_params.dat", row.names = FALSE, col.names = FALSE, sep = "\t", append = TRUE)

       
    }

    # Print progress
    print(paste0("Iteration: ", i, " | Likelihood: ", l))
}
