# Pareto/NBD Model Simulation
# This code simulates customer transaction data following the Pareto/NBD model
# and formats it for use with CLVTools

library(dplyr)
library(lubridate)

# Function to simulate from Pareto/NBD model
simulate_pnbd <- function(n_customers, 
                          beta, s,      # Gamma parameters for M0 (transaction rate)
                          alpha, r,     # Gamma parameters for Lambda0 (dropout rate) 
                          start_date = as.Date("2005-01-01"),
                          observation_period = 365) {
  
  cat("Simulating", n_customers, "customers from Pareto/NBD model...\n")
  
  # Step 1: Draw M0 from gamma distribution Gam(beta, s)
  # Using shape-rate parameterization: shape=s, rate=beta
  M0 <- rgamma(n_customers, shape = s, rate = beta)
  cat("Step 1: Generated transaction rates M0 from Gam(", beta, ",", s, ")\n")
  
  # Step 2: Draw Lambda0 from gamma distribution Gam(alpha, r)  
  # Using shape-rate parameterization: shape=r, rate=alpha
  Lambda0 <- rgamma(n_customers, shape = r, rate = alpha)
  cat("Step 2: Generated dropout rates Lambda0 from Gam(", alpha, ",", r, ")\n")
  
  # Step 3: For each customer, draw their unobserved lifetime Omega ~ Exp(Lambda0)
  Omega <- rexp(n_customers, rate = Lambda0)
  cat("Step 3: Generated customer lifetimes Omega ~ Exp(Lambda0)\n")
  
  # Step 4: For each customer, simulate transaction times until death or observation end
  cat("Step 4: Simulating transaction times...\n")
  
  all_transactions <- data.frame()
  
  for (i in 1:n_customers) {
    if (i %% 500 == 0) cat("  Processing customer", i, "of", n_customers, "\n")
    
    customer_transactions <- data.frame()
    cumulative_time <- 0
    transaction_count <- 0
    
    # Keep generating inter-transaction times until customer dies or observation ends
    while (cumulative_time < min(Omega[i], observation_period)) {
      
      # Draw inter-transaction time from exponential with rate M0[i]
      inter_transaction_time <- rexp(1, rate = M0[i])
      cumulative_time <- cumulative_time + inter_transaction_time
      
      # Only record transaction if it happens before death and within observation period
      if (cumulative_time < min(Omega[i], observation_period)) {
        transaction_count <- transaction_count + 1
        
        # Create transaction record
        transaction_date <- start_date + days(round(cumulative_time))
        
        # Generate a realistic transaction amount (you can modify this)
        # Using a log-normal distribution to get realistic price variation
        transaction_amount <- round(rlnorm(1, meanlog = 3, sdlog = 0.5), 2)
        
        customer_transactions <- rbind(customer_transactions, 
                                       data.frame(
                                         Id = as.character(i),
                                         Date = transaction_date,
                                         Price = transaction_amount
                                       ))
      }
    }
    
    # Only add customers who made at least one transaction
    if (nrow(customer_transactions) > 0) {
      all_transactions <- rbind(all_transactions, customer_transactions)
    }
  }
  
  # Format for CLVTools (matching the apparelTrans format)
  all_transactions$Id <- as.character(all_transactions$Id)
  all_transactions <- all_transactions[order(all_transactions$Id, all_transactions$Date), ]
  
  cat("Simulation complete!\n")
  cat("Generated", nrow(all_transactions), "transactions for", 
      length(unique(all_transactions$Id)), "active customers\n")
  
  return(all_transactions)
}

# Example usage with realistic parameters
# You can adjust these parameters based on your needs
set.seed(123)  # For reproducible results

# Simulate 600 customers (matching the example data)
simulated_data <- simulate_pnbd(
  n_customers = 600,
  beta = 2,      # Rate parameter for transaction rate (higher = lower transaction rates)
  s = 1,         # Shape parameter for transaction rate  
  alpha = 1,     # Rate parameter for dropout rate (higher = higher dropout)
  r = 1,         # Shape parameter for dropout rate
  start_date = as.Date("2005-01-01"),
  observation_period = 365  # One year observation period
)

# Display the results
cat("\nFirst few rows of simulated data:\n")
print(head(simulated_data, 10))

cat("\nLast few rows of simulated data:\n") 
print(tail(simulated_data, 5))

cat("\nData structure:\n")
str(simulated_data)

# Summary statistics
cat("\nSummary statistics:\n")
cat("Number of unique customers:", length(unique(simulated_data$Id)), "\n")
cat("Total number of transactions:", nrow(simulated_data), "\n")
cat("Date range:", as.character(min(simulated_data$Date)), "to", as.character(max(simulated_data$Date)), "\n")
cat("Price range:", round(min(simulated_data$Price), 2), "to", round(max(simulated_data$Price), 2), "\n")

# Transactions per customer distribution
transactions_per_customer <- simulated_data %>%
  group_by(Id) %>%
  summarise(n_transactions = n(), .groups = 'drop')

cat("Transactions per customer summary:\n")
print(summary(transactions_per_customer$n_transactions))

# Function to save data in CLVTools format
save_for_clvtools <- function(data, filename = "simulated_pnbd_data.csv") {
  write.csv(data, filename, row.names = FALSE)
  cat("Data saved to", filename, "in CLVTools format\n")
}

# Uncomment to save the data
# save_for_clvtools(simulated_data)

# Optional: Compare with original apparelTrans structure
cat("\nData is ready for CLVTools! Format matches apparelTrans structure:\n")
cat("- Id: Character vector with customer IDs\n")
cat("- Date: Date vector with transaction dates\n") 
cat("- Price: Numeric vector with transaction amounts\n")