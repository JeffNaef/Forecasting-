# Pareto/NBD Model Simulation
# This code simulates customer transaction data following the Pareto/NBD model
# and formats it for use with CLVTools

library(dplyr)
library(lubridate)

# Function to simulate from Pareto/NBD model
simulate_pnbd <- function(n_customers, 
                          beta, s,      # Gamma parameters for M0 (transaction rate)
                          alpha, r,     # Gamma parameters for Lambda0 (dropout rate) 
                          gamma, q,     # Gamma parameters for N (Spending rate) 
                          p,     # Gamma parameters for Z (Spending)
                          start_date = as.Date("2005-01-01"),
                          seasonality = F ,
                          observation_period = 208) {
  
  cat("Simulating", n_customers, "customers from Pareto/NBD model...\n")
  
  # Step 1: Draw M0 from gamma distribution Gam(beta, s)
  # Using shape-rate parameterization: shape=s, rate=beta
  M0 <- rgamma(n_customers, shape = s, rate = beta)
  cat("Step 1: Generated dropout rates M0 from Gam(", beta, ",", s, ")\n")
 

  
  # Step 2: Draw Lambda0 from gamma distribution Gam(alpha, r)  
  # Using shape-rate parameterization: shape=r, rate=alpha
  Lambda0 <- rgamma(n_customers, shape = r, rate = alpha)
  cat("Step 2: Generated transaction rates Lambda0 from Gam(", alpha, ",", r, ")\n")
  
  # Step 3: Draw Nu from gamma distribution Gam(gamma, q)
  Nu <- rgamma(n_customers, shape = q, rate = gamma)
  # Step 3: For each customer, draw their unobserved lifetime Omega ~ Exp(Lambda0)
  Omega <- rexp(n_customers, rate = M0)
  cat("Step 3: Generated customer lifetimes Omega ~ Exp(M0)\n")
  
  # Step 4: Generate transaction time points T_j starting from zero
  cat("Step 4: Generating transaction time points T_j...\n")
  
  all_transactions <- data.frame()
  
  for (i in 1:n_customers) {
    if (i %% 500 == 0) cat("  Processing customer", i, "of", n_customers, "\n")
    
    customer_transactions <- data.frame()
    
    # Customer's active period is minimum of their lifetime and observation period
    active_period <- min(Omega[i], observation_period)
    
    # Generate transaction time points T_j by cumulating inter-transaction times
    # Inter-transaction times T_{j-1,j} ~ Exp(Lambda0[i])
    # Transaction times T_j = sum_{l=1}^j T_{l-1,l} (starting from T_0 = 0)
    
    transaction_times <- c()  # Will store the actual transaction times T_j
    current_time <- 0  # Start from T_0 = 0
    
    repeat {
      # Generate next inter-transaction time T_{j-1,j} ~ Exp(M0[i])
      inter_transaction_time <- rexp(1, rate = Lambda0[i])
      
      # Calculate next transaction time T_j = T_{j-1} + T_{j-1,j}
      next_transaction_time <- current_time + inter_transaction_time
      
      # Stop if this transaction would occur after customer death or observation end
      if (next_transaction_time >= active_period) {
        break
      }
      
      # Record this transaction time
      transaction_times <- c(transaction_times, next_transaction_time)
      current_time <- next_transaction_time
    }
    
    # Convert transaction times to dates and create transaction records
    if (length(transaction_times) > 0) {
      
      
      for (j in 1:length(transaction_times)) {
        transaction_date <- start_date + days( round(transaction_times[j]*7))
        
        # Generate a realistic transaction amount (need to change this!!!)
        transaction_amount <- round(rgamma(1, shape = p, rate = Nu[i]), 2)
        
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
  n_customers = 2000,
  beta = 20,      # Rate parameter for transaction rate (higher = lower transaction rates)
  s = 2,         # Shape parameter for transaction rate  
  alpha = 8,     # Rate parameter for dropout rate (higher = higher dropout)
  r = 3,         # Shape parameter for dropout rate
  q=10,
  gamma=2,
  p=200,
  start_date = as.Date("2005-01-01"),
  observation_period = 208  # One year observation period
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











### Estimation ##################




library(CLVTools)

##Set up a CLVTools object, whereby we split after 2 years (104 weeks)

clv.sim <- clvdata(simulated_data,  
                       date.format="ymd", 
                       time.unit = "week",
                       estimation.split = 104,
                       name.id = "Id",
                       name.date = "Date",
                       name.price = "Price")


#Estimate the PNBD model
est.pnbd <- latentAttrition(family = pnbd, data=clv.sim)
est.pnbd

summary(est.pnbd)
plot(est.pnbd)

# Now we estimate the spending Part
est.gg <- spending(family = gg, data = clv.sim)

summary(est.gg)
plot(est.gg)

## Finally we can predict all our customers and their expected spending in [h,T+h]:
dt.pred<-predict(est.pnbd,predict.spending = est.gg,prediction.end=52)


