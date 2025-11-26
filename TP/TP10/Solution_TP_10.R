library(data.table)
library(CLVTools)


## Initiate the clvdata and plot the data

#mydata<-fread("data/data_elec.csv")


##############
##Exercise 1: Standard Dataset without covariates
#############

##load packages
library(CLVTools)
library(data.table)




## 1. Load the data and create a CLVTools object with estimation split after 38 weeks and plot the data using \texttt{clvdata}.

## Load data
mydata<-fread("data/cdnow.csv")

clv.cdnow <- clvdata(mydata,  
                     date.format="ymd", 
                     time.unit = "week",
                     estimation.split = 38, #156,
                     name.id = "Id",
                     name.date = "Date",
                     name.price = "Price")

plot(clv.cdnow)
# We see the downward trend typical for the PNBD model.


##2. Estimate the PNBD/GG model and plot the result. 

model.cdnow<-latentAttrition(
  family = pnbd,
  data = clv.cdnow,
  optimx.args = list(method = "BFGS"))

plot(model.cdnow)

est.gg <- spending(family = gg, data = clv.cdnow)

plot(est.gg)


## 3. Predict for the next 38 weeks and evaluate the RMSE of the model. Identify the 10% of customers
## with the highest expected spending and compare it with the 10% highest spenders in the test set
dt.pred <- predict(model.cdnow, predict.spending = est.gg,prediction.end=38)
rmse <- function(x, y){sqrt(mean((x - y)^2))}

rmse(dt.pred$actual.period.spending, dt.pred$predicted.period.spending)


threshold <- quantile(dt.pred$actual.period.spending, 0.90)

# Ids for predicted spending
dt.pred[predicted.period.spending >= threshold]$Id

# Ids for true spending
dt.pred[actual.period.spending >= threshold]$Id


# Get the IDs from each group
ids_predicted <- dt.pred[predicted.period.spending >= threshold]$Id
ids_actual <- dt.pred[actual.period.spending >= threshold]$Id

# Calculate overlap
overlap <- intersect(ids_predicted, ids_actual)
overlap_count <- length(overlap)

# Calculate fraction (relative to each set)
fraction_of_predicted <- overlap_count / length(ids_predicted)
fraction_of_actual <- overlap_count / length(ids_actual)



##############
##Exercise 2: Simulation with Static Covariates
#############

## 1. Create a function that simulates from the PNBD model, but uses a static covariate called "highbuyer" which is 1 for some customer and 0 for others. 
## Simulate first this covariate using highbuyer<-rbinom(n=n_customers,size=1, prob=0.4) and then use your function like this:
# simulated_data <- simulate_pnbd(
#   n_customers = 2000,
#   beta = 20,      # Rate parameter for transaction rate (higher = lower transaction rates)
#   s = 2,         # Shape parameter for transaction rate  
#   alpha = 8,     # Rate parameter for dropout rate (higher = higher dropout)
#   r = 3,         # Shape parameter for dropout rate
#   q=10,
#   gamma=2,
#   p=200,
#   highbuyer=highbuyer,
#   gamma_purch=0.5,
#   gamma_att=0,
#   start_date = as.Date("2005-01-01"),
#   observation_period = 209  # A bit more than 4 years
# )
# Care needs to be taken if you are using the function from the lecture: Some customers will be discarded because they have zero transactions, and the highbuyer covariate needs to be filtered only for those customers that have at least one transaction.
# In the solution we will set n_customers=500, but feel free to try around with the number of customers



##load packages
library(CLVTools)
library(data.table)
library(dplyr)
library(lubridate)

set.seed(2)

# Function to simulate from Pareto/NBD model
simulate_pnbd <- function(n_customers, 
                          beta, s,      # Gamma parameters for M0 (transaction rate)
                          alpha, r,     # Gamma parameters for Lambda0 (dropout rate) 
                          gamma, q,     # Gamma parameters for N (Spending rate) 
                          p,     # Gamma parameters for Z (Spending),
                          highbuyer,
                          gamma_purch,
                          gamma_att,
                          start_date = as.Date("2005-01-01"),
                          observation_period = 208) {
  
  cat("Simulating", n_customers, "customers from Pareto/NBD model...\n")
  
  # Step 1: Draw M0 from gamma distribution Gam(beta, s)
  # Using shape-rate parameterization: shape=s, rate=beta
  M0 <- rgamma(n_customers, shape = s, rate = beta)
  cat("Step 1: Generated dropout rates M0 from Gam(", beta, ",", s, ")\n")
  
  M0<-M0*exp(highbuyer*gamma_att)
  
  # Step 2: Draw Lambda0 from gamma distribution Gam(alpha, r)  
  # Using shape-rate parameterization: shape=r, rate=alpha
  Lambda0 <- rgamma(n_customers, shape = r, rate = alpha)
  
  Lambda0<-Lambda0*exp(highbuyer*gamma_purch)
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
      # Generate next inter-transaction time T_{j-1,j} ~ Exp(Lambda0[i])
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
                                         Price = transaction_amount,
                                         highbuyer=highbuyer[i]
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

n_customers<-500


highbuyer<-rbinom(n=n_customers,size=1, prob=0.4)

simulated_data <- simulate_pnbd(
  n_customers = n_customers,
  beta = 20,      # Rate parameter for transaction rate (higher = lower transaction rates)
  s = 2,         # Shape parameter for transaction rate
  alpha = 8,     # Rate parameter for dropout rate (higher = higher dropout)
  r = 3,         # Shape parameter for dropout rate
  q=10,
  gamma=2,
  p=200,
  highbuyer=highbuyer,
  gamma_purch=0.05,
  gamma_att=0,
  start_date = as.Date("2005-01-01"),
  observation_period = 209  # One year observation period
)

simulated_data<-data.table(simulated_data)


# 2.Create a CLVTools object with estimation split after 104 weeks and plot the data using \texttt{clvdata}. In addition, use \texttt{SetStaticCovariates} with the static covariate ``highbuyer''.
clv.sim <- clvdata(simulated_data,  
                           date.format="ymd", 
                           time.unit = "week",
                           estimation.split = 104, #156,
                           name.id = "Id",
                           name.date = "Date",
                           name.price = "Price")

plot(clv.sim)


# Problem: Some customers do not have transactions
covariates<-simulated_data[, .(highbuyer = unique(highbuyer)), by = Id]

clv.data.sim.cov  <-SetStaticCovariates(clv.sim,
                    data.cov.life  = covariates,
                    names.cov.life = "highbuyer",
                    data.cov.trans = covariates,
                    names.cov.trans = "highbuyer",
                    name.id = "Id")

summary(clv.data.sim.cov)

# 3.Fit 2 models: One without covariates (the standard PNBD/GG) and one with the highbuyer covariate in the transaction and attrition process. Are the parameters for $\gamma_{purch}$, $\gamma_{att}$ significant? Given the true values of $\gamma_{purch}=0.05$, $\gamma_{att}=0$, used in the simulation, should they be significant?\\
# Bonus Question: Using an a $95\%$ confidence interval based on the normal approximation: $\hat{\gamma} \pm \sqrt{Var(\hat{\gamma})}\cdot 1.96$, do the confidence intervals include the true value?
est.pnbd <- latentAttrition(
  family = pnbd,
  data = clv.sim,
  optimx.args = list(method = "Nelder-Mead"))


est.pnbd.static <- latentAttrition(
  formula =~ highbuyer|highbuyer  ,
  family = pnbd,
  data = clv.data.sim.cov,
  optimx.args = list(method = "Nelder-Mead"))


summary(est.pnbd)
summary(est.pnbd.static)

## life.highbuyer (gamma_{app}) is not signficantly different from zero, which is what we would hope, as the true value is zero.
## This in turn also means that the 95% confidence interval includes 0, the true value.
## At the same time from the output of summary(est.pnbd.static), we can construct the following normal approximation of the confidence interval of trans.highbuyer:

#gamma_{puch} +/- \sqrt{Var(gamma_{puch})}*1.96
0.2553 + 0.0836*1.96
0.2553 - 0.0836*1.96

# In this case the confidence interval does actually not include the true value! This shows the difficulty of estimation for 500 customers.

##Predict the GG model
est.gg <- spending(family = gg, data = clv.data.sim.cov)


# 4.Predict with both models 104 weeks into the future. Which one has better RMSE? Which one is preferred by AIC?

h<-104
dt.pred <- predict(est.pnbd, predict.spending = est.gg,prediction.end=h)
dt.pred.static <- predict(est.pnbd.static, predict.spending = est.gg,prediction.end=h)


rmse <- function(x, y){sqrt(mean((x - y)^2))}

#RMSE using the best model
rmse(dt.pred$actual.period.spending, dt.pred$predicted.period.spending)
rmse(dt.pred.static$actual.period.spending, dt.pred.static$predicted.period.spending)

## Interestingly, both models have around the same error, even though AIC is lower for the correct model with covariates.



##############
##Exercise 3: Dataset with covariates
#############


##load packages
library(CLVTools)
library(data.table)

## Load the data.
mydata<-fread("data/electronics.csv")

## 1. Create a CLVTools object with estimation split after 156 weeks and plot the data. What is noteworthy?

clv.electronics <- clvdata(mydata,  
                           date.format="ymd", 
                           time.unit = "week",
                           estimation.split = 156, #156,
                           name.id = "Id",
                           name.date = "Date",
                           name.price = "Price")

plot(clv.electronics)
# Here we see strong seasonality!

## 2. Generate a table of covariates that includes Id and Cov.Date. The table should look like in the lecture: Cov.Date is the start of each week (either sunday or monday), 
## from the week including the first transaction up to the week including the last transaction, while Id is just copied as many times as they are weeks (237). We give some example code on how to achieve this efficiently with data.table.
## Then add "Gender" and "Income" to this table, again each value copied as many times as there are Id's and weeks (as there is only one value per Id)

library(data.table)
library(lubridate)

# Create the new data table ## Hint
cov_dates <- mydata[, {
  # Get the first and last dates
  first_date <- clv.electronics@clv.time@timepoint.estimation.start
  last_date <- clv.electronics@clv.time@timepoint.holdout.end
  
  # Find the Sunday of the week containing first_date
  # wday() returns 1 for Sunday, 2 for Monday, etc.
  first_sunday <- first_date - (wday(first_date) - 1)
  
  # Find the Sunday of the week containing last_date
  last_sunday <- last_date - (wday(last_date) - 1)
  
  # Generate sequence of Sundays
  sundays <- seq(from = first_sunday, to = last_sunday, by = "week")
  
  # Return as data table
  list(Cov.Date = sundays)
}, by = Id]



covariates <- mydata[, .(Gender = first(Gender),Income = first(Income)), by = Id][cov_dates, on = "Id"]

# Create a factor variable (not absolutely necessary)
covariates$Gender<-as.factor(covariates$Gender)

## 3. Having generated the table covariates and filled it with static covariates available in this data set, also generate dynamic regression covariates as done in the lecture.
##   Choose K=3, without the holiday covariate we build in the lecture.


# (2) Fourier terms for dynamic regression
# Set K (number of Fourier pairs)
K <- 3  # You can adjust this

# Create time index t (starts at 0)
covariates[, t := 0:(.N-1), by = Id]

# Generate Fourier terms for each k from 1 to K
for(k in 1:K) {
  # Sine term
  covariates[, paste0("sin_", k) := sin(2 * pi * k * t / 52)]
  
  # Cosine term
  covariates[, paste0("cos_", k) := cos(2 * pi * k * t / 52)]
}




## 4. Estimate 3 PNBD/GG models: First one without any covariates (est.pnbd), the second one with only the static covariates Gender and Income (est.pnbd.static) and the third one with all available covariates (est.pnbd.K3)
## (if this takes too long, simply load the file "est_pnbd_K3.Rdata" and "est_pnbd_static.Rdata").
## Plot all the models. Which one looks best? Which one has the best AIC?
## Hint: Due to a bug, the plotting function struggles with unequal periods, so you need to use something like plot(est.pnbd, prediction.end=clv.electronics@clv.time@timepoint.holdout.end-weeks(1)).

##Create object
nam<-c("Income", "Gender", "sin_1", "cos_1", "sin_2", "cos_2", "sin_3", "cos_3")




clv.dynamic <- SetDynamicCovariates(
  clv.data = clv.electronics,
  data.cov.life = covariates,
  data.cov.trans = covariates,
  names.cov.life =nam,
  names.cov.trans = nam,
  name.id = "Id",
  name.date = "Cov.Date"
)


est.pnbd <- latentAttrition(
  family = pnbd,
  data = clv.electronics,
  optimx.args = list(method = "Nelder-Mead"))


est.pnbd.static <- latentAttrition(
  formula =~ Income + GenderM|Income + GenderM  ,
  family = pnbd,
  data = clv.dynamic,
  optimx.args = list(method = "Nelder-Mead"))


est.pnbd.K3 <- latentAttrition(
  formula =~ Income + GenderM|Income + GenderM + sin_1+cos_1 + sin_2 + cos_2 + sin_3 + cos_3  ,
  family = pnbd,
  data = clv.dynamic,
  optimx.args = list(method = "Nelder-Mead"))

##Generate Plots
plot(est.pnbd, prediction.end=clv.electronics@clv.time@timepoint.holdout.end-weeks(1))
plot(est.pnbd.static, prediction.end=clv.electronics@clv.time@timepoint.holdout.end-weeks(1))
plot(est.pnbd.K3, prediction.end=clv.electronics@clv.time@timepoint.holdout.end-weeks(1))

##Generate AIC:
summary(est.pnbd)
#AIC=33723.6932 
summary(est.pnbd.static)
#AIC=33704.7746 
summary(est.pnbd.K3)
#AIC=33276.0450

##Predict the GG model separately
est.gg <- spending(family = gg, data = clv.electronics)

##In both AIC + the plots, the biggest model est.pnbd.K3 is clearly the best

## 5. Predict with all 3 models h=52 weeks into the future. Which model is the best? Does it align with the graph/AIC?

#Prediction
h<-52
dt.pred <- predict(est.pnbd, predict.spending = est.gg,prediction.end=h)
dt.pred.static <- predict(est.pnbd.static, predict.spending = est.gg,prediction.end=h)
dt.pred.cov <- predict(est.pnbd.K3, predict.spending = est.gg,prediction.end=h)


rmse <- function(x, y){sqrt(mean((x - y)^2))}

#RMSE using the best model
rmse(dt.pred$actual.period.spending, dt.pred$predicted.period.spending)
rmse(dt.pred.static$actual.period.spending, dt.pred.static$predicted.period.spending)
rmse(dt.pred.cov$actual.period.spending, dt.pred.cov$predicted.period.spending)


## The best model is est.pnbd.K3 which aligns with the graphical and AIC analysis! 
## However, surprisingly the difference in RMSE is relatively small.




