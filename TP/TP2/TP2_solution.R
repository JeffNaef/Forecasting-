# Install and load required packages
# install.packages("dCovTS")
library(dCovTS)

# Theoretical distance correlation formula from Székely et al. 
# MEASURING AND TESTING DEPENDENCE BY CORRELATION OF DISTANCES
theoretical_dcor <- function(rho) {
  numerator <- rho * asin(rho) + sqrt(1 - rho^2) - rho * asin(rho / 2) - sqrt(4 - rho^2) + 1
  denominator <- 1 + pi/3 - sqrt(3)
  return(sqrt(numerator / denominator))
}

set.seed(123)

# Simulation parameters
n <- 5000       # sample size
phi <- 0.7      # AR(1) parameter
sigma <- 1      # innovation standard deviation
max_lag <- 10   # maximum lag to compute

# Simulate AR(1) process
X <- arima.sim(n = n, model = list(ar = phi), sd = sigma)

# Compute empirical distance correlation for lags 0:max_lag
emp_dcor <- sapply(0:max_lag, function(h) {
  dcor(X[1:(n-h)], X[(1+h):n])
})

# Compute theoretical distance correlation using linear correlation rho = phi^h
theo_dcor <- sapply(0:max_lag, function(h) {
  theoretical_dcor(phi^h)
})

# Compare in a table
cmp <- data.frame(
  Lag = 0:max_lag,
  Empirical = round(emp_dcor, 4),
  Theoretical = round(theo_dcor, 4)
)
print(cmp)

# Plot both
plot(0:max_lag, emp_dcor, type = "b", col = "blue", pch = 19,
     xlab = "Lag", ylab = "Distance Correlation", 
     main = "Empirical vs Theoretical Distance Correlation (AR1)")
lines(0:max_lag, theo_dcor, type = "b", col = "red", pch = 17)
legend("topright", legend = c("Empirical", "Theoretical"), 
       col = c("blue", "red"), pch = c(19,17))
