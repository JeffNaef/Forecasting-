# Distance Autocorrelation vs Traditional Autocorrelation Demo
# Install required packages if needed
# install.packages(c("dCovTS", "forecast"))

library(patchwork)
library(dCovTS)
library(doSNOW)
library(forecast)

set.seed(123)
n <- 300

# Example 1: Nonlinear dependence that ACF misses
# Create a time series with quadratic dependence
t <- 1:n
x1 <- rnorm(n)
# Create nonlinear dependence: X_t depends on X_{t-1}^2
y1 <- numeric(n)
y1[1] <- x1[1]
for(i in 2:n) {
  y1[i] <- 0.7 * y1[i-1]^2 + 0.3 * rnorm(1)
}


# Example 2: Sine wave transformation (nonlinear but dependent)
# Create a series where X_t = sin(X_{t-1}) + noise
y2 <- numeric(n)
y2[1] <- rnorm(1)
for(i in 2:n) {
  y2[i] <- 0.8 * sin(y2[i-1]) + 0.4 * rnorm(1)
}



# Example 3: ARCH(1,1) model - volatility clustering
# X_t = σ_t * ε_t, where σ_t^2 = α_0 + α_1 * X_{t-1}^2
# This creates dependence in the squared values (volatility) even if X_t appears uncorrelated
alpha0 <- 0.1
alpha1 <- 0.8
sigma2 <- numeric(n)
y3_arch <- numeric(n)

sigma2[1] <- alpha0 / (1 - alpha1)  # Unconditional variance
y3[1] <- sqrt(sigma2[1]) * rnorm(1)

for(i in 2:n) {
  sigma2[i] <- alpha0 + alpha1 * y3[i-1]^2
  y3[i] <- sqrt(sigma2[i]) * rnorm(1)
}


# Visualization comparison
par(mfrow = c(3, 3))

# Plot the series

plot(y1, type = "l", main = expression(Y[t] == 0.7 * Y[t-1]^2 + 0.3 * epsilon[t]), xlab = "Time", ylab = "Value", cex.main = 2)
plot(y2, type = "l", main = expression(Y[t] == 0.8 * sin(Y[t-1]) + 0.4 * epsilon[t]), xlab = "Time", ylab = "Value", cex.main = 2)
plot(y3, type = "l", main = expression(atop(Y[t] == sigma[t] * epsilon[t], sigma[t]^2 == 0.1 + 0.8 * Y[t-1]^2)) , xlab = "Time", ylab = "Value", cex.main = 2)




# 

# Plot traditional ACF
acf(y1, main = "")
acf(y2, main = "")
acf(y3, main = "")

# Plot distance autocorrelation
ADCFplot(y1, main="")
ADCFplot(y2, main="")
ADCFplot(y3, main="")


# Summary comparison
cat("\n=== SUMMARY ===\n")
cat("Distance autocorrelation detects dependence that traditional ACF misses:\n")
cat("- Quadratic series: Distance ACF =", round(dcor_result1[2], 3), 
    "vs Traditional ACF =", round(acf_result1$acf[2], 3), "\n")
cat("- Sine series: Distance ACF =", round(dcor_result2[2], 3), 
    "vs Traditional ACF =", round(acf_result2$acf[2], 3), "\n")
cat("- ARCH series: Distance ACF =", round(dcor_result3[2], 3), 
    "vs Traditional ACF =", round(acf_result3$acf[2], 3), "\n")