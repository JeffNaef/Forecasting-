# ============================================================================
# COMPLETE DYNAMIC FACTOR MODEL EXAMPLE USING dfms PACKAGE
# Implements the DFM with VAR(p) factors estimated via EM algorithm
# ============================================================================

# Install and load required packages
# install.packages("dfms")
library(sparseDFM)

library(doSNOW)
library(fpp3)
library(forecast)
library(patchwork)
library(dCovTS)


# Optional: for data manipulation and visualization
# install.packages("ggplot2")
# install.packages("reshape2")
library(ggplot2)
library(reshape2)

# ============================================================================
# PART 1: SIMULATE DATA (Replace with your actual data)
# ============================================================================

set.seed(123)

# Parameters
n_series <- 50      # number of observed time series
n_time <- 200       # number of time periods
r <- 3              # number of common factors
p <- 2              # VAR lag order for factors

# Simulate factors following VAR(2) process
# F_t = Phi1 * F_{t-1} + Phi2 * F_{t-2} + u_t

# VAR coefficient matrices
Phi1 <- matrix(c(0.5, 0.1, 0.0,
                 0.1, 0.4, 0.1,
                 0.0, 0.1, 0.3), r, r)

# Initialize factors
factors <- matrix(0, n_time, r)
factors[1,] <- rnorm(r)

# Generate factors with VAR(2) dynamics
for(t in 3:n_time) {
  factors[t,] <- Phi1 %*% factors[t-1,]  + rnorm(r, sd = 0.5)
}

# Generate factor loadings (C matrix)
C <- matrix(runif(n_series * r, -1, 1), n_series, r)

# Generate observed data: y_t = C * F_t + e_t
idiosyncratic_error <- matrix(rnorm(n_series * n_time, sd = 0.3), n_time, n_series)
data_matrix <- factors %*% t(C) + idiosyncratic_error

# Add some missing values (optional, to show dfms handles them)
#missing_idx <- sample(1:(n_series * n_time), size = 0.05 * n_series * n_time)
#data_matrix[missing_idx] <- NA

# Take first differences (as required for stationarity)
data_diff <- diff(data_matrix)

# Name the series
colnames(data_diff) <- paste0("Series", 1:n_series)

# ============================================================================
# PART 2: FIT DYNAMIC FACTOR MODEL WITH dfms
# ============================================================================



# Prepare the data
# sparseDFM expects a matrix with time in rows, series in columns
Y <- data_diff  # Already in correct format from your simulation

# Split into training and test sets
n_train <- 180
n_test <- nrow(Y) - n_train

Y_train <- Y[1:n_train, ]
Y_test <- Y[(n_train+1):nrow(Y), ]

# ============================================
# 1. ESTIMATE THE MODEL
# ============================================

## First try to find the correct number of factors
tuneFactors(Y_train)

## It correctly identifies the number of factors to be 3!


# Estimate a sparse DFM with automatic factor selection
# The package uses penalized estimation for sparsity in loadings
fit <- sparseDFM(
  X = Y_train,
  r = 3              # number of factors (you can also let it choose)S
)

# View the results
summary(fit)

plot(fit, type="factor")
plot(fit, type="residual")
plot(fit, type="loading.heatmap")

## Check Distance Autocorrelation of Residuals:
resid<-residuals((fit))
mADCFplot(resid) ##Takes a while


# ============================================
# 5. FORECASTING WITH PREDICTION INTERVALS
# ============================================





# Function to generate prediction intervals via simulation
generate_prediction_intervals <- function(fit, h, n_sim = 1000, level = 0.95) {
  
  ##
  #Y_t=Lambda F_t + \epsilon_t
  #F_t=AF_{t-1} + u_t
  ##
  # Extract model components
  A <- fit$params$A          # VAR coefficients
  Lambda <- fit$params$Lambda  # p x r
  Sigma_u <- fit$params$Sigma_u    # 
  Sigma_epsilon <- fit$params$Sigma_epsilon    # 
  
  n_series <- nrow(Lambda)
  r <- ncol(Lambda)
  p <- fit$p  # VAR lag order
  
  # Storage for simulations
  Y_sim <- array(NA, dim = c(h, n_series, n_sim))
  
  FT1<-predict(fit)$F_hat ## E[F_{T+1}|\mathcal{F}_T]
  
  
  # Simulate from the DFM
  for(sim in 1:n_sim) {
    
    # Draw from factor T+1 by drawing from a Gaussian with N(E[F_{T+1}|\mathcal{F}_T], Sigma_u)
    F_sim <- matrix(NA, h, r)
    F_sim[1, ] <- FT1 + MASS::mvrnorm(1, mu = rep(0, r), Sigma = Sigma_u)
    
    #Simulate from Y_{T+1}
    Y_sim[1, , sim] <- Lambda %*% F_sim[1, ]+ MASS::mvrnorm(1, mu = rep(0, n_series), Sigma = diag(Sigma_epsilon))
    
    # Simulate factors forward using VAR
    for(t in 2:h) {

      # Simulate Factors
      F_sim[t, ] <- F_sim[t-1,] + MASS::mvrnorm(1, mu = rep(0, r), Sigma = Sigma_u)
      
      # Simulate observations
      Y_sim[t, , sim] <- Lambda %*% F_sim[t, ]+ MASS::mvrnorm(1, mu = rep(0, n_series), Sigma = diag(Sigma_epsilon))
      
    }
    
  
  }
  
  # Calculate prediction intervals
  alpha <- 1 - level
  lower <- apply(Y_sim, c(1, 2), quantile, probs = alpha/2, na.rm = TRUE)
  upper <- apply(Y_sim, c(1, 2), quantile, probs = 1 - alpha/2, na.rm = TRUE)
  median_forecast <- apply(Y_sim, c(1, 2), median, na.rm = TRUE)
  
  return(list(
    median = median_forecast,
    lower = lower,
    upper = upper,
    simulations = Y_sim
  ))
}

# Generate prediction intervals
set.seed(456)
pred_intervals <- generate_prediction_intervals(
  fit = fit,
  h = n_test,
  n_sim = 1000,
  level = 0.9
)

# Point forecasts (from predict method)
predictions <- predict(
  object = fit,
  h = n_test,
  X_new = NULL
)
Y_pred <- predictions$X_hat

# # ============================================
# # 6. EVALUATE COVERAGE OF PREDICTION INTERVALS
# # ============================================
# 
# # Check if true values fall within prediction intervals
# in_interval <- (Y_test >= pred_intervals$lower) & (Y_test <= pred_intervals$upper)
# coverage <- mean(in_interval, na.rm = TRUE)
# cat(sprintf("Empirical coverage of 90%% prediction intervals: %.1f%%\n", coverage * 100))
# 
# # Coverage by horizon
# coverage_by_horizon <- rowMeans(in_interval, na.rm = TRUE)
# plot(1:n_test, coverage_by_horizon, type = "l", 
#      ylim = c(0, 1),
#      main = "Prediction Interval Coverage by Horizon",
#      xlab = "Forecast Horizon", ylab = "Coverage Rate")
# abline(h = 0.90, col = "red", lty = 2)
# legend("bottomleft", legend = c("Empirical", "Nominal 90%"), 
#        col = c("black", "red"), lty = c(1, 2))

# ============================================
# 7. VISUALIZE FORECASTS WITH PREDICTION INTERVALS
# ============================================

# Plot forecasts for a few series with prediction intervals
series_to_plot <- c(1, 5, 10)

par(mfrow = c(length(series_to_plot), 1), mar = c(4, 4, 3, 1))
for(i in series_to_plot) {
  
  # Determine y-axis limits
  y_range <- range(c(Y_train[, i], Y_test[, i], 
                     pred_intervals$lower[, i], pred_intervals$upper[, i]),
                   na.rm = TRUE)
  
  # Plot training data
  plot(1:n_train, Y_train[, i], type = "l", 
       xlim = c(1, n_train + n_test),
       ylim = y_range,
       main = paste("Series", i, "- Forecasts with 90% Prediction Intervals"),
       xlab = "Time", ylab = "Value", col = "gray50")
  
  # Add forecast period separator
  abline(v = n_train, col = "gray30", lty = 3, lwd = 1.5)
  
  # Add prediction interval as shaded region
  time_forecast <- (n_train+1):(n_train+n_test)
  polygon(c(time_forecast, rev(time_forecast)),
          c(pred_intervals$lower[, i], rev(pred_intervals$upper[, i])),
          col = rgb(1, 0, 0, 0.2), border = NA)
  
  # Add actual test values
  lines(time_forecast, Y_test[, i], col = "black", lwd = 2)
  
  # Add point forecast (median or mean)
  lines(time_forecast, pred_intervals$median[, i], col = "red", lwd = 2, lty = 1)
  
  # Add legend
  legend("topleft", 
         legend = c("Training Data", "True Values", "Median Forecast", "90% PI"),
         col = c("gray50", "black", "red", rgb(1, 0, 0, 0.2)),
         lty = c(1, 1, 1, 1), lwd = c(1, 2, 2, 10),
         cex = 0.7, bg = "white")
}
par(mfrow = c(1, 1))




# ============================================
# 10. PROBABILITY FORECASTS
# ============================================

# Example: Probability that Series 1 exceeds a threshold at h=1
threshold <- 0.5
prob_exceed <- mean(pred_intervals$simulations[1, 1, ] > threshold)
cat(sprintf("\nP(Series 1 > %.2f at h=1) = %.3f\n", threshold, prob_exceed))

# Predictive density for Series 1 at h=1
hist(pred_intervals$simulations[1, 1, ], breaks = 50, freq = FALSE,
     main = "Predictive Density for Series 1 at h=1",
     xlab = "Value", col = "lightblue", border = "white")
abline(v = Y_test[1, 1], col = "red", lwd = 2, lty = 2)
legend("topright", legend = "True Value", col = "red", lty = 2, lwd = 2)
