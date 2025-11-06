# ============================================================================
# COMPLETE DYNAMIC FACTOR MODEL EXAMPLE USING dfms PACKAGE
# Implements the DFM with VAR(p) factors estimated via EM algorithm
# ============================================================================

# Install and load required packages
# install.packages("dfms")
library(dfms)

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

Phi2 <- matrix(c(0.2, 0.05, 0.0,
                 0.05, 0.2, 0.05,
                 0.0, 0.05, 0.15), r, r)

# Initialize factors
factors <- matrix(0, n_time, r)
factors[1,] <- rnorm(r)
factors[2,] <- rnorm(r)

# Generate factors with VAR(2) dynamics
for(t in 3:n_time) {
  factors[t,] <- Phi1 %*% factors[t-1,] + Phi2 %*% factors[t-2,] + rnorm(r, sd = 0.5)
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

cat("\n=== FITTING DYNAMIC FACTOR MODEL ===\n")
cat("Number of series:", n_series, "\n")
cat("Number of time periods:", nrow(data_diff), "\n")
cat("Number of factors (r):", r, "\n")
cat("VAR lag order (p):", p, "\n\n")

# Fit DFM with EM algorithm
# em.method options:
# - "BM": Banbura & Modugno (2014) - handles arbitrary missing data patterns
# - "DGR": Doz, Giannone & Reichlin (2012) - faster but less flexible
# - "none": Two-step estimation (very fast, no EM iterations)

model <- DFM(data_diff, 
             r = r,                    # number of factors
             p = p,                    # VAR lag order
             em.method = "BM",         # EM algorithm
             verbose = TRUE)           # show convergence

# ============================================================================
# PART 3: EXAMINE RESULTS
# ============================================================================

cat("\n=== MODEL SUMMARY ===\n")
print(summary(model))

# View factor loadings (C matrix)
cat("\n=== FACTOR LOADINGS (First 10 series) ===\n")
print(head(model$C, 10))

# View VAR coefficients for factors
cat("\n=== VAR TRANSITION MATRIX (A) ===\n")
cat("This contains [Phi_1, Phi_2, ..., Phi_p] stacked horizontally\n")
print(model$A)

# Extract estimated factors
factors_estimated <- fitted(model, type = "factors")
cat("\n=== ESTIMATED FACTORS (First 5 observations) ===\n")
print(head(factors_estimated, 5))

# Model fit statistics
cat("\n=== MODEL FIT ===\n")
cat("Log-likelihood:", model$loglik, "\n")
cat("BIC:", BIC(model), "\n")
cat("AIC:", AIC(model), "\n")

# ============================================================================
# PART 4: VISUALIZATIONS
# ============================================================================

cat("\n=== CREATING VISUALIZATIONS ===\n")


# Plot 3: Custom plot - Compare true vs estimated factors (if using simulated data)
if(exists("factors")) {))
  for(i in 1:r) {
    # Align estimated factors with true factors (may need sign/scale adjustment)
    plot(factors[-1, i], type = "l", col = "blue", lwd = 2,
         main = paste("Factor", i, ": True (blue) vs Estimated (red)"),
         ylab = "Value", xlab = "")
    lines(factors_estimated[, i], col = "red", lwd = 2, lty = 2)
    legend("topleft", legend = c("True", "Estimated"), 
           col = c("blue", "red"), lty = c(1, 2), lwd = 2)
  }
}

fitted_values <- fitted(model, type = "data")
par(mfrow = c(2, 2))
for(i in 1:4) {
  plot(data_diff[, i], type = "l", col = "black",
       main = paste("Series", i), ylab = "Value", xlab = "Time")
  lines(fitted_values[, i], col = "red", lty = 2, lwd = 2)
  legend("topleft", legend = c("Actual", "Fitted"), 
         col = c("black", "red"), lty = c(1, 2), lwd = c(1, 2))
}
cat("Saved: fitted_values.pdf\n")

# ============================================================================
# PART 5: MODEL DIAGNOSTICS
# ============================================================================

cat("\n=== MODEL DIAGNOSTICS ===\n")

# Extract residuals
residuals <- resid(model)

# Check for remaining autocorrelation in residuals
pdf("/mnt/user-data/outputs/residual_diagnostics.pdf", width = 10, height = 8)
par(mfrow = c(2, 3))
for(i in 1:min(6, n_series)) {
  acf(na.omit(residuals[, i]), main = paste("ACF - Series", i))
}
dev.off()
cat("Saved: residual_diagnostics.pdf\n")

# Summary statistics of residuals
cat("\nResidual summary (first 5 series):\n")
print(summary(residuals[, 1:5]))

# ============================================================================
# PART 6: FORECASTING
# ============================================================================

cat("\n=== FORECASTING ===\n")

# Forecast h steps ahead
h <- 20  # forecast horizon

 forecast_result <- predict(model, h = h, 
                            output = c("data", "factors"))
# 
# cat("Forecasted", h, "periods ahead\n")
# 
# # Plot forecasts for factors
# par(mfrow = c(r, 1), mar = c(3, 4, 2, 1))
# for(i in 1:r) {
#   # Historical factors
#   plot(factors_estimated[, i], type = "l", 
#        xlim = c(1, nrow(factors_estimated) + h),
#        main = paste("Factor", i, "- Historical and Forecast"),
#        ylab = "Value", xlab = "Time")
#   # Add forecast
#   lines((nrow(factors_estimated) + 1):(nrow(factors_estimated) + h),
#         forecast_result$F_fcst[, i], col = "red", lwd = 2)
#   abline(v = nrow(factors_estimated), lty = 2, col = "gray")
#   legend("topleft", legend = c("Historical", "Forecast"), 
#          col = c("black", "red"), lty = 1, lwd = 2)
# }
# 
# # Plot forecasts for selected series
# par(mfrow = c(2, 2))
# for(i in 1:4) {
#   # Historical data
#   plot(data_diff[, i], type = "l",
#        xlim = c(1, nrow(data_diff) + h),
#        main = paste("Series", i, "- Historical and Forecast"),
#        ylab = "Value", xlab = "Time")
#   # Add forecast
#   lines((nrow(data_diff) + 1):(nrow(data_diff) + h),
#         forecast_result$X_fcst[, i], col = "red", lwd = 2)
#   abline(v = nrow(data_diff), lty = 2, col = "gray")
#   legend("topleft", legend = c("Historical", "Forecast"), 
#          col = c("black", "red"), lty = 1, lwd = 2)
#}
















# ============================================================================
# SIMPLE PREDICTION INTERVALS FOR DFM
# Simulate N realizations forward from the fitted model
# ============================================================================


# ============================================================================
# SIMPLE FUNCTION: Simulate from fitted DFM
# ============================================================================

simulate_dfm <- function(model, h = 12, n_sim = 1000) {
  # Extract model parameters
  C <- model$C        # Factor loadings (n x r)
  A <- model$A                  # VAR transition matrix (r x r*p)
  Q <- model$Q                  # Factor innovation covariance (r x r)
  R <- model$R                  # Observation error covariance (n x n) or (n,)
  
  n_series <- nrow(C)
  r <- ncol(C)
  p <- ncol(A) / r
  
  # Get last p observations of factors for initial conditions
  factors_hist <- fitted(model, type = "factors")
  n_obs <- nrow(factors_hist)
  last_factors <- factors_hist[(n_obs - p + 1):n_obs, , drop = FALSE]
  
  # Storage for simulations
  sim_factors <- array(NA, dim = c(h, r, n_sim))
  sim_data <- array(NA, dim = c(h, n_series, n_sim))
  
  # Simulate
  cat("Simulating", n_sim, "paths...\n")
  
  for(i in 1:n_sim) {
    # Initialize factors with historical values
    factors <- rbind(last_factors, matrix(0, h, r))
    
    # Simulate factors forward
    for(t in (p + 1):(p + h)) {
      # VAR prediction: F_t = Phi_1 * F_{t-1} + ... + Phi_p * F_{t-p}
      factor_mean <- rep(0, r)
      for(lag in 1:p) {
        Phi_lag <- A[, ((lag - 1) * r + 1):(lag * r), drop = FALSE]
        factor_mean <- factor_mean + Phi_lag %*% factors[t - lag, ]
      }
      
      # Add innovation: u_t ~ N(0, Q)
      factors[t, ] <- factor_mean + MASS::mvrnorm(1, mu = rep(0, r), Sigma = Q)
    }
    
    # Extract forecast period
    sim_factors[, , i] <- factors[(p + 1):(p + h), , drop = FALSE]
    
    # Simulate observations: y_t = C * F_t + e_t
    for(t in 1:h) {
      obs_mean <- C %*% sim_factors[t, , i]
      
      # Add observation error: e_t ~ N(0, R)
      if(is.matrix(R)) {
        obs_error <- MASS::mvrnorm(1, mu = rep(0, n_series), Sigma = R)
      } else {
        # If R is diagonal (stored as vector)
        obs_error <- rnorm(n_series, mean = 0, sd = sqrt(diag(R)))
      }
      
      sim_data[t, , i] <- obs_mean + obs_error
    }
    
    if(i %% 100 == 0) cat("  Completed", i, "simulations\n")
  }
  
  cat("Done!\n\n")
  
  return(list(
    factors = sim_factors,  # h x r x n_sim
    data = sim_data         # h x n_series x n_sim
  ))
}

# ============================================================================
# COMPUTE PREDICTION INTERVALS
# ============================================================================

compute_intervals <- function(simulations, conf_level = 0.95) {
  alpha <- 1 - conf_level
  lower_q <- alpha / 2
  upper_q <- 1 - alpha / 2
  
  # For factors
  factors_mean <- apply(simulations$factors, c(1, 2), mean)
  factors_lower <- apply(simulations$factors, c(1, 2), quantile, probs = lower_q)
  factors_upper <- apply(simulations$factors, c(1, 2), quantile, probs = upper_q)
  
  # For data
  data_mean <- apply(simulations$data, c(1, 2), mean)
  data_lower <- apply(simulations$data, c(1, 2), quantile, probs = lower_q)
  data_upper <- apply(simulations$data, c(1, 2), quantile, probs = upper_q)
  
  return(list(
    factors = list(mean = factors_mean, lower = factors_lower, upper = factors_upper),
    data = list(mean = data_mean, lower = data_lower, upper = data_upper),
    conf_level = conf_level
  ))
}


# ============================================================================
# STEP 1: Simulate from the model
# ============================================================================

n_sim <- 1000    # number of simulations

simulations <- simulate_dfm(model, h = h, n_sim = n_sim)

# ============================================================================
# STEP 2: Compute prediction intervals
# ============================================================================

intervals <- compute_intervals(simulations, conf_level = 0.95)

cat("Prediction intervals computed!\n")
cat("  - Forecast horizon:", h, "\n")
cat("  - Number of simulations:", n_sim, "\n")
cat("  - Confidence level:", intervals$conf_level * 100, "%\n\n")

# ============================================================================
# STEP 3: Visualize results
# ============================================================================

# Plot factors with prediction intervals
pdf("/mnt/user-data/outputs/simple_factor_intervals.pdf", width = 12, height = 8)
par(mfrow = c(r, 1), mar = c(3, 4, 2, 1))

factors_hist <- fitted(model, type = "factors")
n_hist <- nrow(factors_hist)

for(i in 1:r) {
  # Plot historical factors
  plot(1:n_hist, factors_hist[, i], type = "l", lwd = 2,
       xlim = c(max(1, n_hist - 50), n_hist + h),
       ylim = range(c(factors_hist[max(1, n_hist - 50):n_hist, i],
                      intervals$factors$lower[, i],
                      intervals$factors$upper[, i])),
       main = paste("Factor", i),
       xlab = "Time", ylab = "Value")
  
  # Add forecast mean
  lines((n_hist + 1):(n_hist + h), intervals$factors$mean[, i],
        col = "blue", lwd = 2)
  
  # Add prediction interval (shaded region)
  polygon(c((n_hist + 1):(n_hist + h), (n_hist + h):(n_hist + 1)),
          c(intervals$factors$lower[, i], rev(intervals$factors$upper[, i])),
          col = rgb(0, 0, 1, 0.2), border = NA)
  
  # Add interval bounds
  lines((n_hist + 1):(n_hist + h), intervals$factors$lower[, i],
        col = "blue", lty = 2)
  lines((n_hist + 1):(n_hist + h), intervals$factors$upper[, i],
        col = "blue", lty = 2)
  
  abline(v = n_hist, lty = 2, col = "gray")
  legend("topleft", 
         legend = c("Historical", "Forecast", "95% PI"),
         col = c("black", "blue", rgb(0, 0, 1, 0.2)),
         lty = c(1, 1, 1), lwd = c(2, 2, 10), bty = "n")
}
dev.off()
cat("Saved: simple_factor_intervals.pdf\n")

# Plot first 6 series with prediction intervals
pdf("/mnt/user-data/outputs/simple_series_intervals.pdf", width = 12, height = 10)
par(mfrow = c(3, 2), mar = c(3, 4, 2, 1))

n_hist_data <- nrow(data_diff)

for(i in 1:6) {
  # Plot historical data
  plot(1:n_hist_data, data_diff[, i], type = "l", lwd = 1.5,
       xlim = c(max(1, n_hist_data - 50), n_hist_data + h),
       ylim = range(c(data_diff[max(1, n_hist_data - 50):n_hist_data, i],
                      intervals$data$lower[, i],
                      intervals$data$upper[, i]), na.rm = TRUE),
       main = paste("Series", i),
       xlab = "Time", ylab = "Value")
  
  # Add forecast mean
  lines((n_hist_data + 1):(n_hist_data + h), intervals$data$mean[, i],
        col = "red", lwd = 2)
  
  # Add prediction interval (shaded region)
  polygon(c((n_hist_data + 1):(n_hist_data + h), (n_hist_data + h):(n_hist_data + 1)),
          c(intervals$data$lower[, i], rev(intervals$data$upper[, i])),
          col = rgb(1, 0, 0, 0.2), border = NA)
  
  # Add interval bounds
  lines((n_hist_data + 1):(n_hist_data + h), intervals$data$lower[, i],
        col = "red", lty = 2)
  lines((n_hist_data + 1):(n_hist_data + h), intervals$data$upper[, i],
        col = "red", lty = 2)
  
  abline(v = n_hist_data, lty = 2, col = "gray")
  legend("topleft", 
         legend = c("Historical", "Forecast", "95% PI"),
         col = c("black", "red", rgb(1, 0, 0, 0.2)),
         lty = c(1, 1, 1), lwd = c(1.5, 2, 10), bty = "n")
}
dev.off()
cat("Saved: simple_series_intervals.pdf\n")

# ============================================================================
# STEP 4: Save results to CSV
# ============================================================================

# Save factor intervals
factor_intervals_df <- data.frame(Horizon = 1:h)
for(i in 1:r) {
  factor_intervals_df[paste0("Factor", i, "_Mean")] <- intervals$factors$mean[, i]
  factor_intervals_df[paste0("Factor", i, "_Lower")] <- intervals$factors$lower[, i]
  factor_intervals_df[paste0("Factor", i, "_Upper")] <- intervals$factors$upper[, i]
}
write.csv(factor_intervals_df, "/mnt/user-data/outputs/simple_factor_intervals.csv", 
          row.names = FALSE)
cat("Saved: simple_factor_intervals.csv\n")

# Save series intervals (first 10 series)
for(i in 1:min(10, n_series)) {
  series_intervals_df <- data.frame(
    Horizon = 1:h,
    Mean = intervals$data$mean[, i],
    Lower = intervals$data$lower[, i],
    Upper = intervals$data$upper[, i]
  )
  write.csv(series_intervals_df, 
            paste0("/mnt/user-data/outputs/simple_series", i, "_intervals.csv"),
            row.names = FALSE)
}
cat("Saved: series intervals for first 10 series\n\n")

# ============================================================================
# BONUS: Access any quantile or probability
# ============================================================================

cat("=== BONUS: Working with simulations ===\n\n")

# Example 1: Get any percentile
series_idx <- 1
horizon <- 6
percentile_10 <- quantile(simulations$data[horizon, series_idx, ], probs = 0.10)
percentile_90 <- quantile(simulations$data[horizon, series_idx, ], probs = 0.90)

cat("For Series", series_idx, "at horizon", horizon, ":\n")
cat("  10th percentile:", round(percentile_10, 4), "\n")
cat("  90th percentile:", round(percentile_90, 4), "\n\n")

# Example 2: Probability that value exceeds a threshold
threshold <- 0
prob_exceed <- mean(simulations$data[horizon, series_idx, ] > threshold)
cat("  Probability of exceeding", threshold, ":", round(prob_exceed, 4), "\n\n")

# Example 3: Full distribution at a specific horizon
cat("Full distribution available as:\n")
cat("  simulations$data[horizon, series, 1:n_sim]\n")
cat("  simulations$factors[horizon, factor, 1:n_sim]\n\n")

cat("All done! Check /mnt/user-data/outputs/ for results.\n")





# ============================================================================
# PART 7: MODEL SELECTION (OPTIONAL)
# ============================================================================

cat("\n=== MODEL SELECTION ===\n")
cat("Comparing models with different numbers of factors...\n")

# Try different numbers of factors
r_candidates <- 1:5
bic_values <- numeric(length(r_candidates))
aic_values <- numeric(length(r_candidates))

for(i in seq_along(r_candidates)) {
  cat("Fitting model with r =", r_candidates[i], "...\n")
  temp_model <- DFM(data_diff, 
                    r = r_candidates[i], 
                    p = p,
                    em.method = "BM",
                    verbose = FALSE)
  aic_values[i] <- temp_model$loglik[length(temp_model$loglik)]
}

# Create comparison table
comparison <- data.frame(
  Factors = r_candidates,
  BIC = bic_values,
  AIC = aic_values
)
print(comparison)

# Find optimal number of factors
cat("\nOptimal number of factors (BIC):", r_candidates[which.min(bic_values)], "\n")
cat("Optimal number of factors (AIC):", r_candidates[which.min(aic_values)], "\n")

# Plot information criteria
pdf("/mnt/user-data/outputs/model_selection.pdf", width = 10, height = 6)
par(mfrow = c(1, 2))
plot(r_candidates, bic_values, type = "b", pch = 19,
     main = "BIC vs Number of Factors", xlab = "Number of Factors", ylab = "BIC")
plot(r_candidates, aic_values, type = "b", pch = 19, col = "blue",
     main = "AIC vs Number of Factors", xlab = "Number of Factors", ylab = "AIC")
dev.off()
cat("Saved: model_selection.pdf\n")
