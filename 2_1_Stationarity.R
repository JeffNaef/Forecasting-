# Illustrating Strong and Weak Stationarity Concepts
# Load required libraries
library(ggplot2)
# install.packages("gridExtra")
library(gridExtra)
library(dplyr)

# Set seed for reproducibility
set.seed(42)

# ============================================================================
# STRONG STATIONARITY ILLUSTRATION
# ============================================================================

# Function to simulate stationary AR(1) process
simulate_stationary_ar1 <- function(n, phi, sigma = 1) {
  # Ensure stationarity |phi| < 1
  if (abs(phi) >= 1) stop("phi must satisfy |phi| < 1 for stationarity")
  
  y <- numeric(n)
  y[1] <- rnorm(1, 0, sigma / sqrt(1 - phi^2))  # Start from stationary distribution
  
  for (t in 2:n) {
    y[t] <- phi * y[t-1] + rnorm(1, 0, sigma)
  }
  return(y)
}

# Simulate a strongly stationary process (AR(1) with |phi| < 1)
n <- 800
phi <- 0.7
ts1 <- simulate_stationary_ar1(n, phi)

# Extract different segments of the same length to show distributional similarity
segment_length <- 200
segments <- list(
  "Segment 1 (t=1-200)" = ts1[1:segment_length],
  "Segment 2 (t=201-400)" = ts1[201:(200+segment_length)],
  "Segment 3 (t=401-600)" = ts1[401:(400+segment_length)],
  "Segment 4 (t=601-800)" = ts1[601:n]
)

# Create data frame for plotting
segment_data <- do.call(rbind, lapply(names(segments), function(name) {
  data.frame(
    time = 1:length(segments[[name]]),
    value = segments[[name]],
    segment = name
  )
}))

# Plot 1: Time series segments showing strong stationarity
p1 <- ggplot(segment_data, aes(x = time, y = value, color = segment)) +
  geom_line(size = 0.8) +
  facet_wrap(~segment, ncol = 2, scales = "free_x") +
  labs(title = "Strong Stationarity: AR(1) Process Segments",
       subtitle = paste("φ =", phi, "- Different time segments have same distribution"),
       x = "Time", y = "Value") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(size = 10))

p1

# Plot 2: Histograms showing distributional similarity
p2 <- ggplot(segment_data, aes(x = value, fill = segment)) +
  geom_histogram(alpha = 0.7, bins = 15, position = "identity") +
  facet_wrap(~segment, ncol = 2) +
  labs(title = "Marginal Distributions of Each Segment",
       subtitle = "Similar distributions across different time periods",
       x = "Value", y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none")


p2



# ============================================================================
# WEAK STATIONARITY ILLUSTRATION
# ============================================================================

# Function to compute sample autocovariance
compute_autocovariance <- function(x, max_lag = 20) {
  n <- length(x)
  x_centered <- x - mean(x)
  
  autocov <- numeric(max_lag + 1)
  for (lag in 0:max_lag) {
    if (lag == 0) {
      autocov[lag + 1] <- sum(x_centered^2) / n
    } else {
      autocov[lag + 1] <- sum(x_centered[1:(n-lag)] * x_centered[(lag+1):n]) / n
    }
  }
  return(autocov)
}

# Simulate multiple realizations of the same AR(1) process
n_realizations <- 4
n_obs <- 600
max_lag <- 15

# Store autocovariances for each realization
autocov_data <- data.frame()
ts_data <- data.frame()

for (i in 1:n_realizations) {
  # Generate realization
  ts_real <- simulate_stationary_ar1(n_obs, phi)
  
  # Store time series data
  ts_temp <- data.frame(
    time = 1:n_obs,
    value = ts_real,
    realization = paste("Realization", i)
  )
  ts_data <- rbind(ts_data, ts_temp)
  
  # Compute autocovariance
  autocov <- compute_autocovariance(ts_real, max_lag)
  
  # Store autocovariance data
  autocov_temp <- data.frame(
    lag = 0:max_lag,
    autocovariance = autocov,
    realization = paste("Realization", i)
  )
  autocov_data <- rbind(autocov_data, autocov_temp)
}

# Theoretical autocovariance for AR(1)
theoretical_autocov <- function(lag, phi, sigma = 1) {
  (sigma^2 / (1 - phi^2)) * phi^abs(lag)
}

theoretical_data <- data.frame(
  lag = 0:max_lag,
  autocovariance = sapply(0:max_lag, function(l) theoretical_autocov(l, phi)),
  type = "Theoretical"
)

# Plot 3: Multiple realizations showing consistent mean
p3 <- ggplot(ts_data, aes(x = time, y = value, color = realization)) +
  geom_line(alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
  facet_wrap(~realization, ncol = 2) +
  labs(title = "Weak Stationarity: Multiple Realizations of AR(1)",
       subtitle = "Constant mean (≈ 0) across all realizations and time",
       x = "Time", y = "Value") +
  theme_minimal() +
  theme(legend.position = "none")

# Plot 4: Autocovariance functions showing dependence only on lag
p4 <- ggplot(autocov_data, aes(x = lag, y = autocovariance, color = realization)) +
  geom_line(size = 1, alpha = 0.7) +
  geom_point(size = 2, alpha = 0.7) +
  geom_line(data = theoretical_data, aes(x = lag, y = autocovariance), 
            color = "black", size = 1.5, linetype = "dashed", inherit.aes = FALSE) +
  labs(title = "Autocovariance Functions: Empirical vs Theoretical",
       subtitle = "Autocovariance depends only on lag |s-t|, not absolute time",
       x = "Lag", y = "Autocovariance",
       color = "Realization") +
  theme_minimal() +
  theme(legend.position = "bottom")

p3
p4


# ============================================================================
# COMPARISON: NON-STATIONARY PROCESS
# ============================================================================

# Generate a non-stationary process (random walk)
set.seed(123)
random_walk <- cumsum(rnorm(n_obs))

# Generate a trend-stationary process
trend_process <- 0.02 * (1:n_obs) + simulate_stationary_ar1(n_obs, 0.5)

# Create comparison data
comparison_data <- data.frame(
  time = rep(1:n_obs, 3),
  value = c(ts1[1:n_obs], random_walk, trend_process),
  process = rep(c("Stationary AR(1)", "Random Walk", "Trend + AR(1)"), each = n_obs)
)

# Plot 5: Comparison of stationary vs non-stationary processes
p5 <- ggplot(comparison_data, aes(x = time, y = value, color = process)) +
  geom_line(size = 0.8) +
  facet_wrap(~process, scales = "free_y", ncol = 1) +
  labs(title = "Stationary vs Non-Stationary Processes",
       subtitle = "Only the AR(1) maintains constant statistical properties over time",
       x = "Time", y = "Value") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(size = 12))


p5

# Display all plots
print("=== STRONG STATIONARITY ===")
print(p1)
print(p2)

print("=== WEAK STATIONARITY ===")
print(p3)
print(p4)

print("=== COMPARISON ===")
print(p5)

# Summary statistics to verify weak stationarity properties
cat("\n=== WEAK STATIONARITY VERIFICATION ===\n")
cat("Sample means across realizations:\n")
for (i in 1:n_realizations) {
  real_data <- ts_data[ts_data$realization == paste("Realization", i), "value"]
  cat(sprintf("Realization %d: Mean = %.3f, Variance = %.3f\n", 
              i, mean(real_data), var(real_data)))
}

cat("\nTheoretical values for AR(1) with φ =", phi, ":\n")
cat(sprintf("Mean = 0 (exact)\n"))
cat(sprintf("Variance = %.3f\n", 1/(1-phi^2)))
cat(sprintf("First-order autocorrelation = %.3f\n", phi))

