# Harmonic Regression: Modeling Seasonality with Sin and Cos Functions
# ========================================================================

# Load required libraries
library(ggplot2)
library(gridExtra)

# Set seed for reproducibility
set.seed(123)

# Parameters
n <- 200              # Number of time points
s <- 12               # Period (e.g., 12 months for yearly seasonality)
b <- 0.5              # Trend coefficient
noise_sd <- 1        # Standard deviation of noise



# s = 12 for monthly data with yearly seasonality
# s = 4 for quarterly data with yearly seasonality
# s = 7 for daily data with weekly seasonality
# s = 52 for weekly data with yearly seasonality



# Generate time variable
t <- 1:n

# Create harmonic terms for different K values
K_values <- c(1, 2, 3)

# True seasonal component (using K=3 for illustration)
K_true <- 3
alpha_true <- c(3, -2, 1)
beta_true <- c(2, 1.5, -1)

# Generate true seasonal component
seasonal_true <- 0
for(k in 1:K_true) {
  seasonal_true <- seasonal_true + 
    alpha_true[k] * sin(2 * pi * k * t / s) + 
    beta_true[k] * cos(2 * pi * k * t / s)
}

# Generate synthetic time series: Y_t = trend + seasonality + noise
Y <- b * t + seasonal_true + rnorm(n, 0, noise_sd)
## We observe a trend and quite complicated seasonality!


data.frame(Y=Y, t=t) |>
  ggplot(aes(x=t, y=Y)) +
  geom_line()


# Function to create harmonic regressors and fit model
fit_harmonic <- function(Y, t, s, K) {
  # Create design matrix
  X <- matrix(1, nrow = length(t), ncol = 1 + 2*K + 1)
  X[, 2] <- t  # Trend
  
  for(k in 1:K) {
    X[, 2 + 2*k - 1] <- sin(2 * pi * k * t / s)  # s_k(t)
    X[, 2 + 2*k] <- cos(2 * pi * k * t / s)      # c_k(t)
  }
  
  # Fit linear model
  model <- lm(Y ~ X - 1)
  fitted_values <- fitted(model)
  
  # Calculate AIC
  aic <- AIC(model)
  
  list(model = model, fitted = fitted_values, aic = aic, X = X)
}

# Fit models with different K values
results <- list()
for(K in K_values) {
  results[[paste0("K", K)]] <- fit_harmonic(Y, t, s, K)
}

# Create comparison data frame
df_plot <- data.frame(
  Time = rep(t, length(K_values) + 1),
  Value = c(Y, sapply(results, function(x) x$fitted)),
  Series = factor(rep(c("Original", paste0("K = ", K_values)), 
                      each = length(t)),
                  levels = c("Original", paste0("K = ", K_values)))
)

# Plot 1: Original data with different K approximations
p1 <- ggplot(df_plot, aes(x = Time, y = Value, color = Series)) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = c("black", "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3")) +
  labs(title = "Harmonic Regression with Different K Values",
       subtitle = "Y_t = bt + Σ[α_k·sin(2πkt/s) + β_k·cos(2πkt/s)] + ε_t",
       x = "Time", y = "Value") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))

# Plot 2: Decomposition for K=3
fit_K3 <- results[["K3"]]
df_decomp <- data.frame(
  Time = rep(t, 3),
  Value = c(Y, b * t, fit_K3$fitted - b * t),
  Component = factor(rep(c("Original Series", "Trend (bt)", "Seasonal Component"), 
                         each = length(t)),
                     levels = c("Original Series", "Trend (bt)", "Seasonal Component"))
)

p2 <- ggplot(df_decomp, aes(x = Time, y = Value)) +
  geom_line(color = "#377EB8", linewidth = 0.7) +
  facet_wrap(~ Component, ncol = 1, scales = "free_y") +
  labs(title = "Decomposition with K = 3",
       x = "Time", y = "Value") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

# Plot 3: Individual harmonic components for K=3
K <- 3
df_harmonics <- data.frame(
  Time = rep(t, K * 2),
  Value = numeric(length(t) * K * 2),
  Harmonic = factor(rep(rep(1:K, each = length(t)), 2)),
  Type = factor(rep(c("sin", "cos"), each = length(t) * K))
)

coefs <- coef(fit_K3$model)
for(k in 1:K) {
  idx_sin <- which(df_harmonics$Harmonic == k & df_harmonics$Type == "sin")
  idx_cos <- which(df_harmonics$Harmonic == k & df_harmonics$Type == "cos")
  
  df_harmonics$Value[idx_sin] <- coefs[2 + 2*k - 1] * sin(2 * pi * k * t / s)
  df_harmonics$Value[idx_cos] <- coefs[2 + 2*k] * cos(2 * pi * k * t / s)
}

p3 <- ggplot(df_harmonics, aes(x = Time, y = Value, color = Type)) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ Harmonic, ncol = 1, 
             labeller = labeller(Harmonic = function(x) paste0("k = ", x))) +
  scale_color_manual(values = c("sin" = "#E41A1C", "cos" = "#377EB8"),
                     labels = c("cos" = "α_k·cos(2πkt/s)", "sin" = "β_k·sin(2πkt/s)")) +
  labs(title = "Individual Harmonic Components (K = 3)",
       x = "Time", y = "Contribution") +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"))

# Display plots
grid.arrange(p1, p2, ncol = 1)
print(p3)

# Print AIC comparison
cat("\n=== Model Comparison (AICc) ===\n")
aic_table <- data.frame(
  K = K_values,
  AIC = sapply(results, function(x) x$aic),
  Parameters = 2 + 2*K_values + 1  # intercept + trend + 2K harmonics + sigma^2
)
print(aic_table)
cat("\nBest model: K =", aic_table$K[which.min(aic_table$AIC)], 
    "(lowest AIC)\n")

##AIC manages to find the corect model!!

# Print coefficients for K=3
cat("\n=== Fitted Coefficients (K = 3) ===\n")
coef_K3 <- coef(fit_K3$model)
cat(sprintf("Trend (b): %.4f\n", coef_K3[2]))
for(k in 1:3) {
  cat(sprintf("α_%d (sin): %.4f, β_%d (cos): %.4f\n", 
              k, coef_K3[2 + 2*k - 1], k, coef_K3[2 + 2*k]))
}
