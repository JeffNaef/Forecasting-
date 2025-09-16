# White Noise Processes
# Generate and plot univariate and bivariate white noise

library(ggplot2)
library(gridExtra)

# Set parameters
n <- 200  # number of observations
set.seed(123)  # for reproducibility

# 1. Univariate white noise: N(0,1)
wn_univariate <- rnorm(n, mean = 0, sd = 1)

# 2. Bivariate white noise: N(0, Sigma) where Sigma = [1, 0.8; 0.8, 1]
library(MASS)
mu <- c(0, 0)
Sigma <- matrix(c(1, 0.8, 0.8, 1), nrow = 2)
wn_bivariate <- mvrnorm(n, mu = mu, Sigma = Sigma)

# Create plots
# Univariate plot
p1 <- ggplot(data.frame(time = 1:n, value = wn_univariate), 
             aes(x = time, y = value)) +
  geom_line(color = "blue", alpha = 0.7) +
  geom_point(color = "blue", size = 0.5) +
  labs(title = "Univariate White Noise N(0,1)",
       x = "Time", y = "Value") +
  theme_minimal()

# Bivariate plots
p2 <- ggplot(data.frame(time = 1:n, X1 = wn_bivariate[,1]), 
             aes(x = time, y = X1)) +
  geom_line(color = "red", alpha = 0.7) +
  geom_point(color = "red", size = 0.5) +
  labs(title = "Bivariate White Noise - Series 1",
       x = "Time", y = "X1") +
  theme_minimal()

p3 <- ggplot(data.frame(time = 1:n, X2 = wn_bivariate[,2]), 
             aes(x = time, y = X2)) +
  geom_line(color = "green", alpha = 0.7) +
  geom_point(color = "green", size = 0.5) +
  labs(title = "Bivariate White Noise - Series 2",
       x = "Time", y = "X2") +
  theme_minimal()

# Scatter plot showing correlation
p4 <- ggplot(data.frame(X1 = wn_bivariate[,1], X2 = wn_bivariate[,2]), 
             aes(x = X1, y = X2)) +
  geom_point(alpha = 0.6, color = "purple") +
  geom_smooth(method = "lm", se = TRUE, color = "orange") +
  labs(title = "Bivariate White Noise Correlation",
       x = "X1", y = "X2") +
  theme_minimal()

# Arrange plots
grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

# Print correlation matrix
cat("Correlation matrix of bivariate white noise:\n")
print(cor(wn_bivariate))

# Print some summary statistics
cat("\nUnivariate white noise summary:\n")
print(summary(wn_univariate))

cat("\nBivariate white noise summary:\n")
print(summary(wn_bivariate))