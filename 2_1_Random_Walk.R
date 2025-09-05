# Random Walk Forecasting Impossibility Simulation
# This script demonstrates why it's impossible to forecast a random walk
# by showing one historical path and multiple possible future realizations

# Load required libraries
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Parameters
n_historical <- 50    # Number of historical time steps
n_future <- 100         # Number of future time steps to simulate
n_simulations <- 1000  # Number of future realizations

# Generate one historical random walk
historical_steps <- rnorm(n_historical, mean = 0, sd = 1)
historical_walk <- cumsum(c(0, historical_steps))
historical_time <- 0:n_historical

# Starting point for future simulations (end of historical walk)
start_value <- historical_walk[length(historical_walk)]
future_time_start <- max(historical_time)

# Generate multiple future realizations
future_walks <- matrix(nrow = n_future + 1, ncol = n_simulations)
future_walks[1, ] <- start_value  # All start from the same point

for (i in 1:n_simulations) {
  future_steps <- rnorm(n_future, mean = 0, sd = 1)
  future_walks[, i] <- cumsum(c(start_value, future_steps))
}

# Create time vector for future
future_time <- future_time_start:(future_time_start + n_future)

# Prepare data for plotting
# Historical data
historical_data <- data.frame(
  time = historical_time,
  value = historical_walk,
  type = "Historical"
)

# Future simulations data
future_data <- data.frame(
  time = rep(future_time, n_simulations),
  value = as.vector(future_walks),
  simulation = rep(1:n_simulations, each = n_future + 1),
  type = "Future Realizations"
)

# Create the plot
p <- ggplot() +
  # Plot future realizations first (so they appear behind the historical line)
  geom_line(data = future_data, 
            aes(x = time, y = value, group = simulation), 
            alpha = 0.1, color = "red", size = 0.3) +
  
  # Add vertical line to separate historical from future
  geom_vline(xintercept = future_time_start, 
             linetype = "dashed", color = "black", size = 1) +
  
  # Plot historical walk
  geom_line(data = historical_data, 
            aes(x = time, y = value), 
            color = "blue", size = 1.2) +
  
  # Add point at the transition
  geom_point(x = future_time_start, y = start_value, 
             color = "black", size = 2) +
  
  # Styling
  labs(
    title = "Random Walk: Impossible to Forecast",
    subtitle = paste("Blue: Historical path | Red: 1000 possible future realizations"),
    x = "Time",
    y = "Value",
    caption = "The fan of future possibilities shows why random walks cannot be predicted"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 10),
    panel.grid.minor = element_blank()
  ) +
  
  # Add annotation
  annotate("text", x = n_historical/2, y = max(historical_walk), 
           label = "Known\nHistory", hjust = 0.5, vjust = 0.5, 
           color = "blue", fontface = "bold") +
  annotate("text", x = future_time_start + n_future/2, y = max(future_walks), 
           label = "Uncertain\nFuture", hjust = 0.5, vjust = 0.5, 
           color = "red", fontface = "bold")

# Display the plot
print(p)

# Save the plot (optional)
ggsave("random_walk_forecast_impossibility.png", plot = p, 
       width = 12, height = 8, dpi = 300)

# Print some summary statistics
cat("Summary Statistics:\n")
cat("Historical walk range:", round(min(historical_walk), 2), "to", round(max(historical_walk), 2), "\n")
cat("Future simulations range:", round(min(future_walks), 2), "to", round(max(future_walks), 2), "\n")
cat("Final historical value:", round(start_value, 2), "\n")
cat("Future endpoint range:", round(min(future_walks[nrow(future_walks), ]), 2), 
    "to", round(max(future_walks[nrow(future_walks), ]), 2), "\n")