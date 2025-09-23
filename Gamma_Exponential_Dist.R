# Load required libraries
library(ggplot2)

# Define the range of x values
x <- seq(0, 5, by = 0.01)

# Define lambda values
lambda_values <- c(0.5, 1.0, 1.5)

# Create data frame for plotting
plot_data <- data.frame()

for (lambda in lambda_values) {
  # Calculate exponential PDF: f(x) = λ * exp(-λ * x)
  pdf_values <- lambda * exp(-lambda * x)
  
  temp_data <- data.frame(
    x = x,
    pdf = pdf_values,
    lambda = factor(paste0("M_0/Lambda_0 = ", lambda))
  )
  
  plot_data <- rbind(plot_data, temp_data)
}

# Create the plot
p <- ggplot(plot_data, aes(x = x, y = pdf, color = lambda)) +
  geom_line(size = 1.2) +
  
  # Customize colors to match original
  scale_color_manual(values = c("#E31A1C", "#1F8A70", "#2E86AB")) +
  
  # Labels and title
  labs(
    title = "Exponential",
    subtitle = "Probability density function",
    x = "x",
    y = "f(x)",
    color = NULL
  ) +
  
  # Theme customizations
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 20)),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 11),
    legend.position = c(0.8, 0.8),
    legend.background = element_rect(fill = "white", color = "gray", size = 0.5),
    legend.margin = margin(6, 6, 6, 6),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  ) +
  
  # Set axis limits to match original
  xlim(0, 5) +
  ylim(0, 1.5) +
  
  # Add axis breaks
  scale_x_continuous(breaks = seq(0, 5, 1), limits = c(0, 5)) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.2), limits = c(0, 1.5))

# Display the plot
print(p)



# Define the range of x values
x <- seq(0, 8, by = 0.01)

# Define parameter combinations (shape r, rate α)
# Using different combinations to show variety of gamma distribution shapes
param_combinations <- list(
  list(r = 1, alpha = 0.5),    # Exponential-like (r=1)
  list(r = 2, alpha = 1.0),    # More bell-shaped
  list(r = 3, alpha = 1.5)     # Even more bell-shaped
)

# Create data frame for plotting
plot_data <- data.frame()

for (params in param_combinations) {
  r <- params$r
  alpha <- params$alpha
  
  # Calculate gamma PDF using shape-rate parameterization:
  # f(x) = (α^r / Γ(r)) * x^(r-1) * exp(-αx)
  # In R, dgamma uses shape and rate parameters
  pdf_values <- dgamma(x, shape = r, rate = alpha)
  
  temp_data <- data.frame(
    x = x,
    pdf = pdf_values,
    params = factor(paste0("r = ", r, ", α = ", alpha))
  )
  
  plot_data <- rbind(plot_data, temp_data)
}

# Create the plot
p <- ggplot(plot_data, aes(x = x, y = pdf, color = params)) +
  geom_line(size = 1.2) +
  
  # Customize colors
  scale_color_manual(values = c("#E31A1C", "#1F8A70", "#2E86AB")) +
  
  # Labels and title
  labs(
    title = "Gamma",
    subtitle = "Probability density function",
    x = "x",
    y = "P(x)",
    color = NULL
  ) +
  
  # Theme customizations
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 20)),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 11),
    legend.position = c(0.8, 0.8),
    legend.background = element_rect(fill = "white", color = "gray", size = 0.5),
    legend.margin = margin(6, 6, 6, 6),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  ) +
  
  # Set axis limits
  xlim(0, 8) +
  ylim(0, NA) +
  
  # Add axis breaks
  scale_x_continuous(breaks = seq(0, 8, 1), limits = c(0, 8)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1))

# Display the plot
print(p)

# Alternative version with different parameter combinations to show variety
plot_data_variety <- data.frame()

param_variety <- list(
  list(r = 0.5, alpha = 1),    # Decreasing function
  list(r = 1, alpha = 1),      # Exponential (special case)
  list(r = 2, alpha = 1),      # Chi-squared-like
  list(r = 5, alpha = 1.5)     # More symmetric, bell-shaped
)

for (params in param_variety) {
  r <- params$r
  alpha <- params$alpha
  
  pdf_values <- dgamma(x, shape = r, rate = alpha)
  
  temp_data <- data.frame(
    x = x,
    pdf = pdf_values,
    params = factor(paste0("r = ", r, ", α = ", alpha))
  )
  
  plot_data_variety <- rbind(plot_data_variety, temp_data)
}

p_variety <- ggplot(plot_data_variety, aes(x = x, y = pdf, color = params)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("#E31A1C", "#1F8A70", "#2E86AB", "#FF7F00")) +
  labs(
    title = "Gamma Distribution Variety",
    subtitle = "Different shape (r) and rate (α) combinations",
    x = "x",
    y = "P(x)",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 20)),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 11),
    legend.position = c(0.75, 0.8),
    legend.background = element_rect(fill = "white", color = "gray", size = 0.5),
    legend.margin = margin(6, 6, 6, 6),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  ) +
  xlim(0, 8) +
  scale_x_continuous(breaks = seq(0, 8, 1), limits = c(0, 8)) +
  scale_y_continuous(breaks = seq(0, 1.5, 0.2))

print(p_variety)

# LaTeX version (uncomment if you have latex2exp package)
# library(latex2exp)
# 
# plot_data_latex <- data.frame()
# 
# for (i in seq_along(param_combinations)) {
#   params <- param_combinations[[i]]
#   r <- params$r
#   alpha <- params$alpha
#   
#   pdf_values <- dgamma(x, shape = r, rate = alpha)
#   
#   temp_data <- data.frame(
#     x = x,
#     pdf = pdf_values,
#     params = factor(paste0("combo_", i))
#   )
#   
#   plot_data_latex <- rbind(plot_data_latex, temp_data)
# }
# 
# p_latex <- ggplot(plot_data_latex, aes(x = x, y = pdf, color = params)) +
#   geom_line(size = 1.2) +
#   scale_color_manual(
#     values = c("#E31A1C", "#1F8A70", "#2E86AB"),
#     labels = c(
#       TeX("$r = 1, \\alpha = 0.5$"),
#       TeX("$r = 2, \\alpha = 1.0$"),
#       TeX("$r = 3, \\alpha = 1.5$")
#     )
#   ) +
#   labs(
#     title = "Gamma",
#     subtitle = "Probability density function",
#     x = "x",
#     y = "P(x)",
#     color = NULL
#   ) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
#     plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 20)),
#     axis.title = element_text(size = 12),
#     axis.text = element_text(size = 10),
#     legend.text = element_text(size = 11),
#     legend.position = c(0.8, 0.8),
#     legend.background = element_rect(fill = "white", color = "gray", size = 0.5),
#     legend.margin = margin(6, 6, 6, 6),
#     panel.grid.minor = element_blank(),
#     panel.border = element_rect(color = "black", fill = NA, size = 0.5)
#   ) +
#   xlim(0, 8) +
#   scale_x_continuous(breaks = seq(0, 8, 1), limits = c(0, 8))
# 
# print(p_latex)