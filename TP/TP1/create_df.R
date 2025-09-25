
# # Load necessary packages
# library(tibble)
# library(readr)
# 
# # Create fake monthly sales data for two smartphone models
# smartphone_sales <- tibble(
#   month = rep(seq.Date(from = as.Date("2025-01-01"), by = "month", length.out = 6), 2),
#   model = rep(c("AlphaX", "BetaY"), each = 6),
#   units_sold = c(150, 180, 200, 190, 220, 250, 120, 140, 160, 155, 170, 200)
# )
# 
# # Save the dataset as a CSV with a meaningful name
# write_csv(smartphone_sales, "TP/data/smartphone_sales.csv")



