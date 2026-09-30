# ============================================================
# Random-trader simulation on S&P 500
# constituent returns, benchmarked against the actual S&P 500
# index.
#
# Each period t, each of N independent traders selects k assets
# completely at random (uniformly, without replacement) from the
# assets with valid returns that period, and holds them equal-
# weighted until the next rebalance. There is no memory across
# periods -- the selection is re-randomized every period.
#
# Two references are plotted alongside the random traders:
#   - "Equal-weight market": the equal-weighted average return of
#     all 505 constituents in this panel each period. (1/N Pf)
#   - "S&P 500 index (approx.)": the actual index level, taken
#     from a public monthly S&P 500 series (no dividends), 
#     aligned to the same month-end dates. This is a
#     genuine external benchmark, but "approximate" because that
#     source dates each month's average price to the 1st of the
#     month rather than the exact month-end close, and excludes
#     dividends (a price-return, not total-return, series).
#
# Data:
#   sp500_monthly_returns_wide.csv  dates x tickers monthly returns
#                                    for 505 S&P 500 constituents
#   sp500_index_reference.csv       date, index_return  (actual
#                                    S&P 500 index, same 60 months)
#
# Output:
#   trader_returns.csv, trader_wealth.csv, market_returns.csv
#   random_traders_sp500.png   (ggplot2)
#
# Packages: ggplot2, dplyr, tidyr, readr, scales
# ============================================================

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

set.seed(42)

N_TRADERS  <- 10000   # number of independent random traders
K_ASSETS   <- 20     # assets each trader holds per period
RETURNS_PATH <- "sp500_monthly_returns_wide.csv"
INDEX_PATH   <- "sp500_index_reference.csv"

# ---- load data ----
wide <- read_csv(RETURNS_PATH, show_col_types = FALSE)
dates <- wide[[1]]
ret_mat <- as.matrix(wide[, -1])       # T x A matrix, NA where missing
n_periods <- nrow(ret_mat)

index_ref <- read_csv(INDEX_PATH, show_col_types = FALSE)
stopifnot(nrow(index_ref) == n_periods)
index_wealth <- cumprod(1 + index_ref$index_return)

# ---- simulate ----
trader_returns <- matrix(NA_real_, nrow = N_TRADERS, ncol = n_periods)
market_returns <- numeric(n_periods)

for (t in seq_len(n_periods)) {
  vals <- ret_mat[t, ]
  vals <- vals[!is.na(vals)]
  a <- length(vals)
  market_returns[t] <- mean(vals)
  
  k <- min(K_ASSETS, a)
  
  # Vectorized sampling-without-replacement for all N traders at once:
  # for each trader (one column of `rnd`), order() of iid uniforms gives
  # a random permutation of the a asset indices; the first k of that
  # permutation are k assets drawn without replacement.
  rnd <- matrix(runif(a * N_TRADERS), nrow = a, ncol = N_TRADERS)
  ord <- apply(rnd, 2, order)                    # a x N_TRADERS
  idx <- ord[seq_len(k), , drop = FALSE]         # k x N_TRADERS asset indices
  
  sampled <- matrix(vals[idx], nrow = k, ncol = N_TRADERS)
  trader_returns[, t] <- colMeans(sampled)
}

trader_wealth <- t(apply(trader_returns, 1, function(r) cumprod(1 + r)))
market_wealth <- cumprod(1 + market_returns)
final_wealth <- trader_wealth[, n_periods]

cat(sprintf("S&P 500 index (approx.) final level:          %.3fx\n", index_wealth[n_periods]))
cat(sprintf("Equal-weight market (all 505 constituents):   %.3fx\n", market_wealth[n_periods]))
cat(sprintf("Random traders (N=%d, k=%d): mean=%.3fx, median=%.3fx, sd=%.3f\n",
            N_TRADERS, K_ASSETS, mean(final_wealth), median(final_wealth), sd(final_wealth)))
cat(sprintf("Pct of random traders beating the equal-weight market: %.1f%%\n",
            100 * mean(final_wealth > market_wealth[n_periods])))
cat(sprintf("Pct of random traders beating the S&P 500 index:       %.1f%%\n",
            100 * mean(final_wealth > index_wealth[n_periods])))

# ---- save outputs ----
colnames(trader_returns) <- as.character(dates)
colnames(trader_wealth)  <- as.character(dates)
write_csv(as.data.frame(trader_returns), "trader_returns.csv")
write_csv(as.data.frame(trader_wealth),  "trader_wealth.csv")
write_csv(data.frame(date = dates, market_return = market_returns), "market_returns.csv")

# ---- palette ----
BLUE     <- "#2a78d6"
ORANGE   <- "#eb6834"
VIOLET   <- "#4a3aa7"
GRID_COL <- "#e3e2dc"
TEXT_SECONDARY <- "#52514e"

# ============================================================
# Shared ingredients for the "zoomed out" context: the full
# population's sampled paths and percentile bands, plus the two
# reference series. These feed the combined panel built below,
# after p2's tail highlights exist.
# ============================================================
q <- apply(trader_wealth, 2, quantile, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
band_df <- data.frame(
  date = dates,
  p05 = q["5%", ], p25 = q["25%", ], p50 = q["50%", ],
  p75 = q["75%", ], p95 = q["95%", ]
)

sample_idx <- sample(N_TRADERS, 150)
sample_paths <- as.data.frame(t(trader_wealth[sample_idx, ]))
colnames(sample_paths) <- paste0("trader_", sample_idx)
sample_paths$date <- dates
sample_long <- pivot_longer(sample_paths, -date, names_to = "trader", values_to = "wealth")

ref_lines <- bind_rows(
  data.frame(date = dates, wealth = market_wealth, series = "Equal-weight market (all 505 constituents)"),
  data.frame(date = dates, wealth = index_wealth,  series = "S&P 500 index (approx.)")
)
ref_lines$series <- factor(ref_lines$series,
                           levels = c("Equal-weight market (all 505 constituents)", "S&P 500 index (approx.)"))

# ============================================================
# Panel 2 (shown first): the 10 most and 10 least successful
# traders, against the S&P 500.
# ============================================================
GREEN <- "#1baf7a"
RED   <- "#e34948"
N_HIGHLIGHT <- 3

top10_idx    <- order(final_wealth, decreasing = TRUE)[seq_len(N_HIGHLIGHT)]
bottom10_idx <- order(final_wealth)[seq_len(N_HIGHLIGHT)]

# idx is already ordered from most- to least-extreme within its group;
# that order becomes each trader's rank (1 = most extreme) and drives
# the fade so the single best/worst trader stands out most.
paths_to_long_ranked <- function(wealth_mat, idx, prefix, label) {
  df <- as.data.frame(t(wealth_mat[idx, , drop = FALSE]))
  colnames(df) <- paste0(prefix, seq_along(idx))
  df$date <- dates
  long <- pivot_longer(df, -date, names_to = "trader", values_to = "wealth")
  long$rank  <- as.integer(sub(prefix, "", long$trader, fixed = TRUE))
  long$group <- label
  long$alpha <- seq(1, 0.45, length.out = length(idx))[long$rank]
  long
}

tail_long <- bind_rows(
  paths_to_long_ranked(trader_wealth, top10_idx,    "top_", "Top 10 traders"),
  paths_to_long_ranked(trader_wealth, bottom10_idx, "bot_", "Bottom 10 traders")
)
tail_long$group <- factor(tail_long$group, levels = c("Top 10 traders", "Bottom 10 traders"))
tail_end <- tail_long %>% filter(date == max(date))

index_df <- data.frame(date = dates, wealth = index_wealth, group = "S&P 500 index (approx.)")

group_levels <- c("Top 10 traders", "Bottom 10 traders", "S&P 500 index (approx.)")
color_map    <- c("Top 10 traders" = GREEN, "Bottom 10 traders" = RED,
                  "S&P 500 index (approx.)" = VIOLET)
linetype_map <- c("Top 10 traders" = "solid", "Bottom 10 traders" = "dashed",
                  "S&P 500 index (approx.)" = "solid")

p2 <- ggplot() +
  geom_line(data = tail_long, aes(date, wealth, group = trader, color = group,
                                  linetype = group, alpha = alpha),
            linewidth = 0.7, show.legend=F) +
  geom_point(data = tail_end, aes(date, wealth, color = group, alpha = alpha), size = 1.6, show.legend=F) +
  geom_line(data = index_df, aes(date, wealth, color = group, linetype = group), linewidth = 1.1, show.legend=F) +
  scale_color_manual(name = NULL, values = color_map, breaks = group_levels) +
  scale_linetype_manual(name = NULL, values = linetype_map, breaks = group_levels) +
  scale_alpha_identity() +
  guides(color = guide_legend(override.aes = list(alpha = 1, linewidth = 1.1))) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = "x"))


cat(sprintf("\nTop %d traders: final wealth %.2fx - %.2fx\n",
            N_HIGHLIGHT, min(final_wealth[top10_idx]), max(final_wealth[top10_idx])))
cat(sprintf("Bottom %d traders: final wealth %.2fx - %.2fx\n",
            N_HIGHLIGHT, min(final_wealth[bottom10_idx]), max(final_wealth[bottom10_idx])))

# ============================================================
# Panel 1 (shown second): the same p2 -- the same highlighted
# tails and the same S&P 500 line -- with the previously-hidden
# population lines (the full sample of traders and the percentile
# bands) added back in underneath. Drawing order matters here: the
# context layers go in first so they sit behind, and the p2 layers
# (tails, index) are added last so they stay on top and just as
# visible as they were in p2 alone -- p1 is a strict superset of p2.
# ============================================================
context_market <- dplyr::filter(ref_lines, series == "Equal-weight market (all 505 constituents)")

p1 <- ggplot() +
  # previously-hidden context
  geom_line(data = sample_long, aes(date, wealth, group = trader),
            color = BLUE, alpha = 0.42, linewidth = 0.3) +
  #geom_ribbon(data = band_df, aes(x = date, ymin = p05, ymax = p95),
  #            fill = BLUE, alpha = 0.12) +
  #geom_ribbon(data = band_df, aes(x = date, ymin = p25, ymax = p75),
  #            fill = BLUE, alpha = 0.22) +
  geom_line(data = band_df, aes(date, p50, color = "Median random trader"),
            linewidth = 0.9, show.legend = FALSE) +
  geom_line(data = context_market, aes(date, wealth, color = series),
            linewidth = 0.9, show.legend = FALSE) +
  # same tails + index as p2, drawn last so they remain on top
  geom_line(data = tail_long, aes(date, wealth, group = trader, color = group,
                                  linetype = group, alpha = alpha),
            linewidth = 0.7, show.legend = FALSE) +
  geom_point(data = tail_end, aes(date, wealth, color = group, alpha = alpha),
             size = 1.6, show.legend = FALSE) +
  geom_line(data = index_df, aes(date, wealth, color = group, linetype = group),
            linewidth = 1.1, show.legend = FALSE) +
  scale_color_manual(
    name = NULL,
    values = c(color_map,
               "Median random trader" = BLUE,
               "Equal-weight market (all 505 constituents)" = ORANGE)
  ) +
  scale_linetype_manual(name = NULL, values = linetype_map, guide = "none") +
  scale_alpha_identity() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = "x"))

# ============================================================
# combine and save -- p2 (tails only) on top, p1 (tails + context) below
# ============================================================

ggsave("Top_Traders.png", p2, width = 9, height = 5.5, dpi = 200, bg = "white")
ggsave("Top_Traders2.png",  p1, width = 9, height = 5.5, dpi = 200, bg = "white")


