# clean ws
rm(list=ls())

# pkg
library(fpp3)
library(dplyr)
library(slider)
library(ggplot2)
library(tidyr)
library(zoo) 

# Example on true data additional model for decomposition
# --- Prepare data ---
beer_data <- aus_production %>%
  select(Quarter, Beer) %>%
  filter_index("1992 Q1" ~ "2000 Q4") %>%
  as_tsibble(index = Quarter)

time <- beer_data$Quarter
beer <- beer_data$Beer

# Convert yearquarter to numeric for plotting
time_num <- as.numeric(time)

# plot
par(mar=c(5,2,2,2))
plot(time_num, beer, type = "l", lwd = 2, col = "blue",
     xlab = "Quarter", ylab = "Beer Production",
     xaxt = "n", main = "Quarterly Beer Production (1992 Q1 - 2000 Q4)")

# Add x-axis with year + quarter
tick_pos <- seq(1, length(time_num), by = 4)  # every year
tick_labels <- format(time[tick_pos], "%Y Q%q")
axis(1, at = time_num[tick_pos], labels = tick_labels, las = 2)


# ---  Manual decomposition ---

# Trend via LOESS
trend_loess <- loess(Beer ~ as.numeric(Quarter), data = beer_data, span = 0.5)
beer_data <- beer_data %>%
  mutate(trend_manual = predict(trend_loess, newdata = beer_data))

# Detrended series
beer_data <- beer_data %>%
  mutate(detrended = Beer - trend_manual)

# Seasonal component: average detrended by quarter
beer_data <- beer_data %>%
  mutate(Qtr = quarter(Quarter, with_year = FALSE)) %>%
  group_by(Qtr) %>%
  mutate(season_manual = mean(detrended, na.rm = TRUE)) %>%
  ungroup()

# Remainder
beer_data <- beer_data %>%
  mutate(remainder_manual = Beer - trend_manual - season_manual)

# ---  STL decomposition ---
fit_stl <- beer_data %>%
  model(stl = STL(Beer ~ season(window = "periodic"), robust = FALSE))

components_stl <- fit_stl %>% components()


# --- Prepare plotting ---
time <- beer_data$Quarter
par(mfrow = c(3,1), mar = c(4,4,2,1)) # 3 panels

cols <- c("blue","red") # Manual vs STL

# ---  Trend ---
plot(time, beer_data$trend_manual, type="l", lwd=2, col=cols[1],
     ylab="Trend", xlab="Quarter", main="Trend Component: Manual vs STL")
lines(time, components_stl$trend, col=cols[2], lwd=2, lty=2)
legend("topleft",bty="n",  legend=c("Manual","STL"), col=cols, lty=c(1,2), lwd=2)

# --- Seasonal ---
plot(time, beer_data$season_manual, type="l", lwd=2, col=cols[1],
     ylab="Seasonal", xlab="Quarter", main="Seasonal Component: Manual vs STL")
lines(time, components_stl$season_year, col=cols[2], lwd=2, lty=2)
abline(h=0, col="grey", lty=3)
legend("topleft", bty="n", legend=c("Manual","STL"), col=cols, lty=c(1,2), lwd=2)

# -- Remainder ---
plot(time, beer_data$remainder_manual, type="l", lwd=2, col=cols[1],
     ylab="Remainder", xlab="Quarter", main="Remainder Component: Manual vs STL")
lines(time, components_stl$remainder, col=cols[2], lwd=2, lty=2)
abline(h=0, col="grey", lty=3)
legend("topleft", bty="n", legend=c("Manual","STL"), col=cols, lty=c(1,2), lwd=2)

# Reset plotting layout
par(mfrow=c(1,1))



# ------------------------------------- on generated data


# ---  Generate synthetic quarterly time series ---
set.seed(123)
n_years <- 8
quarters <- seq(as.yearqtr("1992 Q1"), by = 1/4, length.out = n_years*4)

trend_true <- seq(100, 100 + 5*(n_years*4-1)/4, length.out = n_years*4)  # linear trend
season_true <- rep(c(10, -5, 0, -5), n_years)                             # quarterly seasonal
noise <- rnorm(n_years*4, mean = 0, sd = 2)                               # random noise

X <- trend_true + season_true + noise

# Create tsibble
synthetic_data <- tibble(
  Quarter = yearquarter(quarters),
  X = X,
  trend_true = trend_true,
  season_true = season_true
) %>%
  as_tsibble(index = Quarter)

# --- Manual decomposition ---

# Trend via LOESS
trend_loess <- loess(X ~ as.numeric(Quarter), data = synthetic_data, span = 0.5)
synthetic_data <- synthetic_data %>%
  mutate(trend_manual = predict(trend_loess, newdata = synthetic_data))

# Detrended series
synthetic_data <- synthetic_data %>%
  mutate(detrended = X - trend_manual)

# Seasonal component: average detrended per quarter
synthetic_data <- synthetic_data %>%
  mutate(Qtr = quarter(Quarter, with_year = FALSE)) %>%
  group_by(Qtr) %>%
  mutate(season_manual = mean(detrended, na.rm = TRUE)) %>%
  ungroup()

# Remainder
synthetic_data <- synthetic_data %>%
  mutate(remainder_manual = X - trend_manual - season_manual)

# -- STL decomposition ---
fit_stl <- synthetic_data %>%
  model(stl = STL(X ~ season(window = "periodic"), robust = FALSE))
components_stl <- fit_stl %>% components()

# -- Compute STL remainder ---
remainder_stl <- components_stl$remainder

# --- Set up plotting area: 3 rows, 1 column ---
par(mfrow = c(3, 1), mar = c(4, 4, 2, 1)) # rows, cols, margins

# Colors
cols <- c("black", "blue", "red")

# --- Trend panel ---
plot(synthetic_data$Quarter, synthetic_data$trend_true, type = "l",
     col = cols[1], lwd = 2, ylab = "Trend", xlab = "Quarter",
     main = "Trend: True vs Manual vs STL")
lines(synthetic_data$Quarter, synthetic_data$trend_manual, col = cols[2], lwd = 2, lty = 2)
lines(synthetic_data$Quarter, components_stl$trend, col = cols[3], lwd = 2, lty = 3)
legend("topleft",bty="n", legend = c("True", "Manual", "STL"), col = cols, lty = c(1,2,3), lwd = 2)

# --- Seasonal panel ---
plot(synthetic_data$Quarter, synthetic_data$season_true, type = "l",
     col = cols[1], lwd = 2, ylab = "Seasonal", xlab = "Quarter",
     main = "Seasonal: True vs Manual vs STL")
lines(synthetic_data$Quarter, synthetic_data$season_manual, col = cols[2], lwd = 2, lty = 2)
lines(synthetic_data$Quarter, components_stl$season_year, col = cols[3], lwd = 2, lty = 3)
legend("topleft",bty="n", legend = c("True", "Manual", "STL"), col = cols, lty = c(1,2,3), lwd = 2)

# ---  Remainder panel ---
remainder_true <- synthetic_data$X - synthetic_data$trend_true - synthetic_data$season_true
plot(synthetic_data$Quarter, remainder_true, type = "l",
     col = cols[1], lwd = 2, ylab = "Remainder", xlab = "Quarter",
     main = "Remainder: True vs Manual vs STL")
lines(synthetic_data$Quarter, synthetic_data$remainder_manual, col = cols[2], lwd = 2, lty = 2)
lines(synthetic_data$Quarter, remainder_stl, col = cols[3], lwd = 2, lty = 3)
legend("topleft",bty="n", legend = c("True", "Manual", "STL"), col = cols, lty = c(1,2,3), lwd = 2)

# Reset par
par(mfrow = c(1,1))






# --------------------------- STL decomposition with multiplicative model


# ------------------- Prepare data --------------------
ap_data <- as_tsibble(AirPassengers)
plot(AirPassengers)

# Rename columns
ap_data <- ap_data %>% rename(Month = index, Passengers = value)

# Log-transform for multiplicative decomposition
ap_data <- ap_data %>% mutate(log_Passengers = log(Passengers))

# -------------------- 2️⃣ Manual decomposition on log-scale --------------------
# Trend via LOESS
trend_loess <- loess(log_Passengers ~ as.numeric(Month), data = ap_data, span = 0.5)
ap_data <- ap_data %>% mutate(trend_manual = predict(trend_loess, newdata = ap_data))

# Detrended
ap_data <- ap_data %>%
  mutate(detrended = log_Passengers - trend_manual)

# Seasonal component: average per month
ap_data <- ap_data %>%
  mutate(MonthNum = month(Month)) %>%
  group_by(MonthNum) %>%
  mutate(season_manual = mean(detrended, na.rm = TRUE)) %>%
  ungroup()

# Remainder
ap_data <- ap_data %>% mutate(remainder_manual = log_Passengers - trend_manual - season_manual)

# ------------------- STL decomposition on log-scale --------------------
fit_stl <- ap_data %>% model(stl = STL(log_Passengers ~ season(window="periodic"), robust = FALSE))
components_stl <- fit_stl %>% components()

# ------------------- plotting --------------------
time <- ap_data$Month
par(mfrow=c(3,1), mar=c(4,4,2,1))
cols <- c("blue","red")

# Trend
plot(time, ap_data$trend_manual, type="l", col=cols[1], lwd=2,
     ylab="Trend (log)", xlab="Month", main="Trend Component: Manual vs STL")
lines(time, components_stl$trend, col=cols[2], lwd=2, lty=2)
legend("topleft",bty="n", legend=c("Manual","STL"), col=cols, lty=c(1,2), lwd=2)

# Seasonal
plot(time, ap_data$season_manual, type="l", col=cols[1], lwd=2,
     ylab="Seasonal (log)", xlab="Month", main="Seasonal Component: Manual vs STL")
lines(time, components_stl$season_year, col=cols[2], lwd=2, lty=2)
abline(h=0, col="grey", lty=3)
legend("topleft",bty="n", legend=c("Manual","STL"), col=cols, lty=c(1,2), lwd=2)

# Remainder
plot(time, ap_data$remainder_manual, type="l", col=cols[1], lwd=2,
     ylab="Remainder (log)", xlab="Month", main="Remainder Component: Manual vs STL")
lines(time, components_stl$remainder, col=cols[2], lwd=2, lty=2)
abline(h=0, col="grey", lty=3)
legend("topleft",bty="n", legend=c("Manual","STL"), col=cols, lty=c(1,2), lwd=2)

# Reset layout
par(mfrow=c(1,1))




# generate multiplicative time series

# -------------------- 1️⃣ Generate synthetic multiplicative series --------------------
n_years <- 8
months <- seq(as.yearmon("2000-01"), by = 1/12, length.out = n_years*12)

# True components
trend_true <- seq(50, 150, length.out = n_years*12)         # increasing trend
season_true <- rep(c(1.1, 0.9, 1, 0.95, 1.05, 1.1, 1.2, 1.15, 1, 0.9, 0.95, 1), n_years) # multiplicative seasonal
noise <- rnorm(n_years*12, mean = 1, sd = 0.05)            # multiplicative noise around 1

# Multiplicative series
X <- trend_true * season_true * noise

# Create tsibble
synthetic_data <- tibble(
  Month = yearmonth(months),
  X = X,
  trend_true = trend_true,
  season_true = season_true
) %>% as_tsibble(index = Month)

# -------------------- Manual decomposition on log scale --------------------
synthetic_data <- synthetic_data %>%
  mutate(log_X = log(X))

# Trend via LOESS
trend_loess <- loess(log_X ~ as.numeric(Month), data = synthetic_data, span = 0.5)
synthetic_data <- synthetic_data %>% mutate(trend_manual = predict(trend_loess, newdata = synthetic_data))

# Detrended
synthetic_data <- synthetic_data %>%
  mutate(detrended = log_X - trend_manual)

# Seasonal: average per month
synthetic_data <- synthetic_data %>%
  mutate(MonthNum = month(Month)) %>%
  group_by(MonthNum) %>%
  mutate(season_manual = mean(detrended, na.rm = TRUE)) %>%
  ungroup()

# Remainder
synthetic_data <- synthetic_data %>%
  mutate(remainder_manual = log_X - trend_manual - season_manual)

# ------------------- STL decomposition on log scale --------------------
fit_stl <- synthetic_data %>%
  model(stl = STL(log_X ~ season(window="periodic"), robust=FALSE))
components_stl <- fit_stl %>% components()

# -------------------- Compute remainder for STL --------------------
remainder_stl <- components_stl$remainder

# --------------------  plotting --------------------
time <- synthetic_data$Month
par(mfrow=c(3,1), mar=c(4,4,2,1))
cols <- c("black","blue","red") # True, Manual, STL

# Trend
plot(time, log(synthetic_data$trend_true), type="l", col=cols[1], lwd=2,
     ylab="Trend (log)", xlab="Month", main="Trend: True vs Manual vs STL")
lines(time, synthetic_data$trend_manual, col=cols[2], lwd=2, lty=2)
lines(time, components_stl$trend, col=cols[3], lwd=2, lty=3)
legend("topleft",bty="n", legend=c("True","Manual","STL"), col=cols, lty=c(1,2,3), lwd=2)

# Seasonal
plot(time, log(synthetic_data$season_true), type="l", col=cols[1], lwd=2,
     ylab="Seasonal (log)", xlab="Month", main="Seasonal: True vs Manual vs STL")
lines(time, synthetic_data$season_manual, col=cols[2], lwd=2, lty=2)
lines(time, components_stl$season_year, col=cols[3], lwd=2, lty=3)
abline(h=0, col="grey", lty=3)
legend("topleft",bty="n", legend=c("True","Manual","STL"), col=cols, lty=c(1,2,3), lwd=2)

# Remainder
remainder_true <- synthetic_data$log_X - log(synthetic_data$trend_true) - log(synthetic_data$season_true)
plot(time, remainder_true, type="l", col=cols[1], lwd=2,
     ylab="Remainder (log)", xlab="Month", main="Remainder: True vs Manual vs STL")
lines(time, synthetic_data$remainder_manual, col=cols[2], lwd=2, lty=2)
lines(time, remainder_stl, col=cols[3], lwd=2, lty=3)
abline(h=0, col="grey", lty=3)
legend("topleft",bty="n", legend=c("True","Manual","STL"), col=cols, lty=c(1,2,3), lwd=2)

# Reset layout
par(mfrow=c(1,1))



