library(doSNOW)
library(fpp3)
library(patchwork)
library(dCovTS)
library(scoringRules)
library(distributional)


google_2018 <- gafa_stock |>
  filter(Symbol == "GOOG", year(Date) == 2018) |>
  mutate(Date = row_number())

google_2018 |>
  autoplot(Close) +
  labs(y = "Closing stock price ($USD)")

# Plot the autocorrelation function (acf):
google_2018 |>
  ACF(Close) |>
  autoplot()

# First differencing:
google_2018 |>
  autoplot(difference(Close)) +
  labs(y = "Change in Google closing stock price ($USD)")

## Split into train and test data
n <- nrow(google_2018)
H <- 10 #20  ## How many steps ahead?

# Create train and test sets as tsibbles
train_data <- google_2018 |> 
  slice(1:(n - H))

test_data <- google_2018 |> 
  slice((n - H + 1):n)

## Fit and Predict an AR(1) model using fable
ar1_fit <- train_data |>
  model(ar1 = AR(Close ~ order(1)))

# Forecast
ar1_forecast <- ar1_fit |>
  forecast(h = H)

# Plot
ar1_forecast |>
  autoplot(train_data) +
  autolayer(test_data, Close, color = "red", linewidth = 1) +
  labs(title = "AR(1) Forecast", y = "Close Price")

# Accuracy on training data
ar1_fit |> 
  accuracy()


# Accuracy on test data, including CRPS
ar1_forecast |> 
  accuracy(test_data,measures = list(CRPS = CRPS, RMSE = RMSE, MAE = MAE))



## Manual CRPS/Energy distance values using fable's generate function
## Continue here!!

## Extract and sample from forecast distributions directly
crps_values <- numeric(H)

for(i in 1:H) {
  # Get the distribution for horizon i
  forecast_dist <- ar1_forecast$Close[[i]]
  
  # Generate N=100 samples from this distribution
  samplesi <- distributional::generate(forecast_dist, times = 100)[[1]]
  
  crps_values[i] <- crps_sample(y = test_data$Close[i], 
                                dat =samplesi)
  
}



# Mean CRPS across all forecast horizons
mean_crps_AR <- mean(crps_values)

ar1_forecast |> 
  accuracy(test_data,measures = list(CRPS = CRPS, RMSE = RMSE, MAE = MAE))


mean_crps_AR

######################

## Fit and Predict DRF on data
library(drf)
source("drfown.R")

traindata <- train_data$Close
testdata <- test_data$Close
ntrain <- length(traindata)

X <- as.matrix(traindata[1:(ntrain-1)])
Y <- as.matrix(traindata[2:ntrain])
fit <- drfown(X = X, Y = Y)

B <- 100
Ypred <- matrix(NaN, nrow = H + 1, ncol = B)
Ypred[1, ] <- rep(traindata[ntrain], B)

## Predict a path
for (b in 1:B) {
  for (h in 2:(H + 1)) {
    DRFw <- predict(fit, newdata = Ypred[h - 1, b])$weights
    sig <- abs(ntrain^(-1/5) / drf:::medianHeuristic(Y))
    mean <- Y[sample(1:nrow(Y), size = 1, replace = TRUE, DRFw[1, ])]
    Ypred[h, b] <- rnorm(n = 1, mean = mean, sd = sig)
  }
}

# Create forecast distributions for DRF
point_forecasts <- rowMeans(Ypred[2:(H + 1), ])
lower_80 <- apply(Ypred[2:(H + 1), ], 1, quantile, probs = 0.1)
upper_80 <- apply(Ypred[2:(H + 1), ], 1, quantile, probs = 0.9)
lower_95 <- apply(Ypred[2:(H + 1), ], 1, quantile, probs = 0.025)
upper_95 <- apply(Ypred[2:(H + 1), ], 1, quantile, probs = 0.975)

# Create a fable-compatible forecast object
drf_forecast_data <- tibble(
  Date = test_data$Date,
  Symbol=test_data$Symbol,
  .mean = point_forecasts,
  Close = dist_sample(as.list(as.data.frame(t(Ypred[2:(H + 1), ]))))
) |>
  as_fable(index = Date, key = Symbol, distribution = Close, response = "Close")

# Plot DRF forecast
drf_forecast_data |>
  autoplot(train_data) +
  autolayer(test_data, Close, color = "red", size = 1) +
  labs(title = "DRF(1) Forecast", 
       y = "Close Price", 
       x = "Date")

# Calculate CRPS for DRF
crps_values_DRF <- numeric(H)
for(i in 1:H) {
  crps_values_DRF[i] <- crps_sample(y = testdata[i], 
                                    dat = Ypred[i + 1, ])
}
mean_crps_DRF <- mean(crps_values_DRF)

paste0("AR(1) CRPS: ", round(mean_crps_AR, 2))
paste0("DRF(1) CRPS: ", round(mean_crps_DRF, 2))
