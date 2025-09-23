

library(doSNOW)
library(fpp3)
library(forecast)
library(patchwork)
library(dCovTS)
library(scoringRules)


google_2018 <- gafa_stock |>
  filter(Symbol == "GOOG", year(Date) == c(2018) )
google_2018 |>
  autoplot(Close) +
  labs(y = "Closing stock price ($USD)")


# We see a series that seems quite eradic. To understand it better, we plot the autocorrelation function (acf):
  

google_2018 |>
  ACF(Close) |>
  autoplot()


# We see that there is a clear (linear) dependence between $Y_t$ and its lags $Y_{t-h}$, even up to $h=20$. However, let's see what happens if we use first differencing:


google_2018 |>
  autoplot(difference(Close)) +
  labs(y = "Change in Google closing stock price ($USD)")


## Split into train and test data

n<-length(google_2018$Close)
H<-20  ## How many steps ahead?
traindata<-google_2018$Close[1:(n-H)]
testdata<-google_2018$Close[(n-(H-1)):n]
ntrain<-length(traindata)





## Fit and Predict an AR(1) model on data


library(forecast)

# Fit AR(1) - treats it as a simple numeric vector
ar1_model <- Arima(traindata, order = c(1, 0, 0))

# Forecast
predictions <- forecast(ar1_model, h = H)

test_ts <- ts(testdata, start = ntrain + 1, end = ntrain + length(testdata))

# Plot
autoplot(predictions) +
  autolayer(test_ts, color = "red", size = 1) +
  labs(title = "AR(1) Forecast", y = "Close Price")


##Continue here, add Winkler score!!!

traindata |> Arima(traindata, order = c(1, 0, 0)) |> accuracy(google_stock, list(winkler = winkler_score), level = 80)


## CRPS/Energy distance values
ARsim <- replicate(100, {
  simulate(predictions$model, nsim = H, future = TRUE, bootstrap = FALSE)
})
crps_values <- numeric(H)
for(i in 1:H) {
  crps_values[i] <- crps_sample(y = testdata[i], 
                               dat = ARsim[i, ])
}

# Mean CRPS across all forecast horizons
mean_crps_AR <- mean(crps_values)





## Fit and Predict DRF on data


library(drf)
source("drfown.R")
X<-as.matrix(traindata[1:(ntrain-1)])
Y<-as.matrix(traindata[2:ntrain])

fit<-drfown(X=X, Y=Y)
B<-100

Ypred<-matrix(NaN, nrow=H+1, ncol=B)
Ypred[1,]<-rep(traindata[ntrain],B)

##Predict a path
for (b in 1:B){
  
  for (h in 2:(H+1)){
    # start from last (h-1) prediction in the same "world" b 
  DRFw <- predict(fit, newdata =Ypred[h-1,b] )$weights
  ## Draw path
  sig<-abs(ntrain^(-1/5)/drf:::medianHeuristic(Y))
  mean<-Y[sample(1:nrow(Y), size=1, replace = T, DRFw[1,])]
    
  Ypred[h,b]<-rnorm(n=1, mean = mean, sd =sig )
  
  }
  
}

#Remember that the first row of Ypred is the last observed value

point_forecasts <- rowMeans(Ypred[2:(H+1),]) #pred_matrix[, "Point Forecast"]
lower_80 <- apply(Ypred[2:(H+1),], 1, quantile, probs = 0.1 )  #pred_matrix[, "Lo 80"]
upper_80 <- apply(Ypred[2:(H+1),], 1, quantile, probs = 0.9 ) #pred_matrix[, "Hi 80"]
lower_95 <- apply(Ypred[2:(H+1),], 1, quantile, probs = 0.025 ) #pred_matrix[, "Lo 95"]
upper_95 <- apply(Ypred[2:(H+1),], 1, quantile, probs = 0.975 )

# Create a manual forecast object
manual_forecast <- structure(
  list(
    mean = ts(point_forecasts, start=ntrain+1, end=ntrain+H),
    lower = cbind(lower_80, lower_95),
    upper = cbind(upper_80, upper_95),
    level = c(80, 95),
    x = ts(traindata)  # your original data
  ),
  class = "forecast"
)

# Create a time series object for the test data
test_ts <- ts(testdata, start = ntrain + 1, end = ntrain + length(testdata))

# Plot it
autoplot(manual_forecast) +
  autolayer(test_ts, color = "red", size = 1) +
  labs(title = "DRF(1) Forecast", 
       y = "Close Price", 
       x = "Time")



crps_values_DRF <- numeric(H-1)
for(i in 1:H) {
  crps_values_DRF[i] <- crps_sample(y = testdata[i], 
                               dat = Ypred[i+1,])
}


mean_crps_DRF<-mean(crps_values_DRF)

paste0("AR(1) CRPS: ", round(mean_crps_AR,2))
paste0("DRF(1) CRPS: ", round(mean_crps_DRF,2) )






