# --------------------- TP7 ARIMA models

rm(list=ls())
library(fpp3)


# ----------- ex 1





simulate_arima <- function(Y0, eps0, N, phi, theta, sigma) {
  #' Simulate T observations from an ARMA(1,1) process
  #' 
  #' The ARMA(1,1,1) model is:
  #' X_t = phi * X_{t-1} + eps_t + theta * eps_{t-1}
  #' where X_t=(1-B)Y_t.
  #' 
  #' where eps_t ~ N(0, sigma^2)
  #' 
  #'  Y0 Starting value for Y at time 0
  #'  eps0 Starting value for innovation at time 0
  #'  T Number of observations to simulate
  #'  phi AR(1) coefficient
  #'  theta MA(1) coefficient
  #'  sigma Standard deviation of the innovation process
  #' 
  #' Return: A list containing:
  #'   Y: vector of T simulated values
  #'   eps: vector of T innovation values
  
  # Initialize vectors
  Y <- numeric(N)
  eps <- numeric(N)
  
  # Generate innovations from N(0, sigma^2)
  eps <- rnorm(N, mean = 0, sd = sigma)
  
  # Simulate ARMA(1,1) process
  for (t in 1:N) {
    if (t == 1) {
      Y[t] <- phi * Y0 + eps[t] + theta * eps0
    } else {
      Y[t] <- phi * Y[t-1] + eps[t] + theta * eps[t-1]
    }
  }
  
  # Obtain ARIMA(1,1,1)
  X<-cumsum(Y)
  
  return(list(X = X, eps = eps))
}


# check 
set.seed(123)
n <- 5000                
phi <- 0.6
theta <- -0.5
sigma2 <- 2
lags <- 10    

# --- Simulate with custom function ---

X_custom <- simulate_arima(Y0=0, eps0=0, N=n, phi=phi, theta=theta, sigma=sqrt(sigma2))$X

X_builtin <- arima.sim(model = list(order = c(1, 1, 1),
                                    ar = phi,
                                    ma = theta),
                       n = n,
                       sd = sqrt(sigma2))

plot(X_custom, type="l")
plot(X_builtin)

# Empirical autocovariance
emp_cov_custom <- acf(X_custom, plot = FALSE, lag.max = lags)$acf
emp_cov_builtin <- acf(X_builtin, plot = FALSE, lag.max = lags)$acf

# Theoretical ACF
acf_theo <- ARMAacf(ar = phi, ma = theta, lag.max = lags)
plot(x = 0:lags, y = acf_theo, type="l")
lines(x = 0:lags, y = emp_cov_custom, col = "red", lty = 2)
lines(x = 0:lags, y = emp_cov_builtin, col = "red", lty = 2)



simulate_arima2 <- function(n, p = 0, d = 0, q = 0, phi = NULL, theta = NULL, sigma2 = 1) {
  # -------------------------------------------
  # INPUTS
  # n       : length of simulated series
  # p, q, d : AR, I, MA orders
  # phi     : AR coefficients
  # theta   : MA coefficients
  # sigma2  : innovation variance
  # -------------------------------------------
  
  # Check AR and MA coefficient lengths
  if (p > 0 && length(phi) != p) stop("Length of phi must equal p")
  if (q > 0 && length(theta) != q) stop("Length of theta must equal q")
  
  
  
  # -------------------------------------------
  # BURN-IN to reduce startup effect
  # -------------------------------------------
  n.start <- p + q + 1000
  # Innovations (white noise)
  innov <- rnorm(n, mean = 0, sd = sqrt(sigma2))
  start.innov <- rnorm(n.start, mean = 0, sd = sqrt(sigma2))
  
  # Total length with burn-in
  x <- ts(c(start.innov, innov), start = 1 - n.start)
  
  # -------------------------------------------
  # MA part: filter innovations with MA polynomial
  # X_t = ε_t + θ_1 ε_{t-1} + ... + θ_q ε_{t-q}
  # -------------------------------------------
  if (q > 0) {
    x <- stats::filter(x, c(1, theta), sides = 1)
    # zero out initial transient values
    x[seq_len(q)] <- 0
  }
  
  # -------------------------------------------
  # AR part: recursive filter
  # (1 - φ_1 B - ... - φ_p B^p) X_t = ε_t + ...
  # -------------------------------------------
  if (p > 0) {
    x <- stats::filter(x, phi, method = "recursive")
  }
  
  # -------------------------------------------
  # Remove burn-in
  # -------------------------------------------
  if (n.start > 0) {
    x <- x[-(seq_len(n.start))]
  }
  
  # -------------------------------------------
  # Integrate if d > 0 (invert differencing)
  # -------------------------------------------
  if (d > 0) {
    x <- diffinv(x, differences = d)[-(1:d)]
  }
  
  return(as.ts(x))
}


# simulate ARMA 1 1
set.seed(123)
n <- 5000
phi <- 0.6
theta <- -0.5
sigma2 <- 2
lags <- 10
X_custom2 <- simulate_arima2(n = n, p = 1, d = 0, q = 1,
                             phi = phi, theta = theta, sigma2 = sigma2)

plot(X_custom2, type="l")

# Empirical autocovariance
acf_theo <- ARMAacf(ar = phi, ma = theta, lag.max = lags)
plot(x = 0:lags, y = acf_theo, type="l")
emp_cov_custom <- acf(X_custom2, plot = FALSE, lag.max = lags)$acf
lines(x = 0:lags, y = emp_cov_custom, col = "red", lty = 2, lwd=2)



# ------------------------ ex2

# Consider *aus_arrivals*, the quarterly number of international visitors to Australia 
# from several countries for the period 1981 Q1 -- 2012 Q3.

# 1. Select Japan and plot the data. What can you learn from it? 
# If there is seasonality, what could be the frequency (m).

# look at the data
aus_arrivals
View(aus_arrivals)
# filter Japan
aus_arrivals |>
  filter(Origin == "Japan") |>
  autoplot(Arrivals)

# - There is an increasing trend to about 1996, and slowly decreasing thereafter.
# - The seasonal shape has changed considerably over time.

# 2. What can you learn from the ACF and PACF graphs? See https://otexts.com/fpp3/seasonal-arima.html

# The seasonal part of an AR or MA model will be seen in the seasonal lags of the PACF and ACF. For example, an ARIMA(0,0,0)(0,0,1)_{12}
# model will show:
# - a spike at lag 12 in the ACF but no other significant spikes;
# - exponential decay in the seasonal lags of the PACF (i.e., at lags 12, 24, 36, …).
# Similarly, an ARIMA(0,0,0)(1,0,0)_{12} model will show:
# - exponential decay in the seasonal lags of the ACF;
# - a single significant spike at lag 12 in the PACF.
# In considering the appropriate seasonal orders for a seasonal ARIMA model, restrict attention to the seasonal lags.

aus_arrivals |>
  filter(Origin == "Japan") |>
  gg_tsdisplay(plot_type = "partial")

# 3. Compute an appropriate differencing to obtain stationary data. Plot the result.

# Difference at lag 4 (because quartely data)
aus_arrivals |>
  filter(Origin == "Japan") |>
  autoplot(Arrivals |> difference(lag=4))

# Difference at lag 4 and first difference
aus_arrivals |>
  filter(Origin == "Japan") |>
  autoplot(Arrivals |> difference(lag=4) |> 
             difference())


# KPSS test
aus_arrivals |>
  filter(Origin == "Japan") |>
  mutate(diff = difference(Arrivals, lag = 4) ) |>  # seasonal difference
  features(diff, unitroot_kpss)

aus_arrivals |>
  filter(Origin == "Japan") |>
  mutate(diff = difference(Arrivals, lag = 4) |>  
           difference()) |>  # seasonal and first difference
  features(diff, unitroot_kpss)

# At alpha= 0.05, we do not have evidence against stationarity for both differencing steps.

# 4. Plot the ACF and PACF graphs of the differenced data. 
# Explain/make a guess what would be the corresponding model for the original time series.

aus_arrivals |>
  filter(Origin == "Japan") |>
  gg_tsdisplay(Arrivals |> 
                 difference(lag=4) |> 
                 difference(), plot_type = "partial")

# From the ACF graph of the differenced data: 
# - The non-seasonal lags suggest an MA(1) component.
# - The seasonal lags suggest a seasonal MA(1) component

# From the PACF graph of the differenced data:
#  - The non-seasonal lags might suggest an AR(1) component. But the lag 2 spike in the PACF is larger than the lag 2 spike in the ACF. So an MA(1) is probably better.
# - The seasonal lags show geometric decay which is consistent with a seasonal MA(1).
# - The suggested model is an ARIMA(0,1,1)(0,1,1).


# 5. Use the function ARIMA() to run an automatic selection procedure of the model. 
# Does it give the same model that you chose? If not, which model do you think is better?

aus_arrivals |>
  filter(Origin == "Japan") |>
  model(ARIMA(Arrivals)) |>
  report()

# The resulting model has an additional seasonal AR(1) component compared to what 
# was guessed. We can compare the two models based on the AICc statistic:

aus_arrivals |>
  filter(Origin == "Japan") |>
  model(
    guess = ARIMA(Arrivals ~ pdq(0,1,1) + PDQ(0,1,1)),
    auto = ARIMA(Arrivals)) -> arima_fit

glance(arima_fit)

# The automatic model is only slightly better than guess based on the AICc statistic. 
# We may favor our first guess for a more parsimonious model. 

# 6. Inspect the residuals of both models to see if any auto-correlation pattern remains.

arima_fit |>
  select(guess) |>
  gg_tsresiduals()

resids <- arima_fit |>
  select(auto) |>
  residuals()

# For both models, there is still autocorrelation (e.g., at lag 6) and probably the residual variance is not totally stable.

## We can also check the distance autocorrelation:
library(dCovTS)
arima_fit |>
  select(auto) |>
  residuals() |>
  pull(.resid) |>
  na.omit() |>
  ADCFplot()

# Where unsurprisingly, a similar picture emerges.


# 7. Write the model in terms of the backshift operator, then without using the backshift operator.

# Latex below (see paper/pencil correction)

# $$(1-B)(1-B^4)(1-\Phi B^4)y_t =  (1+\theta B)(1+\Theta B^4) \varepsilon_t$$

# $$\left[1-B - (1 + \Phi)B^4 + (1 + \Phi) B^5 + \Phi B^8 - \Phi B^9\right]y_t =  (1+\theta B + \Theta B^4 + \theta\Theta B^5) \varepsilon_t$$

# $$  y_t - y_{t-1} - (1 + \Phi)y_{t-4} + (1 + \Phi) y_{t-5} + \Phi y_{t-8} - \Phi y_{t-9} =  \varepsilon_t + \theta \varepsilon_{t-1} + \Theta \varepsilon_{t-4} + \theta\Theta \varepsilon_{t-5}.$$

# $$  y_t = y_{t-1} + (1 + \Phi)y_{t-4} - (1 + \Phi) y_{t-5} - \Phi y_{t-8} + \Phi y_{t-9} +  \varepsilon_t + \theta \varepsilon_{t-1} + \Theta \varepsilon_{t-4} + \theta\Theta \varepsilon_{t-5}.$$



# ------------------------------ ex3

# Choose the *Electricity* time series *aus_production*.

# 1. Do the data need transforming? If so, find a suitable transformation.

aus_production |> 
  autoplot(Electricity)  #Yes, these need transforming, as the effect of seasonality appears to increase.

lambda <- aus_production |>
  features(Electricity, guerrero) |>
  pull(lambda_guerrero)
lambda

aus_production |>
  autoplot(box_cox(Electricity, lambda))

# `guerrero()` suggests using Box-Cox transformation with the above parameter lambda.

# 2. Are the data stationary? If not, find an appropriate differencing which yields stationary data.

# The trend and seasonality show that the data are not stationary.

aus_production |>
  gg_tsdisplay(box_cox(Electricity, lambda) |> 
                 difference(4), plot_type = "partial") #quarterly data

aus_production |>
  gg_tsdisplay(box_cox(Electricity, lambda) |> 
                 difference(4) |> 
                 difference(1), plot_type = "partial")

# - It seems that we could have continued with only taking seasonal differences. You may try this option.
# - Then first order difference can be taken as well.

# 3. Identify a couple of ARIMA models that might be useful in describing the time series. 
# Which of your models is the best according to their AIC values?

# From the above graph, an AR(1) or an MA(1) with a seasonal MA(2) might work.
# So an ARIMA(1,1,0)(0,1,2) model for the transformed data.

fit <- aus_production |>
  model(
    manual = ARIMA(box_cox(Electricity, lambda) ~ 0 + pdq(1, 1, 0) + PDQ(0, 1, 2)),
    auto = ARIMA(box_cox(Electricity, lambda))
  ) 

fit |>
  select(auto) |>
  report()

glance(fit)

# Automatic model selection with *ARIMA()* has also taken a 4-MA (and one less seasonal MA), we can compare the AICc values.
# This is a challenging ARIMA model to select manually and the automatic model is clearly better in AICc.

# 4. Estimate the parameters of your best model and do diagnostic testing on the residuals. Do the residuals resemble white noise? If not, try to find another ARIMA model which fits better.

fit |>
  select(auto) |>
  gg_tsresiduals()

# Residuals look ok, except a spike at lag 22 of the ACF. Overall, they seem to resemble a white noise.

# 5. Forecast the next 24 months of data using your preferred model.

fit |>
  select(auto) |>
  forecast(h = "2 years") |>
  autoplot(aus_production)



