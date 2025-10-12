# ACF and PACF for AR and MA models
x = arima.sim(n = 10000, list(order = c(1, 0, 0), ar = 0.5))
plot(acf(x))
plot(pacf(x))
x2 = arima.sim(n = 10000, list(order = c(0, 0, 1), ma = 0.5))
plot(acf(x2))
plot(pacf(x2))




