# clean ws
rm(list=ls())

# pkg
library(forecast)

# ex01 generate from arma 1 0 1, fit model on train, parametric bootstrap sample path, compute mean and quantile on these, plot, compute score for both prediction, report score

simulate_arma11 <- function(Y0, eps0, N, phi, theta, sigma) {
  #' Simulate T observations from an ARMA(1,1) process
  #' 
  #' The ARMA(1,1) model is:
  #' Y_t = phi * Y_{t-1} + eps_t + theta * eps_{t-1}
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
  
  return(list(Y = Y, eps = eps))
}


N <- 200
phi <- 0.6   
theta <- 0.3  
sigma <- 1
Y0<-0
eps0<-0

# Forecasting Package
#y <- arima.sim(n = n, list(ar = phi, ma = theta), sd = sigma)
y<-simulate_arma11(Y0=Y0, eps0=eps0, N=N, phi=phi, theta=theta, sigma=sigma)$Y


plot(y, type="l")

# Divide into train and test set
p_train <- 0.7
n_train <- floor(N * p_train)
y_train <- y[1:n_train]
y_test <- y[(n_train+1):N]
h <- length(y_test)


fit <- Arima(y_train, order = c(1,0,1), include.mean = FALSE)

# fable package
# fit <- leisure |>
#  model(arima = ARIMA(y ~ pdq(1, 0, 1)))


B <- 500  # number of bootstrap paths
boot_preds <- matrix(NA, nrow = h, ncol = B)
# generate paths
for(b in 1:B){
  #boot_path <- simulate(fit, nsim = h, bootstrap = TRUE)
  #boot_preds[,b] <- boot_path
  
  boot_preds[,b] <- simulate_arma11(Y0=y_train[n_train], eps0=fit$residuals[n_train], N=h, phi=fit$coef["ar1"], theta=fit$coef["ma1"], sigma=sqrt(fit$sigma2))$Y
}


pred_mean <- apply(boot_preds, 1, mean)
alpha <- .1
pred_q_upper <- apply(boot_preds, 1, function(x) quantile(x, probs = 1- alpha/2))
pred_q_lower <- apply(boot_preds, 1, function(x) quantile(x, probs = alpha/2))

plot(y, type="l", col="white", main="Bootstrap Forecasts with 90% Prediction Band",
     ylab="y", xlab="t", ylim=c(-5,5))
lines(y[1:n_train])
# plot bootstrap paths
for(b in 1:B){
  lines(n_train + 1:h, boot_preds[,b], col=rgb(0,0,0,alpha=0.05))
}

# quantile forecast band
lines(n_train + 1:h, pred_q_upper, col="red", lwd=2, lty=2)
lines(n_train + 1:h, pred_q_lower, col="red", lwd=2, lty=2)

# shaded area for prediction band
polygon(c(n_train + 1:h, rev(n_train + 1:h)),
        c(pred_q_upper, rev(pred_q_lower)),
        col=rgb(1,0,0,alpha=0.2), border=NA)

# mean forecast
lines(n_train + 1:h, pred_mean, col="blue", lwd=2)

legend("topleft",
       legend=c("Observed","Mean forecast","90% Prediction Band"),
       col=c("black","blue","red"), lty=c(1,1,2), lwd=c(1,2,2), bty="n")

# rmse on predicted mean
rmse_mean <- sqrt(mean((y_test - pred_mean)^2))
rmse_q_upper <- sqrt(mean((y_test - pred_q_upper)^2))

# quantile score on predicted quantile
mean_qs_upper <- mean(
  (1-alpha/2 - 1)*(y_test - pred_q_upper)*(y_test < pred_q_upper) +
    (1-alpha/2)*(y_test - pred_q_upper)*(y_test >= pred_q_upper)
)

# quantile score on mean
mean_qs_mean <- mean(
  (0.5 - 1)*(y_test - pred_mean)*(y_test < pred_mean) +
    (0.5)*(y_test - pred_mean)*(y_test >= pred_mean)
)


# print results
cat(paste0("RMSE on mean forecast: ", round(rmse_mean,4), "\n"))
cat(paste0("Mean Quantile Score on mean forecast: ", round(mean_qs_mean,4), "\n"))

cat(paste0("RMSE on 95% quantile forecast: ", round(rmse_q_upper,4), "\n"))
cat(paste0("Mean Quantile Score on 95% quantile forecast: ", round(mean_qs_upper,4), "\n"))


# ex2 - simulation study: generate from AR2 model, fit AR1 model and AR2 model, compute rmse, mae and qnatile score on predicted 80% quantile, represent effect of model mispecification with boxplot


simulate_ar2 <- function(Y1, Y0, N, phi1, phi2, sigma) {
  #' Simulate T observations from an ARMA(1,1) process
  #' 
  #' The AR(2) model is:
  #' Y_t = phi_1 * Y_{t-1} + phi_2 * Y_{t-2}  + eps_t
  #' 
  #' where eps_t ~ N(0, sigma^2)
  #' 
  #'  Y1,Y0 Starting value for Y at time 0
  #'  N Number of observations to simulate
  #'  sigma Standard deviation of the innovation process
  #' 
  #' Return: A list containing:
  #'   Y: vector of T simulated values
  
  # Initialize vectors
  Y <- numeric(N)
  
  # Generate innovations from N(0, sigma^2)
  eps <- rnorm(N, mean = 0, sd = sigma)
  
  # Simulate AR2 process
  for (t in 2:N) {
    if (t == 2) {
      Y[t] <- phi1 * Y1+ phi2 * Y0 + eps[t]
    } else {
      Y[t] <- phi1 * Y[t-1]+ phi2 * Y[t-2] + eps[t]
    }
  }
  
  return(list(Y = Y))
}


# Simulation
B <- 500
n=500
phi <- c(.9, -.8)
sigma_eps <- .01
p_training = 0.7


alpha <- 0.8  # 80% quantile

# create mat for storing results
mat_results <- matrix(NA, nrow = B, ncol = 6)

for(b in seq(B)){

  set.seed(1234+b)
  #generate y
  y = simulate_ar2(Y1=0, Y0=0, N=500, phi1=0.9, phi2=-0.8, sigma=0.01)$Y
  # split in training and test and proportion given
  y_train = y[1:floor(n*p_training)]
  y_test = y[(floor(n*p_training)+1):n]
  # fit AR1 model
  fit_ar1 <- Arima(y_train, order = c(1,0,0), include.mean = FALSE)
  # fit AR2 model
  fit_ar2 <- Arima(y_train, order = c(2,0,0), include.mean = FALSE)
  # predict on test set with both model
  pred_ar1 <- forecast(fit_ar1, h = length(y_test))
  pred_ar2 <- forecast(fit_ar2, h = length(y_test))
  # compute RMSE for both models
  rmse_ar1 <- sqrt(mean((y_test - pred_ar1$mean)^2))
  rmse_ar2 <- sqrt(mean((y_test - pred_ar2$mean)^2))
  # compute the MAE for both models
  mae_ar1 <- mean(abs(y_test - pred_ar1$mean))
  mae_ar2 <- mean(abs(y_test - pred_ar2$mean))
  # 80% quantile forecasts
  q80_ar1 <- pred_ar1$upper[,1]
  q80_ar2 <- pred_ar2$upper[,1]
  
  # Quantile score 
  qs_ar1 <- (alpha-1) * (y_test - q80_ar1) * (y_test < q80_ar1) +
    (alpha) * (y_test - q80_ar1) * (y_test >= q80_ar1)
  
  qs_ar2 <-  (alpha-1) * (y_test - q80_ar2) * (y_test < q80_ar2) +
    (alpha) * (y_test - q80_ar2) * (y_test >= q80_ar2)
  
  # compute mean score qantile score
  mean_qs_ar1 = mean(qs_ar1)
  mean_qs_ar2 = mean(qs_ar2)
  
  # store results in matrix
  mat_results[b, ] = c(rmse_ar1, rmse_ar2, mae_ar1, mae_ar2, mean_qs_ar1, mean_qs_ar2)
  
  cat(paste0(b, "\n"))
}

# plot results
colnames(mat_results) <- c("RMSE_AR1", "RMSE_AR2", "MAE_AR1", "MAE_AR2", "QS80_AR1", "QS80_AR2")
# boxplot comparing ar1 vs ar 2 for each metric
boxplot(mat_results[,1:2], main = "RMSE comparison AR1 vs AR2", names = c("AR1", "AR2"))
boxplot(mat_results[,3:4], main = "MAE comparison AR1 vs AR2", names = c("AR1", "AR2"))
boxplot(mat_results[,5:6], main = "QS80 comparison AR1 vs AR2", names = c("AR1", "AR2"))
