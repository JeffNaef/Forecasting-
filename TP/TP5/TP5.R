rm(list=ls())

library(forecast)
# ex01 generate from arma 1 0 1, fit model on train, parametric bootstrap sample path, compute mean and quantile on these, plot, compute score for both prediction, report score

n <- 200
phi <- 0.6   
theta <- 0.3  
sigma <- 1

y <- arima.sim(n = n, list(ar = phi, ma = theta), sd = sigma)

p_train <- 0.7
n_train <- floor(n * p_train)
y_train <- y[1:n_train]
y_test <- y[(n_train+1):n]
h <- length(y_test)


fit <- Arima(y_train, order = c(1,0,1), include.mean = FALSE)


B <- 500  # number of bootstrap paths
boot_preds <- matrix(NA, nrow = h, ncol = B)

for(b in 1:B){
  boot_path <- simulate(fit, nsim = h, bootstrap = TRUE)
  boot_preds[,b] <- boot_path
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
  y = arima.sim(model = list(ar = phi), n = n, sd = sigma_eps)
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





















