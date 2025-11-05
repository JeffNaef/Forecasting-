# clean ws
rm(list=ls())

# pkg
library(fpp3)
library(tsibble)

# ----------------------------------- Exercise 1
phi1 = .4
phi2 = .2
sigma=5
sigma2=sigma^2
B =100

# for full likelihood
f1 = function(phi1, phi2, sigma2){
  
  gamma0 = sigma2*(1-phi2) / (1-phi2 - phi1^2*(1+phi2)-phi2^2 +phi2^3)
  gamma1 = phi1*gamma0/(1-phi2)
  gamma2 = gamma0 * (phi1^2 + (1-phi2)*phi2) /(1-phi2) 
  
  
return(c(gamma0, gamma1, gamma2))
  
}

# test

# Function to compute empirical autocovariances for lags 0,1,2
emp_gamma <- function(y){
  sapply(0:2, function(h) cov(y[1:(length(y)-h)], y[(1+h):length(y)]))
}

# Monte Carlo simulation
set.seed(123)
M=1000
n=1000
empirical_matrix <- replicate(M, {
  y <- arima.sim(n = n, list(ar = c(phi1, phi2)), sd = sqrt(sigma2))
  emp_gamma(y)
})

# Prepare for boxplot: each column = lag
empirical_df <- as.data.frame(t(empirical_matrix))
colnames(empirical_df) <- paste0("Lag", 0:2)

# Boxplot with theoretical values
theoretical_gamma = f1(phi1, phi2, sigma2)
# Plot boxplots separately
par(mfrow=c(1,3))
for(i in 1:3){
  boxplot(empirical_df[,i], main = paste0("Lag ", i-1),
          ylab = expression(gamma[h]))
  abline(h = theoretical_gamma[i], col="red")
  points(1, mean(empirical_df[,i]))
}

# Reset plotting layout
par(mfrow = c(1,1))




# ------------------------------------------------------ Exercise 2





# demo optim
# 1. Define the loss function
f <- function(par) {
  x <- par[1]
  y <- par[2]
  (x - 2)^2 + (y + 1)^2
}

# 2. Create grid
x_seq <- seq(-3, 6, length.out = 100)
y_seq <- seq(-5, 3, length.out = 100)
z <- outer(x_seq, y_seq, Vectorize(function(x, y) f(c(x, y))))

# 3. Plot the 3D surface
persp_plot <- persp(
  x_seq, y_seq, z,
  theta = 45, phi = 30, expand = 0.6,
  col = "lightblue",
  shade = 0.5,
  border = NA,
  xlab = "x", ylab = "y", zlab = "f(x, y)",
  main = "3D Loss Surface: f(x, y) = (x - 2)^2 + (y + 1)^2"
)

# 4. Starting and final points
start <- c(-2, 3)
res <- optim(start, f)

# 5. Add points and arrow (projected into 3D)
# Function to project (x, y, z) coordinates onto 2D persp plot
trans3d_fun <- function(x, y, z, pmat) trans3d(x, y, z, pmat)

points(trans3d_fun(start[1], start[2], f(start), persp_plot),
       col = "red", pch = 19)
points(trans3d_fun(res$par[1], res$par[2], f(res$par), persp_plot),
       col = "blue", pch = 19)
arrows(
  x0 = trans3d_fun(start[1], start[2], f(start), persp_plot)$x,
  y0 = trans3d_fun(start[1], start[2], f(start), persp_plot)$y,
  x1 = trans3d_fun(res$par[1], res$par[2], f(res$par), persp_plot)$x,
  y1 = trans3d_fun(res$par[1], res$par[2], f(res$par), persp_plot)$y,
  col = "darkorange", lwd = 2, length = 0.1
)

# 6. Print results
print(res)


cond_loglik <- function(par, y) {
  phi1 <- par[1]
  phi2 <- par[2]
  sigma2 <- exp(par[3])  # enforce positivity
  n <- length(y)
  
  res <- y[3:n] - phi1 * y[2:(n-1)] - phi2 * y[1:(n-2)]
  
  ll <- -0.5 * ( (n-2)*log(2*pi*sigma2) + sum(res^2)/sigma2 )
  return(-ll)  # negative log-likelihood (to minimize)
}


full_loglik <- function(par, y){
  phi1 <- par[1]
  phi2 <- par[2]
  sigma2 <- exp(par[3])
  n <- length(y)
  
  # Theoretical autocovariances
  gamma <- f1(phi1, phi2, sigma2)
  gamma0 <- gamma[1]
  gamma1 <- gamma[2]
  gamma2 <- gamma[3]
  
  # Build 2x2 covariance matrix for first two observations
  Sigma_2 <- matrix(c(gamma0, gamma1,
                      gamma1, gamma0), nrow=2)
  
  # Compute log-likelihood for first two observations
  ll_first2 <- -log(2*pi) - 0.5 * (log(det(Sigma_2)) + t(y[1:2]) %*% solve(Sigma_2) %*% y[1:2])
  
  # Conditional likelihood for y[3:n] given previous two
  res <- y[3:n] - phi1*y[2:(n-1)] - phi2*y[1:(n-2)]
  ll_rest <- -0.5 * ((n-2)*log(2*pi*sigma2) + sum(res^2)/sigma2)
  
  ll_total <- ll_first2 + ll_rest
  return(-as.numeric(ll_total))  # negative log-likelihood
}



# set n for simu
n = 15
B= 500
phi1 = .6
phi2 = .2
sigma=5
sigma2=sigma^2


# matrices to store results
mat_cl <- matrix(NA, nrow=B, ncol=3)
colnames(mat_cl) <- c("phi1","phi2","sigma2")

mat_fl <- matrix(NA, nrow=B, ncol=3)
colnames(mat_cl) <- c("phi1","phi2","sigma2")

mat_ls <- matrix(NA, nrow=B, ncol=3)
colnames(mat_ls) <- c("phi1","phi2","sigma2")

mat_fpp3 <- matrix(NA, nrow=B, ncol=3)
colnames(mat_fpp3) <- c("phi1","phi2","sigma2")

mat_stats_arima <- matrix(NA, nrow=B, ncol=3)
colnames(mat_stats_arima) <- c("phi1","phi2","sigma2")





for(b in seq(B)){
  # b=103
  # generate AR2
  set.seed(123+b)
  
  y = arima.sim(model = list(ar = c(phi1, phi2)), n = n, sd = sigma)
  
  
  # full likelihood
  
  fit_fl_optim <- optim(c(phi1, phi2 , log(sigma)), fn = full_loglik, y = y)
  theta_hat_fl_optim <- c(phi1 = fit_fl_optim$par[1], phi2 = fit_fl_optim$par[2], sigma2 = exp(fit_fl_optim$par[3]))
  mat_fl[b,] <- theta_hat_fl_optim
  
  # Conditional likelihood with optim
  fit_cl_optim <- optim(c(phi1, phi2 , log(sigma)), fn = cond_loglik, y = y)
  theta_hat_cl_optim <- c(phi1 = fit_cl_optim$par[1], phi2 = fit_cl_optim$par[2], sigma2 = exp(fit_cl_optim$par[3]))
  mat_cl[b,] <- theta_hat_cl_optim
  # Least squares by hand
  Y_ls <- y[3:n]
  X_ls <- cbind(y[2:(n-1)], y[1:(n-2)])
  phi_hat_ls <- solve(t(X_ls) %*% X_ls) %*% t(X_ls) %*% Y_ls
  
  # residuals
  res_ls <- Y_ls - X_ls %*% phi_hat_ls
  
  # residual variance
  sigma2_hat_ls <- sum(res_ls^2) / (n - 2)
  
  # store
  theta_hat_ls <- c(phi1 = phi_hat_ls[1], phi2 = phi_hat_ls[2], sigma2 = sigma2_hat_ls)
  mat_ls[b,] <- theta_hat_ls
  
  
  # Built-in arima function
  ## Built-in fable AR(2) 

  y_ts <- tibble(time = 1:n, value = y) %>%
    as_tsibble(index = time)
  fit_fpp3 <- y_ts %>%
    model(ar2 = ARIMA(value ~ 0 + pdq(2,0,0)))
  
  mod <- fit_fpp3$ar2[[1]]
  
  if (!fabletools::is_null_model(mod)) {
    theta_hat_fpp3 <- c(
      coef(fit_fpp3)$estimate[1:2],
      as.numeric(glance(fit_fpp3)[, "sigma2"])
    )
    mat_fpp3[b, ] <- theta_hat_fpp3
  } else {
    mat_fpp3[b, ] <- c(NA, NA, NA)  # fill NA if model failed
  }
  
  # stats arima
  fit = stats::arima(y, order=c(2,0,0), method="ML", include.mean = FALSE)
  theta_hat_stats_arima = c(fit$coef, fit$sigma2)
  mat_stats_arima[b,] <- theta_hat_stats_arima
  
  
  # save in matrix
  cat(paste0(b, "\n"))
}


# boxplot for each parameters comparing each method
boxplot(mat_fl[,1], mat_cl[,1], mat_ls[,1], mat_fpp3[,1],mat_stats_arima[,1], names=c("Full lik","Cond lik","Least sq","fpp3", "stats"), main=expression(phi[1]), ylab=expression(hat(phi)[1]), outline = F)
abline(h=phi1, col="red", lty=2)
boxplot(mat_fl[,2],mat_cl[,2], mat_ls[,2], mat_fpp3[,2],mat_stats_arima[,2],names = c("Full lik","Cond lik","Least sq","fpp3", "stats"), main=expression(phi[2]), ylab=expression(hat(phi)[2]), outline = F)
abline(h=phi2, col="red", lty=2)
boxplot(mat_fl[,3],mat_cl[,3], mat_ls[,3], mat_fpp3[,3],mat_stats_arima[,3],names= c("Full lik","Cond lik","Least sq","fpp3", "stats"), main=expression(sigma^2), ylab=expression(hat(sigma)^2), outline = F)
abline(h=sigma^2, col="red", lty=2)













# ------------------------------------ exercise 3



# Use R to simulate and plot some data from simple ARIMA models.

# 1. Use the following R code to generate data from an AR(1) model with phi_1 = 0.6 and sigma^2=1. 
# The process starts with y_1=0.

y <- numeric(100)
e <- rnorm(100)
for(i in 2:100){
  y[i] <- 0.6*y[i-1] + e[i]}
sim <- tsibble(idx = seq_len(100), y = y, index = idx)
plot(sim, type="l")

ar1 <- function(phi, n = 100L, sigma=.0001) {
  
  y <- numeric(n)
  e <- rnorm(n, sd = sqrt(sigma))
  for (i in 2:n) {
    y[i] <- phi * y[i - 1] + e[i]
  }
  
  tsibble(idx = seq_len(n), y = y, index = idx)
}

# 2. Produce a time plot for the series. How does the plot change as you change phi_1 ?

ar1(0.6)
ar1(0.6) |> autoplot(y) + labs(title=expression(paste(phi, "=0.6")))
ar1(0.99) |> autoplot(y) + labs(title=expression(paste(phi, "=0.95")))
ar1(0.001) |> autoplot(y) + labs(title=expression(paste(phi, "=0.05"))) # when phi_1 = 0, y is a WN
ar1(-0.65) |> autoplot(y) + labs(title=expression(paste(phi, "=-0.65"))) # when phi_1 < 0, y tends to oscillate


# 3. Write your own code to generate data from an MA(1) model with theta_1  =  0.6 and sigma^2=1.

ma1 <- function(theta, n = 100L) {
  y <- numeric(n)
  e <- rnorm(n)
  for (i in 2:n) {
    y[i] <- theta * e[i - 1] + e[i]
  }
  tsibble(idx = seq_len(n), y = y, index = idx)
}

# 4. Produce a time plot for the series. How does the plot change as you change $\theta_1$?

ma1(0.6) |> autoplot(y) + labs(title=expression(paste(theta, "=0.6")))
ma1(0.99) |> autoplot(y) + labs(title=expression(paste(theta, "=0.95")))
ma1(0.001) |> autoplot(y) + labs(title=expression(paste(theta, "=0.05"))) # when \theta_1 = 0, y is a WN
ma1(-0.65) |> autoplot(y) + labs(title=expression(paste(theta, "=-0.65")))

# Increasing the magnitude (absolute value) of \theta_1, will increase the impact of the lagged error term 
# on the current value, making the series more responsive to recent shocks.

# 5. Generate data from an ARMA(1,1) model with $\phi_{1} = 0.6$, $\theta_{1}  = 0.6$ and $\sigma^2=1$.

arma11 <- function(phi, theta, n = 100) {
  y <- numeric(n)
  e <- rnorm(n)
  for (i in 2:n) {
    y[i] <- phi * y[i - 1] + theta * e[i - 1] + e[i]
  }
  tsibble(idx = seq_len(n), y = y, index = idx)
}
arma11(0.6, 0.6) |> autoplot(y)

# 6. Generate data from an AR(2) model with phi_1 =-0.8, phi_2 = 0.3 and sigma^2=1. 
# (Note that these parameters will give a non-stationary series.)

ar2 <- function(phi1, phi2, n = 100) {
  y <- numeric(n)
  e <- rnorm(n)
  for (i in 3:n) {
    y[i] <- phi1 * y[i - 1] + phi2 * y[i - 2] + e[i]
  }
  tsibble(idx = seq_len(n), y = y, index = idx)
}
ar2(-0.8, 0.3) |> autoplot(y) # we violate the condition (phi_2 - phi_1) < 1 --> AR(2) is not stationary

# Plot the latter two series and compare them.

# See graphs above. The non-stationarity of the AR(2) process has led to increasing oscillations







# ACF and PACF for AR and MA models
x = arima.sim(n = 10000, list(order = c(1, 0, 0), ar = 0.5))
plot(acf(x))
plot(pacf(x))
x2 = arima.sim(n = 10000, list(order = c(0, 0, 1), ma = 0.5))
plot(acf(x2))
plot(pacf(x2))



