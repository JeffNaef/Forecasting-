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
