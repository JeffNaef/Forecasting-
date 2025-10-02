rm(list=ls())

# Install and load required packages
# install.packages("dCovTS")
library(dCovTS)

# Theoretical distance correlation formula from Székely et al. 
# MEASURING AND TESTING DEPENDENCE BY CORRELATION OF DISTANCES
theoretical_dcor <- function(rho) {
  # rho = 0
  numerator <- rho * asin(rho) + sqrt(1 - rho^2) - rho * asin(rho / 2) - sqrt(4 - rho^2) + 1
  denominator <- 1 + pi/3 - sqrt(3)
  return(sqrt(numerator / denominator))
}
# empirical autocovariance
empirical_autocov <- function(x, lag.max = 20) {
  n <- length(x)
  x_bar <- mean(x)
  sapply(0:lag.max, function(h) {
    sum((x[1:(n-h)] - x_bar)*(x[(1+h):n] - x_bar)) / n
  })
}
# Function to compute empirical autocorrelation manually
empirical_autocor <- function(x, lag.max = 20) {
  acov <- empirical_autocov(x, lag.max)
  acov / acov[1]
}

# Theoretical distance correlation for standard normals (Székely et al. 2007)
# theoretical_dcor_normal <- function(rho) {
#   # rho = correlation
#   (rho * asin(rho) + sqrt(1 - rho^2) -  rho * asin(rho/2) - sqrt(4 - rho^2) + 1) /
#     (1 + pi/3 - sqrt(3))
# }

# theoretical_dcor_normal(.5)
# theoretical_dcor(.5)^2
# ------------ exercise 1

n <- 500
sigma_wn <- 5
X_wn <- rnorm(n, mean = 0, sd = sigma_wn)

# Plot the time series
plot(X_wn, type = "l", main = "White Noise", ylab = "X_t")

# Compute manual autocovariance and autocorrelation
lag_max <- 20
gamma_wn <- empirical_autocov(X_wn, lag_max)
rho_wn <- empirical_autocor(X_wn, lag_max)

# Compare with acf()
acf_wn = acf(X_wn, lag.max = lag_max, main = "ACF White Noise", plot = TRUE)

all.equal(as.numeric(acf_wn$acf), rho_wn)

# Theoretical ACF for white noise
rho_theo_wn <- c(1, rep(0, lag_max))


# Plot: empirical vs theoretical ACF
plot(0:lag_max, rho_wn, type="b", pch=16, col="red",
     ylim=c(-0.5,1), xlab="Lag", ylab="Autocorrelation",
     main="White Noise: Empirical vs Theoretical ACF")
lines(0:lag_max, rho_theo_wn, type="b", lty=2, pch=1, col="blue")
legend("topright", legend=c("Empirical", "Theoretical"), col=c("red","blue"),
       pch=c(16,1), lty=c(1,2), bty="n")


#----------------------------
# Distance correlation
#----------------------------
library(energy)

dcor_manual <- function(x, lag.max = 20) {
  n <- length(x)
  sapply(0:lag.max, function(h) {
    if(h == 0) return(1)
    energy::dcor(x[1:(n-h)], x[(1+h):n])
  })
}

dcor_wn <- dcor_manual(X_wn, lag_max)

# compare with ADCF from dCovTS
dcor_wn_2 = ADCF(X_wn, MaxLag = lag_max)
all.equal(dcor_wn, as.vector(dcor_wn_2))

# Theoretical distance correlation: WN is i.i.d. => rho=0 for lag>0
dcor_theo <- sapply(0:lag_max, function(h) if(h==0) 1 else theoretical_dcor(0))

# Plot: empirical vs theoretical distance correlation
plot(0:lag_max, dcor_wn, type="b", pch=16, col="red",
     ylim=c(0,1), xlab="Lag", ylab="Distance Correlation",
     main="White Noise: Empirical vs Theoretical Distance Correlation")
lines(0:lag_max, dcor_theo, type="b", lty=2, pch=1, col="blue")
legend("topright", legend=c("Empirical", "Theoretical"), col=c("red","blue"),
       pch=c(16,1), lty=c(1,2), bty="n")

#-----------------------------------------------------------

# ------------ Exercise 2: AR(1)

phi <- 0.7
sigma_ar1 <- 2
n <- 500
eps_ar1 <- rnorm(n, 0, sigma_ar1)
X_ar1 <- numeric(n)

# Set first observation from stationary distribution
X_ar1[1] <- rnorm(1, mean = 0, sd = sigma_ar1 / sqrt(1 - phi^2))

# Generate the rest
for(t in 2:n) X_ar1[t] <- phi * X_ar1[t-1] + eps_ar1[t]

# Plot time series
plot(X_ar1, type="l", main="AR(1) Process", ylab="X_t")

# Empirical autocovariance and autocorrelation
gamma_ar1 <- empirical_autocov(X_ar1, lag_max)
rho_ar1 <- empirical_autocor(X_ar1, lag_max)

# Compare with acf
acf_ar1 <- acf(X_ar1, lag.max=lag_max, main="ACF AR(1)", plot=TRUE)
all.equal(as.numeric(acf_ar1$acf), rho_ar1)

# Theoretical ACF for AR(1)
rho_theo_ar1 <- phi^(0:lag_max)

# Plot: empirical vs theoretical ACF
plot(0:lag_max, rho_ar1, type="b", pch=16, col="red",
     ylim=c(-0.5,1), xlab="Lag", ylab="Autocorrelation",
     main="AR(1): Empirical vs Theoretical ACF")
lines(0:lag_max, rho_theo_ar1, type="b", lty=2, pch=1, col="blue")
legend("topright", legend=c("Empirical","Theoretical"), col=c("red","blue"),
       pch=c(16,1), lty=c(1,2), bty="n")

# Empirical distance correlation
dcor_ar1 <- dcor_manual(X_ar1, lag_max)

# Theoretical distance correlation: for Gaussian AR(1), compute for each lag
dcor_theo_ar1 <- sapply(0:lag_max, function(h) {
  if(h==0) return(1)
  theoretical_dcor(phi^h)
})

# Plot: empirical vs theoretical distance correlation
plot(0:lag_max, dcor_ar1, type="b", pch=16, col="red",
     ylim=c(0,1), xlab="Lag", ylab="Distance Correlation",
     main="AR(1): Empirical vs Theoretical Distance Correlation")
lines(0:lag_max, dcor_theo_ar1, type="b", lty=2, pch=1, col="blue")
legend("topright", legend=c("Empirical","Theoretical"), col=c("red","blue"),
       pch=c(16,1), lty=c(1,2), bty="n")



#------------------------------------------------- Exercice 3


n <- 500
theta <- 0.5
sigma_ma1 <- 2

# Innovations
eps_ma1 <- rnorm(n, 0, sigma_ma1)
X_ma1 <- numeric(n)

# First observation with stationary variance
X_ma1[1] <- rnorm(1, mean = 0, sd = sigma_ma1 * sqrt(1 + theta^2))

# Generate rest of the process
for(t in 2:n) X_ma1[t] <- eps_ma1[t] + theta * eps_ma1[t-1]

# Plot time series
plot(X_ma1, type="l", main="MA(1) Process", ylab="X_t")

# Empirical ACF
lag_max <- 20
rho_ma1 <- empirical_autocor(X_ma1, lag_max)

# Compare with acf()
acf_ma1 <- acf(X_ma1, lag.max=lag_max, main="ACF MA(1)", plot=TRUE)
all.equal(as.numeric(acf_ma1$acf), rho_ma1)

# Theoretical ACF
gamma0_ma1 <- sigma_ma1^2 * (1 + theta^2)
gamma1_ma1 <- sigma_ma1^2 * theta
rho_theo_ma1 <- c(1, gamma1_ma1 / gamma0_ma1, rep(0, lag_max-1))

# Plot: empirical vs theoretical ACF
plot(0:lag_max, rho_ma1, type="b", pch=16, col="red",
     ylim=c(-0.5,1), xlab="Lag", ylab="Autocorrelation",
     main="MA(1): Empirical vs Theoretical ACF")
lines(0:lag_max, rho_theo_ma1, type="b", lty=2, pch=1, col="blue")
legend("topright", legend=c("Empirical","Theoretical"), col=c("red","blue"),
       pch=c(16,1), lty=c(1,2), bty="n")

# Empirical distance correlation
dcor_ma1 <- dcor_manual(X_ma1, lag_max)

# Theoretical distance correlation
dcor_theo_ma1 <- sapply(0:lag_max, function(h) {
  if(h==0) return(1)
  if(h==1) return(theoretical_dcor(gamma1_ma1 / gamma0_ma1))
  return(0)
})

# Plot: empirical vs theoretical distance correlation
plot(0:lag_max, dcor_ma1, type="b", pch=16, col="red",
     ylim=c(0,1), xlab="Lag", ylab="Distance Correlation",
     main="MA(1): Empirical vs Theoretical Distance Correlation")
lines(0:lag_max, dcor_theo_ma1, type="b", lty=2, pch=1, col="blue")
legend("topright", legend=c("Empirical","Theoretical"), col=c("red","blue"),
       pch=c(16,1), lty=c(1,2), bty="n")



