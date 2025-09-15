rm(list=ls())

# -------------- Exercise 1


# Set seed for reproducibility
set.seed(123)

# Parameters
n <- 500           # Length of time series
m_realizations <- 100  # Number of realizations for empirical statistics

# Simulate a single realization of white noise
X <- rnorm(n, mean = 0, sd = 1)

# Plot the time series
plot(1:n, X, type = "l", col = "blue", lwd = 1.5,
     main = "Strongly Stationary Process: White Noise",
     xlab = "Time", ylab = "X_t")

# Compute empirical mean and variance over multiple realizations
means <- numeric(n)
vars <- numeric(n)

for(t in 1:n){
  X_mult <- matrix(rnorm(n * m_realizations, mean = 0, sd = 1), nrow = m_realizations)
  means[t] <- mean(X_mult[, t])
  vars[t] <- var(X_mult[, t])
}

# Plot mean over time
plot(1:n, means, type = "l", col = "darkgreen", lwd = 1.5,
     main = "Empirical Mean over Time",
     xlab = "Time", ylab = "Mean")

# Plot variance over time
plot(1:n, vars, type = "l", col = "purple", lwd = 1.5,
     main = "Empirical Variance over Time",
     xlab = "Time", ylab = "Variance")

# Compute and plot empirical autocovariance
acf(X, type = "covariance", main = "Empirical Autocovariance Function", col = "red")



# ---------------- Exercise 2: consider a weak stationary but not strong stationary process


set.seed(12345)
n <- 500      # number of time points
Nrep <- 1000  # number of independent realizations (for ensemble statistics)

# --- 1) Simulate a single realization ---
U_single <- runif(1, min = -pi, max = pi)   # sample one U
tvec <- 1:n
X_single <- cos(tvec * U_single)

# Plot the single realization
plot(tvec, X_single, type='l', main=paste("One realization of X_t = cos(t U), U =", round(U_single,3)),
     xlab="t", ylab="X_t")

# --- 2) Generate multiple realizations ---
U_vec <- runif(Nrep, min = -pi, max = pi)      # Nrep independent U's
# Create matrix: each row is a realization
X_mat <- t(sapply(U_vec, function(u) cos(tvec * u)))   # Nrep x n

# Empirical mean and variance at each t
mean_t <- colMeans(X_mat)
var_t  <- apply(X_mat, 2, var)

# Plot empirical mean
plot(tvec, mean_t, type='l', main="Empirical mean of X_t across realizations",
     xlab="t", ylab="Mean(X_t)")

# Plot empirical variance
plot(tvec, var_t, type='l', main="Empirical variance of X_t across realizations",
     xlab="t", ylab="Var(X_t)")

# --- 3) Show weak stationarity ---
# The process is weakly stationary if mean is constant and autocovariance depends only on lag
lag_max <- 20
emp_autocov <- function(h) {
  if(h==0) return(mean(colMeans(X_mat^2)))
  m <- 1:(n-h)
  mean(rowMeans(X_mat[, m] * X_mat[, m+h]))
}
emp_gamma <- sapply(0:lag_max, emp_autocov)

# Plot empirical autocovariance vs lag
plot(0:lag_max, emp_gamma, type='h', main="Empirical autocovariance function",
     xlab="lag h", ylab="Cov(X_t, X_{t+h})")


# ---------------Exercise 3: random walk
set.seed(12345)

n <- 500      # number of time points
Nrep <- 1000  # number of independent realizations

# --- 1) Simulate a single realization ---
X <- rnorm(n)         # i.i.d. standard normal increments
S <- cumsum(X)        # random walk
plot(1:n, S, type='l', main="Single realization of a random walk",
     xlab="t", ylab="S_t")

# --- 2) Generate multiple realizations ---
X_mat <- matrix(rnorm(n * Nrep), nrow=Nrep, ncol=n)   # each row is a realization of increments
S_mat <- t(apply(X_mat, 1, cumsum))                  # random walks

# Empirical mean and variance at each t
mean_t <- colMeans(S_mat)
var_t  <- apply(S_mat, 2, var)

# Plot empirical mean
plot(1:n, mean_t, type='l', main="Empirical mean of S_t across realizations",
     xlab="t", ylab="Mean(S_t)")

# Plot empirical variance
plot(1:n, var_t, type='l', main="Empirical variance of S_t across realizations",
     xlab="t", ylab="Var(S_t)")
