# ---------------------------- TP 9

# --------------ex2

library(MASS)  # for mvrnorm

set.seed(123)

# ---- Simulation settings ----
n <- 500         # number of time points
k <- 2          # number of series
A_true <- matrix(c(0.5, 0.2,
                   0.1, 0.4), nrow=2, byrow=TRUE)
A_true
Sigma_true <- matrix(c(1, 0.3,
                       0.3, 1), nrow=2)
n_sim <- 1000   # number of simulations

# ---- Storage for estimates (vectors) ----
A11 <- numeric(n_sim)
A12 <- numeric(n_sim)
A21 <- numeric(n_sim)
A22 <- numeric(n_sim)

Sigma11 <- numeric(n_sim)
Sigma12 <- numeric(n_sim)
Sigma21 <- numeric(n_sim)
Sigma22 <- numeric(n_sim)

# ---- Simulation loop ----
for(sim in 1:n_sim){
  # sim=1
  # Generate VAR(1) process
  Y <- matrix(0, nrow=n, ncol=k)
  Eps <- mvrnorm(n=n, mu=rep(0,k), Sigma=Sigma_true)
  
  for(t in 2:n){
    Y[t,] <- A_true %*% Y[t-1,] + Eps[t,]
  }
  
  # Conditional ML / OLS estimation
  Y_lag <- Y[1:(n-1),]
  Y_current <- Y[2:n,]

  A_hat <- t(solve(t(Y_lag) %*% Y_lag) %*% t(Y_lag) %*% Y_current)
  E_hat <- Y_current - Y_lag %*% A_hat
  Sigma_hat <- t(E_hat) %*% E_hat / (n-1)
  
  # Store estimates in vectors
  A11[sim] <- A_hat[1,1]
  A12[sim] <- A_hat[1,2]
  A21[sim] <- A_hat[2,1]
  A22[sim] <- A_hat[2,2]
  
  Sigma11[sim] <- Sigma_hat[1,1]
  Sigma12[sim] <- Sigma_hat[1,2]
  Sigma21[sim] <- Sigma_hat[2,1]
  Sigma22[sim] <- Sigma_hat[2,2]
  
  cat("Simulation", sim, "completed\n")
}

# ---- Boxplots for each parameter ----
par(mfrow=c(2,2))  # 2x2 layout

boxplot(A11, main="A11", col="lightblue")
abline(h=A_true[1,1], col="red", lwd=2)

boxplot(A12, main="A12", col="lightgreen")
abline(h=A_true[1,2], col="red", lwd=2)

boxplot(A21, main="A21", col="lightpink")
abline(h=A_true[2,1], col="red", lwd=2)

boxplot(A22, main="A22", col="lightyellow")
abline(h=A_true[2,2], col="red", lwd=2)

# Optional: Boxplots for Sigma elements
par(mfrow=c(2,2))
boxplot(Sigma11, main="Sigma11", col="lightblue")
abline(h=Sigma_true[1,1], col="red", lwd=2)

boxplot(Sigma12, main="Sigma12", col="lightgreen")
abline(h=Sigma_true[1,2], col="red", lwd=2)

boxplot(Sigma21, main="Sigma21", col="lightpink")
abline(h=Sigma_true[2,1], col="red", lwd=2)

boxplot(Sigma22, main="Sigma22", col="lightyellow")
abline(h=Sigma_true[2,2], col="red", lwd=2)
