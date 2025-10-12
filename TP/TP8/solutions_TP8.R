# TP8

# clean ws
rm(list=ls())

# pkg
library(fpp3)
library(xlsx)
library(ggplot2)
library(fpp2)

# ---------------- ex1

# a. Plot the series and discuss the main features of the data.

data(books)
str(books)
?books

# Daily sales of paperback and hardcover books at the same store.

plot(books)
plot(books[,1], main = "Paperback")
plot(books[,2], main = "Hardcover")


days <- seq(1, length.out = length(books[,1]), by = 1 ) # by 1 month
paperback <- as.numeric(books[,1] )
hardcover <- as.numeric(books[,2])

books_df <-  
  bind_cols(Dates = days) |> 
  bind_cols(Paperback = paperback) |> 
  bind_cols(Hardcover = hardcover) 


books_df
ggplot(books_df, aes(Dates, Paperback)) +
  geom_line(color = "blue") + 
  geom_line(aes(Dates, Hardcover), color = "red")+
  labs(title = "Books sales") 

# Comments: seasonality and upward trend for both cases. 

# b. Use simple exponential smoothing with the ses function and explore different values 
# of α for the paperback series. Record the within-sample 
# SSE for the one-step forecasts. Plot SSE against α and 
# find which value of α works best. --> α = 0.21
# What is the effect of α on the forecasts? As α increases, the prediction has "shorter memory".

?ses #fpp2, forecast package

paperback <- books[,1]

help("ses")

par(mfrow=c(2,2))

for(i in seq(0.01, 0.99, 0.1)){
  
  
  model.ses = ses(paperback, alpha = i, initial = "simple", h = 1)
  
  alpha_txt = paste("SES alpha:",i,sep=" ")
  
  rmse = round(sqrt(sum(model.ses$residuals^2)/length(model.ses$residuals)),2) 
  
  # sse = round(sum(model.ses$residuals^2),2) not on the same scale
  
  rmse_txt = paste("rmse:",rmse,sep=" ")
  
  title_txt = paste(alpha_txt,rmse_txt, sep =" ")
  
  plot(model.ses, main = title_txt, PI=FALSE)
  lines(fitted(model.ses), lwd=2, col="red")
  
}

# c. Now let ses select the optimal value of α. 
# Use this value to generate forecasts for the next four
# days. Compare your results with 2.

par(mfrow=c(1,1))

model_ses_simple = ses(paperback, initial = "simple", h = 40)
str(model_ses_simple)
summary(model_ses_simple)

plot(model_ses_simple, PI=FALSE)
lines(fitted(model_ses_simple), lwd=2, col="red")

# Similar results: α = 0.2125

# d. Repeat but with initial="optimal". 
# How much difference does an optimal initial level make? We improve the RMSE.  

model_ses_optimal = ses(paperback, initial = "optimal", h = 4)
summary(model_ses_optimal)
plot(model_ses_optimal, PI=FALSE)
lines(fitted(model_ses_optimal), lwd=2, col="red")


######################################################################

# Exercise 2. Use the data set in Exercise 1, to repeat the analysis 
# with several variants of a holt’s model:

# 1. A holt’s model with the holt function in R. Summarize the results, 
# plot the series and add the past forecasts. Start first with some 
# random choices for α and ˜ β (etc.) and proceed after checking and
# choosing the model with the optimal choices for them.

# Note: there are some constraints on the parameters: 0 < α < 1, 
# 0 ⩽ ˜ β ⩽ 1, 0 < γ < 1 − α, 0.8 < Φ < 0.98. 
# Parameter γ is only needed for seasonality effect. 
# Parameter Φ is needed in case of damping effect.

?holt

par(mfrow=c(2,2))

for(i in seq(0.2,0.8,0.2)){
  model.holt = holt(paperback, alpha = i, beta = i,initial = "optimal", h = 1)
  
  alpha_beta_txt = paste("Holt's alpha and beta:", i, sep=" ")
  rmse = round(sqrt(sum(model.holt$residuals^2)/length(model.holt$residuals)),2)
  rmse_txt = paste("rmse:", rmse, sep=" ")
  title_txt = paste(alpha_beta_txt, rmse_txt, sep =" ")
  
  plot(model.holt, main = title_txt, PI=FALSE)
  lines(fitted(model.holt), lwd=2, col="red")
}
par(mfrow=c(1,1))

model.holt = holt(paperback, initial = "optimal", h=40)
summary(model.holt)

plot(model.holt)
lines(fitted(model.holt), lwd=2, col="red")


# 2. A holt’s model with damped trend (see options in ?holt). 
# Summarize it, plot the series and add the past forecasts.

model.holt = holt(paperback, initial = "optimal", h=4, damped=TRUE) #phi, beta, alpha (if null are estimated)
summary(model.holt)
plot(model.holt)
lines(fitted(model.holt), lwd=2, col="red")
?holt

# Note: RMSE are on the same scale and can be compared... the smallest the better!

# 3. A holt’s model with the ets function. Summarize it, 
# plot the decomposition, plot the forecast and add the past forecasts.
# Note: In the ets function, the first letter corresponds to the error type. 
# Use A (the only models seen in class until now).

model.ets = ets(y=paperback, model="AAN", damped=TRUE)
summary(model.ets)
plot(model.ets) # decomposition

plot(forecast(model.ets, h=4), PI=FALSE)
lines(fitted(model.ets), lwd=2, col="red")

###############################################################################

## Exercise 3
# Use the domotic data set (Domotic2.xlsx) available in Moodle under datasets. 
# In this exercise, the weather temperature is to be predicted (WeatherTemperature). 
# This is indeed an important information to set the working parameters of the domotic house.


#1. Load the data, take only the data of April (18.04.2012 - 30.04.2012) and 
#   the temperature column (Temperature_Comedor_Sensor) and plot them. 
#   Do you think it can be predicted using a Holt-Winters method? Why?

# Yes, the seasonality is present so we need to take it into account.

# test = openxlsx::read.xlsx("data/Domotic2.xlsx")
# install.packages("readxl")
domotic = readxl::read_xlsx("TP/data/Domotic2.xlsx")
# domotic = read.xlsx("data/Domotic2.xlsx",sheetName = "Domotic_house")
str(domotic)
head(domotic[, 1:3])
domotic$Date <- as.Date(domotic$Date, origin = "1899-12-30")

april <- domotic[1:1248,1:3] 

plot(april$Temperature_Comedor_Sensor, ylab = "Temperature Sensor", type= "l")

ggplot(april, aes(seq(dim(april)[1]), Temperature_Comedor_Sensor)) +
  geom_line() + labs(x = "Time", y = "Temperature", title = "Temperature")


# 2. Compute the hourly averages over this period and build a ts object with 
# them (seasonality 24 hours). Use this object to make a forecast for May. 
# Plot the forecasts

temp.avg = matrix(april$Temperature_Comedor_Sensor, nc=4, byrow=TRUE)
temp.avg = apply(temp.avg, 1, mean)
temp.ts = ts(temp.avg, frequency=24, start=c(18,1)) #data starts the 18th of April and end the 30th
plot.ts(temp.ts)

?hw

temp.hw = hw(temp.ts, damped=FALSE, seasonal="additive")
summary(temp.hw)
plot(temp.hw, PI=FALSE)
lines(fitted(temp.hw), lwd=2, col="red")

# 3. Compare your forecast with the observed data for May. Does the prediction look good? 

may.df <- domotic[1249:1375,1:3] 

may.df = rbind(may.df,NA) #missing one quarter

temp.avg = matrix(may.df$Temperature_Comedor_Sensor, nc=4, byrow=TRUE)
temp.avg = apply(temp.avg, 1, mean, na.rm=TRUE)

temp.may.ts <- ts(temp.avg, frequency=24, start=c(31,1))

plot(temp.hw, PI=FALSE)
lines(fitted(temp.hw), lwd=2, col="red") # add the past forecasts
lines(temp.may.ts, col="black")


# 4. Can this analysis be repeated for "CO2ComedorSensor" variable? 
# Plot the series of data and comment the previous question.

co2 <- domotic$CO2_Comedor_Sensor
plot.ts(co2) 

#seasonality not clear as before, probably we could think at other methods as well.

###############################################################################

## Exercise 4 

# Data set global_economy contains the annual Exports from many countries. Select one country to analyse.

# 1. Plot the Exports series for selected country and discuss the main features of the data.

# Look at the data
View(global_economy) 

# group by countries
global_economy |> 
  group_by(Country, Code) |> 
  count(n()) 
# print(n=263)  # to print all countries

# Country Luxembourg
lux_export <- global_economy |> 
  filter(Code == "LUX") |> 
  select(Exports)

head(lux_export)

lux_export |> 
  autoplot(Exports) + labs(title = "Luxembourg export")

# Data on Luxembourg export cover the period from 1960 to 2017. 
# Data exhibit an exponentially increasing trend with no seasonality.

# 2. Use one of the viewed methods (simple exponential smoothing, Holt, Holt-Winter) which is appropriated to your series. 
# Justify your choice. Comment on your model’s parameter(s). How well does your model fit the data?

# For series with trend Holt's method is appropriate and it fits well the data in this case. 

par(mfrow=c(1,2))

# Holt method
holt_lux <- forecast::holt(lux_export$Exports, 
                           #PI = F, h=1
)
summary(holt_lux)

plot(holt_lux)
lines(fitted(holt_lux), lwd=2, col="red")

# Holt model with damped trend
holt_lux_damped <- forecast::holt(lux_export$Exports, damped = T)
summary(holt_lux_damped)

plot(holt_lux_damped)
lines(fitted(holt_lux_damped), lwd=2, col="red")

# With ETS function

# AAN model
holt_fit_AAN <- lux_export |>
  model(
    `Holt's method` = ETS(Exports ~ error("A") + trend("A") + season("N"))
  ) 


?ETS

# have a look at the parameters
report(holt_fit_AAN)

# AMN model
holt_fit_AMN <- lux_export |>
  model(
    `Holt's method` = ETS(Exports ~ error("A") + trend("M") + season("N"))
  ) 

# have a look at the parameters
report(holt_fit_AMN)

# MAN model
holt_fit_MAN <- lux_export |>
  model(
    `Holt's method` = ETS(Exports ~ error("M") + trend("A") + season("N"))
  ) 

# have a look at the parameters
report(holt_fit_MAN)

# MMN model
holt_fit_MMN <- lux_export |>
  model(
    `Holt's method` = ETS(Exports ~ error("M") + trend("M") + season("N"))
  ) 

# have a look at the parameters
report(holt_fit_MMN)

# 3. Run model with different values of the parameter(s). 
# Plot it against parameter(s) values. How each parameter does impact the forecast?

# As alpha increases, more importance is given to present observations for the determination of the level (short memory).

# As beta increases, more importance is given to present variations in the level with respect to past level variations (trends, T_t-1).

par(mfrow=c(2,2))

for(i in seq(0.5, 0.9, 0.2)){
  for(j in seq(0.1, 0.5, 0.2)){
    
    holt_par <- forecast::holt(lux_export$Exports, alpha = i, beta = j,
                               initial = "optimal", h = 1)
    
    alpha_beta_txt = paste("Holt's alpha and beta:", i, j, sep=" ")
    rmse = round(sqrt(sum(holt_par$residuals^2)/length(holt_par$residuals)),2)
    rmse_txt = paste("rmse:", rmse, sep=" ")
    title_txt = paste(alpha_beta_txt, rmse_txt, sep =" ")
    
    plot(holt_par, main = title_txt, PI=FALSE)
    lines(fitted(holt_par), lwd=2, col="red")
  }
}

par(mfrow=c(1,1))

# 4. Provide forecast for 5 steps ahead. Does your prediction look good? Please, explain.

# Yes, the prediction looks good because e.g., it is increasing as h increases (taking into account the trend).

# MMN model
holt_fit_MMN <- lux_export |>
  model(
    `Holt's method` = ETS(Exports ~ error("M") + trend("M") + season("N"))
  ) 
# have a look at the parameters
report(holt_fit_MMN)

# components
components(holt_fit_MMN) |>
  autoplot() +
  labs(title = "ETS components")

# forecast
holt_fit_MMN |> 
  forecast(h = 5) |>
  autoplot(lux_export)+
  labs(title="Luxembourg export, ETS(MMN)")

# With forecast, linear Holt case
par(mfrow=c(1,1))

holt_5h <- forecast::holt(lux_export$Exports,
                          initial = "optimal", h = 5)

summary(holt_5h)

plot(holt_5h)
lines(fitted(holt_5h), lwd=2, col="red")


# ----------------------- exercise 5

library(fpp2)

# In this exercise, we use the paperback data set to build a simulation of path of future sales. 
# To make the illustration, we will use an additive error model with damped additive trend.

head(books)
paperback <- books[,1]

# 1. Fit the model to the data. Plot the model and its forecast.

times <- seq(1, dim(books)[1], 1)    # vector for time
books_df <- data.frame(books[, 1:2]) # Paperback and Hardcover as data.frame


books_tsibble <- books_df |>           # create tsibble object
  mutate(Time = times, .before=1) |> 
  as_tsibble(index = Time)

books_tsibble

# plot
autoplot(books_tsibble) + labs(title = "Paperback books")


# ETS model, "AAdN"
model_AAdN <- books_tsibble |> 
  model(
    `SES` = ETS(Paperback ~ error("A") + trend("Ad") + season("N"))
  )

components(model_AAdN) |> print(n=31)
report(model_AAdN)


# Forecast
AAdN_fc <- model_AAdN |>
  forecast(h=10)
AAdN_fc 

# Plot data and forecast
AAdN_fc |>  
  autoplot(books_tsibble) +
  geom_line(aes(y = .fitted), col="#D55E00",
            data = augment(model_AAdN)) 

components(model_AAdN) |>
  autoplot() +
  labs(title = "ETS(A,Ad,N) components")

# 2. Identify the corresponding state space model equation (see ETS table).

# 3. Identify α, β, φ, σ, the last l (level) and b (trend) 
# (use the notation of the table; in-class notation is slightly different)

components(model_AAdN) |> print(n=31)


report(model_AAdN)

a <- unlist(report(model_AAdN))

# last l
l.30 <- a$SES.fit.states.l31         # 208.2084

# last b
b.30 <- a$SES.fit.states.b31         # 0.9943547

# alpha
alpha <- a$SES.fit.par.estimate1    # 0.03259223

# beta
beta <- a$SES.fit.par.estimate2     # 0.0001000242 

# phi
phi <- a$SES.fit.par.estimate3      # 0.98

# sigma
sigma <- sqrt(a$SES.fit.fit.sigma2) # 34.9327

# 4. The following loop corresponds to the state equations. It makes the loop for 1 path.

# set the random seed to have always the same random numbers generation 
set.seed(3909966)     
h.step <- 10     # 10−step ahead 
l.vec <- numeric(h.step) 
b.vec <- numeric(h.step)
y <- numeric (h.step)

## initialization of y_31, b_31 and l_31 (see ets table)
eps <- rnorm(n=1, mean=0 , sd=sigma)
y[1] <- l.30 + phi*b.30 + eps
l.vec[1] <- l.30 + phi*b.30 + alpha*eps
b.vec[1] <- phi*b.30 + beta*eps

## the loop for the next steps
for (h in 2:10) {
  eps <- rnorm(n=1, mean=0, sd=sigma)
  y[h] <- l.vec[h-1] + phi*b.vec[h-1] + eps
  l.vec[h] <- l.vec[h-1] + phi*b.vec [h-1] + alpha*eps
  b.vec[h] <- phi*b.vec[h-1] + beta*eps
}

plot(paperback, xlim=c(0,40), ylim=c(100,300))
points(y~c(31:40), type = "l" , lwd =2)

# 5. In order to repeat the process R times, the code is adapted to store the results in matrices:

set.seed(123) # set the random seed to have always the same random numbers generation

h.step <- 10 # 10-step ahead
R <- 1000 # 1000 replicates
l.mat <- matrix(nr=h.step, nc=R)
b.mat <- matrix(nr=h.step, nc=R)
y <- matrix(nr=h.step, nc=R)

## initialization of y_31, b_31 and l_31
eps <- rnorm (n=R, mean=0 , sd=sigma)

y[1, ] <- l.30 + phi*b.30 + eps #first row
l.mat[1, ] <- l.30 + phi*b.30 + alpha*eps #first row
b.mat[1, ] <- phi*b.30 + beta*eps #first row

## the loop for the next steps
for(h in 2:10) {
  eps <- rnorm(n=R, mean=0, sd=sigma)
  y[h,] <- l.mat[h-1,] + phi*b.mat[h-1,] + eps
  l.mat[h,] <- l.mat[h-1,] + phi*b.mat[h-1,] + alpha*eps
  b.mat[h,] <- phi*b.mat[h-1,] + beta*eps
}

k <- 10 #paths to show in the plot (bounded by R)

plot(paperback, xlim=c(0, 40), ylim=c(100, 300))
for(j in c(1:k)) { 
  points(y[,j] ~ c(31:40), type="l", lwd=2)
}

# 6. Use the simulated y to compute the following statistics:
#a. the prediction intervals at 95% for the 10-days ahead
#b. the distribution (histogram + summary numbers) of the total sales over the next 5-days
#c. the probability that the sales will increase during the next 3-days.
#Note: loop "for" can be used or the apply function.
#apply(y, 1, mean) means "apply function mean for each row of y".
#apply(y, 2, sd) means "apply function sd for each column of y".

## a.
apply(y, 1, quantile, probs=c(0.025, 0.975))

quantile(x = y[10,],probs = c(0.025,0.975)) #alternative (only for t+10)

## b. 
y.tot <- apply(y[1:5, ], 2, sum)
hist(y.tot)
summary(y.tot)

## c.
mean((y[1,]>y[2,])&(y[2,]>y[3,])) #decrease next 3 days 
mean((y[1,]>y[2,])&(y[2,]>y[3,]) == TRUE) #decrease next 3 days 

mean((y[1,]<y[2,])&(y[2,]<y[3,])) #increase next 3 days
