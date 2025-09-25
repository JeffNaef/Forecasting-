# clean ws
rm(list = ls())


#-------------------------------------------
# TP 1
#-------------------------------------------

# ---------------- ressources

# Forecasting book
## https://otexts.com/fpp3   3d edition

# Work on the following resources and understand how the tsibble function works and what are its
# arguments:
# tsibble objects presented in fpp3  https://otexts.com/fpp3/tsibbles.html
# documentation on tsibble objects   https://tsibble.tidyverts.org/

# tibble objects
## https://r4ds.had.co.nz/tibbles.html

# pipes
##  https://r4ds.hadley.nz/workflow-pipes.html 


# ----------- example pipes
mean(log(abs(c(-3, -2, -1, .1, 1, 2, 3))))
c(-3, -2, -1, .1, 1, 2, 3) %>%
  abs() %>%
  log() %>%
  mean()

# 
# iris
# iris
# library(dplyr)
# iris %>%  filter(Species == "setosa") %>%
#   filter(Sepal.Length >1.3) %>%
#   select(Sepal.Length, Species)
# 


# ----------------------------- pkg


library(fpp3)
library(tsibble)
library(dplyr)
library(tsibbledata)

# ------tsibbledata# -------------------------------------- exercise 1

# Load the built-in PBS dataset
data("PBS")
View(PBS)

?mean
?PBS
# Inspect the first rows
head(PBS)

# Check the time index
index(PBS)

# Check the key variables
key_vars(PBS)

# Find the first and last time points
range(PBS%>%pull(Month))

# Count number of observations per key (e.g., per ATC1)
PBS %>%
  count(ATC1_desc)
str(PBS)
colnames(PBS)%>%
  count(ATC2_desc)

# dplyr
# ------------------ exercice 2

# Create a tibble with monthly sales for two products
sales_data <- tibble(
  month = rep(seq.Date(from = as.Date("2025-01-01"), by = "month", length.out = 6), 2),
  product = rep(c("A", "B"), each = 6),
  sales = c(10, 12, 15, 14, 18, 20, 8, 9, 12, 11, 13, 15)
)



attributes(sales_data)

# Convert the tibble into a tsibble
# as_tibble(sales_data, index = sales_data$month, key = sales_data$product)
sales_tsibble <- sales_data %>%
  as_tsibble(index = month, key = product)

# Inspect the tsibble
sales_tsibble
str(sales_tsibble)

# Plot sales over time for each product
ggplot(sales_tsibble, aes(x = month, y = sales, color = product)) +
  geom_line() +
  geom_point() +
  labs(title = "Monthly Sales for Products A and B",
       x = "Month", y = "Sales")





# -------------------- exercice 3

library(readr)
# Read the CSV file
sales_data <- read_csv("TP/data/smartphone_sales.csv")

# Inspect the first rows
head(sales_data)

# Convert to tsibble
sales_tsibble <- sales_data %>%
  as_tsibble(index = month, key = model)

# Inspect the tsibble
sales_tsibble
head(sales_tsibble)

# Optional: Plot monthly sales for each smartphone model
ggplot(sales_tsibble, aes(x = month, y = units_sold, color = model)) +
  geom_line() +
  geom_point() +
  labs(title = "Monthly Smartphone Sales",
       x = "Month", y = "Units Sold")


#-------------------------------------------
## Exercise 4


# 1. Use the help function to explore what the series `gafa_stock`, `PBS`, `vic_elec` 
# and `pelt` represent.

# a. Use `autoplot()` to plot some of the series in these data sets.
# b. What is the time interval of each series?

library(tsibbledata)
gafa_stock

str(gafa_stock)
?gafa_stock
help(gafa_stock)
gafa_stock
data("gafa_stock")
str(gafa_stock)


autoplot(gafa_stock)
gafa_stock |> autoplot()  # %>% from magrittr

gafa_stock$Date
str(gafa_stock)
gafa_stock |>
  mutate(Date = as_date(Date)) |>
  as_tsibble(index = Date) -> gafa_stock_new    # specify the index of each observation following the tsbibble

gafa_stock_new |> 
  filter(Symbol == "AAPL") |> 
  autoplot(Close) +
  labs(title = "Apple stocks", subtitle = "Closing price", y = "USD")



help(PBS)
data(PBS)
PBS
View(PBS)
PBS |> filter(ATC2 == "A10", ATC1 == "A")  |> 
  summarise(Total = sum(Cost)) -> A10 
autoplot(A10)

# group_by not needed in tsibble for the index
interval(PBS)
help(vic_elec)
vic_elec
data(vic_elec)
View(vic_elec)
?vis_elec
vic_elec |> autoplot()
vic_elec |> autoplot(Temperature) + 
  labs(title = "Temperature, Australia", y = "Temperature, C")


vic_elec |> autoplot()
vic_elec |> 
  slice_head(n=500) |> 
  autoplot()

vic_elec |> 
  filter(yearmonth(Time) == yearmonth("2013 June")) |> 
  autoplot(Demand)

yearmonth(vic_elec$Time)

?pelt
help(pelt)

str(pelt)
View(pelt)

?pelt
pelt |> autoplot()
pelt |> autoplot(Lynx)


pelt_longer = pelt |>
  pivot_longer(cols = Hare:Lynx, names_to ="Animal", values_to ="Trappings") 

pelt |>
  pivot_longer(cols = Hare:Lynx, names_to ="Animal", values_to ="Trappings") |>
  autoplot(Trappings)

plot(x = pelt$Year,  y =pelt$Hare, type="l", col="red")
lines(x = pelt$Year,  y =pelt$Lynx, col="blue4")
grid(col= "grey80")

# 2. Use filter() to find what days correspond to the peak closing price for each 
# of the four stocks in gafa_stock.

gafa_stock_new |> 
  group_by(Symbol) |> 
  filter(Close == max(Close))

gafa_stock_new |> 
  group_by(Symbol) |> 
  filter(Close == max(Close)) |>
  select(Symbol, Date, Close)

# 3. The USgas package contains data on the demand for natural gas in the US.
# (a) Install the USgas package.
# install.packages("USgas")
library(USgas)


#--
# (b) Create a tsibble from us_total with year as the index and state as the key.
?us_total
data("us_total")
head(us_total)
str(us_total)

str(us_total)

us_tsibble <- us_total |> 
  as_tsibble(
    index = year,
    key = state
  )


str(us_tsibble)
us_tsibble
str(us_tsibble)
str(us_total)

#--
# (c) Plot the annual natural gas consumption by state for the New England area (comprising the
# states of Maine, Vermont, New Hampshire, Massachusetts, Connecticut and Rhode Island).

unique(us_tsibble$state)




# "b" %in% c("b", "c", "d")

us_tsibble |> 
  filter(state %in% c("Maine", "Vermont", "New Hampshire", "Massachusetts", "Connecticut", "Rhode Island")) |> 
  autoplot() #y


us_tsibble |> 
  filter(state %in% c("Vermont")) |> 
  autoplot()

us_tsibble |> 
  filter(state %in% c("Maine", "Vermont", "New Hampshire", "Massachusetts", "Connecticut", "Rhode Island")) |> 
  summarise(Total = sum(y)) |> 
  autoplot()

us_tsibble |> 
  filter(state %in% c("Maine", "Vermont", "New Hampshire", "Massachusetts", "Connecticut", "Rhode Island")) |> 
  ggplot(aes(year, y)) +
  geom_line() +
  facet_grid(state ~., scales = "free_y") +
  labs(title = "Annual Natural Gas Consumption in New England", y = "Consumption")





# ------------------------------------------ Exercise 5


?aus_production
data("aus_production")
View(aus_production)
colnames(aus_production)
# Quarterly estimates of selected indicators of manufacturing production in Australia.
# Half-hourly data with six series

aus_production |> 
  autoplot(Bricks) + labs(title = "Clay brick production in Australia, mln")


# An upward trend is apparent until 1980, after which the number of clay bricks 
# being produced starts to decline. A seasonal pattern is evident in this data. 
# Some sharp drops in some quarters can also be seen.


## Lynx

?pelt
# Hudson Bay Company trading records for Snowshoe Hare and Canadian Lynx furs from 1845 to 1935.

pelt |> 
  autoplot(Lynx) + labs(title = "Lynx trappings for 1821–1934 in Canada")

# Canadian lynx trappings are cyclic, as the spacing between the peaks 
# is irregular but approximately 10 years.

## Close

?gafa_stock
# historical stock prices from 2014-2018 for Google, Amazon, Facebook and Apple in USD. 

gafa_stock  |> 
  autoplot(Close) + labs(title = "GAFA close stock prices from 2014-2018")

# The four stocks are on different scales, so they are not directly comparable. 
# A plot with faceting would be better.

gafa_stock %>%
  ggplot(aes(x=Date, y=Close, group=Symbol)) +
  geom_line(aes(col=Symbol)) +
  # facet_grid(Symbol ~ ., scales='free') + 
  labs(title = "GAFA close stock prices from 2014-2018")

# The downturn in the second half of 2018 is now very clear, with Facebook 
# taking a big drop (about 20%) in the middle of the year.

# The stocks tend to move roughly together, as you would expect with companies in the same industry.

## Demand

?vic_elec 
# Demand:	Total electricity demand in MWh. half-hourly data.

vic_elec%>%
  head(500)|> 
  autoplot(Demand/1e3) +
  labs(
    x = "Date",
    y = "Demand (GW)",
    title = "Half-hourly electricity demand",
    subtitle = "Victoria, Australia"
  )


# Here the annual seasonality is clear, with high volatility in summer, 
# and peaks in summer and winter. The weekly seasonality is also visible, 
# but the daily seasonality is hidden due to the compression on the horizontal axis.

# Look at the data over a shorter period
vic_elec[350:1900, ] |> 
  autoplot(Demand/1e3) +
  labs(
    x = "Date",
    y = "Demand (GW)",
    title = "Half-hourly electricity demand",
    subtitle = "Victoria, Australia"
  )


# 1e3
# 1e-2


