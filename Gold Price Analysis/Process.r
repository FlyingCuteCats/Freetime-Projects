library(tidyverse)
library(lubridate)
library(here)
library(zoo)

prices <- read.csv(here("PriceSeries.csv"))
prices$Date <- as.Date(prices$Date)

# Randomness Test

series <- sapply(prices[,2:7], function (x) x[!is.na(x)])

for (i in 1:6) {
  lag.plot(series[[i]], set.lags = c(1, 10, 25, 50, 100, 200), 
           main = paste("Lag Plot of", names(series[i])), 
           layout = c(2,3))
  par(mfcol = c(1,3))
  acf(series[[i]], lag.max = 200, 
      main = paste("ACF of", names(series[i])))
  hist(series[[i]], 
       main = names(series[i]), xlab = names(series[i]))
  qqnorm(series[[i]], 
         main = names(series[i]))
  qqline(series[[i]])
}


# US-CN yield series

ggplot(prices, aes(Date, US.10y.yield - CN.10y.yield, colour = factor(year(Date)))) + 
  geom_line()

# Gold and US-CN yield

plot_gold_price_yield <- function(data, year, by_month = FALSE) {  
  # first pick out the year  
  filtered_data <- data |> 
    filter(year(Date) == year)
  
  # then generate the plot  
  p <- filtered_data |> 
    ggplot(aes(x = US.10y.yield - CN.10y.yield, y = Gold)) +
    geom_smooth() +
    geom_point(alpha = 0.5) +
    ggtitle(paste("Gold Price in Year", year, "(US-CN spread)")) +
    labs(x = "US-CN 10y spread")
  
  # decide which variant to plot   
  if (!by_month) {
    p <- p + aes(colour = factor(month(Date))) +
      labs(colour = "Month")
  } else {
    p <- p + aes(colour = day(Date)) +
      facet_wrap(~month(Date)) +
      labs(colour = "Day")
  }  
  print(p)
}

# gold price and US yield

plot_gold_price_us <- function(data, year, by_month = FALSE) {  
  # first pick out the year  
  filtered_data <- data |> 
    filter(year(Date) == year)
  
  # then generate the plot  
  p <- filtered_data |> 
    ggplot(aes(x = US.10y.yield, y = Gold)) +
    geom_smooth() +
    geom_point(alpha = 0.5) +
    ggtitle(paste("Gold Price in Year", year, "(US yield)")) +
    labs(x = "US yield")
  
  # decide which variant to plot   
  if (!by_month) {
    p <- p + aes(colour = factor(month(Date))) +
      labs(colour = "Month")
  } else {
    p <- p + aes(colour = day(Date)) +
      facet_wrap(~month(Date)) +
      labs(colour = "Day")
  }  
  print(p)
}

plot_gold_price_yield(prices, 2024, by_month = F)
plot_gold_price_yield(prices, 2024, by_month = T)
plot_gold_price_yield(prices, 2023, by_month = F)
plot_gold_price_yield(prices, 2023, by_month = T)
plot_gold_price_yield(prices, 2022, by_month = F)
plot_gold_price_yield(prices, 2022, by_month = T)

plot_gold_price_us(prices, 2024, by_month = F)
plot_gold_price_us(prices, 2024, by_month = T)
plot_gold_price_us(prices, 2023, by_month = F)
plot_gold_price_us(prices, 2023, by_month = T)
plot_gold_price_us(prices, 2022, by_month = F)
plot_gold_price_us(prices, 2022, by_month = T)

# decomposition of gold price

goldts <- zoo(prices$Gold, order.by = prices$Date)
#goldts <- na.spline(goldts)
#goldts <- na.fill(goldts, 0)
plot(goldts, ylab = "Gold Price", xlab = "Date")

#startW <- as.numeric(strftime(head(prices$Date, 1), format = "%W"))
#startD <- as.numeric(strftime(head(prices$Date, 1) + 1, format =" %w")) 
#goldts <- ts(goldts, frequency = 5, c(startW, startD))

