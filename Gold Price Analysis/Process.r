library(tidyverse)
library(lubridate)
library(here)
library(zoo)
library(rlang)

prices <- read.csv(here("PriceSeries.csv"))
prices$Date <- as.Date(prices$Date)

# Randomness Test

series <- sapply(prices[,2:ncol(prices)], function (x) x[!is.na(x)])

for (i in 1:length(series)) {
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

plot_gold_price_spread <- function(data, year, yield_year, by_month = FALSE) {  
  # first pick out the year  
  filtered_data <- data |> 
    filter(year(Date) == year)
  
  # Create the column names
  us_column <- paste("US.", yield_year, "y.yield", sep = "")
  cn_column <- paste("CN.", yield_year, "y.yield", sep = "")
  
  # then generate the plot  
  p <- filtered_data |> 
    ggplot(aes(x = !!sym(us_column) - !!sym(cn_column), y = Gold)) +
    geom_smooth() +
    geom_point(alpha = 0.5) +
    ggtitle(paste("Gold Price in Year", year, "(US-CN", yield_year, "year spread)")) +
    labs(paste(x = "US-CN ", yield_year, "y spread", sep=""))
  
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

plot_gold_price_us <- function(data, year, yield_year, by_month = FALSE) {  
  # first pick out the year  
  filtered_data <- data |> 
    filter(year(Date) == year)
  
  # variable name of the yield
  column_name <- paste("US.", yield_year, "y.yield", sep = "")
  
  # then generate the plot  
  p <- filtered_data |> 
    ggplot(aes(x = !!sym(column_name), y = Gold)) +
    geom_smooth() +
    geom_point(alpha = 0.5) +
    ggtitle(paste("Gold Price in Year", year, "(US", yield_year, "yield)")) +
    labs(paste(x = "US ", yield_year, "y yield", sep=""))
  
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

plot_gold_price_spread(prices, 2024, 10, by_month = F)
plot_gold_price_spread(prices, 2024, 10, by_month = T)
plot_gold_price_spread(prices, 2023, 10, by_month = F)
plot_gold_price_spread(prices, 2023, 10, by_month = T)
plot_gold_price_spread(prices, 2022, 10, by_month = F)
plot_gold_price_spread(prices, 2022, 10, by_month = T)

plot_gold_price_us(prices, 2024, 10, by_month = F)
plot_gold_price_us(prices, 2024, 10, by_month = T)
plot_gold_price_us(prices, 2023, 10, by_month = F)
plot_gold_price_us(prices, 2023, 10, by_month = T)
plot_gold_price_us(prices, 2022, 10, by_month = F)
plot_gold_price_us(prices, 2022, 10, by_month = T)

# decomposition of gold price

goldts <- zoo(prices$Gold, order.by = prices$Date)
#goldts <- na.spline(goldts)
#goldts <- na.fill(goldts, 0)
plot(goldts, ylab = "Gold Price", xlab = "Date")

#startW <- as.numeric(strftime(head(prices$Date, 1), format = "%W"))
#startD <- as.numeric(strftime(head(prices$Date, 1) + 1, format =" %w")) 
#goldts <- ts(goldts, frequency = 5, c(startW, startD))

