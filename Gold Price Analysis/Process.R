prices <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Data Analysis Projects/Gold Price/PriceSeries.csv")

library(tidyverse)
library(lubridate)
# library(gridExtra)

# divide by year
t <- as.Date(prices$Date)
prices$Date <- as.Date(prices$Date)
p2024 <- subset(prices, year(Date) == 2024)
p2023 <- subset(prices, year(Date) == 2023)
p2022 <- subset(prices, year(Date) == 2022)

# Randomness Test

series <- sapply(prices[,2:7], function (x) x[!is.na(x)])

for (i in 1:6) {
  lag.plot(series[[i]], lags = 20, 
           main = paste("Lag Plot of", names(series[i])), 
           layout = c(4,5))
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

ggplot(prices, aes(t, US.10y.yield - CN.10y.yield, colour = factor(year(t)))) + 
  geom_line()

# Gold

ggplot(p2024, 
       aes(x = US.10y.yield - CN.10y.yield, 
           y = Gold, 
           colour = factor(month(Date)))
) + 
  geom_smooth() + 
  geom_point(alpha = 0.5) + 
  ggtitle("2024 whole year")

ggplot(p2024, 
       aes(x = US.10y.yield - CN.10y.yield, y = Gold, colour = day(Date))) + 
  geom_smooth() + geom_point(alpha = 0.5) + 
  facet_wrap(~month(Date)) + 
  ggtitle("2024 whole year by month")

ggplot(p2023, 
       aes(x = US.10y.yield - CN.10y.yield, 
           y = Gold, 
           colour = factor(month(Date)))
       ) + 
  geom_smooth() + 
  geom_point(alpha = 0.5) + 
  ggtitle("2023 whole year")

ggplot(p2023, 
       aes(x = US.10y.yield - CN.10y.yield, y = Gold, colour = day(Date))) + 
  geom_smooth() + geom_point(alpha = 0.5) + 
  facet_wrap(~month(Date)) + 
  ggtitle("2023 whole year by month")


ggplot(p2022, aes(x = US.10y.yield - CN.10y.yield, y = Gold, colour = factor(month(Date)))) + 
  geom_smooth() + geom_point(alpha = 0.5) + 
  ggtitle("2022 whole year")
ggplot(p2022, aes(x = US.10y.yield - CN.10y.yield, y = Gold, colour = day(Date))) + 
  geom_smooth() + geom_point(alpha = 0.5) + 
  facet_wrap(~month(Date)) + 
  ggtitle("2022 whole year by month")

# US-DE spread finally gets pumped
ggplot(p2023, aes(x = US.10y.yield - CN.10y.yield, y = US.10y.yield - DE.10y.yield, colour = factor(month(Date)))) + 
  geom_smooth() + geom_point(alpha = 0.5) + 
  ggtitle("2023 whole year")
ggplot(p2023, aes(x = US.10y.yield - CN.10y.yield, y = US.10y.yield - DE.10y.yield, colour = day(Date))) + 
  geom_smooth() + geom_point(alpha = 0.5) + 
  facet_wrap(~month(Date)) + 
  ggtitle("2023 whole year by month")

# How about US-DE spread and SPX? LOADS OF MESS, same as US-CN spread
# ggplot(p2023, aes(x = US.10y.yield - DE.10y.yield, y = SPX, colour = factor(month(Date)))) + geom_smooth() + geom_point(alpha = 0.5) + ggtitle("2023 whole year")
# ggplot(p2023, aes(x = US.10y.yield - DE.10y.yield, y = SPX, colour = day(Date))) + geom_smooth() + geom_point(alpha = 0.5) + facet_wrap(~month(Date)) + ggtitle("2023 whole year by month")
# 
# ggplot(p2022, aes(x = US.10y.yield - DE.10y.yield, y = SPX, colour = factor(month(Date)))) + geom_smooth() + geom_point(alpha = 0.5) + ggtitle("2022 whole year")
# ggplot(p2022, aes(x = US.10y.yield - DE.10y.yield, y = SPX, colour = day(Date))) + geom_smooth() + geom_point(alpha = 0.5) + facet_wrap(~month(Date)) + ggtitle("2022 whole year by month")




# backup code
# ggplot(p2022, aes(x = US.10y.yield - CN.10y.yield, y = US.10y.yield - DE.10y.yield, colour = factor(month(Date)))) + geom_smooth() + geom_point(alpha = 0.5) + ggtitle("2022 whole year")
# ggplot(p2022, aes(x = US.10y.yield - CN.10y.yield, y = US.10y.yield - DE.10y.yield, colour = day(Date))) + geom_smooth() + geom_point(alpha = 0.5) + facet_wrap(~month(Date)) + ggtitle("2022 whole year by month")

#ggplot(prices, aes(x = DXY, y = Gold)) + geom_smooth() + geom_point() + facet_wrap(~month(Date))
#ggplot(prices, aes(x = US.10y.yield - DE.10y.yield, y = Gold)) + geom_smooth() + geom_point() + facet_wrap(~month(Date))
