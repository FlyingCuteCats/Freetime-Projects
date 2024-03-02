library(tidyverse)
library(lubridate)
library(here)

prices <- read.csv(here("PriceSeries.csv"))
prices$Date <- as.Date(prices$Date)

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

prices |> 
  filter(year(Date) == 2024) |> 
  ggplot(aes(x = US.10y.yield - CN.10y.yield, 
           y = Gold, 
           colour = factor(month(Date)))
) + 
  geom_smooth() + 
  geom_point(alpha = 0.5) + 
  ggtitle("Gold Price in Year 2024") + 
  labs(x = "US-CN 10y spread", 
       colour = "Month")

prices |> 
  filter(year(Date) == 2024) |> 
  ggplot(aes(x = US.10y.yield - CN.10y.yield, y = Gold, colour = day(Date))) + 
  geom_smooth() + geom_point(alpha = 0.5) + 
  facet_wrap(~month(Date)) + 
  ggtitle("Gold Price in Year 2024 by month") + 
  labs(x = "US-CN 10y spread", 
       colour = "Month")

prices |> 
  filter(year(Date) == 2023) |> 
  ggplot(aes(x = US.10y.yield - CN.10y.yield, 
           y = Gold, 
           colour = factor(month(Date)))
       ) + 
  geom_smooth() + 
  geom_point(alpha = 0.5) + 
  ggtitle("Gold Price in Year 2023") + 
  labs(x = "US-CN 10y spread", 
       colour = "Month")

prices |> 
  filter(year(Date) == 2023) |> 
  ggplot(aes(x = US.10y.yield - CN.10y.yield, y = Gold, colour = day(Date))) + 
  geom_smooth() + geom_point(alpha = 0.5) + 
  facet_wrap(~month(Date)) + 
  ggtitle("Gold Price in Year 2023 by month") + 
  labs(x = "US-CN 10y spread", 
       colour = "Month")

prices |> 
  filter(year(Date) == 2022) |> 
  ggplot(aes(x = US.10y.yield - CN.10y.yield, y = Gold, colour = factor(month(Date)))) + 
  geom_smooth() + geom_point(alpha = 0.5) + 
  ggtitle("Gold Price in Year 2022") + 
  labs(x = "US-CN 10y spread", 
       colour = "Month")

prices |> 
  filter(year(Date) == 2022) |> 
  ggplot(aes(x = US.10y.yield - CN.10y.yield, y = Gold, colour = day(Date))) + 
  geom_smooth() + geom_point(alpha = 0.5) + 
  facet_wrap(~month(Date)) + 
  ggtitle("Gold Price in Year 2022 by month") + 
  labs(x = "US-CN 10y spread", 
       colour = "Month")

# US-DE spread finally gets pumped

prices |> 
  filter(year(Date) == 2024) |> 
  ggplot(aes(x = US.10y.yield - CN.10y.yield, y = US.10y.yield - DE.10y.yield, colour = factor(month(Date)))) + 
  geom_smooth() + geom_point(alpha = 0.5) + 
  ggtitle("2024 whole year")

prices |> 
  filter(year(Date) == 2024) |> 
  ggplot(aes(x = US.10y.yield - CN.10y.yield, y = US.10y.yield - DE.10y.yield, colour = day(Date))) + 
  geom_smooth() + geom_point(alpha = 0.5) + 
  facet_wrap(~month(Date)) + 
  ggtitle("2024 whole year by month", 
          colour = "Month")

prices |> 
  filter(year(Date) == 2023) |> 
  ggplot(aes(x = US.10y.yield - CN.10y.yield, y = US.10y.yield - DE.10y.yield, colour = factor(month(Date)))) + 
  geom_smooth() + geom_point(alpha = 0.5) + 
  ggtitle("2023 whole year")

prices |> 
  filter(year(Date) == 2023) |> 
  ggplot(aes(x = US.10y.yield - CN.10y.yield, y = US.10y.yield - DE.10y.yield, colour = day(Date))) + 
  geom_smooth() + geom_point(alpha = 0.5) + 
  facet_wrap(~month(Date)) + 
  ggtitle("2023 whole year by month", 
          colour = "Month")
