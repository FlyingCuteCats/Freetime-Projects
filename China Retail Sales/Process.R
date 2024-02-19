library(tidyverse)
library(lubridate)

cngen <- read_csv("/Users/Eliot 1/Library/Mobile Documents/com~apple~CloudDocs/Data Analysis Projects/Retail Sales/CN General Catalogues.csv")
usret <- read_csv("/Users/Eliot 1/Library/Mobile Documents/com~apple~CloudDocs/Data Analysis Projects/Retail Sales/US DataSeries.csv")
cncpi <- read_csv("/Users/Eliot 1/Library/Mobile Documents/com~apple~CloudDocs/Data Analysis Projects/Retail Sales/CN CPI index.csv")
cnspe <- read_csv("/Users/Eliot 1/Library/Mobile Documents/com~apple~CloudDocs/Data Analysis Projects/Retail Sales/CN Specific Catalogues.csv")


# ggplot(usret, aes(
#   x = Date, 
#   y = `Retail Sales Adjusted` / `US CPI Adjusted`)) + 
#   geom_line()

cngen$Date <- as.Date(cngen$Date)
cncpi$Date <- as.Date(cncpi$Date)
cngen |> arrange(Date) -> cngen

#CPI-adjusted Retail Sales

cnadj <- cngen |> 
  select(Date,`Retail Sales (CNY 100M)`) |> 
  left_join(cncpi, by = "Date")

cncpi |> ggplot(aes(x = Date, 
                    y = CPIIndex)) + 
  geom_line() + 
  geom_smooth() + 
  ylab("CPI Index")

cnadj |> ggplot(aes(x = Date, 
                    y = `Retail Sales (CNY 100M)` / CPIIndex)) + 
  geom_line() + 
  geom_smooth() + 
  ylab("Adjusted Retail Sales (CNY 100M)")

cnadj |> filter(year(Date) >= 2015) |> 
  ggplot(aes(x = Date, 
             y = CPIIndex)) + 
  geom_line() + 
  geom_smooth() + 
  ylab("CPI Index")

cnadj |> filter(year(Date) >= 2015) |> 
  ggplot(aes(x = Date, 
                    y = `Retail Sales (CNY 100M)` / CPIIndex)) + 
  geom_line() + 
  geom_smooth() + 
  ylab("Adjusted Retail Sales (CNY 100M)")

#lag plots

series <- sapply(cngen[,2:45], function (x) x[!is.na(x)])

lag.plot(series[[1]], 
         set.lags = c(10,20,30,40,50,60), 
         main = "All Data")
acf(series[[1]], 
    lag.max = 500, 
    main = "All Data")

cngen2015on <- cngen |> filter(year(Date) >= 2015)
series2015on <- sapply(cngen2015on[,2:45], function (x) x[!is.na(x)])
lag.plot(series2015on[[1]], 
         set.lags = c(10,20,30,40,50,60), 
         main = "Data since 2015")
acf(series2015on[[1]], 
    lag.max = 150, 
    main = "Data since 2015")

cngenbefore2015 <- cngen |> filter(year(Date) < 2015)
seriesbefore2015 <- sapply(cngenbefore2015[,2:45], function (x) x[!is.na(x)])
lag.plot(seriesbefore2015[[1]], 
         set.lags = c(10,20,30,40,50,60), 
         main = "Data before 2015")
acf(seriesbefore2015[[1]], 
    lag.max = 150, 
    main = "Data before 2015")

cngen2005_2015 <- cngen |> filter(year(Date) <= 2015, year(Date) >= 2005)
series2005_2015 <- sapply(cngen2005_2015[,2:45], function (x) x[!is.na(x)])
lag.plot(series2005_2015[[1]], 
         set.lags = c(10,20,30,40,50,60), 
         main = "Data btw 2005 and 2015")
acf(series2005_2015[[1]], 
    lag.max = 150, 
    main = "Data btw 2005 and 2015")

ggplot(cngen, 
       aes(x = Date, y = `Retail Sales (CNY 100M)`)) + 
  geom_line() + 
  geom_hline(yintercept = 25000)

ggplot(cngen2015on, 
       aes(x = Date, y = `Retail Sales (CNY 100M)`)) + 
  geom_line() + 
  geom_hline(yintercept = 25000)