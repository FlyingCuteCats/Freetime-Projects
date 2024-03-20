library(tidyverse)
library(lubridate)
library(here)

cngen <- read.csv(here("CN General Catalogues.csv"))
cncpi <- read.csv(here("CN CPI index.csv"))
cnspe <- read.csv(here("CN Specific Catalogues.csv"))


cngen$Date <- as.Date(cngen$Date)
cncpi$Date <- as.Date(cncpi$Date)
cngen |> 
  arrange(Date) -> cngen

cngen <- cngen |> 
  rename("Retail Sales (CNY 100M)" = "Retail.Sales..CNY.100M.", 
         "Retail Sales Tot Per Annu (CNY 100M)" = "Retail.Sales.Tot.Per.Annu..CNY.100M.") 

#####

# Experiment on reproducing data of Jan and Feb

genlunar <- cngen |>
  filter(
  month(Date) == 1 | month(Date) == 2,
  is.na(`Retail Sales (CNY 100M)`) == T) |>
  select(Date, `Retail Sales (CNY 100M)`, `Retail Sales Tot Per Annu (CNY 100M)`)

# Solution 1: simply divide the retail sales by days of month
days_in_first_two_months <- function(year) {
  ifelse(leap_year(year), return(31+29),return(31+28))
}

genlunar <- genlunar |> 
  fill(`Retail Sales Tot Per Annu (CNY 100M)`, 
       .direction = "up") |> 
  mutate(`Retail Sales (CNY 100M)` = 
            `Retail Sales Tot Per Annu (CNY 100M)` * days_in_month(Date) / days_in_first_two_months(year(Date))) |> 
  select(Date, `Retail Sales (CNY 100M)`)

for(i in 1:nrow(genlunar)) {
  cngen$`Retail Sales (CNY 100M)`[cngen$Date == genlunar$Date[i]] <- genlunar$`Retail Sales (CNY 100M)`[i]
}

# Solution 2: consider the days of Lunar New Year Holiday as weight
# lunar <- read.csv(here("Lunar Year Start Date.csv"))

#####

# CPI-adjusted Retail Sales for all years and after 2015

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

# lag plots

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
