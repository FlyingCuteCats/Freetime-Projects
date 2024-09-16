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

# Reproducing data of Jan and Feb by days of month

genlunar <- cngen |>
  filter(
  month(Date) == 1 | month(Date) == 2,
  is.na(`Retail Sales (CNY 100M)`) == T) |>
  select(Date, `Retail Sales (CNY 100M)`, `Retail Sales Tot Per Annu (CNY 100M)`) |> 
  fill(`Retail Sales Tot Per Annu (CNY 100M)`,.direction = "up") |> 
  mutate(`Retail Sales (CNY 100M)` = as.numeric(difftime(
    paste(year(Date),month(Date)+1,"01",sep="-"),
    paste(year(Date),month(Date),"01",sep="-"),units="days")) / as.numeric(difftime(
      paste(year(Date),"03","01",sep="-"),
      paste(year(Date),"01","01",sep="-"),units="days")) * `Retail Sales Tot Per Annu (CNY 100M)`) |> 
  select(!`Retail Sales Tot Per Annu (CNY 100M)`)


#####

# CPI-adjusted Retail Sales for all years and after 2015

cnadj <- cngen |> 
  left_join(genlunar,by=join_by(Date),keep=T) |> 
  mutate(`Retail Sales (CNY 100M)` = coalesce(`Retail Sales (CNY 100M).x`,`Retail Sales (CNY 100M).y`)) |> 
  rename(Date=Date.x) |> 
  select(Date, `Retail Sales (CNY 100M)`) |> 
  left_join(cncpi, by = "Date")

cnadj <- cnadj |> 
  filter(year(Date)>=1987) |> 
  mutate(`Adjusted Retail Sales (CNY 100M)` = `Retail Sales (CNY 100M)` / CPIIndex)

cncpi |> ggplot(aes(x = Date, 
                    y = CPIIndex)) + 
  geom_line() + 
  geom_smooth() + 
  ylab("CPI Index")

cnadj |> ggplot(aes(x = Date, 
                    y = `Adjusted Retail Sales (CNY 100M)`)) + 
  geom_line() + 
  geom_smooth() 

cnadj |> filter(year(Date) >= 2015) |> 
  ggplot(aes(x = Date, 
             y = CPIIndex)) + 
  geom_line() + 
  geom_smooth() + 
  ylab("CPI Index")

cnadj |> filter(year(Date) >= 2015) |> 
  ggplot(aes(x = Date, 
                    y = `Adjusted Retail Sales (CNY 100M)`)) + 
  geom_line() + 
  geom_smooth() 

# decomposition

# library(tidymodels)
# library(tsibble)
# library(fabletools)
# library(fable)
adj <- cnadj |> 
  mutate(Retail_adj = `Retail Sales (CNY 100M)` / CPIIndex) |> 
  select(Date, Retail_adj) |> 
  mutate(Month = yearmonth(Date)) |> 
  select(-Date) |> 
  as_tsibble(index = Month) |> 
  select(Month, Retail_adj)

library(feasts)

dcmp <- adj %>% model(stl = STL(Retail_adj))
components(dcmp) |> head


# lag plots

#series <- sapply(cngen[,2:45], function (x) x[!is.na(x)])
series <- cnadj[,c(1,4)]

lag.plot(series[[2]], 
         set.lags = c(12,12*2,12*3,12*4,12*5,12*6), 
         main = "All Data")
acf(series[[2]], 
    lag.max = 500, 
    main = "All Data")

cngen2015on <- cngen |> filter(year(Date) >= 2015)
series2015on <- sapply(cngen2015on[,2:45], function (x) x[!is.na(x)])
lag.plot(series2015on[[1]], 
         set.lags = c(12,12*2,12*3,12*4,12*5,12*6), 
         main = "Data since 2015")
acf(series2015on[[1]], 
    lag.max = 150, 
    main = "Data since 2015")

cngenbefore2015 <- cngen |> filter(year(Date) < 2015)
seriesbefore2015 <- sapply(cngenbefore2015[,2:45], function (x) x[!is.na(x)])
lag.plot(seriesbefore2015[[1]], 
         set.lags = c(12,12*2,12*3,12*4,12*5,12*6), 
         main = "Data before 2015")
acf(seriesbefore2015[[1]], 
    lag.max = 150, 
    main = "Data before 2015")

cngen2005_2015 <- cngen |> filter(year(Date) <= 2015, year(Date) >= 2005)
series2005_2015 <- sapply(cngen2005_2015[,2:45], function (x) x[!is.na(x)])
lag.plot(series2005_2015[[1]], 
         set.lags = c(12,12*2,12*3,12*4,12*5,12*6), 
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


