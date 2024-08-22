library(tidyverse)
library(lubridate)
library(here)
library(zoo)
library(gridExtra)

### Wrangling

fff_dates <- read.csv(here("FFF_Matrix/FedMeeting_Date.csv"))
fff_dates$Date <- as.Date(fff_dates$Date)
fff_dates |> mutate(Meeting = paste(month.name[month(Date)], year(Date)))

matrix_path <- here("FFF_Matrix/")
matrix_list <- list.files(matrix_path, pattern = "FedMeeting_\\d{8}.csv", full.names = TRUE)
combined_data <- matrix_list |> 
  map_dfr(.f = function(file_name) {
    read_csv(file_name) |> 
      mutate(
        Meeting = str_extract(basename(file_name), "\\d+")
      )
  })
combined_data$Date <- as.Date(combined_data$Date, "%d/%m/%Y")
combined_data$Meeting <- combined_data$Meeting |> 
  as.Date("%Y%m%d")
combined_data <- combined_data |> 
  mutate(DaysDiff = difftime(Meeting, Date, units = "days"), 
         MeetingName = paste(month.name[month(Meeting)], year(Meeting)))

### EDA

fff_2024 <- combined_data |> 
  filter(year(Date) >= 2024) 

fff_2024 |> 
  filter(Meeting > Sys.Date()) |> 
  ggplot(aes(x=Date)) + 
  geom_line(aes(y = `(525-550)`, colour = "(525-550)")) + 
  geom_line(aes(y = `(500-525)`, colour = "(500-525)")) + 
  geom_line(aes(y = `(475-500)`, colour = "(475-500)")) + 
  facet_wrap(~Meeting) + 
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey")

### Matrix


matrix_table <- function(data, targetrange) {
  columnList = c("Date", targetrange, "Meeting", "DaysDiff")
  fff_target <- data |> 
    select(all_of(columnList))
  
  fff_matrix_3mo <- fff_target |> 
    group_by(Date) |> 
    filter(Meeting > Date) |> 
    arrange(Date, Meeting) |> 
    slice(1:3) |> 
    summarise(Sentiment_3mon = sum(.data[[targetrange]] * as.numeric(DaysDiff))/sum(as.numeric(DaysDiff)))
  
  fff_matrix_8mo <- fff_target |> 
    group_by(Date) |> 
    filter(Meeting > Date) |> 
    arrange(Date, Meeting) |> 
    slice(1:8) |> 
    summarise(Sentiment_8mon = sum(.data[[targetrange]] * as.numeric(DaysDiff))/sum(as.numeric(DaysDiff)))
  
  fff_matrix <- merge(fff_matrix_3mo, fff_matrix_8mo, by = "Date", all = T)
  fff_matrix <- fff_matrix |> mutate(TargetRange = targetrange)
  return(fff_matrix)
}

matrix_list <- rbind(matrix_table(fff_2024, "(525-550)"), 
                     matrix_table(fff_2024, "(500-525)"), 
                     matrix_table(fff_2024, "(475-500)"))

matrix_list |> ggplot(aes(x = Date)) + 
  facet_wrap(~TargetRange) + 
  geom_line(aes(y = Sentiment_3mon, colour = "Sentiment (3-month ST)")) + 
  geom_line(aes(y = Sentiment_8mon, colour = "Sentiment (8-month LT)")) + 
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey") + 
  labs(title = "Sentiment for Various FFF rates", 
       y = "Sentiment")

matrix_list |> ggplot(aes(x = Date)) + 
  geom_line(aes(y = Sentiment_3mon, colour = TargetRange)) + 
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey") + 
  labs(title = "Short Term (3 month) Sentiment for Various FFF rates", 
       y = "Sentiment")

matrix_list |> ggplot(aes(x = Date)) + 
  geom_line(aes(y = Sentiment_8mon, colour = TargetRange)) + 
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey") + 
  labs(title = "Long Term (8 month) Sentiment for Various FFF rates", 
       y = "Sentiment")

## this is a test of dominating target range with sentiment as weight

matrix_list_dom <- matrix_list |> 
  group_by(Date) |> 
  summarise(Sentiment_3mon_dom=max(Sentiment_3mon)*c(5.375,5.125,4.875)[which.max(Sentiment_3mon)], 
            Sentiment_8mon_dom=max(Sentiment_8mon)*c(5.375,5.125,4.875)[which.max(Sentiment_8mon)])

matrix_list_dom |> ggplot(aes(x=Date)) + 
  geom_line(aes(y=Sentiment_3mon_dom, colour="Sentiment_3mon_dom")) + 
  geom_line(aes(y=Sentiment_8mon_dom, colour="Sentiment_8mon_dom"))

matrix_list_dom |> ggplot(aes(x=Date, y=1-1/(Sentiment_3mon_dom/Sentiment_8mon_dom))) + 
  geom_line()

prices |> 
  merge(matrix_list_dom, by="Date") |> 
  ggplot(aes(x=US.10y.yield, y=Sentiment_8mon_dom)) + 
  geom_smooth() + 
  geom_point(alpha=0.5) + 
  aes(colour = factor(month(Date)))