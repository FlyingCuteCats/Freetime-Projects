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
  ggplot(aes(x=Date, y=`(500-525)`)) + 
  geom_line() + 
  labs(title = "25 bps reduction probability by meeting") + 
  facet_wrap(~Meeting) + 
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey")

fff_2024 |> 
  ggplot(aes(x=Date, y=`(475-500)`)) + 
  geom_line() + 
  labs(title = "50 bps reduction probability by meeting") + 
  facet_wrap(~Meeting) + 
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey")

# grid.arrange(p1, p2, nrow = 2)

### Matrix

## Say we need to compute the matrix for the target
## range of rate 500-525

matrix_table <- function(data, targetrange) {
  columnList = c("Date", targetrange, "MeetingName", "Meeting", "DaysDiff")
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
  geom_line(aes(y = Sentiment_3mon, colour = "Sentiment (ST)")) + 
  geom_line(aes(y = Sentiment_8mon, colour = "Sentiment (LT)")) + 
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey") + 
  labs(title = "Sentiment for Various FFF rates", 
       y = "Sentiment")