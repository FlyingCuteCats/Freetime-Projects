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
  labs(title = "25 bps reduction") + 
  facet_wrap(~Meeting) + 
  geom_hline(yintercept = 0.5, colour = "grey")

fff_2024 |> 
  ggplot(aes(x=Date, y=`(475-500)`)) + 
  geom_line() + 
  labs(title = "50 bps reduction") + 
  facet_wrap(~Meeting) + 
  geom_hline(yintercept = 0.5, colour = "grey")

# grid.arrange(p1, p2, nrow = 2)

### Matrix

## Say we need to compute the matrix for the target
## range of rate 500-525

targetrange = "(500-525)"
columnList = c("Date", targetrange, "MeetingName", "Meeting", "DaysDiff")
fff_target <- fff_2024 |> 
  select(all_of(columnList))

