library(tidyverse)
library(lubridate)
library(here)
library(zoo)

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
        MeetingName = str_extract(basename(file_name), "\\d+")
      )
  })
combined_data$Date <- as.Date(combined_data$Date, "%d/%m/%Y")
combined_data$Meeting <- combined_data$Meeting |> 
  str_replace("^(\\d{4})(\\d{2})(\\d{2})$", "\\1-\\2-\\3") |> 
  as.Date()
combined_data <- combined_data |> 
  mutate(MeetingName = paste(month.name[month(Meeting)], year(Meeting)), 
         DaysDiff = difftime(Meeting, Date, units = "days"))

### Matrix Calculation

