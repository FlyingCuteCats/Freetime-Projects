library(tidyverse)
library(lubridate)
library(here)
library(zoo)

fff_dates <- read.csv(here("FFF_Matrix/FedMeeting_Date.csv"))
fff_dates$Date <- as.Date(fff_dates$Date)
fff_dates |> mutate(Meeting = paste(month.name[month(Date)], year(Date)))

matrix_path <- here("FFF_Matrix/")
matrix_list <- list.files(matrix_path, pattern = "FedMeeting_\\d{8}.csv", full.names = TRUE)
combined_data <- map_df(matrix_list, read_csv)

# fff_2024_01 <- read.csv(here("FFF_Matrix/FedMeeting_20240131.csv"))
# fff_2024_01$Date <- as.Date(fff_2024_01$Date, "%d/%m/%Y")
