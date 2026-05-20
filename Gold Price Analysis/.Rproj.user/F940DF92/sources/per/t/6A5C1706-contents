library(tidyverse)
library(lubridate)
library(here)
library(zoo)
library(gridExtra)

### Wrangling

# fff_dates <- read.csv(here("FFF_Matrix/FedMeeting_Date.csv"))
# fff_dates$Date <- as.Date(fff_dates$Date)
# fff_dates |> mutate(Meeting = paste(month.name[month(Date)], year(Date)))

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

date_start <- "2024-06-01"
target_ranges <- c("(525-550)", 
                   "(500-525)", 
                   "(475-500)", 
                   "(450-475)", 
                   "(425-450)", 
                   "(400-425)", 
                   "(375-400)",
                   "(350-375)",
                   "(325-350)",
                   "(300-325)",
                   "(275-300)",
                   "(250-275)")
half_line <- geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey")

fff <- combined_data |> 
  filter(date(Date) >= date_start) 

fff |> 
  filter(Meeting > Sys.Date()) |> 
  ggplot(aes(x=Date)) + 
  facet_wrap(~Meeting) + 
  half_line + 
  lapply(target_ranges, function(Range) {
    range_sym <- rlang::sym(Range)  
    geom_line(aes(y = !!range_sym, colour = Range))
  })

### Matrix

# Function for generating a matrix for a target range
matrix_table <- function(data, targetrange, n) {
  if (length(targetrange) == 1) {
    targetrange <- list(targetrange)
  } else {
    targetrange <- as.list(targetrange)
  }
  
  results <- lapply(targetrange, function(tr) {
    columnList <- c("Date", tr, "Meeting", "DaysDiff")
    fff_matrix <- data |> 
      select(all_of(columnList)) |> 
      group_by(Date) |> 
      filter(Meeting > Date) |> 
      arrange(Date, Meeting) |> 
      slice(1:n) |> 
      summarise(!!sentiment_sym(n) := sum(.data[[tr]] * as.numeric(DaysDiff))/sum(as.numeric(DaysDiff)), .groups = 'drop') |> 
      mutate(TargetRange = tr)
    return(fff_matrix)
  })
  
  combined_results <- bind_rows(results)
  return(combined_results)
}

# Function for generating a single graph of sentiment for some number of month

matrix_graph_single <- function(data, n) {
  newColumnName <- rlang::sym(str_glue("Sentiment_",n,"mon"))
  data |> ggplot(aes(x = Date)) + 
    geom_line(aes(y = !!newColumnName, colour = TargetRange)) + 
    half_line + 
    labs(title = str_glue(n,"-month Sentiment for Various FFF rates"), 
         y = "Sentiment")
}

# Function of the Sentiment symbol in the matrix table

sentiment_sym <- function(x) {
  return(rlang::sym(str_glue("Sentiment_",x,"mon")))
}

# main 

sentiment_num = c(3,8)

matrix_list <- merge(matrix_table(fff, target_ranges, 3),
                     matrix_table(fff, target_ranges, 8))

matrix_list <- matrix_list |> 
  group_by(TargetRange) |> 
  arrange(TargetRange, Date) |> 
  mutate(d_3mon = if_else(row_number() == 1, 0, Sentiment_3mon-lag(Sentiment_3mon)), 
         d_8mon = if_else(row_number() == 1, 0, Sentiment_8mon-lag(Sentiment_8mon))) |> 
  mutate(dd_3mon = if_else(row_number() == 1 | row_number()== 2, 0, d_3mon-lag(d_3mon)), 
         dd_8mon = if_else(row_number() == 1 | row_number()== 2, 0, d_8mon-lag(d_8mon))) |> 
  ungroup()

matrix_na_list <- matrix_list |> 
  group_by(TargetRange) |> 
  summarise(na_ratio=sum(is.na(Sentiment_8mon))/n()) |> 
  filter(na_ratio>0.95) |> 
  pull(TargetRange)

matrix_list |> 
  filter(!TargetRange %in% matrix_na_list) |> 
  ggplot(aes(x = Date)) + 
  facet_wrap(~TargetRange) + 
  half_line + 
  labs(title = "Sentiment for Various FFF rates", 
       y = "Sentiment", 
       colour = "Months") + 
  lapply(sentiment_num, function(x) {
    geom_line(aes(y = !!sentiment_sym(x), colour = str_glue("Sentiment (",x," months)")))
  })

lapply(sentiment_num,function(x) {matrix_graph_single(matrix_list,x)})

## this is a test of dominating target range with sentiment as weight

mean_from_range <- function(input_string) {
  numeric_part <- gsub("[^0-9-]", "", input_string)
  numbers <- as.numeric(strsplit(numeric_part, "-")[[1]])
  mean(numbers)/100
}

target_mean <- sapply(target_ranges, mean_from_range)
target_mean <- sort(target_mean)

matrix_list_dom <- matrix_list |> 
  group_by(Date) |> 
  summarise(Sentiment_3mon_dom=max(Sentiment_3mon, na.rm=T)*target_mean[which.max(Sentiment_3mon)], 
            Sentiment_8mon_dom=max(Sentiment_8mon, na.rm=T)*target_mean[which.max(Sentiment_8mon)], 
            Domina_fff_3mon=target_mean[which.max(Sentiment_3mon)],
            Domina_fff_8mon=target_mean[which.max(Sentiment_8mon)])

matrix_list_dom |> ggplot(aes(x=Date)) + 
  geom_line(aes(y=Sentiment_3mon_dom, colour="Sentiment_3mon_dom")) + 
  geom_line(aes(y=Sentiment_8mon_dom, colour="Sentiment_8mon_dom"))

matrix_list_dom |> ggplot(aes(x=Date)) + 
  geom_line(aes(y=Domina_fff_3mon, colour="Domina_fff_3mon")) + 
  geom_line(aes(y=Domina_fff_8mon, colour="Domina_fff_8mon"))

matrix_list_dom |> ggplot(aes(
  x=Date, 
  y=Sentiment_3mon_dom-Sentiment_8mon_dom)) + 
  geom_line()

prices |> 
  merge(matrix_list_dom, by="Date") |> 
  ggplot(aes(x=Gold, y=Sentiment_3mon_dom-Sentiment_8mon_dom)) + 
  geom_smooth() + 
  geom_point(alpha=0.5) + 
  aes(colour = factor(month(Date)))