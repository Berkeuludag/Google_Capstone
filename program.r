# Load libraries
library(tidyverse)
library(lubridate)
library(hms)
library(data.table)

# Load original .csv files for a year's worth of data from August 2023 to July 2024
aug08_df <- read_csv("202308-divvy-tripdata.csv") 
sep09_df <- read_csv("202309-divvy-tripdata.csv") 
oct10_df <- read_csv("202310-divvy-tripdata.csv")
nov11_df <- read_csv("202311-divvy-tripdata.csv") 
dec12_df <- read_csv("202312-divvy-tripdata.csv")
jan01_df <- read_csv("202401-divvy-tripdata.csv") 
feb02_df <- read_csv("202402-divvy-tripdata.csv") 
mar03_df <- read_csv("202403-divvy-tripdata.csv")
apr04_df <- read_csv("202404-divvy-tripdata.csv")
may05_df <- read_csv("202405-divvy-tripdata.csv") 
jun06_df <- read_csv("202406-divvy-tripdata.csv") 
jul07_df <- read_csv("202407-divvy-tripdata.csv") 

# Merge all data frames into one year view
cyclistic_df <- rbind(aug08_df, sep09_df, oct10_df, nov11_df, dec12_df, jan01_df, feb02_df, mar03_df, apr04_df, may05_df, jun06_df, jul07_df)

# Remove individual month data frames to clear up space in the environment
remove(aug08_df, sep09_df, oct10_df, nov11_df, dec12_df, jan01_df, feb02_df, mar03_df, apr04_df, may05_df, jun06_df, jul07_df)

# Create new data frame to contain new columns
cyclistic_date <- cyclistic_df

# Calculate ride length in minutes
cyclistic_date$ride_length <- difftime(cyclistic_df$ended_at, cyclistic_df$started_at, units = "mins")

# Create columns for day of week, month, day, year, time, hour
cyclistic_date$date <- as.Date(cyclistic_date$started_at)
cyclistic_date$day_of_week <- wday(cyclistic_df$started_at)
cyclistic_date$day_of_week <- format(as.Date(cyclistic_date$date), "%A")
cyclistic_date$month <- format(as.Date(cyclistic_date$date), "%m")
cyclistic_date$day <- format(as.Date(cyclistic_date$date), "%d")
cyclistic_date$year <- format(as.Date(cyclistic_date$date), "%Y")
cyclistic_date$time <- as_hms((cyclistic_df$started_at))
cyclistic_date$hour <- hour(cyclistic_date$time)

# Create column for different seasons
cyclistic_date <- cyclistic_date %>% mutate(season = 
  case_when(month == "03" ~ "Spring",
            month == "04" ~ "Spring",
            month == "05" ~ "Spring",
            month == "06" ~ "Summer",
            month == "07" ~ "Summer",
            month == "08" ~ "Summer",
            month == "09" ~ "Fall",
            month == "10" ~ "Fall",
            month == "11" ~ "Fall",
            month == "12" ~ "Winter",
            month == "01" ~ "Winter",
            month == "02" ~ "Winter")
)

# Create column for different times of day
cyclistic_date <- cyclistic_date %>% mutate(time_of_day = 
  case_when(hour == "0" ~ "Night",
            hour == "1" ~ "Night",
            hour == "2" ~ "Night",
            hour == "3" ~ "Night",
            hour == "4" ~ "Night",
            hour == "5" ~ "Night",
            hour == "6" ~ "Morning",
            hour == "7" ~ "Morning",
            hour == "8" ~ "Morning",
            hour == "9" ~ "Morning",
            hour == "10" ~ "Morning",
            hour == "11" ~ "Morning",
            hour == "12" ~ "Afternoon",
            hour == "13" ~ "Afternoon",
            hour == "14" ~ "Afternoon",
            hour == "15" ~ "Afternoon",
            hour == "16" ~ "Afternoon",
            hour == "17" ~ "Afternoon",
            hour == "18" ~ "Evening",
            hour == "19" ~ "Evening",
            hour == "20" ~ "Evening",
            hour == "21" ~ "Evening",
            hour == "22" ~ "Evening",
            hour == "23" ~ "Evening")
)

# Clean the data
cyclistic_date <- na.omit(cyclistic_date)
cyclistic_date <- distinct(cyclistic_date)
cyclistic_date <- cyclistic_date[!(cyclistic_date$ride_length <= 0),]
cyclistic_date <- cyclistic_date %>% select(-c(ride_id, start_station_id, end_station_id, start_lat, start_lng, end_lat, end_lng)) 

# View the final data
View(cyclistic_date)

# Total rides
total_rides <- nrow(cyclistic_date)

# Member type counts
member_counts <- cyclistic_date %>% group_by(member_casual) %>% count(member_casual)

# Total rides by member type
bike_counts <- cyclistic_date %>% group_by(member_casual, rideable_type) %>% count(rideable_type)

# Total rides by type of bike
total_bike_counts <- cyclistic_date %>% group_by(rideable_type) %>% count(rideable_type)
