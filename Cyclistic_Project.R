# Installing the packages
install.packages("tidyverse")
install.packages("janitor")
install.packages("lubridate")
install.packages("skimr")
install.packages("here")
install.packages("dplyr")

# Load packages
library(tidyverse)
library(janitor)
library(lubridate)
library(ggplot2)
library(skimr)
library(here)
library(dplyr)
library(readr)

# Reading in the .csv files
Jan_2021 <- read.csv("Desktop/Documents/Data_Analytics/Google_Data_Analytics_Professionial_Certification/Case_Studies/Case_Study_1/Cyclistic/Data/originonal_csv/2021_01_divvy_trip_data.csv")
Feb_2021 <- read.csv("Desktop/Documents/Data_Analytics/Google_Data_Analytics_Professionial_Certification/Case_Studies/Case_Study_1/Cyclistic/Data/originonal_csv/2021_02_divvy_trip_data.csv")
Mar_2021 <- read.csv("Desktop/Documents/Data_Analytics/Google_Data_Analytics_Professionial_Certification/Case_Studies/Case_Study_1/Cyclistic/Data/originonal_csv/2021_03_divvy_trip_data.csv")
Apr_2021 <- read.csv("Desktop/Documents/Data_Analytics/Google_Data_Analytics_Professionial_Certification/Case_Studies/Case_Study_1/Cyclistic/Data/originonal_csv/2021_04_divvy_trip_data.csv")
May_2021 <- read.csv("Desktop/Documents/Data_Analytics/Google_Data_Analytics_Professionial_Certification/Case_Studies/Case_Study_1/Cyclistic/Data/originonal_csv/2021_05_divvy_trip_data.csv")
Jun_2021 <- read.csv("Desktop/Documents/Data_Analytics/Google_Data_Analytics_Professionial_Certification/Case_Studies/Case_Study_1/Cyclistic/Data/originonal_csv/2021_06_divvy_trip_data.csv")
Jul_2021 <- read.csv("Desktop/Documents/Data_Analytics/Google_Data_Analytics_Professionial_Certification/Case_Studies/Case_Study_1/Cyclistic/Data/originonal_csv/2021_07_divvy_trip_data.csv")
Aug_2021 <- read.csv("Desktop/Documents/Data_Analytics/Google_Data_Analytics_Professionial_Certification/Case_Studies/Case_Study_1/Cyclistic/Data/originonal_csv/2021_08_divvy_trip_data.csv")
Sep_2021 <- read.csv("Desktop/Documents/Data_Analytics/Google_Data_Analytics_Professionial_Certification/Case_Studies/Case_Study_1/Cyclistic/Data/originonal_csv/2021_09_divvy_trip_data.csv")
Oct_2021 <- read.csv("Desktop/Documents/Data_Analytics/Google_Data_Analytics_Professionial_Certification/Case_Studies/Case_Study_1/Cyclistic/Data/originonal_csv/2021_10_divvy_trip_data.csv")
Nov_2021 <- read.csv("Desktop/Documents/Data_Analytics/Google_Data_Analytics_Professionial_Certification/Case_Studies/Case_Study_1/Cyclistic/Data/originonal_csv/2021_11_divvy_trip_data.csv")
Dec_2021 <- read.csv("Desktop/Documents/Data_Analytics/Google_Data_Analytics_Professionial_Certification/Case_Studies/Case_Study_1/Cyclistic/Data/originonal_csv/2021_12_divvy_trip_data.csv")

# str(data set name)
str(Jan_2021)
str(Feb_2021)
str(Mar_2021)
str(Apr_2021)
str(May_2021)
str(Jun_2021)
str(Jul_2021)
str(Aug_2021)
str(Sep_2021)
str(Oct_2021)
str(Nov_2021)
str(Dec_2021)

# Creating a combined single data set name
Merged_df_2021 <- bind_rows(Jan_2021, Feb_2021, Mar_2021, Apr_2021, May_2021, Jun_2021, Jul_2021, Aug_2021, Sep_2021, Oct_2021, Nov_2021, Dec_2021)

# Clean & remove any spaces
Merged_df_2021 <- clean_names(Merged_df_2021)
remove_empty(Merged_df_2021, which = c())

# Creating column for day of the week
Merged_df_2021$day_of_week <- wday(Merged_df_2021$started_at, label = T, abbr = T)

# Setting up trip duration
# Start Hour
Merged_df_2021$starting_hour <- format(as.POSIXct(Merged_df_2021$started_at), "%H")
# Month
Merged_df_2021$month <- format(as.Date(Merged_df_2021$started_at), "%m")

# Trip Duration
Merged_df_2021$trip_duration <- difftime(Merged_df_2021$ended_at, Merged_df_2021$started_at, units = 'min')

# Clean one more time
Cleaned_merged_df_2021 <- Merged_df_2021[!(Merged_df_2021$trip_duration <= 0),]

# Create a new data frame with only members
annual_member_df <- filter(Cleaned_merged_df_2021, member_casual == "member")

# Create a new data frame with only casual riders
casual_member_df <- filter(Cleaned_merged_df_2021, member_casual == "casual")

# Creating a bar graph that compares number of rides by member type
ggplot(data = Cleaned_merged_df_2021) +
  geom_bar(mapping = aes(x = day_of_week, fill = member_casual), position = "dodge") +
  labs(x = "Day of Week", y = "Number of Rides", fill = "Member Type", title = "Number of Rides by Member Type")
ggsave("number_of_rides_by_member_type.png")

# Creating a bar graph that compares number or rides per month by member type
ggplot(data = Cleaned_merged_df_2021) +
  geom_bar(mapping = aes(x = month, fill = member_casual), position = "dodge") +
  labs(x = "Month", y = "Number of Rides", fill = "Member Type", title = "Number of Rides per Month")
ggsave("number_of_rides_per_month.png")

# Creating a bar graph per day per hour comparing the member types
ggplot(data = Cleaned_merged_df_2021) +
  geom_bar(mapping = aes(x = starting_hour, fill = member_casual)) +
  facet_wrap(~day_of_week) +
  labs(x = "Starting Hour", y = "Number of Rides", fill = "Member Type", title = "Hourly Use of Bikes Throughout the Week") +
  theme(axis.text = element_text(size = 5))
ggsave("hourly_use_of_bikes_throughout_the_week.png", dpi = 1000)

# Creating a bar graph to show the amount of rides by member type
ggplot(data = Cleaned_merged_df_2021) +
  geom_bar(mapping = aes(x = member_casual, fill = member_casual)) +
  labs(x = "Member Type", y = "Number of Rides", fill = "Member Type", title = "Number of Rides by Member Type")
ggsave("number_of_rides_by_member_type.png")

# Creating a bar graph to compare the different types of bikes used
ggplot(data = Cleaned_merged_df_2021) +
  geom_bar(mapping = aes(x = rideable_type, fill = rideable_type)) +
  labs(x = "Bike Type", y = "Amount the type was used", fill = "Bike Type", title = "Amount of Bike Types Used")
ggsave("amount_of_bike_types_used.png")

# Creating a bar graph to see the brake down with casual riders
ggplot(data = casual_member_df) +
  geom_bar(mapping = aes(x = rideable_type, fill = rideable_type)) +
  labs(x = "Bike Type", y = "Amount the type was used", fill = "Bike Type", title = "Amount of Bike Types Used by Casual Riders")
ggsave("amount_of_bike_types_used_casual_member.png")

# Creating a bar graph to see the breakdown with annual members
ggplot(annual_member_df) +
  geom_bar(mapping = aes(x = rideable_type, fill = rideable_type)) +
  labs(x = "Bike Type", y = "Amount the type was used", fill = "Bike Type", title = "Amount of Bike Types Used by Annual Members")
ggsave("amount_of_bike_types_used_annual_member.png")
