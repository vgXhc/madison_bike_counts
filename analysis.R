library(tidyverse)
library(lubridate)
library(rnoaa)

#weather station ID for CHARMANY FARM, WI station on Mineral Pt Rd
id <- "GHCND:USC00471416"

#get daily observation weather data; max date range is 1 year
weather <- ncdc(datasetid='GHCND', stationid='GHCND:USC00471416',startdate ='2014-10-09', enddate ='2019-08-31', add_units = TRUE)

##get data
cc_counts <- read_csv("https://opendata.arcgis.com/datasets/367cb53685c74628b4975d8f65d20748_0.csv", col_types = "ci-") %>% mutate(location = "Cap City at North Shore")
sw_counts <- read_csv("https://opendata.arcgis.com/datasets/8860784eb30e4a45a6f853b5f81949f2_0.csv", col_types = "ci-") %>% mutate(location = "SW Path at Randall")
#combine two counter locations
counts <- bind_rows(cc_counts, sw_counts)
#fix date column, drop NA
counts <- counts %>% mutate(Count_Date = mdy_hm(str_sub(Count_Date, 6))) %>% drop_na

#count number by weekday and location
counts %>%
  mutate(day = wday(Count_Date, label = TRUE)) %>%
  group_by(location, day) %>%
  summarize(sum = sum(Count)) %>%
  ggplot(aes(day, sum, fill = location)) +
  geom_col(position = "dodge")

#count number by hour of day and location
counts %>%
  filter(wday(Count_Date) %in% c(2:6)) %>% #filter to weekdays only
  mutate(hour = hour(Count_Date)) %>%
  group_by(location, hour) %>%
  summarize(sum = sum(Count)) %>%
  ggplot(aes(hour, sum, fill = location)) +
  geom_col(position = "dodge")

#count number by year and location
#to do: either filter for only complete years or make it a riders/day metric
counts %>%
  mutate(year = year(Count_Date), day = day(Count_Date)) %>%
  group_by(location, year, day) %>%
  summarise(sum = n()) %>%
  ggplot(aes(year, sum, fill = location)) +
  geom_col(position = "dodge")