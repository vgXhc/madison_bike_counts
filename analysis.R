library(tidyverse)
library(purrr)
library(lubridate)
library(rnoaa)

#weather station ID for CHARMANY FARM, WI station on Mineral Pt Rd
id <- "GHCND:USC00471416"

#get daily observation weather data
# max date range is 1 year, but there's also a 1000 records limit
# with 6 values/day, you can only get about 160 days per call
# solution: get each datatype and year separately

#define function to pull one year's worth of weather data  
get_weather <- function(year, data_type) {
  df <- ncdc(datasetid='GHCND', 
             stationid='GHCND:USC00471416',
             startdate = paste0(year, "-01-01"), 
             enddate = paste0(year, "-12-31"), 
             add_units = TRUE, 
             limit = 1000,
             datatypeid = data_type)
  df$data
}

tmp2 <- ncdc_datatypes(stationid = "72641014837", limit = 1000)
view(tmp2$data)
tmp <- ghcnd_stations(locationid='FIPS:55025', datatypeid = "", limit = 1000)
view(tmp$data)
isd_stations(refresh = TRUE)
isd_stations_search(lat = 43.066667, lon = -89.400000, radius = 10)





# get data for Madison airport station. Data dictionary: ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-format-document.pdf
## set up year vector
x <- c(2014:2019)
## define function to get data
get_isd <- function(year) {isd(usaf = "726410", wban = "14837", year = year)}
## map function over years
climate <- x %>% map_df(get_isd)

# filter rows with missing air temperature and duplicate temp entries
# (the latter is I believe from 24 h rows)
# create new date/time variable
# fix temperature units to degree Celsius
# round time to nearest hours
hourly_temp <- climate %>% 
  filter(temperature != "+9999", call_letter != "99999") %>% 
  mutate(date_time = paste(date, time, sep = " ")) %>% 
  mutate(date_time = round_date(ymd_hm(date_time, tz = "UTC"), "hour")) %>%
  mutate(temperature = as.double(temperature) / 10) %>%
  select(date_time, temperature)




df <- ncdc(datasetid='GHCND', 
           stationid='GHCND:USW00014837',
           startdate = "2018-01-01", 
           enddate = "2018-01-06", 
           add_units = TRUE, 
           limit = 1000)

# list of years and data types
combo <- list(year = c(2014:2019), id = c("PRCP", "SNOW", "SNWD", "TMAX", "TMIN", "TOBS"))
#create list of all possible combinations; transpose so that map2 can digest it
args <- combo %>%
  cross() %>%
  transpose()

# map over args to get weather data
weather <- map2_df(args[[1]], args[[2]], get_weather)  


##get bike counter data
cc_counts <- read_csv("https://opendata.arcgis.com/datasets/367cb53685c74628b4975d8f65d20748_0.csv", col_types = "ci-") %>% mutate(location = "Cap City at North Shore")
sw_counts <- read_csv("https://opendata.arcgis.com/datasets/8860784eb30e4a45a6f853b5f81949f2_0.csv", col_types = "ci-") %>% mutate(location = "SW Path at Randall")
#combine two counter locations
counts <- bind_rows(cc_counts, sw_counts)
#some data prep for counts
counts2 <- counts %>% 
  mutate(Count_Date = mdy_hm(str_sub(Count_Date, 6), tz = "US/Central")) %>% #fix date and time
  drop_na %>% 
  mutate(Count = ifelse(Count == 0, 1, Count)) %>% #convert 0 counts to 1 to allow log transform
  mutate(log_count = log(Count)) %>% #create value for log of count
  mutate(dayofweek = wday(Count_Date)) %>% 
  mutate(weekendind = ifelse(dayofweek %in% c(1:5), "weekday", "weekend"))


  
#join hourly temperature to counts
df <- counts2 %>% left_join(hourly_temp, by = c("Count_Date" = "date_time"))

lin <- df %>% 
  filter(location == "Cap City at North Shore") %>% 
  lm(Count ~ temperature + weekendind)
summary(lin)

lin <- lm(Count ~ temperature+hour(Count_Date)+wday(Count_Date), df)
summary(lin)

qplot(temperature, log_count, data = df)
df %>% 
  group_by(location) %>% 
  ggplot(aes(temperature, log_count)) +
  geom_point() +
  facet_wrap(~location)

df %>% 
  cor(df$temperature, hour(df$Count_Date), use = "complete.obs")

summary(lin)
df %>% 
  filter(hour(Count_Date) %in% c(6:10), wday(Count_Date) %in% c(1:5)) %>% 
  ggplot(aes(temperature, Count)) +
  geom_point(alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~ location)

qplot(y = Count, x = temperature, data = df, geom = "point")

round_date(res$date_time, "hour")

#count number by weekday and location
counts2 %>%
  mutate(day = wday(Count_Date, label = TRUE)) %>%
  group_by(location, day) %>%
  summarize(sum = sum(Count)) %>%
  ggplot(aes(day, sum, fill = location)) +
  geom_col(position = "dodge")

#count number by hour of day and location
counts %>%
  filter(wday(Count_Date) %in% c(1:5)) %>% #filter to weekdays only
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