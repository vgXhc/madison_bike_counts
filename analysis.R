library(tidyverse)
library(purrr)
library(lubridate)
library(rnoaa)
library(timeDate) #needed for dealing with holidays



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





## get climate data for Madison airport station 
# Data dictionary: ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-format-document.pdf
# Helpful document for understanding the data structure: https://www.visualcrossing.com/blog/how-we-process-integrated-surface-database-historical-weather-data
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
  drop_na %>% 
  mutate(Count_Date = mdy_hm(Count_Date, tz = "US/Central"), #fix date and time
         location = as.factor(location),
         Count = ifelse(Count == 0, 1, Count), #convert 0 counts to 1 to allow log transform
         log_count = log(Count), #create value for log of count
         dayofweek = wday(Count_Date),
         weekendind = ifelse(dayofweek %in% c(1:5), "weekday", "weekend"))

counts2 %>% 
  filter(Count_Date >= ymd("2018-06-15") & Count_Date <= ymd("2018-07-05")) %>% 
  ggplot(aes(Count_Date, Count)) +
  geom_line()


counts2 %>% 
  filter(Count_Date >= ymd("2018-07-02") & Count_Date <= ymd("2018-07-04") & location == "SW Path at Randall") %>% 
  ggplot(aes(Count_Date, Count)) +
  geom_col()

# check for consecutive identical non-zero values
dupes <- rle(counts2$Count)
head(dupes)

tibble(length = dupes$lengths, values = dupes$values) %>% 
  #filter(values != 1 & length >1) %>% 
  arrange(desc(length)) %>%
  group_by(values) %>%
  ggplot(aes(length, values)) +
  geom_point(alpha = 0.2)

counts2 %>% 
  filter(Count_Date >= ymd("2018-07-02") & Count_Date <= ymd("2018-07-04")) %>% 
  group_by(location) %>% 
ggplot(aes(Count_Date, Count, fill = location)) +
  geom_col(position = "dodge")

# IQR moving average
thresholds <- counts2 %>%
  filter(location == "SW Path at Randall" & Count_Date >= ymd("2018-07-02") - days(27) & Count_Date <= ymd("2018-07-02")) %>% 
  group_by(hour(Count_Date)) %>% 
  summarize(hourly_IQR = IQR(Count),
            q3 = quantile(Count, 3/4),
            q1 = quantile(Count, 1/4)) %>% 
  mutate(upper_thresh = q3 + 2* hourly_IQR, #establishing the upper threshold
         lower_thresh = q1 - 2* hourly_IQR)  #and the lower threshold (which can be negative)

counts_ts <- counts2 %>% 
  select(Count_Date, Count) %>% 
as.ts() %>% 
  stl(t.window=13, s.window="periodic", robust=TRUE) %>%
  autoplot()
as.ts(counts2) %>%
  stl(t.window=13, s.window="periodic", robust=TRUE) %>%
  autoplot()

library(imputeTS)
counts_ts <- counts2 %>% 
  filter(location == "SW Path at Randall") %>% 
  mutate(Count = ifelse(Count > 500, NA, Count)) %>% 
  select(Count_Date, Count) %>% 
  as.ts()

high <- counts2 %>% 
  filter(location == "SW Path at Randall")

high <-  which(high$Count > 500)
counts_ts_int <- na.interpolation(counts_ts, option = "linear")
counts_ts_int[high,]
counts_ts_int[high+1,]

counts2 %>% 
  mutate(same = lead(Count) == Count,
         same2 = same == TRUE & lead(same) == TRUE) %>% 
  arrange(desc(same2))

counts2 %>% 
  group_by(location, year(Count_Date)) %>% 
  summarize(avg_daily = mean(Count*24))

difference <- which(diff(counts2$Count) == 0)

df <- counts2[difference,] %>% 
  filter(Count != 1) %>% 
  arrange(Count_Date)

counts2 %>%
  mutate(runlength = rle(Count))


counts2 %>% 
  mutate(nextvalue = diff(Count, 1)) %>% 
  filter(nextvalue == 0 & Count > 0)
  
counts2 %>% 
  filter(Count_Date >= ymd("2019-07-02") & Count_Date <= ymd("2019-07-04")) %>% 
  ggplot(aes(Count_Date, Count)) +
  geom_col()

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

counts2 %>%
  filter(year(Count_Date) %in% c(2015:2018)) %>% 
  filter(location != "Cap City at North Shore" | year(Count_Date) != 2015) %>% 
  group_by(location, year(Count_Date)) %>%
  summarize(sum = sum(Count)) %>%
  ggplot(aes(x = `year(Count_Date)`, sum, fill = location)) +
  geom_col(position = "dodge")

counts2 %>%
  mutate(hour = hour(Count_Date)) %>%
  group_by(location, hour, weekendind) %>%
  summarize(sum = sum(Count)) %>%
  ggplot(aes(hour, sum, fill = location)) +
  geom_col(position = "dodge") +
  facet_wrap(~ weekendind)

#counts per year
counts2 %>%
  mutate(year = year(Count_Date)) %>% 
  group_by(location, year) %>%
  summarize(sum = sum(Count)) %>%
  ggplot(aes(year, sum, fill = location)) +
  geom_col(position = "dodge")

#count number by year and location
#to do: either filter for only complete years or make it a riders/day metric
counts %>%
  mutate(year = year(Count_Date), day = day(Count_Date)) %>%
  group_by(location, year, day) %>%
  summarise(sum = n()) %>%
  ggplot(aes(year, sum, fill = location)) +
  geom_col(position = "dodge")



