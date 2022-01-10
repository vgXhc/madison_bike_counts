library(tidyverse)
library(lubridate)
library(readxl)
library(RColorBrewer)
library(gghighlight)
library(hrbrthemes)

## create function for creating month intervals
## takes year and month as input and creates interval for that month, taking
## into account the number of days in each month.
create_interval_month <- function(y, m, tz){
  last_day <- days_in_month(ymd(paste(y, m, "01", sep = "-")))
  start_date <- paste(y, m, "01", sep = "-")
  end_date <- paste(y, m, last_day, sep = "-")
  return(interval(start = start_date, end = end_date, tzone = tz))
}



#read Excel files with counter data
x <- c("Count_Date", "Count")

cc_counts <- read_csv("data/Eco-Totem_Capital_City_Trail_Bike_Counts_2022_01_06.csv",
                      skip = 1,
                      col_names = x,
                      col_types = "ci-") %>% 
  mutate(location = "Cap City at North Shore") %>% 
  mutate(Count_Date = mdy_hm(Count_Date, tz = "US/Central"))

sw_counts <- read_csv("data/Eco-Totem_Southwest_Path_Bike_Counts_2022_01_06.csv", 
                      skip = 1, 
                      col_names = x,
                      col_types = "ci-") %>% 
  mutate(location = "SW Path at Randall") %>% 
  mutate(Count_Date = mdy_hm(Count_Date, tz = "US/Central"))

counts <- bind_rows(cc_counts, sw_counts)

counts2 <- counts %>% 
  drop_na %>%
  mutate(location = as.factor(location)) %>%

  mutate(dayofweek = wday(Count_Date, label = TRUE)) %>% #turning on labels makes this more intuitive
  mutate(weekendind = ifelse(dayofweek %in% c("Sat", "Sun"), "weekend", "weekday"))

#cleaning previously identified bad data  
 counts3 <- counts2 %>% 
    filter(location == "SW Path at Randall" | location == "Cap City at North Shore" & (Count_Date <= ymd_hms("2019-02-01 03:00:00", tz = "US/Central") | Count_Date >= ymd_hms("2019-03-12 00:00:00", tz = "US/Central"))) %>%  #remove long zero-count run
    filter(location != "SW Path at Randall" | Count < 500) #remove SW counts over 500
 
 # UW classes suspended March 16, schools closed
 #safer at home order: March 24
 
 int_20 <- interval("2020-03-16", "2020-04-05") #first day of spring break until 3 Sundays after
 int_19 <- interval("2019-03-16", "2019-04-05")
 int_18 <- interval("2018-03-16", "2018-04-05")
 int_17 <- interval("2017-03-16", "2017-04-05")
 int_16 <- interval("2016-03-16", "2016-04-05")
 
 int_20a <- interval("2020-03-24", "2020-04-05")
 int_19a <- interval("2019-03-24", "2019-04-05")
 int_18a <- interval("2018-03-24", "2018-04-05")
 int_17a <- interval("2017-03-24", "2017-04-05")
 int_16a <- interval("2016-03-24", "2016-04-05")

#new interval for updated data: Two weeks up to 4/21
 int_20_0421 <- interval("2020-04-07", "2020-04-21")

#interval for winter bike event
 int_21_jan_feb <- interval("2021-01-02", "2021-02-28")


 
# list of intervals for comparison with Streetlight data (compares May numbers)
int_may_pre <- map(2016:2019, create_interval_month, m = 5, tz = "US/Central")
int_may_post <- create_interval_month(2020, 5, "US/Central")

# June numbers
int_june_pre <- map(2016:2019, create_interval_month, m = 6, tz = "US/Central")
int_june_post <- create_interval_month(2020, 6, "US/Central")

# April
int_apr_pre <- map(2016:2019, create_interval_month, m = 4, tz = "US/Central")
int_apr_post <- create_interval_month(2020, 4, "US/Central")

# hourly pattern comparison
June_2019 <- create_interval_month(2019, 6, "US/Central")
June_2020 <- create_interval_month(2020, 6, "US/Central")

counts3 %>% 
  filter(Count_Date %within% June_2020 | Count_Date %within% June_2019) %>% 
  filter(weekendind == "weekday") %>% 
  mutate(year = year(Count_Date), hour = hour(Count_Date)) %>% 
  group_by(year, hour, location) %>% 
  ggplot(aes(hour, Count)) +
  geom_col() +
  gghighlight(hour %in% c(7, 8, 16, 17)) +
  facet_wrap(~location + year) +
  labs(subtitle = "Hourly weekday bike counts June 2019 and 2020",
       title = "Bike rush hour is gone",
       caption = "Data: City of Madison, Eco Counter\n Viz: Harald Kliems for Madison Bikes",
       fill = element_blank()) +
  scale_x_continuous(name = "Time of day") +
  scale_y_continuous(name = "bikes counted") +
  theme_ft_rc() +
  theme(panel.grid.minor.x = element_blank())

#weekday hourly 
counts3 %>% 
  filter(Count_Date %within% int_21_jan_feb & dayofweek == "Fri") %>% 
  group_by(location, hr = hour(Count_Date)) %>% 
  summarize(hourly = sum(Count)/n()) %>% 
  ggplot(aes(hr, hourly), fill = location) +
  geom_col(position = "dodge") +
  scale_x_continuous(breaks = c(6, 8, 10, 12, 14, 16, 18),
                     labels = c("6am", "8am", "10am", "noon", "2pm", "4pm", "6pm"),
                     name = element_blank()) +
  # geom_hline(yintercept = 25) +
  gghighlight(hourly > 30, use_group_by = FALSE) +
  facet_wrap(~location) +
  labs(title = "Friday traffic Jan/Feb 2021",
       #subtitle = "Avoid the Cap City between 10 am and 8pm; avoid the SW Path between 1 and 7 pm",
       caption = "Data: City of Madison, Eco Counter. Visualization: Harald Kliems for Madison Bikes",
       fill = element_blank()) +
  scale_y_continuous(name = "average number of cyclists/hour") +
  theme(axis.ticks.x = element_blank()) +
  theme_minimal()

#ggsave(paste0("weekday_hourly_", Sys.Date(), ".png"), p_weekday, scale = 0.7, width = 16, height = 9, dpi = 300)

p_weekend <- counts3 %>% 
  filter(Count_Date %within% int_21_jan_feb & weekendind == "weekend") %>% 
  group_by(location, hr = hour(Count_Date)) %>% 
  summarize(hourly = sum(Count)/n()) %>% 
  ggplot(aes(hr, hourly), fill = location) +
  geom_col(position = "dodge") +
  scale_x_continuous(breaks = c(6, 8, 10, 12, 14, 16, 18),
                     labels = c("6am", "8am", "10am", "noon", "2pm", "4pm", "6pm"),
                     name = element_blank()) +
  # geom_hline(yintercept = 25) +
  gghighlight(hourly > 30, use_group_by = FALSE) +
  facet_wrap(~locatio)n +
  labs(title = "How to avoid the WEEKEND crowds",
       subtitle = "Avoid the Cap City between 9 am and 8 pm; avoid the SW Path between 10 am and 6 pm",
       caption = "Data: City of Madison, Eco Counter. Visualization: Harald Kliems for Madison Bikes",
       fill = element_blank()) +
  scale_y_continuous(name = "average number of cyclists/hour") +
  theme(axis.ticks.x = element_blank())
  #       panel.grid.major.x = element_blank(),
  #       panel.grid.minor.x = element_blank()) +
  # scale_fill_brewer(palette = "Set2")

#ggsave(paste0("weekend_hourly_", Sys.Date(), ".png"), p_weekend, scale = 0.7, width = 16, height = 9, dpi = 300)

#ggsave("weekend_hourly_square.png", p_weekend, scale = 1, width = 9, height = 9, dpi = 300)

#weekend hourly
counts3 %>% 
  filter(Count_Date %within% int_20 & weekendind == "weekend") %>% 
  group_by(location, hour(Count_Date)) %>% 
  summarize(hourly = sum(Count)/n()) %>% 
  ggplot(aes(`hour(Count_Date)`, hourly, color = location)) +
  geom_line() +
  scale_x_time()
 
 counts5 <- counts3 %>% 
   filter(Count_Date %within% list(int_16a, int_17a, int_18a, int_19a, int_20a)) %>% 
   mutate(prepost = ifelse(Count_Date %within% int_20a, "post", "pre"))

 #produce bar graph to compare May to June numbers for both locations
 options(scipen = 999)
counts3 %>% 
   filter(Count_Date %within% int_may_post |
            Count_Date %within% int_may_pre | 
            Count_Date %within% int_apr_post |
            Count_Date %within% int_june_post |
            Count_Date %within% int_june_pre) %>% 
   mutate(prepost =  ifelse(Count_Date %within% int_may_post | Count_Date %within% int_june_post | Count_Date %within% int_apr_post, "post", "pre")) %>% 
   group_by(prepost, year = year(Count_Date), weekendind) %>%
   summarise(total = sum(Count)) %>% 
   ggplot(aes(year, total, label = round(total,0))) +
  geom_area(aes(fill = weekendind), alpha = .8) +
   labs(title = "Bike counts April to June",
        subtitle = "2020 counts were at record highs, driven by weekend riding",
        caption = "Data: City of Madison, Eco Counter\n Viz: Harald Kliems for Madison Bikes",
        fill = element_blank()) +
   scale_x_continuous(name = element_blank()) +
   scale_y_continuous(name = "bikes counted") +
  theme_modern_rc() +
  theme(panel.grid.minor.x = element_blank())
 
 p <- counts5 %>% 
   group_by(prepost, year(Count_Date), weekendind) %>% 
   summarise(avg_ct = sum(Count)/24) %>% 
   ggplot(aes(`year(Count_Date)`, avg_ct, label = round(avg_ct,0))) +
   geom_bar(position = "dodge", stat = "identity", aes(fill = weekendind)) +
   geom_text(aes(group = weekendind), position = position_dodge(width = .9), vjust = -0.2) +
   labs(title = "Average daily bike counts during the COVID-19 pandemic are similar to previous years",
        subtitle = "Reporting periods: Mar 15 to Apr 5",
        caption = "Data: City of Madison, Eco Counter. Visualization: Harald Kliems for Madison Bikes",
        fill = element_blank()) +
   scale_x_continuous(name = element_blank()) +
   scale_y_continuous(name = "average number of cyclists/day") +
   theme(axis.ticks.x = element_blank(),
         panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank()) +
   scale_fill_brewer(palette = "Set2")
 
 
 
  counts4 <- counts3 %>% 
   filter(Count_Date %within% list(int_16, int_17, int_18, int_19, int_20)) %>% 
   mutate(prepost = ifelse(Count_Date %within% int_20, "post", "pre"))
 
 counts4 %>% 
   group_by(prepost, day(Count_Date)) %>% 
   summarise(avg_ct = mean(Count)) %>% 
   ggplot(aes(prepost, avg_ct)) +
   geom_col()
 
 

#Plot comparing COVID pandemic bike counts with the same period in prior years 
p <- counts4 %>% 
   group_by(prepost, year(Count_Date), weekendind) %>% 
   summarise(avg_ct = sum(Count)/24) %>% 
   ggplot(aes(`year(Count_Date)`, avg_ct, label = round(avg_ct,0))) +
   geom_bar(position = "dodge", stat = "identity", aes(fill = weekendind)) +
   geom_text(aes(group = weekendind), position = position_dodge(width = .9), vjust = -0.2) +
   labs(title = "Average daily bike counts during the COVID-19 pandemic are similar to previous years",
   subtitle = "Reporting periods: Mar 16 to Apr 5",
   caption = "Data: City of Madison, Eco Counter. Visualization: Harald Kliems for Madison Bikes",
   fill = element_blank()) +
   scale_x_continuous(name = element_blank()) +
   scale_y_continuous(name = "average number of cyclists/day") +
   theme(axis.ticks.x = element_blank(),
         panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank()) +
   scale_fill_brewer(palette = "Set2")

#ggsave("avg_counts.png", plot = p, scale = 0.7, width = 16, height = 9, dpi = 150)


 
 pre <- counts3 %>% 
   filter(Count_Date %within% list(int_18, int_17, int_16))
 
 post <- counts3 %>% 
   filter(Count_Date %within% int_20)
 
 pre %>% 
   filter(weekendind == "weekday") %>% 
   group_by(year(Count_Date), location) %>% 
   summarise(avg_ct = sum(Count)/24) %>% 
   ungroup() %>% 
   summarize(mean(avg_ct))
 
post %>% 
   filter(weekendind == "weekday") %>% 
   group_by(year(Count_Date), location) %>% 
   summarise(avg_ct = sum(Count)/24) %>% 
  ungroup() %>% 
  summarize(mean(avg_ct))
