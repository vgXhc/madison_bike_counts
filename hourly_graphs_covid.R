library(tidyverse)
library(lubridate)
library(readxl)
library(RColorBrewer)

x <- c("Count_Date", "Count")

cc_counts <- read_excel("JNPath_4_6_2020.xlsx", skip = 3, col_names = x) %>% mutate(location = "Cap City at North Shore")

sw_counts <- read_excel("SWPath_4_6_2020.xlsx", skip = 3, col_names = x) %>% mutate(location = "SW Path at Randall")

#read updated data
cc_counts2 <- read_excel("CapCityPath_JohnNolen.xlsx", skip = 3, col_names = x) %>% mutate(location = "Cap City at North Shore")

counts <- bind_rows(cc_counts, sw_counts)

counts2 <- counts %>% 
  drop_na %>%
  mutate(Count = as.integer(Count), location = as.factor(location)) %>%
  mutate(Count_Date = ymd_hms(Count_Date, tz = "US/Central")) %>% #specifying the time zone will help when merging climate data later
  mutate(dayofweek = wday(Count_Date, label = TRUE)) %>% #turning on labels makes this more intuitive
  mutate(weekendind = ifelse(dayofweek %in% c("Sat", "Sun"), "weekend", "weekday"))
  
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

#weekday hourly 
p_weekday <- counts3 %>% 
  filter(Count_Date %within% int_20 & weekendind == "weekday") %>% 
  group_by(location, hr = hour(Count_Date)) %>% 
  summarize(hourly = sum(Count)/n()) %>% 
  ggplot(aes(hr, hourly), fill = location) +
  geom_col(position = "dodge") +
  scale_x_continuous(breaks = c(6, 8, 10, 12, 14, 16, 18),
                     labels = c("6am", "8am", "10am", "noon", "2pm", "4pm", "6=pm"),
                     name = element_blank()) +
  # geom_hline(yintercept = 25) +
  gghighlight(hourly > 30, use_group_by = FALSE) +
  facet_wrap(~location) +
  labs(title = "How to avoid the WEEKDAY crowds",
       subtitle = "Avoid the Cap City between 11 am and 8pm; avoid the SW Path between 1 and 7 pm",
       caption = "Data: City of Madison, Eco Counter. Visualization: Harald Kliems for Madison Bikes",
       fill = element_blank()) +
  scale_y_continuous(name = "average number of cyclists/hour") +
  theme(axis.ticks.x = element_blank())

ggsave("weekday_hourly.png", p_weekday, scale = 0.7, width = 16, height = 9, dpi = 300)

p_weekend <- counts3 %>% 
  filter(Count_Date %within% int_20 & weekendind == "weekend") %>% 
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
  labs(title = "How to avoid the WEEKEND crowds",
       subtitle = "Avoid the Cap City between 10 am and 7 pm; avoid the SW Path between noon and 6 pm",
       caption = "Data: City of Madison, Eco Counter. Visualization: Harald Kliems for Madison Bikes",
       fill = element_blank()) +
  scale_y_continuous(name = "average number of cyclists/hour") +
  theme(axis.ticks.x = element_blank())
  #       panel.grid.major.x = element_blank(),
  #       panel.grid.minor.x = element_blank()) +
  # scale_fill_brewer(palette = "Set2")

ggsave("weekend_hourly.png", p_weekend, scale = 0.7, width = 16, height = 9, dpi = 300)

ggsave("weekend_hourly_square.png", p_weekend, scale = 1, width = 9, height = 9, dpi = 300)

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

ggsave("avg_counts.png", plot = p, scale = 0.7, width = 16, height = 9, dpi = 150)


 
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