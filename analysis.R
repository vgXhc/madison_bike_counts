library(tidyverse)
library(lubridate)

##get data
cc_counts <- read_csv("https://opendata.arcgis.com/datasets/367cb53685c74628b4975d8f65d20748_0.csv", col_types = "ci-") %>% mutate(location = "Cap City at North Shore")
sw_counts <- read_csv("https://opendata.arcgis.com/datasets/8860784eb30e4a45a6f853b5f81949f2_0.csv", col_types = "ci-") %>% mutate(location = "SW Path at Randall")
#combine two counter locations
counts <- bind_rows(cc_counts, sw_counts)
#fix date column
counts <- counts %>% mutate(Count_Date = mdy_hm(str_sub(Count_Date, 6)))

counts %>% ggplot(aes(Count_Date, Count, color = location)) +
  geom_point(alpha = 0.7)