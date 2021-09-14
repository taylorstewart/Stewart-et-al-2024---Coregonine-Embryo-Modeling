#### CLEAR THE ENVIRONMENT FIRST ---------------------------------------------

rm(list = ls(all.names = TRUE))


#### LOAD PACKAGES -----------------------------------------------------------

library(dplyr)
library(data.table)
library(readxl)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(stringr)
library(lubridate)
library(lubridateExtras)


#### LOAD TEMPERATURE DATA -----------------------------------------------------------------------

temp.all <- read_excel("data/lake-superior-thunder-bay/lake-superior-thunder-bay-temperature.xlsx", sheet = "temp", skip = 28) %>% 
  mutate(yday = yday(date))

## 
ggplot(temp.all, aes(x = date, y = temp_c)) + 
  geom_line() + theme_few() + 
  labs(y = 'Water Temperature (°C)', x = "") + 
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %d") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  facet_wrap(~year, scales = "free_x")


spawn <- read_excel("data/lake-superior-thunder-bay/lake-superior-thunder-bay-spawning.xlsx", sheet = "lake-superior-thunder-bay-spawn", skip = 30) %>%
  filter(percent.ripe > 10, year != 2017) %>% 
  group_by(year) %>% 
  filter(row_number() == 1) %>% 
  mutate(end.date = date+(8*86400)) %>% 
  select(year, start.date = date, end.date) %>% ungroup() %>% 
  mutate(start.yday = yday(start.date),
         end.yday = yday(end.date)) %>% 
  summarize(mean.start.yday = mean(start.yday),
            mean.end.yday = mean(end.yday))

spawn.temp <- temp.all %>% 
  group_by(year) %>% 
  filter(yday >= spawn$mean.start.yday, yday <= spawn$mean.end.yday) %>% 
  arrange(date) %>%
  filter(row_number() %in% c(1, n())) %>% ungroup() %>% 
  mutate(spawn.group = rep(c("start", "end"), times = 5)) %>% 
  group_by(spawn.group) %>% 
  summarize(temp_c = mean(temp_c))

