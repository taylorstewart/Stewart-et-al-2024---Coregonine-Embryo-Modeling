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

temp.all <- read_excel("data/lake-geneva/lake-geneva-temperature.xlsx", sheet = "temp", skip = 28) %>% 
  mutate(yday = yday(date)) %>% 
  filter(year >= 2010, !is.na(temp_c))

## 
ggplot(temp.all, aes(x = date, y = temp_c)) + 
  geom_line() + theme_few() + 
  labs(y = 'Water Temperature (°C)', x = "") + 
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %d") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  facet_wrap(~year, scales = "free_x")


spawn <- read_excel("data/lake-geneva/lake-geneva-spawning.xlsx", sheet = "lake-geneva-spawning", skip = 30) %>% 
  filter(percent.abundance > 10, !is.na(temp_c)) %>% 
  group_by(year) %>% 
  arrange(date) %>%
  filter(row_number() %in% c(1, n())) %>% ungroup() %>% 
  mutate(spawn.group = rep(c("start.date", "end.date"), times = 4)) %>%
  select(year, date, spawn.group) %>% 
  pivot_wider(names_from = spawn.group, values_from = date)
  
  
spawn.temp <- temp.all %>% left_join(spawn) %>%
  group_by(year) %>% 
  filter(date >= start.date, date <= end.date) %>% 
  arrange(date) %>%
  filter(row_number() %in% c(1, n())) %>% ungroup() %>% 
  mutate(spawn.group = rep(c("start", "end"), times = 4)) %>% 
  group_by(spawn.group) %>% 
  summarize(temp_c = mean(temp_c))
  
