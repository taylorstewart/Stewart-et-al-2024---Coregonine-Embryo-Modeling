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

temp.1 <- read_excel("data/lake-geneva/lake-geneva-temperature-littoral.xlsx", sheet = "2010")
temp.2 <- read_excel("data/lake-geneva/lake-geneva-temperature-littoral.xlsx", sheet = "2011")
temp.3 <- read_excel("data/lake-geneva/lake-geneva-temperature-littoral.xlsx", sheet = "2012")
temp.4 <- read_excel("data/lake-geneva/lake-geneva-temperature-littoral.xlsx", sheet = "2013")
temp.5 <- read_excel("data/lake-geneva/lake-geneva-temperature-littoral.xlsx", sheet = "2014")
temp.6 <- read_excel("data/lake-geneva/lake-geneva-temperature-littoral.xlsx", sheet = "2015")
temp.7 <- read_excel("data/lake-geneva/lake-geneva-temperature-littoral.xlsx", sheet = "2016")
temp.8 <- read_excel("data/lake-geneva/lake-geneva-temperature-littoral.xlsx", sheet = "2017")
temp.9 <- read_excel("data/lake-geneva/lake-geneva-temperature-littoral.xlsx", sheet = "2018")
temp.10 <- read_excel("data/lake-geneva/lake-geneva-temperature-littoral.xlsx", sheet = "2019")
temp.11 <- read_excel("data/lake-geneva/lake-geneva-temperature-littoral.xlsx", sheet = "2020")
temp.12 <- read_excel("data/lake-geneva/lake-geneva-temperature-littoral.xlsx", sheet = "2021")

temp.all <- bind_rows(temp.1, temp.2, temp.3, temp.4, temp.5, temp.6, temp.7, temp.8, temp.9, temp.10, temp.11, temp.12) %>% 
  mutate(yday = yday(date)) %>% 
  filter(!is.na(temp_c))
rm(temp.1, temp.2, temp.3, temp.4, temp.5, temp.6, temp.7, temp.8, temp.9, temp.10, temp.11, temp.12)


## 
ggplot(temp.all, aes(x = date, y = temp_c)) + 
  geom_line() + theme_few() + 
  labs(y = 'Water Temperature (°C)', x = "") + 
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %d") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  facet_wrap(~year.class, scales = "free_x")


spawn <- read_excel("data/lake-geneva/lake-geneva-spawning.xlsx", sheet = "lake-geneva-spawning") %>% 
  filter(percent.abundance > 10, !is.na(temp.c)) %>% 
  group_by(year) %>% 
  arrange(date) %>%
  filter(row_number() %in% c(1, n())) %>% ungroup() %>% 
  mutate(spawn.group = rep(c("start.date", "end.date"), times = 4)) %>%
  select(year.class = year, date, spawn.group) %>% 
  pivot_wider(names_from = spawn.group, values_from = date)
  
  
spawn.temp <- temp.all %>% left_join(spawn) %>%
  group_by(year.class) %>% 
  filter(date >= start.date, date <= end.date) %>% 
  group_by(year.class) %>% 
  arrange(date) %>%
  filter(row_number() %in% c(1, n())) %>% ungroup() %>% 
  mutate(spawn.group = rep(c("start", "end"), times = 4)) %>% 
  group_by(spawn.group) %>% 
  summarize(temp_c = mean(temp_c))
  
