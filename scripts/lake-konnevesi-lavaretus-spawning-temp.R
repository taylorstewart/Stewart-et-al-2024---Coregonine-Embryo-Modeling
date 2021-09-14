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

temp.all <- read_excel("data/lake-konnevesi/lake-konnevesi-temperature.xlsx", sheet = "temp", skip = 28) %>% 
  filter(year >= 2019, depth_m == 4.5, !is.na(temp_c)) %>% 
  mutate(yday = yday(date))


spawn.start <- read_excel("data/lake-konnevesi/lake-konnevesi-spawning-lavaretus.xlsx", sheet = "lake-konnevesi-spawning", skip = 27) %>%
  group_by(year) %>% 
  filter(row_number() == 1) %>% 
  select(year, start.date = date)
spawn.end <- read_excel("data/lake-konnevesi/lake-konnevesi-spawning-lavaretus.xlsx", sheet = "lake-konnevesi-spawning", skip = 27) %>%
  group_by(year) %>% 
  filter(row_number() == n()) %>% 
  select(year, end.date = date)
spawn <- left_join(spawn.start, spawn.end)


spawn.temp <- temp.all %>% left_join(spawn) %>%
  group_by(year) %>% 
  filter(date >= start.date, date <= end.date) %>% 
  group_by(year) %>% 
  arrange(date) %>%
  filter(row_number() %in% c(1, n())) %>% ungroup() %>% 
  mutate(spawn.group = rep(c("start", "end"), times = 3)) %>% 
  group_by(spawn.group) %>% 
  summarize(temp_c = mean(temp_c))

