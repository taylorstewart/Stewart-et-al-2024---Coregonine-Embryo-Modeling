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

temp.1 <- read_excel("data/lake-konnevesi/lake-konnevesi-temperature.xlsx", sheet = "2019") %>% filter(depth_m == 4.5, !is.na(temp_c))
temp.2 <- read_excel("data/lake-konnevesi/lake-konnevesi-temperature.xlsx", sheet = "2020") %>% filter(depth_m == 4.5, !is.na(temp_c))
temp.3 <- read_excel("data/lake-konnevesi/lake-konnevesi-temperature.xlsx", sheet = "2021") %>% filter(depth_m == 4.5, !is.na(temp_c))

temp.all <- bind_rows(temp.1, temp.2, temp.3) %>% 
  mutate(yday = yday(date))
rm(temp.1, temp.2, temp.3)


spawn.start <- read_excel("data/lake-konnevesi/lake-konnevesi-spawning.xlsx", sheet = "lake-konnevesi-spawning") %>%
  group_by(year) %>% 
  filter(row_number() == 1) %>% 
  select(year, start.date = date)
spawn.end <- read_excel("data/lake-konnevesi/lake-konnevesi-spawning.xlsx", sheet = "lake-konnevesi-spawning") %>%
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

