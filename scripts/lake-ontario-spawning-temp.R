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


#### LOAD DATA -----------------------------------------------------------------------------------

temp.all <- read_excel("data/lake-ontario/lake-ontario-temperature.xlsx", sheet = "temp", skip = 28) %>% 
  mutate(yday = yday(date))


spawn <- read_excel("data/lake-ontario/lake-ontario-spawning.xlsx", sheet = "lake-ontario-spawning", skip = 31) %>%
  filter(prop.ripe != 0) %>% 
  group_by(year) %>% 
  slice(which.max(prop.ripe)) %>% 
  mutate(end.date = date+(8*86400)) %>% 
  select(year, start.date = date, end.date)

spawn.temp <- temp.all %>% left_join(spawn) %>%
  group_by(year) %>% 
  filter(date >= start.date, date <= end.date) %>% 
  group_by(year) %>% 
  arrange(date) %>%
  filter(row_number() %in% c(1, n())) %>% ungroup() %>% 
  mutate(spawn.group = rep(c("start", "end"), times = 3)) %>% 
  group_by(spawn.group) %>% 
  summarize(temp_c = mean(temp_c))

