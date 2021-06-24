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

temp.1 <- read_excel("data/lake-superior-apostle-islands/lake-superior-apostle-islands-temperature.xlsx", sheet = "2016")
temp.2 <- read_excel("data/lake-superior-apostle-islands/lake-superior-apostle-islands-temperature.xlsx", sheet = "2017")
temp.3 <- read_excel("data/lake-superior-apostle-islands/lake-superior-apostle-islands-temperature.xlsx", sheet = "2018")

temp.all <- bind_rows(temp.1, temp.2, temp.3) %>% 
  mutate(yday = yday(date))
rm(temp.1, temp.2, temp.3)


spawn <- read_excel("data/lake-superior-apostle-islands/lake-superior-apostle-islands-spawning.xlsx", sheet = "lake-superior-apostle-spawning") %>%
  filter(percent.ripe > 5, date != as.Date("2016-11-14"), date != as.Date("2016-11-21")) %>% 
  group_by(year) %>% 
  filter(row_number() == 1) %>% 
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
  summarize(temp.c = mean(temp.c))

