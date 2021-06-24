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

temp.1 <- read_excel("data/lake-superior-thunder-bay/lake-superior-thunder-bay-temperature.xlsx", sheet = "2018")
temp.2 <- read_excel("data/lake-superior-thunder-bay/lake-superior-thunder-bay-temperature.xlsx", sheet = "2019")
temp.3 <- read_excel("data/lake-superior-thunder-bay/lake-superior-thunder-bay-temperature.xlsx", sheet = "2020")
temp.4 <- read_excel("data/lake-superior-thunder-bay/lake-superior-thunder-bay-temperature.xlsx", sheet = "2021")

temp.all <- bind_rows(temp.1, temp.2, temp.3, temp.4) %>% 
  mutate(yday = yday(date))
rm(temp.1, temp.2, temp.3, temp.4)


spawn <- read_excel("data/lake-superior-thunder-bay/lake-superior-thunder-bay-spawning.xlsx", sheet = "lake-superior-thunder-bay-spawn") %>%
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
  mutate(spawn.group = rep(c("start", "end"), times = 4)) %>% 
  group_by(spawn.group) %>% 
  summarize(temp.c = mean(temp.c))





  



