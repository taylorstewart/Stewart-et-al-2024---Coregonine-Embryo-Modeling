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

temp.1 <- read_excel("data/lake-superior-thunder-bay/lake-superior-thunder-bay-temperature.xlsx", sheet = "1997")
temp.2 <- read_excel("data/lake-superior-thunder-bay/lake-superior-thunder-bay-temperature.xlsx", sheet = "1998")
temp.3 <- read_excel("data/lake-superior-thunder-bay/lake-superior-thunder-bay-temperature.xlsx", sheet = "1999")
temp.4 <- read_excel("data/lake-superior-thunder-bay/lake-superior-thunder-bay-temperature.xlsx", sheet = "2000")
temp.5 <- read_excel("data/lake-superior-thunder-bay/lake-superior-thunder-bay-temperature.xlsx", sheet = "2001")
temp.6 <- read_excel("data/lake-superior-thunder-bay/lake-superior-thunder-bay-temperature.xlsx", sheet = "2002")
temp.7 <- read_excel("data/lake-superior-thunder-bay/lake-superior-thunder-bay-temperature.xlsx", sheet = "2003")
temp.8 <- read_excel("data/lake-superior-thunder-bay/lake-superior-thunder-bay-temperature.xlsx", sheet = "2004")
temp.9 <- read_excel("data/lake-superior-thunder-bay/lake-superior-thunder-bay-temperature.xlsx", sheet = "2005")
temp.10 <- read_excel("data/lake-superior-thunder-bay/lake-superior-thunder-bay-temperature.xlsx", sheet = "2006")
temp.11 <- read_excel("data/lake-superior-thunder-bay/lake-superior-thunder-bay-temperature.xlsx", sheet = "2007")
temp.12 <- read_excel("data/lake-superior-thunder-bay/lake-superior-thunder-bay-temperature.xlsx", sheet = "2018")
temp.13 <- read_excel("data/lake-superior-thunder-bay/lake-superior-thunder-bay-temperature.xlsx", sheet = "2019")
temp.14 <- read_excel("data/lake-superior-thunder-bay/lake-superior-thunder-bay-temperature.xlsx", sheet = "2020")
temp.15 <- read_excel("data/lake-superior-thunder-bay/lake-superior-thunder-bay-temperature.xlsx", sheet = "2021")

temp.all <- bind_rows(temp.1, temp.2, temp.3, temp.4, temp.5, temp.6, temp.7, temp.8, temp.9, temp.10, temp.11, temp.12, temp.13, temp.14, temp.15) %>% 
  mutate(yday = yday(date))
rm(temp.1, temp.2, temp.3, temp.4, temp.5, temp.6, temp.7, temp.8, temp.9, temp.10, temp.11, temp.12, temp.13, temp.14, temp.15)

## 
ggplot(temp.all, aes(x = date, y = temp_c)) + 
  geom_line() + theme_few() + 
  labs(y = 'Water Temperature (°C)', x = "") + 
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %d") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  facet_wrap(~year, scales = "free_x")


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
  mutate(spawn.group = rep(c("start", "end"), times = 15)) %>% 
  group_by(spawn.group) %>% 
  summarize(temp_c = mean(temp_c))





  



