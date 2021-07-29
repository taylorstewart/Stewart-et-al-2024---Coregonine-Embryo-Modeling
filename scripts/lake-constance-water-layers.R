library(tidyverse)
library(lubridate)
library(data.table)


temp <- fread("data/lake-constance/lake-constance-upper-temperature-historical-model.csv")


#### CALCULATE DATE OF 10C ISOTHERM AND SPAWN DATE -----------------------------------------------

temp.10.hist <- temp %>% filter(depth_m == 10) %>% 
  mutate(year = year(datetime),
         month = month(datetime),
         day = day(datetime),
         yday = yday(datetime)) %>% 
  filter(month %in% c(10, 11, 12)) %>% 
  group_by(year) %>%
  arrange(abs(temp_c - 10)) %>%
  slice(1)

temp.10.hist.doy <- temp.10.hist %>% mutate(spawn.yday = 259 + (0.26 * yday))


#### CALCULATE DAILY TEMP BETWEEN 200-250 M ------------------------------------------------------

temp.bot.hist <- temp %>% filter(depth_m >= 200, depth_m <= 252) %>% 
  mutate(year = year(datetime),
         month = month(datetime),
         day = day(datetime),
         yday = yday(datetime)) %>% 
  filter(month %in% c(10, 11, 12, 1, 2, 3, 4, 5, 6))

temp.bot.hist.mean <- temp.bot.hist %>% 
  group_by(datetime, year, month, day, yday) %>% 
  summarize(temp_c = mean(temp_c))




