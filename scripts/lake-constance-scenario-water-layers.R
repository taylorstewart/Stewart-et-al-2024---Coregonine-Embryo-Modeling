#### CLEAR THE ENVIRONMENT FIRST -----------------------------------------------------------------

rm(list = ls(all.names = TRUE))


#### LOAD PACKAGES -------------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(data.table)


climate.simulations.green <- fread("data/climate-simulations/simstrat-summaries/lake-green-temperature-scenario-profile.csv")
climate.simulations.rappbode <- fread("data/climate-simulations/simstrat-summaries/lake-rappbode-temperature-scenario-profile.csv")
climate.simulations.stechlin <- fread("data/climate-simulations/simstrat-summaries/lake-stechlin-temperature-scenario-profile.csv")

## Combine all lakes
climate.simulations <- bind_rows(climate.simulations.green, climate.simulations.rappbode, climate.simulations.stechlin)


#### AVERAGE DAILY TEMP ACROSS LAKES -------------------------------------------------------------

climate.simulations.all <- climate.simulations %>% 
  group_by(date, scenario, depth) %>% 
  summarize(temp_c = mean(temp_c)) %>% 
  select(date, scenario, depth_m = depth, temp_c)


#### CALCULATE DATE OF 10C ISOTHERM AND SPAWN DATE -----------------------------------------------

temp.10 <- climate.simulations.all %>% filter(depth_m <= 10) %>% 
  group_by(date, scenario) %>% 
  summarize(temp_c = mean(temp_c)) %>% 
  mutate(year = year(date),
         month = month(date),
         day = day(date),
         yday = yday(date)) %>% 
  filter(month %in% c(10, 11, 12))

temp.10.start <- temp.10 %>% 
  group_by(scenario, year) %>%
  arrange(abs(temp_c - 10)) %>%
  slice(1)

temp.10.doy <- temp.10.start %>% mutate(spawn.yday = 259 + (0.26 * yday))

#write.csv(temp.10.doy, "data/lake-constance/lake-constance-scenarios-spawning.csv", row.names = FALSE)


#### CALCULATE DAILY TEMP BETWEEN 200-250 M ------------------------------------------------------

temp.bot <- climate.simulations %>% group_by(date, scenario, lake) %>% 
  filter(depth >= max(depth)-10, depth <= max(depth)) %>% 
  summarize(temp_c = mean(temp_c)) %>% 
  select(date, lake, scenario, temp_c) %>% 
  ungroup() %>% 
  mutate(year = year(date),
         month = month(date),
         day = day(date),
         yday = yday(date)) %>% 
  filter(month %in% c(10, 11, 12, 1, 2, 3, 4, 5, 6))

temp.bot.mean <- temp.bot %>% 
  group_by(date, scenario, year, month, day, yday) %>% 
  summarize(temp_c = mean(temp_c))

#write.csv(temp.bot.mean, "data/lake-constance/lake-constance-temperature.csv", row.names = FALSE)

