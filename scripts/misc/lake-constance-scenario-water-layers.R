#### CLEAR THE ENVIRONMENT FIRST -----------------------------------------------------------------

rm(list = ls(all.names = TRUE))


#### LOAD PACKAGES -------------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(data.table)


#climate.simulations.annecy <- fread("data/climate-simulations/simstrat-summaries/lake-annecy-temperature-scenario-profile.csv")
#climate.simulations.biel <- fread("data/climate-simulations/simstrat-summaries/lake-biel-temperature-scenario-profile.csv")
#climate.simulations.bourget <- fread("data/climate-simulations/simstrat-summaries/lake-bourget-temperature-scenario-profile.csv")
#climate.simulations.geneva <- fread("data/climate-simulations/simstrat-summaries/lake-geneva-temperature-scenario-profile.csv")
climate.simulations.green <- fread("data/climate-simulations/simstrat-summaries/lake-green-temperature-scenario-profile.csv")
#climate.simulations.lz <- fread("data/climate-simulations/simstrat-summaries/lake-lower-zurich-temperature-scenario-profile.csv")
#climate.simulations.neuchatel <- fread("data/climate-simulations/simstrat-summaries/lake-neuchatel-temperature-scenario-profile.csv")
climate.simulations.rappbode <- fread("data/climate-simulations/simstrat-summaries/lake-rappbode-temperature-scenario-profile.csv")
climate.simulations.stechlin <- fread("data/climate-simulations/simstrat-summaries/lake-stechlin-temperature-scenario-profile.csv")

## Combine all lakes
climate.simulations <- bind_rows(#climate.simulations.annecy, climate.simulations.biel,
                                 #climate.simulations.bourget, climate.simulations.geneva, 
                                 climate.simulations.green, #climate.simulations.lz, 
                                 #climate.simulations.neuchatel, 
                                 climate.simulations.rappbode, climate.simulations.stechlin)


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

write.csv(temp.10, "data/lake-constance/lake-constance-temperature-spawning.csv", row.names = FALSE)

temp.10.start <- temp.10 %>% 
  group_by(scenario, year) %>%
  arrange(abs(temp_c - 10)) %>%
  slice(1)

temp.10.doy <- temp.10.start %>% mutate(spawn.yday = 259 + (0.26 * yday))

write.csv(temp.10.doy, "data/lake-constance/lake-constance-scenarios-spawning.csv", row.names = FALSE)


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

write.csv(temp.bot.mean, "data/lake-constance/lake-constance-temperature.csv", row.names = FALSE)


#### COMPARE SCENARIO VERSUS HISTORICAL MODEL ----------------------------------------------------

source("scripts/lake-constance-water-layers.R")

spawn <- temp.10.hist.doy %>% 
  mutate(scenario = "observed") %>% 
  select(date = datetime, scenario, everything()) %>% 
  filter(year < 2006)
spawn.scenario <- temp.10.doy %>% 
  filter(scenario == "Historical", year > 1980)

spawn.all <- bind_rows(spawn, spawn.scenario) %>% 
  select(year, scenario, spawn.yday) %>% 
  pivot_wider(names_from = scenario, values_from = spawn.yday) %>% 
  mutate(spawn.diff = observed - Historical)

spawn.all.diff <- mean(spawn.all$spawn.diff)


temp.bot.hist.mean.comp <- temp.bot.hist.mean %>% 
  mutate(scenario = "observed") %>% 
  select(date = datetime, scenario, everything()) %>% 
  filter(year < 2006)
temp.bot.mean.comp <- temp.bot.mean %>% 
  filter(scenario == "Historical", year > 1980)

temp.bot.all <- bind_rows(temp.bot.hist.mean.comp, temp.bot.mean.comp) %>% 
  select(date, year, month, day, scenario, temp_c) %>% 
  pivot_wider(names_from = scenario, values_from = temp_c) %>% 
  mutate(temp.diff = observed - Historical) %>% 
  filter(!is.na(temp.diff))

temp.bot.all.diff <- mean(temp.bot.all$temp.diff)


