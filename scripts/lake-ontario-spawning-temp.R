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


### CREATE A LIST OF FILES ----------------------------------------------------------------------

simulation.files <- list.files('data/climate-simulations/simstrat-summaries/byClimateDepth/', pattern = "csv", full.names = TRUE)


#### LOAD DATA -----------------------------------------------------------------------------------

simulation.data <- do.call(rbind, lapply(simulation.files, function(i) {
  data <- fread(i)
}))

model.locations <- read_excel("data/model-lake-locations.xlsx", sheet = "coords") %>% 
  filter(lake == "ontario")

simulation.data.filt <- simulation.data %>% 
  filter(climate.group == model.locations$climate.group,
         depth.group == model.locations$depth.group)


#### LOAD TEMPERATURE DATA -----------------------------------------------------------------------

temp.1 <- read_excel("data/lake-ontario/lake-ontario-temperature-oswego-river.xlsx", sheet = "2012")
temp.2 <- read_excel("data/lake-ontario/lake-ontario-temperature-oswego-river.xlsx", sheet = "2013")
temp.3 <- read_excel("data/lake-ontario/lake-ontario-temperature-oswego-river.xlsx", sheet = "2014")
temp.4 <- read_excel("data/lake-ontario/lake-ontario-temperature-oswego-river.xlsx", sheet = "2015")
temp.5 <- read_excel("data/lake-ontario/lake-ontario-temperature-oswego-river.xlsx", sheet = "2016")
temp.6 <- read_excel("data/lake-ontario/lake-ontario-temperature-oswego-river.xlsx", sheet = "2017")
temp.7 <- read_excel("data/lake-ontario/lake-ontario-temperature-oswego-river.xlsx", sheet = "2018")
temp.8 <- read_excel("data/lake-ontario/lake-ontario-temperature-oswego-river.xlsx", sheet = "2019")
temp.9 <- read_excel("data/lake-ontario/lake-ontario-temperature-oswego-river.xlsx", sheet = "2020")
temp.10 <- read_excel("data/lake-ontario/lake-ontario-temperature-oswego-river.xlsx", sheet = "2021")

temp.all <- bind_rows(temp.1, temp.2, temp.3, temp.4, temp.5, temp.6, temp.7, temp.8, temp.9, temp.10) %>% 
  mutate(yday = yday(date))
rm(temp.1, temp.2, temp.3, temp.4, temp.5, temp.6, temp.7, temp.8, temp.9, temp.10)


spawn <- read_excel("data/lake-ontario/lake-ontario-spawning.xlsx", sheet = "lake-ontario-spawning") %>%
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
  summarize(temp.c = mean(temp.c))


  



