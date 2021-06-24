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
  filter(lake == "thunder bay")

simulation.data.filt <- simulation.data %>% 
  filter(climate.group == model.locations$climate.group,
         depth.group == model.locations$depth.group)


#### LOAD TEMPERATURE DATA -----------------------------------------------------------------------

temp.1 <- read_excel("data/lake-superior-thunder-bay/lake-superior-thunder-bay-temperature.xlsx", sheet = "2018")
temp.2 <- read_excel("data/lake-superior-thunder-bay/lake-superior-thunder-bay-temperature.xlsx", sheet = "2019")
temp.3 <- read_excel("data/lake-superior-thunder-bay/lake-superior-thunder-bay-temperature.xlsx", sheet = "2020")
temp.4 <- read_excel("data/lake-superior-thunder-bay/lake-superior-thunder-bay-temperature.xlsx", sheet = "2021")

temp.all <- bind_rows(temp.1, temp.2, temp.3, temp.4) %>% 
  mutate(yday = yday(date))
rm(temp.1, temp.2, temp.3, temp.4)

## Stewart et al. 2021
model.superior <- read_excel("data/model-structural-parameters.xlsx", sheet = "coefs") %>% 
  filter(lake == "Lake Superior")


spawn <- read_excel("data/lake-superior-thunder-bay/lake-superior-thunder-bay-spawning.xlsx", sheet = "lake-superior-thunder-bay-spawn") %>%
  filter(year != 2017, prop.ripe != 0) %>% 
  group_by(year) %>% 
  slice(which.max(prop.ripe)) %>% 
  mutate(end.date = date+(6*86400)) %>% 
  select(year, start.date = date, end.date)

test2 <- temp.all %>% left_join(spawn) %>%
  group_by(year) %>% 
  filter(date >= start.date, date <= end.date) %>% ungroup() %>% 
  summarize(start.spawn.temp = max(temp.c),
            end.spawn.temp = min(temp.c))

ggplot(test, aes(x = year, y = hatch.yday)) +
  geom_boxplot() +
  coord_flip() +
  theme_bw()




  



