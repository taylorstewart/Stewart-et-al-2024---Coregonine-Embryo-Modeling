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
  filter(lake == "apostle islands")

simulation.data.filt <- simulation.data %>% 
  filter(climate.group == model.locations$climate.group,
         depth.group == model.locations$depth.group)


#### LOAD TEMPERATURE DATA -----------------------------------------------------------------------

temp.1 <- read_excel("data/lake-superior-apostle-islands/lake-superior-apostle-islands-temperature.xlsx", sheet = "2016")
temp.2 <- read_excel("data/lake-superior-apostle-islands/lake-superior-apostle-islands-temperature.xlsx", sheet = "2017")
temp.3 <- read_excel("data/lake-superior-apostle-islands/lake-superior-apostle-islands-temperature.xlsx", sheet = "2018")

temp.all <- bind_rows(temp.1, temp.2, temp.3) %>% 
  mutate(yday = yday(date))
rm(temp.1, temp.2, temp.3)

## Stewart et al. 2021
model.superior <- read_excel("data/model-structural-parameters.xlsx", sheet = "coefs") %>% 
  filter(lake == "Lake Superior")


spawn <- read_excel("data/lake-superior-apostle-islands/lake-superior-apostle-islands-spawning.xlsx", sheet = "lake-superior-apostle-spawning") %>%
  filter(year != 2018) %>% 
  group_by(year) %>% 
  slice(which.max(prop.ripe)) %>% 
  mutate(end.date = date+(6*86400)) %>% 
  select(year, start.date = date, end.date)


test <- do.call(rbind, lapply(unique(spawn$year), function(i) {
  temp.spawn <- temp.all %>% left_join(spawn) %>%
    filter(year == i) %>% 
    filter(date >= start.date, date <= end.date)
  
  do.call(rbind, lapply(unique(temp.spawn$date), function(j) {
    ## Filter temp profiles by start and end dates
    temp.spawn.year <- temp.all %>%
      filter(date >= j, year == year(j)+1)
    
    ## Take antilog from daily semilog output, accumulate across days
    model.superior.perc <- temp.spawn.year %>% 
      mutate(perc.day = (10^(model.superior$a + model.superior$b * temp.c + model.superior$c * temp.c^2))*100,
             perc.cum = cumsum(perc.day),) %>% 
      filter(perc.cum <= 100) %>%
      mutate(ADD = cumsum(temp.c))
    
    model.superior.perc.max <- model.superior.perc %>% group_by(year) %>% 
      filter(perc.cum == max(perc.cum)) %>% 
      select(date, year, temp.c, ADD) %>% 
      mutate(spawn.date = j,
             dpf = date-spawn.date, 
             hatch.yday = yday(date)) %>% 
      select(spawn.date, hatch.date = date, year, temp.c, hatch.yday, dpf, ADD)
  }))
})) %>% 
  mutate(year = factor(year))

test2 <- temp.all %>% left_join(spawn) %>%
  group_by(year) %>% 
  filter(date >= start.date, date <= end.date) %>% ungroup() %>% 
  summarize(start.spawn.temp = max(temp.c),
            end.spawn.temp = min(temp.c))

ggplot(test, aes(x = year, y = hatch.yday)) +
  geom_boxplot() +
  coord_flip() +
  theme_bw()




  



