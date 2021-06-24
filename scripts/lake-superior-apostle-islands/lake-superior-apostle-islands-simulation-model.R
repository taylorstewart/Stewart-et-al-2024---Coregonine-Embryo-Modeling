#### CLEAR THE ENVIRONMENT FIRST ---------------------------------------------

rm(list = ls(all.names = TRUE))


#### LOAD PACKAGES -----------------------------------------------------------

library(tidyverse)
library(data.table)
library(readxl)
library(ggplot2)
library(ggthemes)
library(stringr)
library(lubridate)
library(lubridateExtras)


#### CREATE A LIST OF FILES ----------------------------------------------------------------------

simulation.files <- list.files('data/climate-simulations/simstrat-summaries/byClimateDepth/', pattern = "csv", full.names = TRUE)


#### LOAD DATA -----------------------------------------------------------------------------------

simulation.data <- do.call(rbind, lapply(simulation.files, function(i) {
  data <- fread(i)
}))

model.locations <- read_excel("data/model-population-parameters.xlsx", sheet = "bio-parameters") %>% 
  filter(population == "apostle islands")

simulation.data.filt <- simulation.data %>% 
  filter(climate.group == model.locations$climate.group,
         depth.group == model.locations$depth.group)


## Stewart et al. 2021
model.superior <- read_excel("data/model-structural-parameters.xlsx", sheet = "coefficients") %>% 
  filter(lake == "Lake Superior")


lapply(unique(simulation.data.filt$year.class), function(i) {
  simulation.data.annual <- simulation.data.filt %>% filter(year == i)
  
  model.locations$start.spawn.temp_c
  
  model.locations$end.spawn.temp_c
  
  
})



