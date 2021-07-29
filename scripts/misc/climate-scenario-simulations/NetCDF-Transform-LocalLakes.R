#### CLEAR THE ENVIRONMENT FIRST ---------------------------------------------

rm(list = ls(all.names = TRUE))


#### LOAD PACKAGES -----------------------------------------------------------

library(tidync)
library(dplyr)
library(stringr)

z
climate.files.watertemp <- grep(pattern = "(?=.*simstrat)(?=.*watertemp)",
                                x = list.files(path = "/Users/taylor/Downloads", recursive = TRUE, full.names = TRUE),
                                value = TRUE, perl = TRUE)
climate.files.bottemp <- grep(pattern = "(?=.*simstrat)(?=.*bottemp)",
                                x = list.files(path = "/Volumes/home/Coregonine-Modeling-ClimateSimulations", recursive = TRUE, full.names = TRUE),
                                value = TRUE, perl = TRUE)


lapply(climate.files.watertemp, function(i) {
  print(i)
  
  impact.model <- "simstrat"
  climate.forcing <- str_match(i, "simstrat_\\s*(.*?)\\s*_ewembi")[2]
  climate.scenario <- str_match(i, "ewembi_\\s*(.*?)\\s*_nosoc")[2]
  variable <- str_match(i, "co2_\\s*(.*?)\\s*_")[2]
  lake <- str_match(i, "temp_\\s*(.*?)\\s*_daily")[2]
  dates <- gsub("_", "-", str_match(i, "daily_\\s*(.*?)\\s*.nc4")[2])
  
  ## Load NetCDF files
  climate.output <- tidync(i)
  
  ## Transform into data frame
  climate.output.df <- climate.output %>% 
    hyper_tibble() %>% 
    mutate(date = as.Date(time, origin = "1661-01-01"),
           watertemp = round(watertemp - 273.15, 2),
           lake = lake) %>% 
    select(date, lake, depth, watertemp)
  
  write.csv(climate.output.df, paste0("data/climate-simulations/simstrat/", impact.model, "-", 
                                      climate.forcing, "-", climate.scenario, "-", variable, "-", 
                                      lake, "-", dates, ".csv"), row.names = FALSE)
})


lapply(climate.files.bottemp, function(i) {
  print(i)
  
  impact.model <- "simstrat"
  climate.forcing <- str_match(i, "simstrat_\\s*(.*?)\\s*_ewembi")[2]
  climate.scenario <- str_match(i, "ewembi_\\s*(.*?)\\s*_nosoc")[2]
  variable <- str_match(i, "co2_\\s*(.*?)\\s*_")[2]
  lake <- str_match(i, "temp_\\s*(.*?)\\s*_daily")[2]
  dates <- gsub("_", "-", str_match(i, "daily_\\s*(.*?)\\s*.nc4")[2])
  
  ## Load NetCDF files
  climate.output <- tidync(i)
  
  ## Transform into data frame
  climate.output.df <- climate.output %>% 
    hyper_tibble() %>% 
    mutate(date = as.Date(time, origin = "1661-01-01"),
           bottemp = round(bottemp - 273.15, 2),
           lake = lake) %>% 
    select(date, lake, bottemp)
  
  write.csv(climate.output.df, paste0("data/climate-simulations/simstrat/", impact.model, "-", 
                                      climate.forcing, "-", climate.scenario, "-", variable, "-", 
                                      lake, "-", dates, ".csv"), row.names = FALSE)
})
