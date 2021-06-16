#### CLEAR THE ENVIRONMENT FIRST ---------------------------------------------

rm(list = ls(all.names = TRUE))


#### LOAD PACKAGES -----------------------------------------------------------

library(tidync)
library(dplyr)
library(stringr)


climate.files <- list.files("~/Downloads", pattern = "simstrat", full.names = TRUE)

lapply(climate.files, function(i) {
  
  impact.model <- str_match(i, "Downloads/(.*?)\\s*_gfdl")[2]
  climate.forcing <- str_match(i, "simstrat_\\s*(.*?)\\s*_ewembi")[2]
  climate.scenario <- str_match(i, "ewembi_\\s*(.*?)\\s*_nosoc")[2]
  variable <- str_match(i, "co2_\\s*(.*?)\\s*_")[2]
  lake <- str_match(i, "temp_\\s*(.*?)\\s*_daily")[2]
  
  ## Load NetCDF files
  climate.output <- tidync(i)
  
  ## Transform into data frame
  climate.output.df <- climate.output %>% 
    hyper_tibble() %>% 
    mutate(date = as.Date(time, origin = "1661-01-01"),
           watertemp = watertemp - 273.15,
           lake = lake) %>% 
    select(date, lake, lat, lon, depth, watertemp)
  
  write.csv(climate.output.df, paste0("data/climate-simulations/simstrat/", impact.model, "-", 
                                      climate.forcing, "-", climate.scenario, "-", variable, "-", 
                                      lake, "-2006-2099.csv"), row.names = FALSE)
  })

