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


### CREATE A LIST OF FILES ----------------------------------------------------------------------

climate.files <- grep(pattern = "(?=.*simstrat)(?=.*bottemp)",
                      x = list.files(path = 'data/climate-simulations', recursive = TRUE, full.names = TRUE), 
                      value = TRUE, perl = TRUE)


#### LOAD DATA -----------------------------------------------------------------------------------

## bottom temp only
climate.simulations <- do.call(rbind, lapply(climate.files, function(i) {
  print(i)
  
  model <- str_match(i, "simstrat-\\s*(.*?)\\s*-")[2]
  
  if(model == "gfdl") {
    scenario <- str_match(i, "esm2m-\\s*(.*?)\\s*-bottemp")[2]
  } else if(model == "hadgem2") {
    scenario <- str_match(i, "es-\\s*(.*?)\\s*-bottemp")[2]
  } else if(model == "ipsl") {
    scenario <- str_match(i, "lr-\\s*(.*?)\\s*-bottemp")[2]
  } else if(model == "miroc5") {
    scenario <- str_match(i, "miroc5-\\s*(.*?)\\s*-bottemp")[2]
  }
  
  if(scenario == "historical") {
    lake <- str_match(i, "bottemp-\\s*(.*?)\\s*-1861")[2]
  } else {
    lake <- str_match(i, "bottemp-\\s*(.*?)\\s*-2006")[2]
  }

  if(scenario == "rcp26") {
    scenario.upper <- "RCP 2.6"
  } else if(scenario == "rcp60") {
    scenario.upper <- "RCP 6.0"
  } else if(scenario == "rcp85") {
    scenario.upper <- "RCP 8.5"
  } else if(scenario == "historical") {
    scenario.upper <- "Historical"
  }
  
  data <- fread(i) %>% 
    mutate(lake = toupper(lake),
           scenario = scenario.upper,
           year = year(date),
           month = month(date),
           yday = yday(date)) %>% 
    filter(month %in% c(9, 10, 11, 12, 1, 2, 3, 4, 5, 6)) %>% 
    mutate(year.class = ifelse(yday > 240, year + 1, year)) %>% 
    filter(year.class != 2006, year.class != 2100)
}))


#### AVERAGE DAILY TEMP ACROSS MODELS ------------------------------------------------------------

climate.simulations.comb <- climate.simulations %>% 
  group_by(lake, scenario, year, year.class, date, month, yday) %>% 
  summarize(temp_c = mean(bottemp)) %>% ungroup()


#### LOAD LAKE LOCATIONS AND JOIN WITH TEMP DATA -------------------------------------------------

lake.locations <- read_excel("data/ISIMIP-local-lake-locations.xlsx", sheet = "Sheet1") %>% 
  mutate(lake = toupper(lake))
climate.simulations.lakes <- left_join(climate.simulations.comb, lake.locations) %>% 
  filter(include == "y" & include.depth == "y")


#### LOOP TO CREATE CSV FOR EACH LAKE ---------------------------------------------------------

lapply(unique(climate.simulations.lakes$year.class), function(j) {
  print(j)
  tmp <- climate.simulations.lakes %>% 
    filter(year.class == j)
  
  lapply(unique(tmp$climate.group), function(k) {
    tmp2 <- tmp %>% 
      filter(climate.group == k)
    
    lapply(unique(tmp2$depth.group), function(l) {
      tmp3 <- tmp2 %>% 
        filter(depth.group == l)
      
      daily.mean <- tmp3 %>% group_by(climate.group, depth.group, scenario, year.class, date, year, month, yday) %>% 
        summarize(mean.temp_c = round(mean(temp_c), 2),
                  mean.max.depth_m = round(mean(max.depth_m), 2))
      
      write.csv(daily.mean, paste0("data/climate-simulations/simstrat-summaries/byClimateDepth/", gsub(" ", "", k), "-", l, "-", j, ".csv"), row.names = FALSE)
    })
  })
})
