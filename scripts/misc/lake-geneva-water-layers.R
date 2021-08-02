library(rLakeAnalyzer)
library(tidyverse)
library(data.table)


climate.files.geneva <- grep(pattern = "(?=.*simstrat)(?=.*watertemp)(?=.*geneva)",
                             x = list.files(path = 'data/climate-simulations', recursive = TRUE, full.names = TRUE), 
                             value = TRUE, perl = TRUE)
climate.files.annecy <- grep(pattern = "(?=.*simstrat)(?=.*watertemp)(?=.*annecy)",
                             x = list.files(path = 'data/climate-simulations', recursive = TRUE, full.names = TRUE), 
                             value = TRUE, perl = TRUE)
climate.files.bourget <- grep(pattern = "(?=.*simstrat)(?=.*watertemp)(?=.*bourget)",
                              x = list.files(path = 'data/climate-simulations', recursive = TRUE, full.names = TRUE), 
                              value = TRUE, perl = TRUE)
climate.files.biel <- grep(pattern = "(?=.*simstrat)(?=.*watertemp)(?=.*biel)",
                           x = list.files(path = 'data/climate-simulations', recursive = TRUE, full.names = TRUE), 
                           value = TRUE, perl = TRUE)
climate.files.lz <- grep(pattern = "(?=.*simstrat)(?=.*watertemp)(?=.*zurich)",
                         x = list.files(path = 'data/climate-simulations', recursive = TRUE, full.names = TRUE), 
                         value = TRUE, perl = TRUE)
climate.files.neuchatel <- grep(pattern = "(?=.*simstrat)(?=.*watertemp)(?=.*neuchatel)",
                                x = list.files(path = 'data/climate-simulations', recursive = TRUE, full.names = TRUE), 
                                value = TRUE, perl = TRUE)
climate.files.rappbode <- grep(pattern = "(?=.*simstrat)(?=.*watertemp)(?=.*rappbode)",
                               x = list.files(path = 'data/climate-simulations', recursive = TRUE, full.names = TRUE), 
                               value = TRUE, perl = TRUE)
climate.files.stechlin <- grep(pattern = "(?=.*simstrat)(?=.*watertemp)(?=.*stechlin)",
                               x = list.files(path = 'data/climate-simulations', recursive = TRUE, full.names = TRUE), 
                               value = TRUE, perl = TRUE)

## Geneva
climate.simulations.geneva <- do.call(rbind, lapply(climate.files.geneva, function(i) {
  print(i)

  model <- str_match(i, "simstrat-\\s*(.*?)\\s*-")[2]
  
  if(model == "gfdl") {
    scenario <- str_match(i, "esm2m-\\s*(.*?)\\s*-watertemp")[2]
  } else if(model == "hadgem2") {
    scenario <- str_match(i, "es-\\s*(.*?)\\s*-watertemp")[2]
  } else if(model == "ipsl") {
    scenario <- str_match(i, "lr-\\s*(.*?)\\s*-watertemp")[2]
  } else if(model == "miroc5") {
    scenario <- str_match(i, "miroc5-\\s*(.*?)\\s*-watertemp")[2]
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
    mutate(scenario = scenario.upper) 
})) %>% 
  filter(date > as.Date("1899-12-31")) %>% 
  group_by(date, scenario, depth) %>% 
  ## summarize each lake across models
  summarize(temp_c = mean(watertemp))

## Annecy
climate.files.annecy <- do.call(rbind, lapply(climate.files.annecy, function(i) {
  print(i)
  
  model <- str_match(i, "simstrat-\\s*(.*?)\\s*-")[2]
  
  if(model == "gfdl") {
    scenario <- str_match(i, "esm2m-\\s*(.*?)\\s*-watertemp")[2]
  } else if(model == "hadgem2") {
    scenario <- str_match(i, "es-\\s*(.*?)\\s*-watertemp")[2]
  } else if(model == "ipsl") {
    scenario <- str_match(i, "lr-\\s*(.*?)\\s*-watertemp")[2]
  } else if(model == "miroc5") {
    scenario <- str_match(i, "miroc5-\\s*(.*?)\\s*-watertemp")[2]
  }
  
  if(scenario == "historical") {
    lake <- str_match(i, "watertemp-\\s*(.*?)\\s*-1861")[2]
  } else {
    lake <- str_match(i, "watertemp-\\s*(.*?)\\s*-2006")[2]
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
    mutate(scenario = scenario.upper)
})) %>% 
  filter(date > as.Date("1899-12-31")) %>% 
  group_by(date, scenario, depth) %>% 
  ## summarize each lake across models
  summarize(temp_c = mean(watertemp))

## Bourget
climate.simulations.bourget <- do.call(rbind, lapply(climate.files.bourget, function(i) {
  print(i)
  
  model <- str_match(i, "simstrat-\\s*(.*?)\\s*-")[2]
  
  if(model == "gfdl") {
    scenario <- str_match(i, "esm2m-\\s*(.*?)\\s*-watertemp")[2]
  } else if(model == "hadgem2") {
    scenario <- str_match(i, "es-\\s*(.*?)\\s*-watertemp")[2]
  } else if(model == "ipsl") {
    scenario <- str_match(i, "lr-\\s*(.*?)\\s*-watertemp")[2]
  } else if(model == "miroc5") {
    scenario <- str_match(i, "miroc5-\\s*(.*?)\\s*-watertemp")[2]
  }
  
  if(scenario == "historical") {
    lake <- str_match(i, "watertemp-\\s*(.*?)\\s*-1861")[2]
  } else {
    lake <- str_match(i, "watertemp-\\s*(.*?)\\s*-2006")[2]
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
    mutate(scenario = scenario.upper)
})) %>% 
  filter(date > as.Date("1899-12-31")) %>% 
  group_by(date, scenario, depth) %>% 
  ## summarize each lake across models
  summarize(temp_c = mean(watertemp))

## Biel
climate.simulations.biel <- do.call(rbind, lapply(climate.files.biel, function(i) {
  print(i)
  
  model <- str_match(i, "simstrat-\\s*(.*?)\\s*-")[2]
  
  if(model == "gfdl") {
    scenario <- str_match(i, "esm2m-\\s*(.*?)\\s*-watertemp")[2]
  } else if(model == "hadgem2") {
    scenario <- str_match(i, "es-\\s*(.*?)\\s*-watertemp")[2]
  } else if(model == "ipsl") {
    scenario <- str_match(i, "lr-\\s*(.*?)\\s*-watertemp")[2]
  } else if(model == "miroc5") {
    scenario <- str_match(i, "miroc5-\\s*(.*?)\\s*-watertemp")[2]
  }
  
  if(scenario == "historical") {
    lake <- str_match(i, "watertemp-\\s*(.*?)\\s*-1861")[2]
  } else {
    lake <- str_match(i, "watertemp-\\s*(.*?)\\s*-2006")[2]
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
    mutate(scenario = scenario.upper)
})) %>% 
  filter(date > as.Date("1899-12-31")) %>% 
  group_by(date, scenario, depth) %>% 
  ## summarize each lake across models
  summarize(temp_c = mean(watertemp))

## Lower-Zurich
climate.simulations.lz <- do.call(rbind, lapply(climate.files.lz, function(i) {
  print(i)
  
  model <- str_match(i, "simstrat-\\s*(.*?)\\s*-")[2]
  
  if(model == "gfdl") {
    scenario <- str_match(i, "esm2m-\\s*(.*?)\\s*-watertemp")[2]
  } else if(model == "hadgem2") {
    scenario <- str_match(i, "es-\\s*(.*?)\\s*-watertemp")[2]
  } else if(model == "ipsl") {
    scenario <- str_match(i, "lr-\\s*(.*?)\\s*-watertemp")[2]
  } else if(model == "miroc5") {
    scenario <- str_match(i, "miroc5-\\s*(.*?)\\s*-watertemp")[2]
  }
  
  if(scenario == "historical") {
    lake <- str_match(i, "watertemp-\\s*(.*?)\\s*-1861")[2]
  } else {
    lake <- str_match(i, "watertemp-\\s*(.*?)\\s*-2006")[2]
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
    mutate(scenario = scenario.upper)
})) %>% 
  filter(date > as.Date("1899-12-31")) %>% 
  group_by(date, scenario, depth) %>% 
  ## summarize each lake across models
  summarize(temp_c = mean(watertemp))

## Neuchatel
climate.simulations.neuchatel <- do.call(rbind, lapply(climate.files.neuchatel, function(i) {
  print(i)
  
  model <- str_match(i, "simstrat-\\s*(.*?)\\s*-")[2]
  
  if(model == "gfdl") {
    scenario <- str_match(i, "esm2m-\\s*(.*?)\\s*-watertemp")[2]
  } else if(model == "hadgem2") {
    scenario <- str_match(i, "es-\\s*(.*?)\\s*-watertemp")[2]
  } else if(model == "ipsl") {
    scenario <- str_match(i, "lr-\\s*(.*?)\\s*-watertemp")[2]
  } else if(model == "miroc5") {
    scenario <- str_match(i, "miroc5-\\s*(.*?)\\s*-watertemp")[2]
  }
  
  if(scenario == "historical") {
    lake <- str_match(i, "watertemp-\\s*(.*?)\\s*-1861")[2]
  } else {
    lake <- str_match(i, "watertemp-\\s*(.*?)\\s*-2006")[2]
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
    mutate(scenario = scenario.upper)
})) %>% 
  filter(date > as.Date("1899-12-31")) %>% 
  group_by(date, scenario, depth) %>% 
  ## summarize each lake across models
  summarize(temp_c = mean(watertemp))

## Rappbode
climate.simulations.rappbode <- do.call(rbind, lapply(climate.files.rappbode, function(i) {
  print(i)
  
  model <- str_match(i, "simstrat-\\s*(.*?)\\s*-")[2]
  
  if(model == "gfdl") {
    scenario <- str_match(i, "esm2m-\\s*(.*?)\\s*-watertemp")[2]
  } else if(model == "hadgem2") {
    scenario <- str_match(i, "es-\\s*(.*?)\\s*-watertemp")[2]
  } else if(model == "ipsl") {
    scenario <- str_match(i, "lr-\\s*(.*?)\\s*-watertemp")[2]
  } else if(model == "miroc5") {
    scenario <- str_match(i, "miroc5-\\s*(.*?)\\s*-watertemp")[2]
  }
  
  if(scenario == "historical") {
    lake <- str_match(i, "watertemp-\\s*(.*?)\\s*-1861")[2]
  } else {
    lake <- str_match(i, "watertemp-\\s*(.*?)\\s*-2006")[2]
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
    mutate(scenario = scenario.upper)
})) %>% 
  filter(date > as.Date("1899-12-31")) %>% 
  group_by(date, scenario, depth) %>% 
  ## summarize each lake across models
  summarize(temp_c = mean(watertemp))

## Stechlin
climate.simulations.stechlin <- do.call(rbind, lapply(climate.files.stechlin, function(i) {
  print(i)
  
  model <- str_match(i, "simstrat-\\s*(.*?)\\s*-")[2]
  
  if(model == "gfdl") {
    scenario <- str_match(i, "esm2m-\\s*(.*?)\\s*-watertemp")[2]
  } else if(model == "hadgem2") {
    scenario <- str_match(i, "es-\\s*(.*?)\\s*-watertemp")[2]
  } else if(model == "ipsl") {
    scenario <- str_match(i, "lr-\\s*(.*?)\\s*-watertemp")[2]
  } else if(model == "miroc5") {
    scenario <- str_match(i, "miroc5-\\s*(.*?)\\s*-watertemp")[2]
  }
  
  if(scenario == "historical") {
    lake <- str_match(i, "watertemp-\\s*(.*?)\\s*-1861")[2]
  } else {
    lake <- str_match(i, "watertemp-\\s*(.*?)\\s*-2006")[2]
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
    mutate(scenario = scenario.upper)
})) %>% 
  filter(date > as.Date("1899-12-31")) %>% 
  group_by(date, scenario, depth) %>% 
  ## summarize each lake across models
  summarize(temp_c = mean(watertemp))

## Combine all lakes
climate.simulations <- bind_rows(climate.simulations.geneva, climate.files.annecy, 
                                 climate.simulations.bourget, climate.simulations.biel, 
                                 climate.simulations.lz, climate.simulations.neuchatel, 
                                 climate.simulations.rappbode, climate.simulations.stechlin)


#### AVERAGE DAILY TEMP ACROSS LAKES -------------------------------------------------------------

climate.simulations.all <- climate.simulations %>% 
  group_by(date, scenario, depth) %>% 
  summarize(temp_c = mean(temp_c)) %>% 
  select(date, scenario, depth_m = depth, temp_c)


geneva.layers <- do.call(rbind, lapply(unique(climate.simulations.all$scenario), function(s) {
  tmp <- climate.simulations.all %>% filter(scenario == s)
  
  do.call(rbind, lapply(unique(tmp$date), function(d) {
    print(d)
    temp.filt <- tmp %>% filter(date == as.Date(d))
    
    for(j in 0:15) {
      df1 <- try({
        wtr.layer(depth = temp.filt$depth_m, measure = temp.filt$temp_c, z0 = j, nseg = 4)
      }, silent = TRUE)
      if(class(df1) == "try-error") {
        j <- j+1
      } else break
    }
    
    segs <- as.data.frame(df1$segments) %>% 
      mutate(date = d,
             scenario = s,
             layer.name = c("surface", "cline_top", "cline_bottom", "bottom")) %>% 
      select(date, scenario, layer.name, layer.depth_m = segment_depth, layer.temp_c = segment_measure)
  }))
}))

write.csv(geneva.layers, "data/lake-geneva/lake-geneva-temperature-layers.csv", row.names = FALSE)

