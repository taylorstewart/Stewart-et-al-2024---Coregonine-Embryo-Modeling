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

climate.files.TB <- grep(pattern = "(?=.*vic-lake)(?=.*thunder-bay)",
                      x = list.files(path = 'data/climate-simulations', recursive = TRUE, full.names = TRUE), 
                      value = TRUE, perl = TRUE)
climate.files.APIS <- grep(pattern = "(?=.*vic-lake)(?=.*apostle-islands)",
                           x = list.files(path = 'data/climate-simulations', recursive = TRUE, full.names = TRUE), 
                           value = TRUE, perl = TRUE)
climate.files.CB <- grep(pattern = "(?=.*vic-lake)(?=.*chaumont-bay)",
                         x = list.files(path = 'data/climate-simulations', recursive = TRUE, full.names = TRUE), 
                         value = TRUE, perl = TRUE)
climate.files.LK <- grep(pattern = "(?=.*vic-lake)(?=.*lake-konnevesi)",
                         x = list.files(path = 'data/climate-simulations', recursive = TRUE, full.names = TRUE), 
                         value = TRUE, perl = TRUE)


climate.data.tb <- do.call(rbind, lapply(climate.files.TB, function(i) {
  print(i)
  scenario <- str_match(i, "esm2m-\\s*(.*?)\\s*-TB")[2]
  current.year <- as.numeric(str_match(i, "TB-\\s*(.*?)\\s*.csv")[2])
  previous.year <- as.numeric(str_match(i, "TB-\\s*(.*?)\\s*.csv")[2])-1
  
  if(previous.year != 2010) {
    previous.year.file <- paste0("data/climate-simulations/lake-superior-thunder-bay/vic-lake-gfdl-esm2m-", scenario, "-TB-", previous.year, ".csv")
    current.year.file <- paste0("data/climate-simulations/lake-superior-thunder-bay/vic-lake-gfdl-esm2m-", scenario, "-TB-", current.year, ".csv")
    
    tmp.previous <- fread(previous.year.file)
    tmp.current <- fread(current.year.file)
    tmp <- bind_rows(tmp.previous, tmp.current) %>% 
      filter(date >= paste0(previous.year, "-09-01"),
             date <= paste0(current.year, "-07-01")) %>% 
      mutate(year.class = current.year, 
             scenario = scenario, 
             lake = "thunder-bay") %>% 
      select(lake, scenario, date, year.class, lat, lon, depth, watertemp) %>% 
      group_by(lake, scenario, date, lat, lon) %>% 
      slice(which.max(depth))
  }
}))

climate.data.apis <- do.call(rbind, lapply(climate.files.APIS, function(i) {
  print(i)
  scenario <- str_match(i, "esm2m-\\s*(.*?)\\s*-APIS")[2]
  current.year <- as.numeric(str_match(i, "APIS-\\s*(.*?)\\s*.csv")[2])
  previous.year <- as.numeric(str_match(i, "APIS-\\s*(.*?)\\s*.csv")[2])-1
  
  if(previous.year != 2010) {
    previous.year.file <- paste0("data/climate-simulations/lake-superior-apostle-islands/vic-lake-gfdl-esm2m-", scenario, "-APIS-", previous.year, ".csv")
    current.year.file <- paste0("data/climate-simulations/lake-superior-apostle-islands/vic-lake-gfdl-esm2m-", scenario, "-APIS-", current.year, ".csv")
    
    tmp.previous <- fread(previous.year.file)
    tmp.current <- fread(current.year.file)
    tmp <- bind_rows(tmp.previous, tmp.current) %>% 
      filter(date >= paste0(previous.year, "-09-01"),
             date <= paste0(current.year, "-07-01")) %>% 
      mutate(year.class = current.year, 
             scenario = scenario, 
             lake = "apostle-islands") %>% 
      select(lake, scenario, date, year.class, lat, lon, depth, watertemp) %>% 
      group_by(lake, scenario, date, lat, lon) %>% 
      slice(which.max(depth))
  }
}))

climate.data.cb <- do.call(rbind, lapply(climate.files.CB, function(i) {
  print(i)
  scenario <- str_match(i, "esm2m-\\s*(.*?)\\s*-CB")[2]
  current.year <- as.numeric(str_match(i, "CB-\\s*(.*?)\\s*.csv")[2])
  previous.year <- as.numeric(str_match(i, "CB-\\s*(.*?)\\s*.csv")[2])-1
  
  if(previous.year != 2010) {
    previous.year.file <- paste0("data/climate-simulations/lake-ontario-chaumont-bay/vic-lake-gfdl-esm2m-", scenario, "-CB-", previous.year, ".csv")
    current.year.file <- paste0("data/climate-simulations/lake-ontario-chaumont-bay/vic-lake-gfdl-esm2m-", scenario, "-CB-", current.year, ".csv")
    
    tmp.previous <- fread(previous.year.file)
    tmp.current <- fread(current.year.file)
    tmp <- bind_rows(tmp.previous, tmp.current) %>% 
      filter(lon == -76.25,
             date >= paste0(previous.year, "-09-01"),
             date <= paste0(current.year, "-07-01")) %>% 
      mutate(year.class = current.year, 
             scenario = scenario, 
             lake = "lake-ontario") %>% 
      select(lake, scenario, date, year.class, lat, lon, depth, watertemp) %>% 
      group_by(lake, scenario, date, lat, lon) %>% 
      slice(which.max(depth))
  }
}))

climate.data.lk <- do.call(rbind, lapply(climate.files.LK, function(i) {
  print(i)
  scenario <- str_match(i, "esm2m-\\s*(.*?)\\s*-LK")[2]
  current.year <- as.numeric(str_match(i, "LK-\\s*(.*?)\\s*.csv")[2])
  previous.year <- as.numeric(str_match(i, "LK-\\s*(.*?)\\s*.csv")[2])-1
  
  if(previous.year != 2010) {
    previous.year.file <- paste0("data/climate-simulations/lake-konnevesi/vic-lake-gfdl-esm2m-", scenario, "-LK-", previous.year, ".csv")
    current.year.file <- paste0("data/climate-simulations/lake-konnevesi/vic-lake-gfdl-esm2m-", scenario, "-LK-", current.year, ".csv")
    
    tmp.previous <- fread(previous.year.file)
    tmp.current <- fread(current.year.file)
    tmp <- bind_rows(tmp.previous, tmp.current) %>% 
      filter(lon == 26.75, 
             date >= paste0(previous.year, "-09-01"),
             date <= paste0(current.year, "-07-01")) %>% 
      mutate(year.class = current.year, 
             scenario = scenario, 
             lake = "lake-konnevesi") %>% 
      select(lake, scenario, date, year.class, lat, lon, depth, watertemp) %>% 
      group_by(lake, scenario, date, lat, lon) %>% 
      slice(which.max(depth))
  }
}))

climate.data <- bind_rows(climate.data.tb, climate.data.apis, climate.data.cb, climate.data.lk) %>% 
  mutate(group = paste0(lat, ", ", lon, " (", depth, "-m)"),
         watertemp = ifelse(watertemp < 0, 0.1, watertemp))

#unique(climate.data$lake)
lapply("apostle-islands", function(i) {
  tmp <- climate.data %>% filter(lake == i)
  
  lapply(unique(climate.data$scenario), function(j) {
    tmp2 <- tmp %>% filter(scenario == j)
    
    lapply(unique(climate.data$year.class), function(k) {
      tmp3 <- tmp2 %>% filter(year.class == k)
      
      if(i == "thunder-bay") {
        lake.name <- "Lake Superior (Thunder Bay)"
      } else if(i == "apostle-islands") {
        lake.name <- "Lake Superior (Apostle Islands)"
      } else if(i == "lake-ontario") {
        lake.name <- "Lake Ontario (Chaumont Bay)"
      } else if(i == "lake-konnevesi") {
        lake.name <- "Lake Southern Konnevesi"
      }
      
      if(j == "rcp26") {
        scenario.name <- "RCP 2.6"
      } else if(j == "rcp60") {
        scenario.name <- "RCP 6.0"
      } else if(j == "rcp85") {
        scenario.name <- "RCP 8.5"
      }
      
      title <- paste(lake.name, scenario.name, k, sep = " ")
      
      ggplot(tmp3, aes(x = date, y = watertemp, group == group, color = group)) +
        geom_line(size = 0.8) +
        scale_x_date(date_breaks = "1 month", date_labels =  "%b %d") +
        scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 4), expand = c(0, 0)) +
        labs(y = "Bottom Water Temperature (°C)", title = title) + 
        theme_few() + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              axis.text = element_text(size = 12),
              axis.title.y = element_text(size = 15),
              axis.title.x = element_blank(),
              legend.position = "top", 
              legend.title = element_blank(),
              legend.text = element_text(size = 15),
              strip.text = element_text(size = 12),
              panel.spacing = unit(1, "lines"))
      
      ggsave(paste0("figures/climate-scenarios/vic-lake/", i, "/", i, "-", j, "-", k, ".png"), dpi = 300, width = 10, height = 6)
    })
  })
})


