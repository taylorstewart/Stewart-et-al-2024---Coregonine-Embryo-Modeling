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

climate.files <- grep(pattern = "(?=.*simstrat)(?=.*watertemp)(?=.*annecy)",
                      x = list.files(path = 'data/climate-simulations', recursive = TRUE, full.names = TRUE), 
                      value = TRUE, perl = TRUE)


#### LOAD DATA -----------------------------------------------------------------------------------

## bottom temp only
climate.simulations <- do.call(rbind, lapply(climate.files, function(i) {
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
    mutate(lake = toupper(lake),
           scenario = scenario.upper,
           model = model,
           year = year(date),
           month = month(date),
           yday = yday(date)) %>% 
    filter(month %in% c(9, 10, 11, 12, 1, 2, 3, 4, 5, 6)) %>% 
    mutate(year.class = ifelse(yday > 240, year + 1, year)) %>% 
    filter(year.class != 2006, year.class != 2100)
})) %>% 
  filter(year.class >= 1900)


#### AVERAGE DAILY TEMP ACROSS MODELS ------------------------------------------------------------

climate.simulations.comb <- climate.simulations %>% 
  group_by(date, lake, depth, scenario, year, month, yday, year.class) %>% 
  summarize(watertemp = mean(watertemp)) %>% ungroup() %>% 
  mutate(model = "all") %>% 
  select(date, lake, depth, watertemp, scenario, model, year, month, yday, year.class)

climate.simulations.all <- bind_rows(climate.simulations, climate.simulations.comb)

#### LOOP TO CREATE FIGURE FOR EACH LAKE ---------------------------------------------------------

lapply(seq(1900, 2090, 10), function(j) {
  tmp <- climate.simulations.all %>% 
    filter(year.class == j)
  
  lapply(seq(0, 20, 1), function(k) {
    tmp2 <- tmp %>% 
      filter(depth == k)
    
    lake <- unique(tmp2$lake)
    
    if(length(unique(tmp2$scenario)) == 1) {
      width <- 7
    } else {
      width <- 14
    }
    
    #write.csv(tmp2, paste0("data/climate-simulations/simstrat-summaries/", tolower(lake), "/", j, "-", lake, "-", k, "m.csv"), row.names = FALSE)
    
    title <- paste0(j, "; ", lake, "; ", k, "-m Depth")
    
    ggplot(tmp2, aes(x = date, y = watertemp, color = model)) +
      geom_hline(yintercept = 8.08, color = "gray70") +
      geom_hline(yintercept = 6.53, color = "gray70") +
      geom_line(size = 0.8) +
      scale_color_manual(values = c("#c7e9b4", "#7fcdbb", "#41b6c4", "#2c7fb8", "#253494")) +
      scale_y_continuous(limits = c(0, 35), breaks = seq(0, 35, 5), expand = c(0, 0)) +
      scale_x_date(date_breaks = "1 month", date_labels =  "%b %d", expand = c(0, 5)) + 
      labs(y = "Bottom Water Temperature (°C)", title = title) + 
      theme_few() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text = element_text(size = 12),
            axis.title.y = element_text(size = 15),
            axis.title.x = element_blank(),
            legend.title = element_blank(), 
            legend.position = "top", 
            legend.text = element_text(size = 15),
            plot.title = element_text(hjust = 0.5),
            strip.text = element_text(size = 12),
            panel.spacing = unit(1, "lines")) +
      facet_wrap(~scenario, nrow = 1)
    
    ggsave(paste0("figures/climate-scenarios/simstrat-summaries/lake-", lake, "/", j, "-", toupper(lake), "-", k, "m.png"), dpi = 150, width = width, height = 6)
  })
})

