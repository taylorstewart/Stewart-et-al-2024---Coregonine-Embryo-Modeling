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

climate.files <- grep(pattern = "(?=.*simstrat)(?=.*watertemp)(?=.*geneva)",
                      x = list.files(path = 'data/climate-simulations', recursive = TRUE, full.names = TRUE), 
                      value = TRUE, perl = TRUE)

lake <- str_match(climate.files[1], "watertemp-\\s*(.*?)\\s*-2006")[2]


#### LOAD DATA -----------------------------------------------------------------------------------

## bottom temp only
climate.simulations <- do.call(rbind, lapply(climate.files, function(i) {
  print(i)
  scenario <- str_match(i, "esm2m-\\s*(.*?)\\s*-watertemp")[2]

  if(scenario == "rcp26") {
    scenario.upper <- "RCP 2.6"
  } else if(scenario == "rcp60") {
    scenario.upper <- "RCP 6.0"
  } else if(scenario == "rcp85") {
    scenario.upper <- "RCP 8.5"
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


#### LOOP TO CREATE FIGURE FOR EACH LAKE ---------------------------------------------------------

lapply(seq(2010, 2020, 10), function(j) {
  tmp <- climate.simulations %>% 
    filter(year.class == j)
  
  lapply(seq(0, 20, 1), function(k) {
    tmp2 <- tmp %>% 
      filter(depth == k)
    
    write.csv(tmp2, paste0("data/climate-simulations/simstrat-summaries/", lake, "/", j, "-", toupper(lake), "-", k, "m.csv"), row.names = FALSE)
    
    title <- paste0(j, "; ", toupper(lake), "; ", k, "-m Depth")
  
    ggplot(tmp2, aes(x = date, y = watertemp, color = scenario)) +
      geom_line(size = 0.8) +
      scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
      scale_y_continuous(limits = c(4, 30), breaks = seq(5, 30, 5), expand = c(0, 0)) +
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
            panel.spacing = unit(1, "lines"))
    
    ggsave(paste0("figures/climate-scenarios/simstrat-summaries/", lake, "/", j, "-", toupper(lake), "-", k, "m.png"), dpi = 300, width = 8, height = 6)
  })
})

