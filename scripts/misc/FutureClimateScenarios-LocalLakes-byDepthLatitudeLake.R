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

climate.files <- grep(pattern = "(?=.*simstrat)(?=.*bottemp)",
                      x = list.files(path = 'data/climate-simulations', recursive = TRUE, full.names = TRUE), 
                      value = TRUE, perl = TRUE)


#### LOAD DATA -----------------------------------------------------------------------------------

## bottom temp only
climate.simulations <- do.call(rbind, lapply(climate.files, function(i) {
  print(i)
  scenario <- str_match(i, "esm2m-\\s*(.*?)\\s*-bottemp")[2]
  lake <- str_match(i, "bottemp-\\s*(.*?)\\s*-2006")[2]
  
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


#### LOAD LAKE LOCATIONS AND JOIN WITH TEMP DATA -------------------------------------------------

lake.locations <- read_excel("data/climate-simulations/local-lake-locations.xlsx", sheet = "Sheet1") %>% 
  mutate(lake = toupper(lake))
climate.simulations.lakes <- left_join(climate.simulations, lake.locations) %>% 
  filter(include == "y") %>% 
  mutate(lakeDepth = paste0(lake, " (", max.depth.m, "-m)"))


#### LOOP TO CREATE FIGURE FOR EACH LAKE ---------------------------------------------------------

lapply(unique(climate.simulations.lakes$year.class), function(j) {
  tmp <- climate.simulations.lakes %>% 
    filter(year.class == j)
  
  lapply(unique(tmp$lat.group), function(k) {
    tmp2 <- tmp %>% 
      filter(lat.group == k)
    
    lapply(unique(tmp2$depth.group), function(l) {
      tmp3 <- tmp2 %>% 
        filter(depth.group == l)
      
      lapply(unique(tmp3$continent), function(m) {
        tmp4 <- tmp3 %>% 
          filter(continent == m)
        
      
        if(k == "low") {
          lat.label <- "Low Latitude (40-50°)"
        } else if(k == "mid") {
          lat.label <- "Mid Latitude (50-60°)"
        } else if(k == "high") {
          lat.label <- "High Latitude (60-70°)"
        }
        
        title <- paste0(m, ";  ", j, ";  ", lat.label, ";  ", l)
    
        ggplot(tmp4, aes(x = date, y = bottemp, color = scenario)) +
          geom_line(size = 0.8) +
          scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
          scale_y_continuous(limits = c(-0.25, 27), breaks = seq(0, 27, 3), expand = c(0, 0)) +
          scale_x_date(date_breaks = "1 month", date_labels =  "%b %d") + 
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
          facet_wrap(~lakeDepth, ncol = 3)
        
        if(length(unique(tmp4$lake)) == 1) {
          width <- 8; height <- 6
        } else if(length(unique(tmp4$lake)) == 2) {
          width <- 8; height <- 6
        } else if(length(unique(tmp4$lake)) == 3) {
          width <- 12; height <- 6
        } else if(length(unique(tmp4$lake)) == 4) {
          width <- 12; height <- 12
        } else if(length(unique(tmp4$lake)) == 5) {
          width <- 12; height <- 12
        } else if(length(unique(tmp4$lake)) == 6) {
          width <- 12; height <- 12
        } else {
          width <- 16; height <- 18
        }
        
        ggsave(paste0("figures/climate-scenarios/simstrat-summaries/byLatDepthLake/", m, "-", k, "-", l, "-", j, ".png"), dpi = 300, width = width, height = height)
      })
    })
  })
})
