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
  filter(include == "y", 
         climate.group %in% c("Northern Temperate", "Northern Cool"), 
         depth.group %in% c("0-10m", "10-25m")) %>% 
  mutate(lakeDepth = paste0(lake, " (", max.depth.m, "-m)"))


#### LOOP TO CREATE FIGURE FOR EACH LAKE ---------------------------------------------------------

lapply(unique(climate.simulations.lakes$year.class), function(j) {
  tmp <- climate.simulations.lakes %>% 
    filter(year.class == j)
  
  lapply(unique(tmp$climate.group), function(k) {
    tmp2 <- tmp %>% 
      filter(climate.group == k)
    
    lapply(unique(tmp2$depth.group), function(l) {
      tmp3 <- tmp2 %>% 
        filter(depth.group == l)
      
      title <- paste0(j, ";  ", k, ";  ", l)
      
      ggplot(tmp3, aes(x = date, y = bottemp, color = scenario)) +
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
      
      if(length(unique(tmp3$lake)) == 1) {
        width <- 8; height <- 6
      } else if(length(unique(tmp3$lake)) == 2) {
        width <- 8; height <- 6
      } else if(length(unique(tmp3$lake)) == 3) {
        width <- 12; height <- 6
      } else if(length(unique(tmp3$lake)) == 4) {
        width <- 12; height <- 12
      } else if(length(unique(tmp3$lake)) == 5) {
        width <- 12; height <- 12
      } else if(length(unique(tmp3$lake)) == 6) {
        width <- 12; height <- 12
      } else {
        width <- 16; height <- 18
      }
      
      ggsave(paste0("figures/climate-scenarios/simstrat-summaries/byClimateDepthLake/", gsub(" ", "", k), "-", l, "-", j, ".png"), dpi = 300, width = width, height = height)
    })
  })
})


lapply(unique(climate.simulations.lakes$year.class), function(j) {
  tmp <- climate.simulations.lakes %>% 
    filter(year.class == j)
  
  lapply(unique(tmp$climate.group), function(k) {
    tmp2 <- tmp %>% 
      filter(climate.group == k)
    
    lapply(unique(tmp2$depth.group), function(l) {
      tmp3 <- tmp2 %>% 
        filter(depth.group == l)
      
      daily.mean <- tmp3 %>% group_by(climate.group, depth.group, scenario, year.class, date, year, month, yday) %>% 
        summarize(mean.temp.c = mean(bottemp),
                  mean.max.depth.m = mean(max.depth.m))
      
      write.csv(daily.mean, paste0("data/climate-simulations/simstrat-summaries/byClimateDepth/", gsub(" ", "", k), "-", l, "-", j, ".csv"), row.names = FALSE)
      
      title <- paste0(j, ";  ", k, ";  ", l)
      
      
      ggplot(daily.mean, aes(x = date, y = mean.temp.c, color = scenario)) +
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
              panel.spacing = unit(1, "lines"))
      
      ggsave(paste0("figures/climate-scenarios/simstrat-summaries/byClimateDepth/", gsub(" ", "", k), "-", l, "-", j, ".png"), dpi = 300, width = 8, height = 6)
    })
  })
})
