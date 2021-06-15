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
  
  data <- fread(i) %>% 
    mutate(scenario = scenario, 
           decade = year(floor_decade(date)),
           year = year(date), 
           month = month(date), 
           week = week(date),
           day = day(date),
           yday = yday(date)) %>% 
    filter(month %in% c(9, 10, 11, 12, 1, 2, 3, 4, 5, 6)) %>% 
    mutate(year.class = ifelse(yday > 240, year + 1, year)) %>% 
    filter(year.class != 2006, year.class != 2100)
}))


#### LOAD LAKE LOCATIONS AND JOIN WITH TEMP DATA -------------------------------------------------

lake.locations <- read_excel("data/climate-simulations/local-lake-locations.xlsx", sheet = "Sheet1")
climate.simulations.lakes <- left_join(climate.simulations, lake.locations) %>% 
  filter(include == "y")


#### SUMMARIZE DAILY TEMPERATURE BY CONTINENT,  DEPTH,  AND LATITUDE -----------------------------

climate.simulations.mean <- climate.simulations.lakes %>% 
  group_by(scenario, continent, lat.group, depth.group, date, decade, year, month, week, day, yday, year.class) %>% 
  summarize(mean.bottemp = mean(bottemp),
            n = n()) %>% 
  mutate(lat.group = factor(lat.group, ordered = TRUE, levels = c("low", "mid", "high"), 
                            labels = c("Low Latitude (40-50°)", "Mid Latitude (50-60°)", "High Latitude (60-70°)")),
         scenario = factor(scenario, ordered = TRUE, levels = c("rcp26", "rcp60", "rcp85"), 
                            labels = c("RCP 2.6", "RCP 6.0", "RCP 8.5")))


#### LOOP TO CREATE FIGURE FOR EACH LAKE ---------------------------------------------------------

lapply(unique(climate.simulations.mean$year.class), function(j) {
  tmp <- climate.simulations.mean %>% 
    filter(year.class == j)
  
  lapply(unique(tmp$continent), function(k) {
    tmp2 <- tmp %>% 
      filter(continent == k)
    
    title <- paste0(k, ";  ", j)
    
    
    ggplot(tmp2, aes(x = date, y = mean.bottemp, color = scenario)) +
      geom_line(size = 0.8) +
      scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
      scale_y_continuous(limits = c(0, 27), breaks = seq(0, 27, 3), expand = c(0, 0)) +
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
      facet_grid(rows = vars(depth.group), cols = vars(lat.group))
    
    ggsave(paste0("figures/climate-scenarios/simstrat-summaries/byLatDepth/", k, "-", j, ".png"), dpi = 300, width = 14, height = 10)
    
    write.csv(tmp2, paste0("data/climate-simulations/simstrat-summaries/byLatDepth/", k, "-", j, ".csv"), row.names = FALSE)
  })
})
