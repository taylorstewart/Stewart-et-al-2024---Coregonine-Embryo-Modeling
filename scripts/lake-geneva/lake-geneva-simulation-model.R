#### CLEAR THE ENVIRONMENT FIRST ---------------------------------------------

rm(list = ls(all.names = TRUE))


#### LOAD PACKAGES -----------------------------------------------------------

library(tidyverse)
library(data.table)
library(readxl)
library(ggplot2)
library(ggthemes)
library(stringr)
library(lubridate)


#### CREATE A LIST OF FILES ----------------------------------------------------------------------

simulation.files <- grep(pattern = "(?=.*simstrat)(?=.*watertemp)(?=.*geneva)",
                         x = list.files(path = 'data/climate-simulations', recursive = TRUE, full.names = TRUE), 
                         value = TRUE, perl = TRUE)


#### LOAD DATA -----------------------------------------------------------------------------------

simulation.data <- do.call(rbind, lapply(simulation.files, function(i) {
  print(i)
  lake <- str_match(i, "watertemp-\\s*(.*?)\\s*-2006")[2]
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
})) %>% 
  filter(year.class >= 2010)

model.locations <- read_excel("data/model-population-parameters.xlsx", sheet = "bio-parameters") %>% 
  filter(population == "geneva")

simulation.data.filt <- simulation.data %>% 
  filter(depth == model.locations$spawning.depth.m) %>% 
  rename(temp_c = watertemp)


## Stewart et al. 2021
model.parameters <- read_excel("data/model-structural-parameters.xlsx", sheet = "coefficients") %>% 
  filter(lake == "Lake Geneva")


simulation.model.hatch <- do.call(rbind, lapply(unique(simulation.data.filt$year.class), function(i) {
  ## filter to a single winter
  simulation.data.annual <- simulation.data.filt %>% filter(year.class == i)
  
  ## Calculate a 5-day center moving average to smooth temperature curve
  ## Smoothing prevents issues below trying to find the start and stop from large daily temp deviations
  simulation.data.annual.ma <- simulation.data.annual %>% group_by(scenario) %>% 
    mutate(temp.ma_c = frollmean(temp_c, n = 5, align = "center"))
  
  ## Calculate the start date of spawning period
  spawn.start.date <- simulation.data.annual.ma %>% 
    group_by(scenario) %>% 
    filter(temp.ma_c <= model.locations$start.spawn.temp_c) %>% 
    arrange(date) %>% 
    slice(1) %>% select(scenario, year.class, spawn.start.date = date)
  
  ## Calculate the end date of spawning period
  spawn.end.date <- simulation.data.annual.ma %>% 
    group_by(scenario) %>% 
    filter(temp.ma_c <= model.locations$end.spawn.temp_c) %>% 
    arrange(date) %>% 
    slice(1) %>% 
    select(scenario, year.class, spawn.end.date = date) %>% 
    ## subtract one day to correct for temp less than (spawning ends day of temp threshold)
    mutate(spawn.end.date = as.Date(spawn.end.date)-1)
  
  if(nrow(spawn.end.date) != 3) {
    spawn.end.date <- spawn.start.date %>% left_join(spawn.end.date) %>% 
      mutate(spawn.end.date = as.Date(ifelse(is.na(spawn.end.date) == TRUE, as.Date(spawn.start.date)+7, spawn.end.date), origin = "1970-01-01")) %>% 
      select(-spawn.start.date)
  } else {
    spawn.end.date
    }
  
  ## Combine start and end dates; Filter to each day in spawning period
  spawn.period.temp <- simulation.data.annual.ma %>% 
    left_join(spawn.start.date) %>% 
    left_join(spawn.end.date) %>% 
    mutate(spawn.length_days = as.Date(spawn.end.date) - as.Date(spawn.start.date),
           spawn.end.date = as.Date(ifelse(spawn.length_days > 30, spawn.start.date+30, spawn.end.date), origin = "1970-01-01"),
           spawn.length_days = as.Date(spawn.end.date) - as.Date(spawn.start.date)) %>%
    group_by(scenario) %>% 
    filter(date >= spawn.start.date, date <= spawn.end.date)
  
  
  ## Loop across all climate scenarios
  do.call(rbind, lapply(unique(spawn.period.temp$scenario), function(j) {
    spawn.period.temp.scenario <- spawn.period.temp %>% filter(scenario == j)
    
    ## Run model for each day in the spawning period
    temp.hatch <- do.call(rbind, lapply(unique(spawn.period.temp.scenario$date), function(k) {
      simulation.data.model <- simulation.data.annual %>% filter(scenario == j, date >= k)
      
      spawn.temp_c <- simulation.data.model %>% slice(1) %>% pull(temp_c)
      spawn.length_days <- spawn.period.temp.scenario %>% slice(1) %>% pull(spawn.length_days) %>% as.numeric()
      
      ## Take antilog from daily semilog output, accumulate across days
      simulation.data.model.output <- simulation.data.model %>% 
        mutate(perc.day = (10^(model.parameters$a + model.parameters$b * temp_c))*100,
               perc.cum = cumsum(perc.day)) %>% 
        filter(perc.cum <= 100) %>%
        mutate(ADD = cumsum(temp_c))
      
      ## Extract hatch date
      simulation.data.model.output.max <- simulation.data.model.output %>% 
        slice(which.max(perc.cum)) %>% 
        mutate(spawn.date = k,
               spawn.yday = yday(spawn.date),
               spawn.yday = ifelse(spawn.yday < 100, spawn.yday+365, spawn.yday),
               spawn.length_days = spawn.length_days,
               spawn.temp_c = spawn.temp_c,
               dpf = as.Date(date)-as.Date(k), 
               hatch.yday = yday(date)) %>% 
        select(scenario, year.class, spawn.date, spawn.yday, spawn.length_days, spawn.temp_c, hatch.date = date, hatch.yday, hatch.temp_c = temp_c, dpf, ADD)
    })) %>% 
      mutate(spawn.peak.date = mean(spawn.date),
             spawn.peak.yday = yday(spawn.peak.date),
             spawn.peak.yday = ifelse(spawn.peak.yday < 100, spawn.peak.yday+365, spawn.peak.yday),
             hatch.peak.date = mean(hatch.date),
             hatch.peak.yday = yday(hatch.peak.date),
             hatch.length_days = length(unique(hatch.yday))) %>% 
      select(1:3, spawn.peak.date, spawn.peak.yday, 4:7, hatch.peak.date, hatch.peak.yday, 8, hatch.length_days, 9:12)
  }))
})) %>% mutate(year.class = factor(year.class),
               decade = factor(year(floor_date(hatch.date, years(10)))),
               spawn.peak.yday.plot = as.Date(spawn.peak.yday, origin = "1970-01-01"),
               hatch.yday.plot = as.Date(hatch.yday, origin = "1970-01-01"))


ggplot(simulation.model.hatch, aes(x = spawn.peak.yday.plot, y = hatch.yday.plot)) +
  geom_tile(aes(fill = decade)) +
  scale_x_date(date_breaks = "2 weeks", date_labels =  "%b %d", expand = c(0, 5)) + 
  scale_y_date(date_breaks = "2 weeks", date_labels =  "%b %d", expand = c(0, 5)) + 
  #scale_x_continuous(limits = c(312, 355), breaks = seq(315, 355, 5), expand = c(0, 0.2)) +
  #scale_y_continuous(limits = c(85, 126), breaks = seq(85, 125, 5), expand = c(0, 0.2)) +
  scale_fill_manual(values = c("#ffffcc", "#ffeda0", "#fed976", "#feb24c", 
                               "#fd8d3c", "#fc4e2a", "#e31a1c", "#bd0026", "#800026")) +
  labs(x = "Mean Spawn Date", y = "Hatch Date") +
  theme_few() +
  theme(panel.background = element_rect(fill = "grey90", colour = "grey90"),
        axis.title.x = element_text(color = "Black", size = 15, margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(color = "Black", size = 15, margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks.length = unit(2, 'mm'),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.key.size = unit(1.0, 'cm'),
        legend.key = element_rect(color = "black"),
        strip.text = element_text(size = 10),
        panel.spacing = unit(1.5, "lines")) +
  facet_wrap(~scenario)

ggsave("figures/lake-geneva/lake-geneva-simulation-heatmap.png", width = 14, height = 7, dpi = 300)


