#### CLEAR THE ENVIRONMENT FIRST ---------------------------------------------

rm(list = ls(all.names = TRUE))


#### LOAD PACKAGES -----------------------------------------------------------

library(tidyverse)
library(data.table)
library(readxl)
library(ggthemes)
library(lubridate)


#### CREATE A LIST OF FILES ----------------------------------------------------------------------

simulation.files <- list.files('data/climate-simulations/simstrat-summaries/byClimateDepth/', pattern = "csv", full.names = TRUE)


#### LOAD DATA -----------------------------------------------------------------------------------

simulation.data <- do.call(rbind, lapply(simulation.files, function(i) {
  data <- fread(i)
})) %>% 
  filter(year.class >= 2010)

model.locations <- read_excel("data/model-population-parameters.xlsx", sheet = "bio-parameters") %>% 
  filter(population == "apostle islands")

simulation.data.filt <- simulation.data %>% 
  filter(climate.group == model.locations$climate.group,
         depth.group == model.locations$depth.group)


## Stewart et al. 2021
model.parameters <- read_excel("data/model-structural-parameters.xlsx", sheet = "coefficients") %>% 
  filter(lake == "Lake Superior")


simulation.model.hatch <- do.call(rbind, lapply(unique(simulation.data.filt$year.class), function(i) {
  ## filter to a single winter
  simulation.data.annual <- simulation.data.filt %>% filter(year.class == i)
  
  ## Calculate a 5-day center moving average to smooth temperature curve
    ## Smoothing prevents issues below trying to find the start and stop from large daily temp deviations
  simulation.data.annual.ma <- simulation.data.annual %>% group_by(scenario) %>% 
    mutate(temp.ma_c = frollmean(mean.temp.c, n = 5, align = "center"))
  
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
  
  ## Combine start and end dates; Filter to each day in spawning period
  spawn.period.temp <- simulation.data.annual.ma %>% 
    left_join(spawn.start.date) %>% 
    left_join(spawn.end.date) %>% 
    mutate(spawn.length_days = as.Date(spawn.end.date) - as.Date(spawn.start.date),
           spawn.end.date = as.Date(ifelse(spawn.length_days > 20, spawn.start.date+20, spawn.end.date), origin = "1970-01-01"),
           spawn.length_days = as.Date(spawn.end.date) - as.Date(spawn.start.date)) %>%
    group_by(scenario) %>% 
    filter(date >= spawn.start.date, date <= spawn.end.date) 
  
  ## Calculate the rate of temperature change and divide number of spawning females propotionally
  spawner.abundance <- spawn.period.temp %>% 
    group_by(scenario) %>% 
    mutate(temp.diff_c = lag(mean.temp.c) - mean.temp.c,
           temp.diff_c = ifelse(is.na(temp.diff_c) == TRUE, model.locations$start.spawn.temp_c - mean.temp.c, temp.diff_c),
           temp.diff.prop = temp.diff_c / sum(temp.diff_c),
           temp.diff.prop = ifelse(temp.diff.prop < 0, 0, temp.diff.prop),
           daily.spawner.abundance = round(1000 * temp.diff.prop, 0),
           daily.spawner.abundance = ifelse(daily.spawner.abundance == 0, 1, daily.spawner.abundance),
           daily.eggs = daily.spawner.abundance * 100) %>% 
    select(-temp.diff_c, -temp.diff.prop, -daily.spawner.abundance)
  
  ## Loop across all climate scenarios
  do.call(rbind, lapply(unique(spawner.abundance$scenario), function(j) {
    spawn.period.temp.scenario <- spawner.abundance %>% filter(scenario == j)
    
    ## Run model for each day in the spawning period
    temp.hatch <- do.call(rbind, lapply(unique(spawn.period.temp.scenario$date), function(k) {
      simulation.data.model <- simulation.data.annual %>% filter(scenario == j, date >= k)
      
      spawn.temp_c <- simulation.data.model %>% slice(1) %>% pull(mean.temp.c)
      spawn.length_days <- spawn.period.temp.scenario %>% slice(1) %>% pull(spawn.length_days) %>% as.numeric()
      daily.eggs <- spawn.period.temp.scenario %>% filter(date == k) %>% pull(daily.eggs)
      
      ## Take antilog from daily semilog output, accumulate across days
      simulation.data.model.output <- simulation.data.model %>% 
        mutate(perc.day = (10^(model.parameters$a + model.parameters$b * mean.temp.c + model.parameters$c * mean.temp.c^2))*100,
               perc.cum = cumsum(perc.day),) %>% 
        filter(perc.cum <= 100) %>%
        mutate(ADD = cumsum(mean.temp.c))
      
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
        select(scenario, year.class, spawn.date, spawn.yday, spawn.length_days, spawn.temp_c, hatch.date = date, hatch.yday, hatch.temp_c = mean.temp.c, dpf, ADD)
      
      simulation.data.model.output.max.rep <- simulation.data.model.output.max %>% slice(rep(1:n(), each = daily.eggs)) %>% 
        mutate(daily.egg.rep = 1:n())
    })) %>% 
      mutate(spawn.peak.date = mean(spawn.date),
             spawn.peak.yday = yday(spawn.peak.date),
             spawn.peak.yday = ifelse(spawn.peak.yday < 100, spawn.peak.yday+365, spawn.peak.yday),
             hatch.peak.date = mean(hatch.date),
             hatch.peak.yday = yday(hatch.peak.date),
             hatch.length_days = length(unique(hatch.yday))) %>% 
      select(1:3, spawn.peak.date, spawn.peak.yday, 4:7, hatch.peak.date, hatch.peak.yday, 8, hatch.length_days, 9:12)
  }))
})) %>% mutate(decade = factor(year(floor_date(hatch.date, years(10)))),
               spawn.yday.plot = as.Date(spawn.yday, origin = "1970-01-01"),
               spawn.peak.yday.plot = as.Date(spawn.peak.yday, origin = "1970-01-01"),
               hatch.yday.plot = as.Date(hatch.yday, origin = "1970-01-01"))


#### VISUALIZATIONS ------------------------------------------------------------------------------

ggplot(simulation.model.hatch, aes(x = spawn.peak.yday.plot, y = hatch.yday.plot)) +
  geom_tile(aes(fill = decade), color = "gray70") +
  scale_x_date(date_breaks = "2 weeks", date_labels =  "%b %d", expand = c(0, 5)) + 
  scale_y_date(date_breaks = "2 weeks", date_labels =  "%b %d", expand = c(0, 5)) + 
  #scale_x_continuous(limits = c(325, 370), breaks = seq(325, 370, 5), expand = c(0, 0.2)) +
  #scale_y_continuous(limits = c(95, 135), breaks = seq(95, 135, 5), expand = c(0, 0.2)) +
  scale_fill_manual(values = c("#ffffcc", "#ffeda0", "#fed976", "#feb24c", 
                               "#fd8d3c", "#fc4e2a", "#e31a1c", "#bd0026", "#800026")) +
  labs(x = "Mean Spawn Date", y = "Hatch Date") +
  theme_few() +
  theme(axis.title.x = element_text(color = "Black", size = 15, margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(color = "Black", size = 15, margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks.length = unit(2, 'mm'),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.key.size = unit(1.0, 'cm'),
        strip.text = element_text(size = 10),
        panel.spacing = unit(1.5, "lines")) +
  facet_wrap(~scenario)

ggsave("figures/lake-superior-apostle-islands/lake-superior-apostle-islands-simulation-heatmap.png", width = 14, height = 7, dpi = 300)



ggplot(simulation.model.hatch, aes(x = spawn.yday, y = factor(year.class))) +
  geom_density_ridges2(aes(fill = scenario), color = "black", stat = "binline", binwidth = 1, scale = 0.95, alpha = 0.5) +
  #geom_line(data = test2, aes(x = spawn.peak.yday, y = factor(year.class), group = 2)) +
  #geom_point(data = test2, aes(x = spawn.peak.yday, y = factor(year.class)), group = 2, color = "black", shape = 21) +
  coord_flip() +
  theme_bw() +
  facet_wrap(~scenario, ncol = 1)



simulation.model.hatch.spawnPlot <- simulation.model.hatch %>% group_by(scenario) %>% distinct(spawn.date, .keep_all = TRUE)

ggplot(simulation.model.hatch.spawnPlot, aes(x = year.class, y = spawn.yday.plot)) +
  geom_tile(aes(fill = as.numeric(dpf)), color = "gray70") +
  scale_x_continuous(limits = c(2010, 2100), breaks = seq(2010, 2100, 5), expand = c(0, 0.2)) +
  scale_y_date(limits = c(as.Date("1970-11-17"), as.Date("1971-01-14")),
               date_breaks = "7 days", date_labels =  "%b %d", expand = c(0, 0)) + 
  #scale_y_continuous(limits = c(320, 380), breaks = seq(320, 380, 10), expand = c(0, 0)) +
  scale_fill_gradient2(limits = c(110, 150), breaks = seq(110, 150, 10),
                       low = "#d7191c", mid = "#ffffcc", high = "#2c7bb6", midpoint = 130) +
  labs(x = "Year", y = "Spawning Date", fill = "Incubation\nLength\n(# of Days)\n") +
  guides(fill = guide_colourbar(ticks.colour = "black", ticks.linewidth = 0.75,
                                frame.colour = "black", frame.linewidth = 1.5)) +
  theme_few() +
  theme(axis.title.x = element_text(color = "Black", size = 18, margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(color = "Black", size = 18, margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.ticks.length = unit(2, 'mm'),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.key.width = unit(1.0, 'cm'),
        legend.key.height = unit(2.0, 'cm'),
        strip.text = element_text(size = 12),
        panel.spacing = unit(1.5, "lines"),
        panel.border = element_rect(size = 1.5),
        plot.margin = unit(c(2, 2, 2, 5), 'mm')) +
  facet_wrap(~scenario)

ggsave("figures/lake-superior-apostle-islands/lake-superior-apostle-islands-simulation-heatmap2.png", width = 18, height = 7, dpi = 300)

