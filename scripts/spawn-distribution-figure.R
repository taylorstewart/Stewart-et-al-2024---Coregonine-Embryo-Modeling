#### CLEAR THE ENVIRONMENT FIRST ---------------------------------------------

rm(list = ls(all.names = TRUE))


#### LOAD PACKAGES -----------------------------------------------------------

library(tidyverse)
library(data.table)
library(readxl)
library(ggthemes)
library(lubridate)
library(gridExtra)
library(grid)

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
           spawn.end.date = as.Date(ifelse(spawn.length_days > 30, spawn.start.date+30, spawn.end.date), origin = "1970-01-01"),
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
           daily.eggs = daily.spawner.abundance * 500)
  }))

test <- simulation.model.hatch %>% select(scenario, year.class, date, mean.temp.c, temp.diff_c, daily.spawner.abundance, daily.eggs) %>% 
  filter(scenario == "RCP 2.6", year.class == 2099) %>% 
  group_by(year.class) %>% 
  mutate(day = 1:n())


plot.temp <- ggplot(test, aes(x = day, y = mean.temp.c)) +
  geom_line() +
  scale_x_continuous(limits = c(0, 14), breaks = seq(0, 14, 2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(2.95, 4.3), breaks = seq(3.0, 4.25, 0.25), expand = c(0, 0.0005)) +
  labs(y = "Temperature (°C)") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 20, margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.ticks.length = unit(1.5, 'mm'),
        legend.title = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(2, "lines"),
        plot.margin = unit(c(5, 5, 5, 5), 'mm')) +
  facet_wrap(~year.class)

plot.temp.diff <- ggplot(test, aes(x = day, y = temp.diff_c)) +
  geom_bar(stat = "identity", fill = "gray70", color = "black") +
  scale_x_continuous(limits = c(0, 14), breaks = seq(0, 14, 2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, 0.05), expand = c(0, 0.0005)) +
  labs(y = "Temperature Change (°C)") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 20, margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.ticks.length = unit(1.5, 'mm'),
        legend.title = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(2, "lines"),
        plot.margin = unit(c(5, 5, 5, 5), 'mm')) +
  facet_wrap(~year.class)

plot.spawn <- ggplot(test, aes(x = day, y = daily.spawner.abundance)) +
  geom_bar(stat = "identity", fill = "gray70", color = "black") +
  scale_x_continuous(limits = c(0, 14), breaks = seq(0, 14, 2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 140), breaks = seq(0, 140, 20), expand = c(0, 0.0005)) +
  labs(y = "Spawner Abundance") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 20, margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.ticks.length = unit(1.5, 'mm'),
        legend.title = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(2, "lines"),
        plot.margin = unit(c(5, 5, 5, 5), 'mm')) +
  facet_wrap(~year.class)


plot.all <- grid.arrange(
  arrangeGrob(plot.temp,
              plot.temp.diff,
              plot.spawn,
              ncol = 1),
  arrangeGrob(textGrob("Day of Spawning Period", x = 0.55, just = "bottom", gp = gpar(cex = 1.5, fontfamily = "Arial"))),
  heights = c(1, 0.03)
)

ggsave("figures/spawn-distribution.tiff", plot = plot.all, width = 6, height = 11, dpi = 300)

  
