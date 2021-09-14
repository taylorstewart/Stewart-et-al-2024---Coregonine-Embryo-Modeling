#### CLEAR THE ENVIRONMENT FIRST ---------------------------------------------

rm(list = ls(all.names = TRUE))


#### LOAD PACKAGES -----------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(data.table)
library(ggthemes)
library(readxl)
library(gridExtra)
library(grid)
library(cowplot)


#### LOAD TEMPERATURE DATA -----------------------------------------------------------------------

temp.all <- read_excel("data/lake-geneva/lake-geneva-temperature.xlsx", sheet = "temp", skip = 28) %>% 
  mutate(yday = yday(date)) %>% 
  filter(year >= 2010)

## Calculate a 5-day center moving average to smooth temperature curve
## Smoothing prevents issues below trying to find the start and stop from large daily temp deviations
temp.all.ma <- temp.all %>% group_by(year) %>% 
  mutate(temp.ma_c = frollmean(temp_c, n = 5, align = "center")) %>% 
  filter(!is.na(temp.ma_c))

## 
ggplot(temp.all.ma, aes(x = date, y = temp.ma_c)) + 
  geom_line() + theme_few() + 
  ylab('Water Temperature (°C)') + 
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %d") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  + 
  facet_wrap(~year, scales = "free_x")


#### CALCULATE MEAN SPAWNING DATE ----------------------------------------------------------------

model.locations <- read_excel("data/model-population-parameters.xlsx", sheet = "bio-parameters", skip = 34) %>% 
  filter(lake == "Lake Geneva")


#### CALCULATE MEAN SPAWNING DATE ----------------------------------------------------------------

## Calculate the start date of spawning period
spawn.start.date <- temp.all.ma %>% 
  group_by(year) %>% 
  filter(temp.ma_c <= model.locations$start.spawn.temp_c) %>% 
  arrange(date) %>% 
  slice(1) %>% select(year, spawn.start.date = date)

## Calculate the end date of spawning period
spawn.end.date <- temp.all.ma %>% 
  group_by(year) %>% 
  filter(temp.ma_c <= model.locations$end.spawn.temp_c) %>% 
  arrange(date) %>% 
  slice(1) %>% 
  select(year, spawn.end.date = date) %>% 
  ## subtract one day to correct for temp less than (spawning ends day of temp threshold)
  mutate(spawn.end.date = as.Date(spawn.end.date)-1)

## Combine start and end dates; Filter to each day in spawning period
spawn.period.temp <- temp.all.ma %>% 
  left_join(spawn.start.date) %>% 
  left_join(spawn.end.date) %>% 
  mutate(spawn.length_days = as.Date(spawn.end.date) - as.Date(spawn.start.date),
         spawn.end.date = as.Date(ifelse(spawn.length_days > 30, as.Date(spawn.start.date)+30, spawn.end.date), origin = "1970-01-01"),
         spawn.length_days = as.Date(spawn.end.date) - as.Date(spawn.start.date)) %>%
  group_by(year) %>% 
  filter(date >= spawn.start.date, date <= spawn.end.date)

mu.spawn <- spawn.period.temp %>% 
  group_by(year) %>% 
  summarize(mu.spawn.date = mean(date)) %>% 
  mutate(mu.spawn.yday = yday(mu.spawn.date))


#### CALCULATE MEAN HATCHING DATE ----------------------------------------------------------------

mu.hatch <- read_excel("data/lake-geneva/lake-geneva-hatching.xlsx", sheet = "lake-geneva-hatching", skip = 27) %>%
  group_by(year) %>% 
  summarize(mu.hatch.date = as.Date(weighted.mean(date, abundance), format = "%Y-%m-%d")) %>% 
  mutate(mu.hatch.yday = yday(mu.hatch.date)) %>% 
  summarize(mu.hatch = as.integer(mean(mu.hatch.yday))) %>% pull()


## Filter temp profiles by start and end dates, calculate daily degree-days

temp.inc <- temp.all.ma %>% left_join(mu.spawn) %>%
  group_by(year) %>% 
  filter(date >= mu.spawn.date, yday <= mu.hatch) %>% 
  select(-mu.spawn.date, -mu.spawn.yday)


## Find ADD at hatch
temp.ADD <- temp.inc %>% group_by(year) %>% 
  mutate(ADD = cumsum(temp.ma_c)) %>% 
  filter(ADD == max(ADD)) %>% 
  select(date, year, temp.ma_c, ADD)


#### EUROPEAN WHITEFISH MODELS -------------------------------------------------------------------

## Polynomial: y = a + bx + cx^2
## Semilog: log10(y) = log10(a) + log10(b)x + log10(c)x^2
## Antilog: 10^(log(y))

## Eckmann 1987 (uses natural log)
model.EC <- read_excel("data/model-structural-parameters.xlsx", sheet = "coefficients", skip = 32) %>% 
  filter(lake == "Lake Constance", morph == "littoral", source == "Eckmann (1987)")

## Take antilog from daily semilog output, accumulate across days
model.EC.perc <- temp.all.ma %>% left_join(mu.spawn) %>% 
  group_by(year) %>% 
  filter(date >= mu.spawn.date) %>% 
  mutate(perc.day = (10^(model.EC$a + model.EC$b * temp.ma_c + model.EC$c * temp.ma_c^2))*100,
         perc.cum = cumsum(perc.day)) %>% 
  filter(perc.cum <= 100) %>%
  mutate(ADD = cumsum(temp.ma_c))

model.EC.perc.max <- model.EC.perc %>% group_by(year) %>% 
  filter(perc.cum == max(perc.cum)) %>% 
  select(date, year, temp.ma_c, ADD) %>% 
  mutate(model = "EC")


## Stewart et al. 2021
model.geneva <- read_excel("data/model-structural-parameters.xlsx", sheet = "coefficients", skip = 32) %>% 
  filter(lake == "Lake Geneva")

## Take antilog from daily semilog output, accumulate across days
model.geneva.perc <- temp.all.ma %>%left_join(mu.spawn) %>% 
  group_by(year) %>% 
  filter(date >= mu.spawn.date) %>% 
  mutate(perc.day = (10^(model.geneva$a + model.geneva$b * temp.ma_c))*100,
         perc.cum = cumsum(perc.day),) %>% 
  filter(perc.cum <= 100) %>%
  mutate(ADD = cumsum(temp.ma_c))

model.geneva.perc.max <- model.geneva.perc %>% group_by(year) %>% 
  filter(perc.cum == max(perc.cum)) %>% 
  select(date, year, temp.ma_c, ADD) %>% 
  mutate(model = "GV")


#### COMBINE ALL MODEL HATCHING ESTIMATES --------------------------------------------------------

model.hatching.all <- temp.ADD %>% 
  mutate(model = "EP") %>% 
  bind_rows(., model.EC.perc.max, model.geneva.perc.max) %>% 
  mutate(model = factor(model, ordered = TRUE, levels = c("EP", "GV", "EC")),
         yday = yday(date))

model.hatching.all.comp <- model.hatching.all %>% 
  group_by(model) %>% 
  summarize(mean.ADD = mean(ADD)) %>% 
  select(model, mean.ADD) %>% 
  pivot_wider(names_from = model, values_from = mean.ADD) %>% 
  mutate(diff = EP-GV)


#### VISUALIZATIONS ------------------------------------------------------------------------------

ggplot(temp.all.ma, aes(x = date, y = temp.ma_c)) + 
  geom_line(size = 0.8) +
  geom_vline(data = data.frame(year = unique(temp.all.ma$year), 
                               date = temp.inc %>% group_by(year) %>% slice(1) %>% pull(date)),
             aes(xintercept = date), color = "gray25", linetype = "dashed", show.legend = FALSE) +
  geom_point(data = model.hatching.all, aes(x = as.POSIXct(date), y = temp.ma_c, fill = model, shape = model), size = 3) +
  scale_shape_manual("", values = c(21, 22, 23),
                     labels = c("Empirical Hatching  ", "Unpublished Thonon Data  ", "Eckmann, 1987")) +
  scale_fill_manual("", values = c("lightsalmon", "cornflowerblue", "forestgreen"),
                    labels = c("Empirical Hatching  ", "Unpublished Thonon Data  ", "Eckmann, 1987")) +
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %d") + 
  labs(y = "Water Temperature (°C)") + 
  theme_few() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        legend.position = "top", 
        legend.text = element_text(size = 15),
        strip.text = element_text(size = 12),
        panel.spacing = unit(1, "lines")) + 
  facet_wrap(~year, scales = "free_x")

ggsave("figures/lake-geneva/lake-geneva-model-comparison-temp.png", height = 5.5, width = 9.5, dpi = 300)


plot.add <- ggplot(model.hatching.all, aes(x = factor(year), y = ADD, group = model)) +
  geom_point(aes(fill = model, shape = model), size = 3) + 
  scale_shape_manual("", values = c(21, 22, 23),
                     labels = c("Empirical Hatching  ", "Unpublished Thonon Data  ", "Eckmann, 1987")) +
  scale_fill_manual("", values = c("lightsalmon", "cornflowerblue", "forestgreen"),
                    labels = c("Empirical Hatching  ", "Unpublished Thonon Data  ", "Eckmann, 1987")) +
  labs(y = "Degree-days") +
  theme_few() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        legend.position = "top", 
        legend.text = element_text(size = 15),
        strip.text = element_text(size = 12),
        panel.spacing = unit(1, "lines"))

plot.date <- ggplot(model.hatching.all, aes(x = factor(year), y = yday, group = model)) +
  geom_point(aes(fill = model, shape = model), size = 3) + 
  scale_shape_manual("", values = c(21, 22, 23),
                     labels = c("Empirical Hatching  ", "Unpublished Thonon Data  ", "Eckmann, 1987")) +
  scale_fill_manual("", values = c("lightsalmon", "cornflowerblue", "forestgreen"),
                    labels = c("Empirical Hatching  ", "Unpublished Thonon Data  ", "Eckmann, 1987")) +
  labs(y = "Julian Date") +
  theme_few() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        legend.position = "top", 
        legend.text = element_text(size = 15),
        strip.text = element_text(size = 12),
        panel.spacing = unit(1, "lines"))

## Combine all figures
plot.all <- grid.arrange(
  arrangeGrob(get_legend(plot.add),
              nrow = 1,
              ncol = 1),
  arrangeGrob(
    arrangeGrob(plot.add + theme(legend.position = "none", axis.title.x = element_blank()),
                nrow = 1),
    arrangeGrob(plot.date + theme(legend.position = "none", axis.title.x = element_blank()), 
                nrow = 1),
    ncol = 2,
    widths = c(1, 1)
  ),
  heights = c(0.05, 1.0)
)

ggsave("figures/lake-geneva/lake-geneva-model-comparison-point.png", plot = plot.all, width = 8, height = 5, dpi = 300)

