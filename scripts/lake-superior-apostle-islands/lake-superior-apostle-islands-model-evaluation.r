#### CLEAR THE ENVIRONMENT FIRST ---------------------------------------------

rm(list = ls(all.names = TRUE))


#### LOAD PACKAGES -----------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggthemes)
library(data.table)
library(readxl)
library(gridExtra)
library(grid)
library(cowplot)


#### LOAD TEMPERATURE DATA -----------------------------------------------------------------------

temp.1 <- read_excel("data/lake-superior-apostle-islands/lake-superior-apostle-islands-temperature.xlsx", sheet = "2016")
temp.2 <- read_excel("data/lake-superior-apostle-islands/lake-superior-apostle-islands-temperature.xlsx", sheet = "2017")
temp.3 <- read_excel("data/lake-superior-apostle-islands/lake-superior-apostle-islands-temperature.xlsx", sheet = "2018")

temp.all <- bind_rows(temp.1, temp.2, temp.3) %>% 
  mutate(yday = yday(date))
rm(temp.1, temp.2, temp.3)

## Calculate a 5-day center moving average to smooth temperature curve
## Smoothing prevents issues below trying to find the start and stop from large daily temp deviations
temp.all.ma <- temp.all %>% group_by(year) %>% 
  mutate(temp.ma_c = frollmean(temp.c, n = 5, align = "center")) %>% 
  filter(!is.na(temp.ma_c))

## 
ggplot(temp.all.ma, aes(x = date, y = temp.ma_c)) + 
  geom_line() + theme_few() + 
  ylab('Water Temperature (°C)') + 
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %d") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  + 
  facet_wrap(~year, scales = "free_x")


model.locations <- read_excel("data/model-population-parameters.xlsx", sheet = "bio-parameters") %>% 
  filter(population == "apostle islands")


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
         spawn.end.date = as.Date(ifelse(spawn.length_days > 30, spawn.start.date+30, spawn.end.date), origin = "1970-01-01"),
         spawn.length_days = as.Date(spawn.end.date) - as.Date(spawn.start.date)) %>%
  group_by(year) %>% 
  filter(date >= spawn.start.date, date <= spawn.end.date)

mu.spawn <- spawn.period.temp %>% 
  group_by(year) %>% 
  summarize(mu.spawn.date = mean(date)) %>% 
  mutate(mu.spawn.yday = yday(mu.spawn.date))


#### CALCULATE MEAN HATCHING DATE ----------------------------------------------------------------

mu.hatch <- read_excel("data/lake-superior-apostle-islands/lake-superior-apostle-islands-hatching.xlsx", sheet = "lake-superior-apostle-hatching") %>% 
  select(year, mu.hatch.date = mean.date) %>% 
  mutate(mu.hatch.yday = yday(mu.hatch.date))


## Filter temp profiles by start and end dates
temp.spawn <- temp.all.ma %>% left_join(mu.spawn) %>%
  group_by(year) %>% 
  filter(yday >= mu.spawn.yday) %>% 
  select(-mu.spawn.yday, -mu.spawn.date)
temp.hatch <- temp.all.ma %>% left_join(mu.hatch) %>% 
  group_by(year) %>% 
  filter(yday <= mu.hatch.yday) %>% 
  select(-mu.hatch.yday, -mu.hatch.date)
temp.inc <- bind_rows(temp.spawn, temp.hatch) %>% arrange(date)

## Find ADD at hatch
temp.ADD <- temp.inc %>% group_by(year) %>% 
  mutate(ADD = cumsum(temp.ma_c)) %>% 
  filter(ADD == max(ADD)) %>% 
  select(date, year, temp.ma_c, ADD)


#### XXXXXXXXX -----------------------
## Polynomial: y = a + bx + cx^2
## Semilog: log10(y) = log10(a) + log10(b)x + log10(c)x^2
## Antilog: 10^(log(y))

## Colby and Brooke
model.CB <- read_excel("data/model-structural-parameters.xlsx", sheet = "coefficients") %>% 
  filter(lake == "Pickeral Lake")

## Take antilog from daily semilog output, accumulate across days
model.CB.perc <- temp.all.ma %>% left_join(mu.spawn) %>% 
  group_by(year) %>% 
  filter(date >= mu.spawn.date) %>% 
  mutate(perc.day = (10^(model.CB$a + model.CB$b * temp.ma_c + model.CB$c * temp.ma_c^2))*100,
         perc.cum = cumsum(perc.day)) %>% 
  filter(perc.cum <= 100) %>%
  mutate(ADD = cumsum(temp.ma_c))

model.CB.perc.max <- model.CB.perc %>% group_by(year) %>% 
  filter(perc.cum == max(perc.cum)) %>% 
  select(date, year, temp.ma_c, ADD) %>% 
  mutate(model = "CB")


## Stewart et al. 2021
model.superior <- read_excel("data/model-structural-parameters.xlsx", sheet = "coefficients") %>% 
  filter(lake == "Lake Superior")

## Take antilog from daily semilog output, accumulate across days
model.ST.perc <- temp.all.ma %>% left_join(mu.spawn) %>% 
  group_by(year) %>% 
  filter(date >= mu.spawn.date) %>% 
  mutate(perc.day = (10^(model.superior$a + model.superior$b * temp.ma_c + model.superior$c * temp.ma_c^2))*100,
         perc.cum = cumsum(perc.day),) %>% 
  filter(perc.cum <= 100) %>%
  mutate(ADD = cumsum(temp.ma_c))

model.ST.perc.max <- model.ST.perc %>% group_by(year) %>% 
  filter(perc.cum == max(perc.cum)) %>% 
  select(date, year, temp.ma_c, ADD) %>% 
  mutate(model = "ST")


#### COMBINE ALL MODEL HATCHING ESTIMATES --------------------------------------------------------

model.hatching.all <- temp.ADD %>% 
  mutate(model = "EP") %>% 
  bind_rows(., model.ST.perc.max, model.CB.perc.max) %>% 
  mutate(model = factor(model, ordered = TRUE, levels = c("EP", "ST", "CB")),
         jday = yday(date))


#### VISUALIZATIONS ------------------------------------------------------------------------------

ggplot(temp.all.ma, aes(x = date, y = temp.ma_c)) + 
  geom_line(size = 0.8) +
  geom_vline(data = data.frame(year = unique(temp.all.ma$year), 
                               date = temp.inc %>% group_by(year) %>% slice(1) %>% pull(date)),
             aes(xintercept = date), color = "gray25", linetype = "dashed", show.legend = FALSE) +
  geom_point(data = model.hatching.all, aes(x = as.POSIXct(date), y = temp.ma_c, fill = model, shape = model), size = 3) +
  scale_shape_manual("", values = c(21, 22, 23),
                    labels = c("Observed Hatching  ", "Stewart et al., 2021  ", "Colby & Brooke, 1973")) +
  scale_fill_manual("", values = c("lightsalmon", "cornflowerblue", "forestgreen"),
                    labels = c("Observed Hatching  ", "Stewart et al., 2021  ", "Colby & Brooke, 1973")) +
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

ggsave("figures/lake-superior-apostle-islands/lake-superior-apostle-islands-model-comparison-temp.png", height = 6.5, width = 14, dpi = 300)


plot.add <- ggplot(model.hatching.all, aes(x = factor(year), y = ADD, group = model)) +
  geom_point(aes(fill = model, shape = model), size = 3) + 
  scale_shape_manual("", values = c(21, 22, 23),
                     labels = c("Observed Hatching  ", "Stewart et al., 2021  ", "Colby & Brooke, 1973")) +
  scale_fill_manual("", values = c("lightsalmon", "cornflowerblue", "forestgreen"),
                    labels = c("Observed Hatching  ", "Stewart et al., 2021  ", "Colby & Brooke, 1973")) +
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

plot.date <- ggplot(model.hatching.all, aes(x = factor(year), y = jday, group = model)) +
  geom_point(aes(fill = model, shape = model), size = 3) + 
  scale_shape_manual("", values = c(21, 22, 23),
                     labels = c("Observed Hatching  ", "Stewart et al., 2021  ", "Colby & Brooke, 1973")) +
  scale_fill_manual("", values = c("lightsalmon", "cornflowerblue", "forestgreen"),
                    labels = c("Observed Hatching  ", "Stewart et al., 2021  ", "Colby & Brooke, 1973")) +
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

ggsave("figures/lake-superior-apostle-islands/lake-superior-apostle-islands-model-comparison-point.png", plot = plot.all, width = 8, height = 5, dpi = 300)

