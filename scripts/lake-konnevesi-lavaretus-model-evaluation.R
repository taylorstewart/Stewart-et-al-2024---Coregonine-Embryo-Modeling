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

temp.1 <- read_excel("data/lake-konnevesi-lavaretus/lake-konnevesi-temperature.xlsx", sheet = "2019") %>% filter(depth_m == 4.5, !is.na(temp_c))
temp.2 <- read_excel("data/lake-konnevesi-lavaretus/lake-konnevesi-temperature.xlsx", sheet = "2020") %>% filter(depth_m == 4.5, !is.na(temp_c))
temp.3 <- read_excel("data/lake-konnevesi-lavaretus/lake-konnevesi-temperature.xlsx", sheet = "2021") %>% filter(depth_m == 4.5, !is.na(temp_c))

temp.all <- bind_rows(temp.1, temp.2, temp.3) %>% 
  mutate(yday = yday(date))
rm(temp.1, temp.2, temp.3)

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

spawn.start <- read_excel("data/lake-konnevesi-lavaretus/lake-konnevesi-spawning-lavaretus.xlsx", sheet = "lake-konnevesi-spawning") %>%
  group_by(year) %>% 
  filter(row_number() == 1) %>% 
  select(year, start.date = date)
spawn.end <- read_excel("data/lake-konnevesi-lavaretus/lake-konnevesi-spawning-lavaretus.xlsx", sheet = "lake-konnevesi-spawning") %>%
  group_by(year) %>% 
  filter(row_number() == n()) %>% 
  select(year, end.date = date)
spawn <- left_join(spawn.start, spawn.end)


## Combine start and end dates; Filter to each day in spawning period
mu.spawn <- temp.all.ma %>% 
  left_join(spawn) %>% 
  group_by(year) %>% 
  filter(date >= start.date, date <= end.date) %>% 
  summarize(mu.spawn.date = mean(date))


#### CALCULATE MEAN HATCHING DATE ----------------------------------------------------------------

mu.hatch <- read_excel("data/lake-konnevesi-lavaretus/lake-konnevesi-hatching-lavaretus.xlsx", sheet = "lake-konnevesi-hatching") %>% 
  group_by(year) %>% 
  summarize(mu.hatch.date = as.Date(mean(date), format = "%Y-%m-%d"))


#### FILTER TEMP PROFILES BY SPAWNING AND HATCHING DATES -----------------------------------------

temp.inc <- temp.all.ma %>% left_join(mu.spawn) %>%
  left_join(mu.hatch) %>% 
  group_by(year) %>% 
  filter(date >= mu.spawn.date, date <= mu.hatch.date) %>% 
  select(-mu.spawn.date, -mu.hatch.date)

## Find ADD at hatch
temp.ADD <- temp.inc %>% group_by(year) %>% 
  mutate(ADD = cumsum(temp.ma_c)) %>% 
  filter(ADD == max(ADD)) %>% 
  select(date, year, temp.ma_c, ADD)


#### EUROPEAN WHITEFISH MODELS -------------------------------------------------------------------

## Polynomial: y = a + bx + cx^2
## Semilog: log10(y) = log10(a) + log10(b)x + log10(c)x^2
## Antilog: 10^(log(y))

## Eckmann, 1987
model.EK <- read_excel("data/model-structural-parameters.xlsx", sheet = "coefficients") %>% 
  filter(lake == "Lake Constance", species == "lavaretus macrophthalmus")

## Take antilog from daily semilog output, accumulate across days
model.EK.perc <- temp.all.ma %>% left_join(mu.spawn) %>% 
  group_by(year) %>% 
  filter(date >= mu.spawn.date) %>% 
  mutate(perc.day = (10^(model.EK$a + model.EK$b * temp.ma_c + model.EK$c * temp.ma_c^2))*100,
         perc.cum = cumsum(perc.day)) %>% 
  filter(perc.cum <= 100) %>%
  mutate(ADD = cumsum(temp.ma_c))

model.EK.perc.max <- model.EK.perc %>% group_by(year) %>% 
  filter(perc.cum == max(perc.cum)) %>% 
  select(date, year, temp.ma_c, ADD) %>% 
  mutate(model = "EK")


## Stewart et al. 2021
model.konnevesi <- read_excel("data/model-structural-parameters.xlsx", sheet = "coefficients") %>% 
  filter(lake == "Lake Southern Konnevesi", species == "lavaretus")

## Take antilog from daily semilog output, accumulate across days
model.ST.perc <- temp.all.ma %>% left_join(mu.spawn) %>% 
  group_by(year) %>% 
  filter(date >= mu.spawn.date) %>% 
  mutate(perc.day = (10^(model.konnevesi$a + model.konnevesi$b * temp.ma_c + model.konnevesi$c * temp.ma_c^2))*100,
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
  bind_rows(., model.ST.perc.max, model.EK.perc.max) %>% 
  mutate(model = factor(model, ordered = TRUE, levels = c("EP", "ST", "EK")),
         yday = yday(date))

model.hatching.all.comp <- model.hatching.all %>% 
  group_by(model) %>% 
  summarize(mean.ADD = mean(ADD)) %>% 
  select(model, mean.ADD) %>% 
  pivot_wider(names_from = model, values_from = mean.ADD) %>% 
  mutate(diff = EP-ST)



temp.spawn.obs <- read_excel("data/lake-konnevesi-lavaretus/lake-konnevesi-spawning-lavaretus.xlsx", sheet = "lake-konnevesi-spawning") %>% 
  group_by(year) %>% 
  filter(row_number() == 1 | row_number() == n()) %>% 
  mutate(hatch.date = rep(c("start.spawn", "end.spawn"), each = 1)) %>% 
  select(-month, -day) %>% 
  pivot_wider(names_from = hatch.date, values_from = date) %>% 
  left_join(temp.all.ma) %>% 
  filter(date >= start.spawn, date <= end.spawn)

temp.hatch.obs <- read_excel("data/lake-konnevesi-lavaretus/lake-konnevesi-hatching-lavaretus.xlsx", sheet = "lake-konnevesi-hatching") %>% 
  group_by(year) %>% 
  filter(row_number() == 1 | row_number() == n()) %>% 
  mutate(hatch.date = rep(c("start.hatch", "end.hatch"), each = 1)) %>% 
  select(-month, -day) %>% 
  pivot_wider(names_from = hatch.date, values_from = date) %>% 
  left_join(temp.all.ma) %>% 
  filter(date >= start.hatch, date <= end.hatch)


#### VISUALIZATIONS ------------------------------------------------------------------------------

ggplot(temp.all.ma, aes(x = date, y = temp.ma_c)) + 
  geom_line(size = 0.8) +
  geom_line(data = temp.spawn.obs, color = "lightsalmon") +
  geom_line(data = temp.hatch.obs, color = "lightsalmon") +
  geom_point(data = filter(model.hatching.all, model != "EP"), aes(x = as.POSIXct(date), y = temp.ma_c, fill = model, shape = model), size = 3) +
  scale_shape_manual("", values = c(22, 23),
                     labels = c("Stewart et al., 2021  ", "Eckmann, 1987")) +
  scale_fill_manual("", values = c("cornflowerblue", "forestgreen"),
                    labels = c("Stewart et al., 2021  ", "Eckmann, 1987")) +
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

ggsave("figures/lake-konnevesi/lake-konnevesi-lavaretus-model-comparison.png", height = 5.5, width = 9.5, dpi = 300)


plot.add <- ggplot(model.hatching.all, aes(x = factor(year), y = ADD, group = model)) +
  geom_point(aes(fill = model, shape = model), size = 3) + 
  scale_shape_manual("", values = c(21, 22, 23),
                     labels = c("Empirical Hatching  ", "Stewart et al., 2021  ", "Eckmann, 1987")) +
  scale_fill_manual("", values = c("lightsalmon", "cornflowerblue", "forestgreen"),
                    labels = c("Empirical Hatching  ", "Stewart et al., 2021  ", "Eckmann, 1987")) +
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
                     labels = c("Empirical Hatching  ", "Stewart et al., 2021  ", "Eckmann, 1987")) +
  scale_fill_manual("", values = c("lightsalmon", "cornflowerblue", "forestgreen"),
                    labels = c("Empirical Hatching  ", "Stewart et al., 2021  ", "Eckmann, 1987")) +
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

#ggsave("figures/lake-konnevesi/lake-konnevesi-model-comparison-point.png", plot = plot.all, width = 8, height = 5, dpi = 300)

