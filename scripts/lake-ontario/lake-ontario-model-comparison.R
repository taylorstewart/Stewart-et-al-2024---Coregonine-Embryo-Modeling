#### CLEAR THE ENVIRONMENT FIRST ---------------------------------------------

rm(list = ls(all.names = TRUE))


#### LOAD PACKAGES -----------------------------------------------------------

library(ggplot2)
library(lubridate)
library(ggthemes)
library(dplyr)
library(readxl)
library(gridExtra)
library(grid)
library(cowplot)


#### LOAD TEMPERATURE DATA -----------------------------------------------------------------------

temp <- read_excel("data/lake-ontario/lake-ontario-temp-embayments-bottom.xlsx", sheet = "chaumont-bay") %>% 
  group_by(date, year) %>% 
  summarize(temp.c = mean(temp.c)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(month = month(date),
         day = day(date),
         yday = yday(date)) %>% 
  filter(month != 6| day < 2)

## 
ggplot(temp, aes(x = date, y = temp.c)) + 
  geom_line() + theme_few() + 
  ylab('Water Temperature (°C)') + 
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %d") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  + 
  facet_wrap(~year, scales = "free_x")


#### CALCULATE MEAN SPAWNING DATE ----------------------------------------------------------------

mu.spawn <- read_excel("data/lake-ontario/lake-ontario-spawning.xlsx", sheet = "lake-ontario-spawning") %>% 
  filter(date > "2006-11-10", prop.ripe > 0) %>% 
  group_by(year) %>% 
  summarize(mu.spawn.date = as.Date(weighted.mean(date, prop.ripe), format = "%Y-%m-%d")) %>% 
  mutate(mu.spawn.yday = yday(mu.spawn.date)) %>% 
  summarize(mu.spawn = mean(mu.spawn.yday)) %>% pull()
  

#### CALCULATE MEAN HATCHING DATE ----------------------------------------------------------------

mu.hatch <- read_excel("data/lake-ontario/lake-ontario-hatching.xlsx", sheet = "lake-ontario-hatching") %>%
  group_by(year) %>% 
  summarize(mu.hatch.date = as.Date(weighted.mean(date, mean.density), format = "%Y-%m-%d")) %>% 
  mutate(mu.hatch.yday = yday(mu.hatch.date)) %>% 
  summarize(mu.hatch = mean(mu.hatch.yday)) %>% pull()


## Filter temp profiles by start and end dates

temp.spawn <- temp %>% filter(yday >= mu.spawn)
temp.hatch <- temp %>% filter(yday <= mu.hatch)
temp.inc <- bind_rows(temp.spawn, temp.hatch) 

temp.ADD <- temp.inc %>% group_by(year) %>% 
  mutate(ADD = cumsum(temp.c)) %>% 
  filter(ADD == max(ADD)) %>% 
  select(date, year, temp.c, ADD)


#### XXXXXXXXX -----------------------
## Polynomial: y = a + bx + cx^2
## Semilog: log10(y) = log10(a) + log10(b)x + log10(c)x^2
## Antilog: 10^(log(y))

## Colby and Brooke
model.CB <- read_excel("data/model-structural-parameters.xlsx", sheet = "coefs") %>% 
  filter(lake == "Pickeral Lake")

## Take antilog from daily semilog output, accumulate across days
model.CB.perc <- temp %>%  
  filter(date >= as.Date(mu.spawn, origin = paste0(year-2, "-12-31"))) %>% 
  mutate(perc.day = (10^(model.CB$a + model.CB$b * temp.c + model.CB$c * temp.c^2))*100,
         perc.cum = cumsum(perc.day)) %>% 
  filter(perc.cum <= 100) %>%
  mutate(ADD = cumsum(temp.c))

model.CB.perc.max <- model.CB.perc %>% group_by(year) %>% 
  filter(perc.cum == max(perc.cum)) %>% 
  select(date, year, temp.c, ADD) %>% 
  mutate(model = "CB")


## Stewart et al. 2021
model.ontario <- read_excel("data/model-structural-parameters.xlsx", sheet = "coefs") %>% 
  filter(lake == "Lake Ontario")

## Take antilog from daily semilog output, accumulate across days
model.ontario.perc <- temp %>% 
  filter(date >= as.Date(mu.spawn, origin = paste0(year-2, "-12-31"))) %>% 
  mutate(perc.day = (10^(model.ontario$a + model.ontario$b * temp.c + model.ontario$c * temp.c^2))*100,
         perc.cum = cumsum(perc.day),) %>% 
  filter(perc.cum <= 100) %>%
  mutate(ADD = cumsum(temp.c))

model.ontario.perc.max <- model.ontario.perc %>% group_by(year) %>% 
  filter(perc.cum == max(perc.cum)) %>% 
  select(date, year, temp.c, ADD) %>% 
  mutate(model = "ST")


#### COMBINE ALL MODEL HATCHING ESTIMATES --------------------------------------------------------

model.hatching.all <- temp.ADD %>% 
  mutate(model = "EP") %>% 
  bind_rows(., model.CB.perc.max, model.ontario.perc.max) %>% 
  mutate(model = factor(model, ordered = TRUE, levels = c("EP", "ST", "CB")),
         jday = yday(date))


#### VISUALIZATIONS ------------------------------------------------------------------------------xs

ggplot(temp, aes(x = date, y = temp.c)) + 
  geom_line(size = 0.8) +
  geom_vline(data = temp.inc[1,1], aes(xintercept = date), color = "gray25", linetype = "dashed", show.legend = FALSE) +
  geom_point(data = model.hatching.all, aes(x = as.POSIXct(date), y = temp.c, fill = model, shape = model), size = 3) +
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

ggsave("figures/lake-ontario-model-comparison.png", height = 6.5, width = 14, dpi = 300)


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

ggsave("figures/lake-ontario-add-yday.png", plot = plot.all, width = 8, height = 5, dpi = 300)


