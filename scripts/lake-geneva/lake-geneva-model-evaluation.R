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

temp.1 <- read_excel("data/lake-geneva/lake-geneva-temperature-sonde-interpolated.xlsx", sheet = "2013")
temp.2 <- read_excel("data/lake-geneva/lake-geneva-temperature-sonde-interpolated.xlsx", sheet = "2014")
temp.3 <- read_excel("data/lake-geneva/lake-geneva-temperature-sonde-interpolated.xlsx", sheet = "2015")

temp.all <- bind_rows(temp.1, temp.2, temp.3) %>% 
  filter(depth.m == 6) %>% 
  mutate(yday = yday(date),
         date2 = as.Date(ifelse(yday > 250, paste0("2019-", month, "-", day), paste0("2020-", month, "-", day)), format = "%Y-%m-%d"))
rm(temp.1, temp.2, temp.3)

## 
ggplot(temp.all, aes(x = date2, y = temp.c)) + 
  geom_line() + theme_few() + 
  ylab('Water Temperature (°C)') + 
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %d") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  + 
  facet_wrap(~year)


#### CALCULATE MEAN SPAWNING DATE ----------------------------------------------------------------

mu.spawn <- read_excel("data/lake-geneva/lake-geneva-spawning.xlsx", sheet = "lake-geneva-spawning") %>% 
  filter(spawn.abundance > 0) %>% 
  group_by(year) %>% 
  summarize(mu.spawn.date = as.Date(weighted.mean(date, spawn.abundance), format = "%Y-%m-%d"),
            mu.spawn.temp = weighted.mean(temp.c, spawn.abundance, na.rm = TRUE)) %>% 
  mutate(mu.spawn.yday = yday(mu.spawn.date),
         mu.spawn.yday = ifelse(mu.spawn.yday < 30, mu.spawn.yday+365, mu.spawn.yday)) %>% 
  summarize(mu.spawn = mean(mu.spawn.yday)) %>% 
  mutate(mu.spawn = ifelse(mu.spawn > 365, mu.spawn-365, mu.spawn)) %>% 
  pull()


#### CALCULATE MEAN HATCHING DATE ----------------------------------------------------------------



## Filter temp profiles by start and end dates, calculate daily degree-days

if(mu.spawn > 0 & mu.spawn < 30) {
  temp.inc <- temp.all %>% group_by(year) %>% filter(yday >= mu.spawn, yday < 240)
} else {
  temp.spawn <- temp.all %>% group_by(year) %>% filter(yday >= mu.spawn)
  temp.hatch <- temp.all %>% group_by(year) %>% filter(yday <= mu.hatch)
  temp.inc <- bind_rows(temp.spawn, temp.hatch) 
}

## Find ADD at hatch
#temp.ADD <- temp.inc %>% group_by(year) %>% 
#  mutate(ADD = cumsum(temp.c)) %>% 
#  filter(ADD == max(ADD)) %>% 
#  select(date, year, temp.c, ADD)



#### EUROPEAN WHITEFISH MODELS -------------------------------------------------------------------

## Polynomial: y = a + bx + cx^2
## Semilog: log10(y) = log10(a) + log10(b)x + log10(c)x^2
## Antilog: 10^(log(y))

## Eckmann 1987 (uses natural log)
model.EC <- read_excel("data/model-structural-parameters.xlsx", sheet = "coefs") %>% 
  filter(lake == "Lake Constance", morph == "littoral", source == "Eckmann (1987)")

## Take antilog from daily semilog output, accumulate across days
model.EC.perc <- temp.all %>%
  group_by(year) %>% 
  filter(date >= as.Date(mu.spawn, origin = paste0(year, "-01-01"))) %>% 
  mutate(perc.day = (10^(model.EC$a + model.EC$b * temp.c + model.EC$c * temp.c^2))*100,
         perc.cum = cumsum(perc.day),) %>% 
  filter(perc.cum <= 100) %>%
  mutate(ADD = cumsum(temp.c))

model.EC.perc.max <- model.EC.perc %>% group_by(year) %>% 
  filter(perc.cum == max(perc.cum)) %>% 
  select(date, year, temp.c, ADD) %>% 
  mutate(model = "EC")


## Stewart et al. 2021
model.geneva <- read_excel("data/model-structural-parameters.xlsx", sheet = "coefs") %>% 
  filter(lake == "Lake Geneva")

## Take antilog from daily semilog output, accumulate across days
model.geneva.perc <- temp.all %>%
  group_by(year) %>% 
  filter(date >= as.Date(mu.spawn, origin = paste0(year, "-01-01"))) %>% 
  mutate(perc.day = (10^(model.geneva$a + model.geneva$b * temp.c))*100,
         perc.cum = cumsum(perc.day),) %>% 
  filter(perc.cum <= 100) %>%
  mutate(ADD = cumsum(temp.c))

model.geneva.perc.max <- model.geneva.perc %>% group_by(year) %>% 
  filter(perc.cum == max(perc.cum)) %>% 
  select(date, year, temp.c, ADD) %>% 
  mutate(model = "TLB")


#### COMBINE ALL MODEL HATCHING ESTIMATES --------------------------------------------------------

model.hatching.all <- bind_rows(model.EC.perc.max, model.geneva.perc.max) %>% 
  mutate(model = factor(model, ordered = TRUE, levels = c("TLB", "EC")),
         yday = yday(date))


#### VISUALIZATIONS ------------------------------------------------------------------------------

ggplot(temp.all, aes(x = date, y = temp.c)) + 
  geom_line(size = 0.8) +
  geom_vline(data = data.frame(year = unique(temp.all$year), 
                               date = temp.inc %>% group_by(year) %>% slice(1) %>% pull(date)),
             aes(xintercept = date), color = "gray25", linetype = "dashed", show.legend = FALSE) +
  geom_point(data = model.hatching.all, aes(x = as.POSIXct(date), y = temp.c, fill = model, shape = model), size = 3) +
  scale_shape_manual("", values = c(22, 23),
                     labels = c("Unpublished Thonon Data  ", "Eckmann, 1987")) +
  scale_fill_manual("", values = c("cornflowerblue", "forestgreen"),
                    labels = c("Unpublished Thonon Data  ", "Eckmann, 1987")) +
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
  scale_shape_manual("", values = c(22, 23),
                     labels = c("Unpublished Thonon Data  ", "Eckmann, 1987")) +
  scale_fill_manual("", values = c("cornflowerblue", "forestgreen"),
                    labels = c("Unpublished Thonon Data  ", "Eckmann, 1987")) +
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
  scale_shape_manual("", values = c(22, 23),
                     labels = c("Unpublished Thonon Data  ", "Eckmann, 1987")) +
  scale_fill_manual("", values = c("cornflowerblue", "forestgreen"),
                    labels = c("Unpublished Thonon Data  ", "Eckmann, 1987")) +
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

