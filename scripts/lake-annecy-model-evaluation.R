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

temp.1 <- read_excel("data/lake-annecy/lake-annecy-temperature.xlsx", sheet = "2005") %>% 
  filter(date <= "2005-05-01")
temp.2 <- read_excel("data/lake-annecy/lake-annecy-temperature.xlsx", sheet = "2006") %>% 
  filter(date >= "2005-12-15", date <= "2006-05-01")

temp.all <- bind_rows(temp.1, temp.2) %>% 
  mutate(yday = yday(date))
rm(temp.1, temp.2)

## 
ggplot(temp.all, aes(x = date, y = temp.c)) + 
  geom_line() + theme_few() + 
  ylab('Water Temperature (°C)') + 
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %d") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  + 
  facet_wrap(~year, scales = "free_x")


#### CALCULATE MEAN SPAWNING DATE ----------------------------------------------------------------

mu.spawn <- data.frame(year = unique(temp.all$year),
                       mu.spawn.yday = rep(yday(as.Date("2018-12-27")), length(unique(temp.all$year)))) %>% 
  mutate(mu.spawn.date = as.Date(mu.spawn.yday, origin = paste0(year-2, "-12-31")))
#mu.spawn <- read_excel("data/lake-annecy/lake-annecy-spawning.xlsx", sheet = "lake-annecy-spawning") %>% 
#  filter(spawn.abundance > 0) %>% 
#  group_by(year) %>% 
#  summarize(mu.spawn.date = as.Date(weighted.mean(date, spawn.abundance), format = "%Y-%m-%d"),
#            mu.spawn.temp = weighted.mean(temp.c, spawn.abundance, na.rm = TRUE)) %>% 
#  mutate(mu.spawn.yday = yday(mu.spawn.date),
#         mu.spawn.yday = ifelse(mu.spawn.yday < 30, mu.spawn.yday+365, mu.spawn.yday)) %>% 
#  ungroup() %>% 
#  summarize(mu.spawn = mean(mu.spawn.yday)) %>% 
#  mutate(mu.spawn = ifelse(mu.spawn > 365, mu.spawn-365, mu.spawn)) %>% 
#  pull()


#### CALCULATE MEAN HATCHING DATE ----------------------------------------------------------------

mu.hatch <- read_excel("data/lake-annecy/lake-annecy-hatching.xlsx", sheet = "lake-annecy-hatching") %>% 
  group_by(year) %>% 
  summarize(mu.hatch.date = as.Date(weighted.mean(date, hatch.abundance), format = "%Y-%m-%d")) %>% 
  mutate(mu.hatch.yday = yday(mu.hatch.date))

## Filter temp profiles by start and end dates, calculate daily degree-days
temp.spawn <- temp.all %>% left_join(mu.spawn) %>%
  group_by(year) %>% 
  filter(yday >= mu.spawn.yday) %>% 
  select(-mu.spawn.yday, -mu.spawn.date)
temp.hatch <- temp.all %>% left_join(mu.hatch) %>% 
  group_by(year) %>% 
  filter(yday <= mu.hatch.yday) %>% 
  select(-mu.hatch.yday, -mu.hatch.date)
temp.inc <- bind_rows(temp.spawn, temp.hatch) %>% arrange(date)

## Find ADD at hatch
temp.ADD <- temp.inc %>% group_by(year) %>% 
  mutate(ADD = cumsum(temp.c)) %>% 
  filter(ADD == max(ADD)) %>% 
  select(date, year, temp.c, ADD)


#### EUROPEAN WHITEFISH MODELS -------------------------------------------------------------------

## Polynomial: y = a + bx + cx^2
## Semilog: log10(y) = log10(a) + log10(b)x + log10(c)x^2
## Antilog: 10^(log(y))

## Eckmann 1987 (uses natural log)
model.EC <- read_excel("data/model-structural-parameters.xlsx", sheet = "coefs") %>% 
  filter(lake == "Lake Constance", morph == "littoral", source == "Eckmann (1987)")

## Take antilog from daily semilog output, accumulate across days
model.EC.perc <- temp.all %>% left_join(mu.spawn) %>% 
  group_by(year) %>% 
  filter(date >= mu.spawn.date) %>% 
  mutate(perc.day = (10^(model.EC$a + model.EC$b * temp.c + model.EC$c * temp.c^2))*100,
         perc.cum = cumsum(perc.day),) %>% 
  filter(perc.cum <= 100) %>%
  mutate(ADD = cumsum(temp.c))

model.EC.perc.max <- model.EC.perc %>% group_by(year) %>% 
  filter(perc.cum == max(perc.cum)) %>% 
  select(date, year, temp.c, ADD) %>% 
  mutate(model = "EC")


## Stewart et al. 2021
model.data.TLB <- read_excel("/Users/taylor/SynologyDrive/Cisco-Climate-Change/Coregonine-Temp-Embryo-EuropeanWhitefish/data/Coregonine-Temperature-Experiment-EuropeFrance-Hatch.xlsx", sheet = "hatching") %>% 
  filter(population == "leman") %>% 
  mutate(eye = as.numeric(eye),
         hatch = as.numeric(hatch)) %>% 
  filter(!is.na(eye), !is.na(hatch), !is.na(dpf), hatch == 1, include.incubation == "y") %>% 
  rename(temp.c = temperature) %>%
  group_by(temp.c, dpf) %>% 
  summarize(n = n()) %>% ungroup() %>%
  arrange(temp.c, dpf) %>% 
  group_by(temp.c) %>% 
  mutate(total.n = sum(n),
         prop.n = n/total.n,
         cum.prop = cumsum(prop.n)) %>% 
  filter(abs(cum.prop - 0.5) == min(abs(cum.prop - 0.5))) %>% 
  mutate(dpf.recip = dpf^-1,
         log.dpf.recip = log10(dpf.recip))

## Fit Semilog Model
model.TLB <- lm(log.dpf.recip ~ temp.c + I(temp.c^2), data = model.data.TLB)

## Take antilog from daily semilog output, accumulate across days
model.TLB.perc <- temp.all %>% 
  left_join(mu.spawn) %>% 
  group_by(year) %>% 
  filter(date >= mu.spawn.date) %>% 
  mutate(perc.day = (10^(coef(model.TLB)[[1]] + coef(model.TLB)[[2]] * temp.c))*100,
         perc.cum = cumsum(perc.day),) %>% 
  filter(perc.cum <= 100) %>%
  mutate(ADD = cumsum(temp.c))

model.TLB.perc.max <- model.TLB.perc %>% group_by(year) %>% 
  filter(perc.cum == max(perc.cum)) %>% 
  select(date, year, temp.c, ADD) %>% 
  mutate(model = "TLB")


#### COMBINE ALL MODEL HATCHING ESTIMATES --------------------------------------------------------

model.hatching.all <- temp.ADD %>% 
  mutate(model = "EP") %>% 
  bind_rows(., model.EC.perc.max, model.TLB.perc.max) %>% 
  mutate(model = factor(model, ordered = TRUE, levels = c("EP", "TLB", "EC")),
         yday = yday(date))


#### VISUALIZATIONS ------------------------------------------------------------------------------

ggplot(temp.all, aes(x = date, y = temp.c)) + 
  geom_line(size = 0.8) +
  geom_vline(data = data.frame(year = unique(temp.all$year), 
                               date = temp.inc %>% group_by(year) %>% slice(1) %>% pull(date)),
             aes(xintercept = date), color = "gray25", linetype = "dashed", show.legend = FALSE) +
  geom_point(data = model.hatching.all, aes(x = as.POSIXct(date), y = temp.c, fill = model, shape = model), size = 3) +
  scale_shape_manual("", values = c(21, 22, 23),
                     labels = c("Observed Hatching  ", "Unpublished Thonon Data  ", "Eckmann, 1987")) +
  scale_fill_manual("", values = c("lightsalmon", "cornflowerblue", "forestgreen"),
                    labels = c("Observed Hatching  ", "Unpublished Thonon Data  ", "Eckmann, 1987")) +
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

ggsave("figures/lake-annecy/lake-annecy-model-comparison-temp.png", height = 5.5, width = 9.5, dpi = 300)


plot.add <- ggplot(model.hatching.all, aes(x = factor(year), y = ADD, group = model)) +
  geom_point(aes(fill = model, shape = model), size = 3) + 
  scale_shape_manual("", values = c(21, 22, 23),
                     labels = c("Observed Hatching  ", "Unpublished Thonon Data  ", "Eckmann, 1987")) +
  scale_fill_manual("", values = c("lightsalmon", "cornflowerblue", "forestgreen"),
                    labels = c("Observed Hatching  ", "Unpublished Thonon Data  ", "Eckmann, 1987")) +
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
                     labels = c("Observed Hatching  ", "Unpublished Thonon Data  ", "Eckmann, 1987")) +
  scale_fill_manual("", values = c("lightsalmon", "cornflowerblue", "forestgreen"),
                    labels = c("Observed Hatching  ", "Unpublished Thonon Data  ", "Eckmann, 1987")) +
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

ggsave("figures/lake-annecy/lake-annecy-model-comparison-point.png", plot = plot.all, width = 8, height = 5, dpi = 300)

