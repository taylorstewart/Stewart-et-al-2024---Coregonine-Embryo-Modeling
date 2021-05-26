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

temp.1 <- read_excel("data/LakeKonnevesi_Temp.xlsx", sheet = "2018")
temp.2 <- read_excel("data/LakeKonnevesi_Temp.xlsx", sheet = "2019")

temp.all <- bind_rows(temp.1, temp.2) %>% 
  mutate(jday = yday(date),
         date2 = as.Date(ifelse(jday > 250, paste0("2019-", month, "-", day), paste0("2020-", month, "-", day)), format = "%Y-%m-%d"))
rm(temp.1, temp.2)

## 
ggplot(temp.all, aes(x = date2, y = temp.c.ma)) + 
  geom_line() + theme_few() + 
  ylab('Water Temperature (°C)') + 
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %d") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  + 
  facet_wrap(~year)


#### CALCULATE MEAN SPAWNING DATE ----------------------------------------------------------------

mu.spawn.albula <- temp.all %>% group_by(year) %>% 
  filter(spawning.albula == "y") %>% 
  summarize(mu.spawn = as.Date(mean(date), format = "%Y-%m-%d")) %>% 
  left_join(., temp.all, by = c("mu.spawn" = "date", "year")) %>% 
  select(year, mu.spawn, temp.spawn = temp.c.ma)
mu.spawn.lavaretus <- temp.all %>% group_by(year) %>% 
  filter(spawning.lavaretus == "y") %>% 
  summarize(mu.spawn = as.Date(mean(date), format = "%Y-%m-%d")) %>% 
  left_join(., temp.all, by = c("mu.spawn" = "date", "year")) %>% 
  select(year, mu.spawn, temp.spawn = temp.c.ma)


#### CALCULATE MEAN HATCHING DATE ----------------------------------------------------------------

mu.hatch.albula <- temp.all %>% group_by(year) %>% 
  filter(hatching.albula == "y") %>% 
  summarize(mu.hatch = as.Date(mean(date), format = "%Y-%m-%d")) %>% 
  left_join(., temp.all, by = c("mu.hatch" = "date", "year")) %>% 
  select(year, mu.hatch, temp.hatch = temp.c.ma)
mu.hatch.lavaretus <- temp.all %>% group_by(year) %>% 
  filter(hatching.lavaretus == "y") %>% 
  summarize(mu.hatch = as.Date(mean(date), format = "%Y-%m-%d")) %>% 
  left_join(., temp.all, by = c("mu.hatch" = "date", "year")) %>% 
  select(year, mu.hatch, temp.hatch = temp.c.ma)


## Filter temp profiles by start and end dates, calculate daily degree-days

temp.albula.filter <- left_join(mu.spawn.albula, mu.hatch.albula) %>% 
  left_join(temp.all, .) %>% 
  group_by(year) %>% 
  filter(date >= mu.spawn, date <= mu.hatch) %>% 
  mutate(ADD = cumsum(temp.c.ma))
temp.lavaretus.filter <- left_join(mu.spawn.lavaretus, mu.hatch.lavaretus) %>% 
  left_join(temp.all, .) %>% 
  group_by(year) %>% 
  filter(date >= mu.spawn, date <= mu.hatch) %>% 
  mutate(ADD = cumsum(temp.c.ma))

## Find ADD at hatch
temp.albula.ADD <- temp.albula.filter %>% group_by(year) %>% 
  filter(ADD == max(ADD))
temp.lavaretus.ADD <- temp.lavaretus.filter %>% group_by(year) %>% 
  filter(ADD == max(ADD))


#### VENDACE MODELS ------------------------------------------------------------------------------

## Polynomial: y = a + bx + cx^2
## Semilog: log10(y) = log10(a) + log10(b)x + log10(c)x^2
## Antilog: 10^(log(y))

## Luczynski and Kirklewska, 1984
model.data.LK <- data.frame(temp.c = c(9.9, 8.4, 6.6, 4.9, 2.9, 2.0, 1.1,1.1, 1.1),
                            dpf = c(45, 56, 77, 101, 137, 156, 176, 183, 176)) %>% 
  mutate(dpf.recip = dpf^-1,
         log.dpf.recip = log10(dpf.recip))

## Fit Semilog Model
model.LK <- lm(log.dpf.recip ~ temp.c + I(temp.c^2), data = model.data.LK)

## Take antilog from daily semilog output, accumulate across days
model.LK.perc <- temp.all %>% left_join(mu.spawn.albula) %>% 
  group_by(year) %>% 
  filter(date >= mu.spawn) %>% 
  mutate(perc.day = (10^(coef(model.LK)[[1]] + coef(model.LK)[[2]] * temp.c.ma + coef(model.LK)[[3]] * temp.c.ma^2))*100,
         perc.cum = cumsum(perc.day),) %>% 
  filter(perc.cum <= 100) %>%
  mutate(ADD = cumsum(temp.c.ma))

model.LK.perc.max <- model.LK.perc %>% group_by(year) %>% 
  filter(perc.cum == max(perc.cum)) %>% 
  select(date, year, temp.c.ma, ADD) %>% 
  mutate(model = "LK")


## Stewart et al. 2021
model.data.albula.ST <- read_excel("/Users/taylor/SynologyDrive/Cisco-Climate-Change/Coregonine-Temp-Embryo/data/Coregonine-Temperature-Experiment-FI-Hatch.xlsx", sheet = "2019HatchingData") %>% 
  mutate(eye = as.numeric(eye),
         hatch = as.numeric(hatch)) %>% 
  filter(!is.na(eye), !is.na(hatch), !is.na(dpf), hatch == 1, include.incubation == "y") %>% 
  rename(temp.c = temperature) %>% 
  mutate(dpf.recip = dpf^-1,
         log.dpf.recip = log10(dpf.recip)) %>% 
  filter(species == "albula")

## Fit Semilog Model
model.albula.ST <- lm(log.dpf.recip ~ temp.c + I(temp.c^2), data = model.data.albula.ST)

## Take antilog from daily semilog output, accumulate across days
model.albula.ST.perc <- temp.all %>% left_join(mu.spawn.albula) %>% 
  group_by(year) %>% 
  filter(date >= mu.spawn) %>% 
  mutate(perc.day = (10^(coef(model.albula.ST)[[1]] + coef(model.albula.ST)[[2]] * temp.c.ma + coef(model.albula.ST)[[3]] * temp.c.ma^2))*100,
         perc.cum = cumsum(perc.day),) %>% 
  filter(perc.cum <= 100) %>%
  mutate(ADD = cumsum(temp.c.ma))

model.albula.ST.perc.max <- model.albula.ST.perc %>% group_by(year) %>% 
  filter(perc.cum == max(perc.cum)) %>% 
  select(date, year, temp.c.ma, ADD) %>% 
  mutate(model = "ST")


#### EUROPEAN WHITEFISH MODELS -------------------------------------------------------------------

## Eckmann 1987
model.data.EC <- data.frame(temp.c = rep(c(2.5, 4, 7, 10), each = 3),
                            dpf = c(95, 95, 107, 71, 82, 84, 43, 48, 50, 29, 32, 33)) %>% 
  mutate(dpf.recip = dpf^-1,
         log.dpf.recip = log10(dpf.recip))

## Fit Semilog Model
model.EC <- lm(log.dpf.recip ~ temp.c + I(temp.c^2), data = model.data.EC)

## Take antilog from daily semilog output, accumulate across days
model.EC.perc <- temp.all %>% left_join(mu.spawn.lavaretus) %>% 
  group_by(year) %>% 
  filter(date >= mu.spawn) %>% 
  mutate(perc.day = (10^(coef(model.EC)[[1]] + coef(model.EC)[[2]] * temp.c.ma + coef(model.EC)[[3]] * temp.c.ma^2))*100,
         perc.cum = cumsum(perc.day),) %>% 
  filter(perc.cum <= 100) %>%
  mutate(ADD = cumsum(temp.c.ma))

model.EC.perc.max <- model.EC.perc %>% group_by(year) %>% 
  filter(perc.cum == max(perc.cum)) %>% 
  select(date, year, temp.c.ma, ADD) %>% 
  mutate(model = "EC")


## Stewart et al. 2021
model.data.lavaretus.ST <- read_excel("/Users/taylor/SynologyDrive/Cisco-Climate-Change/Coregonine-Temp-Embryo/data/Coregonine-Temperature-Experiment-FI-Hatch.xlsx", sheet = "2019HatchingData") %>% 
  mutate(eye = as.numeric(eye),
         hatch = as.numeric(hatch)) %>% 
  filter(!is.na(eye), !is.na(hatch), !is.na(dpf), hatch == 1, include.incubation == "y") %>% 
  rename(temp.c = temperature) %>% 
  mutate(dpf.recip = dpf^-1,
         log.dpf.recip = log10(dpf.recip)) %>% 
  filter(species == "lavaretus")

## Fit Semilog Model
model.lavaretus.ST <- lm(log.dpf.recip ~ temp.c + I(temp.c^2), data = model.data.lavaretus.ST)

## Take antilog from daily semilog output, accumulate across days
model.lavaretus.ST.perc <- temp.all %>% left_join(mu.spawn.lavaretus) %>% 
  group_by(year) %>% 
  filter(date >= mu.spawn) %>% 
  mutate(perc.day = (10^(coef(model.lavaretus.ST)[[1]] + coef(model.lavaretus.ST)[[2]] * temp.c.ma + coef(model.lavaretus.ST)[[3]] * temp.c.ma^2))*100,
         perc.cum = cumsum(perc.day),) %>% 
  filter(perc.cum <= 100) %>%
  mutate(ADD = cumsum(temp.c.ma))

model.lavaretus.ST.perc.max <- model.lavaretus.ST.perc %>% group_by(year) %>% 
  filter(perc.cum == max(perc.cum)) %>% 
  select(date, year, temp.c.ma, ADD) %>% 
  mutate(model = "ST")


#### COMBINE ALL MODEL HATCHING ESTIMATES --------------------------------------------------------

model.hatching.albula <- mu.hatch.albula %>% 
  rename(date = "mu.hatch", temp.c.ma = "temp.hatch") %>% 
  left_join(temp.albula.filter) %>% 
  select(date, year, temp.c.ma, ADD) %>% 
  mutate(model = "EP") %>% 
  bind_rows(., model.LK.perc.max, model.albula.ST.perc.max) %>% 
  mutate(model = factor(model, ordered = TRUE, levels = c("EP", "ST", "LK")),
         jday = yday(date))

model.hatching.lavaretus <- mu.hatch.lavaretus %>% 
  rename(date = "mu.hatch", temp.c.ma = "temp.hatch") %>% 
  left_join(temp.lavaretus.filter) %>% 
  select(date, year, temp.c.ma, ADD) %>% 
  mutate(model = "EP") %>% 
  bind_rows(., model.EC.perc.max, model.lavaretus.ST.perc.max) %>% 
  mutate(model = factor(model, ordered = TRUE, levels = c("EP", "ST", "EC")),
         jday = yday(date))


#### VISUALIZATIONS ------------------------------------------------------------------------------

ggplot(temp.all, aes(x = date, y = temp.c.ma)) + 
  geom_line(size = 0.8) +
  geom_vline(data = mu.spawn.albula, aes(xintercept = as.POSIXct(mu.spawn)), color = "gray25", linetype = "dashed", show.legend = FALSE) +
  geom_point(data = model.hatching.albula, aes(x = as.POSIXct(date), y = temp.c.ma, fill = model, shape = model), size = 3) +
  scale_shape_manual("", values = c(21, 22, 23),
                     labels = c("Empirical Hatching  ", "Stewart et al., 2021  ", "Luczynski & Kirklewska, 1984")) +
  scale_fill_manual("", values = c("lightsalmon", "cornflowerblue", "forestgreen"),
                    labels = c("Empirical Hatching  ", "Stewart et al., 2021  ", "Luczynski & Kirklewska, 1984")) +
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

ggsave("figures/LakeKonnevesi-Albula-ModelComparison.png", height = 5.5, width = 9.5, dpi = 300)


plot.add.albula <- ggplot(model.hatching.albula, aes(x = factor(year), y = ADD, group = model)) +
  geom_point(aes(fill = model, shape = model), size = 3) + 
  scale_shape_manual("", values = c(21, 22, 23),
                     labels = c("Empirical Hatching  ", "Stewart et al., 2021  ", "Luczynski & Kirklewska, 1984")) +
  scale_fill_manual("", values = c("lightsalmon", "cornflowerblue", "forestgreen"),
                    labels = c("Empirical Hatching  ", "Stewart et al., 2021  ", "Luczynski & Kirklewska, 1984")) +
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

plot.date.albula <- ggplot(model.hatching.albula, aes(x = factor(year), y = jday, group = model)) +
  geom_point(aes(fill = model, shape = model), size = 3) + 
  scale_shape_manual("", values = c(21, 22, 23),
                     labels = c("Empirical Hatching  ", "Stewart et al., 2021  ", "Luczynski & Kirklewska, 1984")) +
  scale_fill_manual("", values = c("lightsalmon", "cornflowerblue", "forestgreen"),
                    labels = c("Empirical Hatching  ", "Stewart et al., 2021  ", "Luczynski & Kirklewska, 1984")) +
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
plot.all.albula <- grid.arrange(
  arrangeGrob(get_legend(plot.add.albula),
              nrow = 1,
              ncol = 1),
  arrangeGrob(
    arrangeGrob(plot.add.albula + theme(legend.position = "none", axis.title.x = element_blank()),
                nrow = 1),
    arrangeGrob(plot.date.albula + theme(legend.position = "none", axis.title.x = element_blank()), 
                nrow = 1),
    ncol = 2,
    widths = c(1, 1)
  ),
  heights = c(0.05, 1.0)
)

ggsave("figures/LakeKonnevesi-Albula-ADD-Jday.png", plot = plot.all.albula, width = 8, height = 5, dpi = 300)


# VISUALIZATION -------------------------------------------------------------------------------

ggplot(temp.all, aes(x = date, y = temp.c.ma)) + 
  geom_line(size = 0.8) +
  geom_vline(data = mu.spawn.lavaretus, aes(xintercept = as.POSIXct(mu.spawn)), color = "gray25", linetype = "dashed", show.legend = FALSE) +
  geom_point(data = model.hatching.lavaretus, aes(x = as.POSIXct(date), y = temp.c.ma, fill = model, shape = model), size = 3) +
  scale_shape_manual("", values = c(21, 22, 23),
                     labels = c("Empirical Hatching  ", "Stewart et al., 2021  ", "Eckmann, 1987")) +
  scale_fill_manual("", values = c("lightsalmon", "cornflowerblue", "forestgreen"),
                    labels = c("Empirical Hatching  ", "Stewart et al., 2021  ", "Eckmann, 1987")) +
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

ggsave("figures/LakeKonnevesi-Lavaretus-ModelComparison.png", height = 5.5, width = 9.5, dpi = 300)


plot.add.lavaretus <- ggplot(model.hatching.lavaretus, aes(x = factor(year), y = ADD, group = model)) +
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

plot.date.lavaretus <- ggplot(model.hatching.lavaretus, aes(x = factor(year), y = jday, group = model)) +
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
plot.all.lavaretus <- grid.arrange(
  arrangeGrob(get_legend(plot.add.lavaretus),
              nrow = 1,
              ncol = 1),
  arrangeGrob(
    arrangeGrob(plot.add.lavaretus + theme(legend.position = "none", axis.title.x = element_blank()),
                nrow = 1),
    arrangeGrob(plot.date.lavaretus + theme(legend.position = "none", axis.title.x = element_blank()), 
                nrow = 1),
    ncol = 2,
    widths = c(1, 1)
  ),
  heights = c(0.05, 1.0)
)

ggsave("figures/LakeKonnevesi-Lavaretus-ADD-Jday.png", plot = plot.all.lavaretus, width = 8, height = 5, dpi = 300)

