#### CLEAR THE ENVIRONMENT FIRST ---------------------------------------------

rm(list = ls(all.names = TRUE))


#### LOAD PACKAGES -----------------------------------------------------------

library(ggplot2)
library(lubridate)
library(ggthemes)
library(dplyr)
library(readxl)
library(zoo)
library(gridExtra)
library(grid)
library(cowplot)


#### LOAD TEMPERATURE DATA -----------------------------------------------------------------------

temp.1 <- read_excel("data/LakeSuperior_TB_Temp.xlsx", sheet = "2016")
temp.2 <- read_excel("data/LakeSuperior_TB_Temp.xlsx", sheet = "2017")
temp.3 <- read_excel("data/LakeSuperior_TB_Temp.xlsx", sheet = "2018")

temp.all <- bind_rows(temp.1, temp.2, temp.3)
rm(temp.1, temp.2, temp.3)

## 
ggplot(temp.all, aes(x = date, y = temp.c.ma)) + 
  geom_line() + theme_few() + 
  ylab('Water Temperature (°C)') + 
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %d") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  facet_wrap(~year, scales = "free_x")


#### CALCULATE MEAN SPAWNING DATE ----------------------------------------------------------------

mu.spawn <- temp.all %>% group_by(year) %>% 
  filter(spawning == "y") %>% 
  summarize(mu.spawn = as.Date(mean(date), format = "%Y-%m-%d")) %>% 
  left_join(., temp.all, by = c("mu.spawn" = "date", "year")) %>% 
  select(year, mu.spawn, temp.spawn = temp.c.ma)


#### CALCULATE MEAN HATCHING DATE ----------------------------------------------------------------

mu.hatch <- temp.all %>% group_by(year) %>% 
  filter(hatching == "y") %>% 
  summarize(mu.hatch = as.Date(mean(date), format = "%Y-%m-%d")) %>% 
  left_join(., temp.all, by = c("mu.hatch" = "date", "year")) %>% 
  select(year, mu.hatch, temp.hatch = temp.c.ma)


## Filter temp profiles by start and end dates

temp.filter <- left_join(mu.spawn, mu.hatch) %>% 
  left_join(temp.all, .) %>% 
  group_by(year) %>% 
  filter(date >= mu.spawn, date <= mu.hatch) %>% 
  mutate(ADD = cumsum(temp.c.ma))

temp.ADD <- temp.filter %>% group_by(year) %>% 
  filter(ADD == max(ADD))

#### XXXXXXXXX -----------------------
## Polynomial: y = a + bx + cx^2
## Semilog: log10(y) = log10(a) + log10(b)x + log10(c)x^2
## Antilog: 10^(log(y))

## Colby and Brooke
model.data.CB <- data.frame(temp.c = c(9.95, 8.9, 7.75, 6.7, 5.6, 3.35, 1.7, 0.5),
                       dpf = c(37.74, 46.33, 59.61, 73.41, 91.63, 146.29, 186.64, 234.53)) %>% 
  mutate(dpf.recip = dpf^-1,
         log.dpf.recip = log10(dpf.recip))

## Fit Semilog Model
model.CB <- lm(log.dpf.recip ~ temp.c + I(temp.c^2), data = model.data.CB)

## Take antilog from daily semilog output, accumulate across days
model.CB.perc <- temp.all %>% left_join(mu.spawn) %>% 
  group_by(year) %>% 
  filter(date >= mu.spawn) %>% 
  mutate(perc.day = (10^(coef(model.CB)[[1]] + coef(model.CB)[[2]] * temp.c.ma + coef(model.CB)[[3]] * temp.c.ma^2))*100,
         perc.cum = cumsum(perc.day)) %>% 
  filter(perc.cum <= 100) %>%
  mutate(ADD = cumsum(temp.c.ma))

model.CB.perc.max <- model.CB.perc %>% group_by(year) %>% 
  filter(perc.cum == max(perc.cum)) %>% 
  select(date, year, temp.c.ma, ADD) %>% 
  mutate(model = "CB")

## Stewart et al. 2021
model.data.ST <- read_excel("/Users/taylor/SynologyDrive/Cisco-Climate-Change/Coregonine-Temp-Embryo/data/Coregonine-Temperature-Experiment-NA-Hatch.xlsx", sheet = "2020HatchingData") %>% 
  filter(is.na(notes) | notes != "empty well", block != "A" | population != "superior") %>% 
  mutate(eye = as.numeric(eye),
         hatch = as.numeric(hatch)) %>% 
  filter(!is.na(eye), !is.na(hatch), !is.na(dpf), hatch == 1, include.incubation == "y") %>% 
  rename(temp.c = temperature) %>% 
  mutate(dpf.recip = dpf^-1,
         log.dpf.recip = log10(dpf.recip)) %>% 
  filter(population == "superior")

## Fit Semilog Model
model.ST <- lm(log.dpf.recip ~ temp.c + I(temp.c^2), data = model.data.ST)

## Take antilog from daily semilog output, accumulate across days
model.ST.perc <- temp.all %>% left_join(mu.spawn) %>% 
  group_by(year) %>% 
  filter(date >= mu.spawn) %>% 
  mutate(perc.day = (10^(coef(model.ST)[[1]] + coef(model.ST)[[2]] * temp.c.ma + coef(model.ST)[[3]] * temp.c.ma^2))*100,
         perc.cum = cumsum(perc.day),) %>% 
  filter(perc.cum <= 100) %>%
  mutate(ADD = cumsum(temp.c.ma))

model.ST.perc.max <- model.ST.perc %>% group_by(year) %>% 
  filter(perc.cum == max(perc.cum)) %>% 
  select(date, year, temp.c.ma, ADD) %>% 
  mutate(model = "ST")


#### COMBINE ALL MODEL HATCHING ESTIMATES --------------------------------------------------------

model.hatching.all <- mu.hatch %>% 
  rename(date = "mu.hatch", temp.c.ma = "temp.hatch") %>% 
  left_join(temp.filter) %>% 
  select(date, year, temp.c.ma, ADD) %>% 
  mutate(model = "EP") %>% 
  bind_rows(., model.CB.perc.max, model.ST.perc.max) %>% 
  mutate(model = factor(model, ordered = TRUE, levels = c("EP", "ST", "CB")),
         jday = yday(date))


#### VISUALIZATIONS ------------------------------------------------------------------------------


ggplot(temp.all, aes(x = date, y = temp.c.ma)) + 
  geom_line(size = 0.8) +
  geom_vline(data = mu.spawn, aes(xintercept = as.POSIXct(mu.spawn)), color = "gray25", linetype = "dashed", show.legend = FALSE) +
  geom_point(data = model.hatching.all, aes(x = as.POSIXct(date), y = temp.c.ma, fill = model, shape = model), size = 3) +
  scale_shape_manual("", values = c(21, 22, 23),
                    labels = c("Empirical Hatching  ", "Stewart et al., 2021  ", "Colby & Brooke, 1973")) +
  scale_fill_manual("", values = c("lightsalmon", "cornflowerblue", "forestgreen"),
                    labels = c("Empirical Hatching  ", "Stewart et al., 2021  ", "Colby & Brooke, 1973")) +
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

ggsave("figures/LakeSuperior-TB-ModelComparison.png", height = 6.5, width = 14, dpi = 300)


plot.add <- ggplot(model.hatching.all, aes(x = factor(year), y = ADD, group = model)) +
  geom_point(aes(fill = model, shape = model), size = 3) + 
  scale_shape_manual("", values = c(21, 22, 23),
                     labels = c("Empirical Hatching  ", "Stewart et al., 2021  ", "Colby & Brooke, 1973")) +
  scale_fill_manual("", values = c("lightsalmon", "cornflowerblue", "forestgreen"),
                    labels = c("Empirical Hatching  ", "Stewart et al., 2021  ", "Colby & Brooke, 1973")) +
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
                     labels = c("Empirical Hatching  ", "Stewart et al., 2021  ", "Colby & Brooke, 1973")) +
  scale_fill_manual("", values = c("lightsalmon", "cornflowerblue", "forestgreen"),
                    labels = c("Empirical Hatching  ", "Stewart et al., 2021  ", "Colby & Brooke, 1973")) +
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

ggsave("figures/LakeSuperior-TB-ADD-Jday.png", plot = plot.all, width = 8, height = 5, dpi = 300)

