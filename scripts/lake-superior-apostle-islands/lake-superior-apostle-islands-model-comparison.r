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

temp.1 <- read_excel("data/lake-superior-apostle-islands/lake-superior-apostle-islands-temperature.xlsx", sheet = "2016")
temp.2 <- read_excel("data/lake-superior-apostle-islands/lake-superior-apostle-islands-temperature.xlsx", sheet = "2017")
temp.3 <- read_excel("data/lake-superior-apostle-islands/lake-superior-apostle-islands-temperature.xlsx", sheet = "2018")

temp.all <- bind_rows(temp.1, temp.2, temp.3)
rm(temp.1, temp.2, temp.3)

## 
ggplot(temp.all, aes(x = date, y = temp.c)) + 
  geom_line() + theme_few() + 
  ylab('Water Temperature (°C)') + 
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %d") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  + 
  facet_wrap(~year, scales = "free_x")


#### CALCULATE MEAN SPAWNING DATE ----------------------------------------------------------------

mu.spawn <- temp.all %>% group_by(year) %>% 
  filter(spawning == "y") %>% 
  summarize(mu.spawn = as.Date(mean(date), format = "%Y-%m-%d")) %>% 
  left_join(., temp.all, by = c("mu.spawn" = "date", "year")) %>% 
  select(year, mu.spawn, temp.spawn = temp.c)


#### CALCULATE MEAN HATCHING DATE ----------------------------------------------------------------

mu.hatch <- temp.all %>% group_by(year) %>% 
  filter(hatching == "y") %>% 
  summarize(mu.hatch = as.Date(mean(date), format = "%Y-%m-%d")) %>% 
  left_join(., temp.all, by = c("mu.hatch" = "date", "year")) %>% 
  select(year, mu.hatch, temp.hatch = temp.c)


## Filter temp profiles by start and end dates

temp.filter <- left_join(mu.spawn, mu.hatch) %>% 
  left_join(temp.all, .) %>% 
  group_by(year) %>% 
  filter(date >= mu.spawn, date <= mu.hatch) %>% 
  mutate(ADD = cumsum(temp.c))

temp.ADD <- temp.filter %>% group_by(year) %>% 
  filter(ADD == max(ADD))

#### XXXXXXXXX -----------------------
## Polynomial: y = a + bx + cx^2
## Semilog: log10(y) = log10(a) + log10(b)x + log10(c)x^2
## Antilog: 10^(log(y))

## Colby and Brooke
model.CB <- read_excel("data/model-structural-parameters.xlsx", sheet = "coefs") %>% 
  filter(lake == "Pickeral Lake")

## Take antilog from daily semilog output, accumulate across days
model.CB.perc <- temp.all %>% left_join(mu.spawn) %>% 
  group_by(year) %>% 
  filter(date >= mu.spawn) %>% 
  mutate(perc.day = (10^(model.CB$a + model.CB$b * temp.c + model.CB$c * temp.c^2))*100,
         perc.cum = cumsum(perc.day)) %>% 
  filter(perc.cum <= 100) %>%
  mutate(ADD = cumsum(temp.c))

model.CB.perc.max <- model.CB.perc %>% group_by(year) %>% 
  filter(perc.cum == max(perc.cum)) %>% 
  select(date, year, temp.c, ADD) %>% 
  mutate(model = "CB")


## Stewart et al. 2021
model.superior <- read_excel("data/model-structural-parameters.xlsx", sheet = "coefs") %>% 
  filter(lake == "Lake Superior")

## Take antilog from daily semilog output, accumulate across days
model.ST.perc <- temp.all %>% left_join(mu.spawn) %>% 
  group_by(year) %>% 
  filter(date >= mu.spawn) %>% 
  mutate(perc.day = (10^(model.superior$a + model.superior$b * temp.c + model.superior$c * temp.c^2))*100,
         perc.cum = cumsum(perc.day),) %>% 
  filter(perc.cum <= 100) %>%
  mutate(ADD = cumsum(temp.c))

model.ST.perc.max <- model.ST.perc %>% group_by(year) %>% 
  filter(perc.cum == max(perc.cum)) %>% 
  select(date, year, temp.c, ADD) %>% 
  mutate(model = "ST")


#### COMBINE ALL MODEL HATCHING ESTIMATES --------------------------------------------------------

model.hatching.all <- mu.hatch %>% 
  rename(date = "mu.hatch", temp.c = "temp.hatch") %>% 
  left_join(temp.filter) %>% 
  select(date, year, temp.c, ADD) %>% 
  mutate(model = "EP") %>% 
  bind_rows(., model.CB.perc.max, model.ST.perc.max) %>% 
  mutate(model = factor(model, ordered = TRUE, levels = c("EP", "ST", "CB")),
         jday = yday(date))


#### VISUALIZATIONS ------------------------------------------------------------------------------


ggplot(temp.all, aes(x = date, y = temp.c)) + 
  geom_line(size = 0.8) +
  geom_vline(data = mu.spawn, aes(xintercept = as.POSIXct(mu.spawn)), color = "gray25", linetype = "dashed", show.legend = FALSE) +
  geom_point(data = model.hatching.all, aes(x = as.POSIXct(date), y = temp.c, fill = model, shape = model), size = 3) +
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

ggsave("figures/LakeSuperior-APIS-ModelComparison.png", height = 6.5, width = 14, dpi = 300)


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

ggsave("figures/LakeSuperior-APIS-ADD-Jday.png", plot = plot.all, width = 8, height = 5, dpi = 300)

