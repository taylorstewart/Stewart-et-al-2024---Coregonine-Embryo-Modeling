#### CLEAR THE ENVIRONMENT FIRST ---------------------------------------------

rm(list = ls(all.names = TRUE))


#### LOAD PACKAGES -----------------------------------------------------------

library(tidyverse)
library(readxl)
library(ggplot2)
library(ggthemes)


temp.sat1 <- read_excel("data/lake-superior-thunder-bay/lake-superior-thunder-bay-temperature.xlsx", sheet = "2017") %>% 
  mutate(source = "NOAA")
temp.sat2 <- read_excel("data/lake-superior-thunder-bay/lake-superior-thunder-bay-temperature.xlsx", sheet = "2018") %>% 
  mutate(source = "NOAA")
temp.sat3 <- read_excel("data/lake-superior-thunder-bay/lake-superior-thunder-bay-temperature.xlsx", sheet = "2019") %>% 
  mutate(source = "NOAA")
temp.sat4 <- read_excel("data/lake-superior-thunder-bay/lake-superior-thunder-bay-temperature.xlsx", sheet = "2020") %>% 
  mutate(source = "NOAA")
temp.sat5 <- read_excel("data/lake-superior-thunder-bay/lake-superior-thunder-bay-temperature.xlsx", sheet = "2021") %>% 
  mutate(source = "NOAA")
temp.sat <- bind_rows(temp.sat1, temp.sat2, temp.sat3, temp.sat4, temp.sat5)


temp.tb1 <- read_excel("raw-data/lake-superior/Temperature/BarePoint/lake-superior-thunder-bay-temperature-BarePoint.xlsx", sheet = "2017") %>% 
  mutate(source = "Bare Point")
temp.tb2 <- read_excel("raw-data/lake-superior/Temperature/BarePoint/lake-superior-thunder-bay-temperature-BarePoint.xlsx", sheet = "2018") %>% 
  mutate(source = "Bare Point")
temp.tb3 <- read_excel("raw-data/lake-superior/Temperature/BarePoint/lake-superior-thunder-bay-temperature-BarePoint.xlsx", sheet = "2019") %>% 
  mutate(source = "Bare Point")
temp.tb4 <- read_excel("raw-data/lake-superior/Temperature/BarePoint/lake-superior-thunder-bay-temperature-BarePoint.xlsx", sheet = "2020") %>% 
  mutate(source = "Bare Point")
temp.tb5 <- read_excel("raw-data/lake-superior/Temperature/BarePoint/lake-superior-thunder-bay-temperature-BarePoint.xlsx", sheet = "2021") %>% 
  mutate(source = "Bare Point")
temp.tb <- bind_rows(temp.tb1, temp.tb2, temp.tb3, temp.tb4, temp.tb5)


temp.all <- bind_rows(temp.sat, temp.tb) %>% 
  mutate(source = factor(source, ordered = TRUE, levels = c("NOAA", "Bare Point")))


ggplot(temp.all, aes(x = date, y = temp_c)) + 
  geom_line(aes(color = source)) + 
  #geom_point(data = temp.bw, aes(y = temp.c.surface), shape = 4, size = 4, stroke = 1) +
  #geom_point(data = temp.bw, aes(y = temp.c.bottom), shape = 3, size = 4, stroke = 1) +
  labs(y = 'Water Temperature (°C)') + 
  #geom_hline(yintercept = 4) +
  scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e")) +
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b-%d") + 
  theme_few() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        legend.title = element_blank(), 
        legend.position = "top",
        legend.text = element_text(size = 12)) +
  facet_wrap(~year, scales = "free_x")

ggsave("figures/lake-superior-thunder-bay/lake-superior-thunder-bay-water-temp-source.png", dpi = 300, width = 14, height = 8)

