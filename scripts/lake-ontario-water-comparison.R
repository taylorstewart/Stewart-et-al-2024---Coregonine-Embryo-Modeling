#### CLEAR THE ENVIRONMENT FIRST ---------------------------------------------

rm(list = ls(all.names = TRUE))


#### LOAD PACKAGES -----------------------------------------------------------

library(tidyverse)
library(readxl)
library(ggplot2)
library(ggthemes)


temp.sat1 <- read_excel("raw-data/lake-ontario/lake-ontario-temp-chaumont-bay-swt.xlsx", sheet = "2018") %>% 
  mutate(temp.c = ifelse(temp.c < 0, 0.1, temp.c)) %>% 
  select(date, temp.c, year) %>% 
  mutate(source = "Chaumont Bay Satellite SWT")
temp.sat2 <- read_excel("raw-data/lake-ontario/lake-ontario-temp-chaumont-bay-swt.xlsx", sheet = "2019") %>% 
  mutate(temp.c = ifelse(temp.c < 0, 0.1, temp.c)) %>% 
  select(date, temp.c, year) %>% 
  mutate(source = "Chaumont Bay Satellite SWT")
temp.sat3 <- read_excel("raw-data/lake-ontario/lake-ontario-temp-chaumont-bay-swt.xlsx", sheet = "2020") %>% 
  mutate(temp.c = ifelse(temp.c < 0, 0.1, temp.c)) %>% 
  select(date, temp.c, year) %>% 
  mutate(source = "Chaumont Bay Satellite SWT")
temp.sat <- bind_rows(temp.sat1, temp.sat2, temp.sat3)

temp.roch1 <- read_excel("raw-data/lake-ontario/lake-ontario-temp-rochester.xlsx", sheet = "2018") %>% 
  mutate(temp.c = ifelse(temp.c < 0, 0.1, temp.c)) %>% 
  mutate(source = "Rochester Water Intake")
temp.roch2 <- read_excel("raw-data/lake-ontario/lake-ontario-temp-rochester.xlsx", sheet = "2019") %>% 
  mutate(temp.c = ifelse(temp.c < 0, 0.1, temp.c)) %>% 
  mutate(source = "Rochester Water Intake")
temp.roch3 <- read_excel("raw-data/lake-ontario/lake-ontario-temp-rochester.xlsx", sheet = "2020") %>% 
  mutate(temp.c = ifelse(temp.c < 0, 0.1, temp.c)) %>% 
  mutate(source = "Rochester Water Intake")
temp.roch <- bind_rows(temp.roch1, temp.roch2, temp.roch3)

temp.oswego1 <- read_excel("raw-data/lake-ontario/lake-ontario-temp-oswego-river.xlsx", sheet = "2018") %>% 
  mutate(source = "Oswego USGS River Gauge")
temp.oswego2 <- read_excel("raw-data/lake-ontario/lake-ontario-temp-oswego-river.xlsx", sheet = "2019") %>% 
  mutate(source = "Oswego USGS River Gauge")
temp.oswego3 <- read_excel("raw-data/lake-ontario/lake-ontario-temp-oswego-river.xlsx", sheet = "2020") %>% 
  mutate(source = "Oswego USGS River Gauge")
temp.oswego <- bind_rows(temp.oswego1, temp.oswego2, temp.oswego3)

temp.noaa1 <- read_excel("data/lake-ontario/lake-ontario-temperature-NOAA.xlsx", sheet = "2018") %>% 
  mutate(source = "NOAA")
temp.noaa2 <- read_excel("data/lake-ontario/lake-ontario-temperature-NOAA.xlsx", sheet = "2019") %>% 
  mutate(source = "NOAA")
temp.noaa3 <- read_excel("data/lake-ontario/lake-ontario-temperature-NOAA.xlsx", sheet = "2020") %>% 
  mutate(source = "NOAA")
temp.noaa <- bind_rows(temp.noaa1, temp.noaa2, temp.noaa3) %>% 
  select(date, year, temp.c = temp_c, source)


temp.chau <- read_excel("data/lake-ontario/lake-ontario-temperature-embayments-bottom.xlsx", sheet = "chaumont-bay") %>% 
  group_by(date, year) %>% 
  summarize(temp.c = mean(temp.c)) %>% 
  mutate(source = "Chaumont Bay Bottom")
temp.sodus <- read_excel("data/lake-ontario/lake-ontario-temperature-embayments-bottom.xlsx", sheet = "sodus-bay") %>% 
  group_by(date, year) %>% 
  summarize(temp.c = mean(temp.c)) %>% 
  mutate(source = "Sodus Bay Bottom")


temp.bw <- read_excel("raw-data/lake-ontario/LakeOntario_Chaumont_WaterTemp_BW.xlsx", sheet = "Sheet1") %>% 
  select(date = OP_DATE, temp.c.surface = SURFACE_TEMP_degC, temp.c.bottom = NEAR_BOTTOM_TEMP_degC, year) %>% 
  group_by(date, year) %>% 
  summarize(temp.c.surface = mean(temp.c.surface, na.rm = TRUE),
            temp.c.bottom = mean(temp.c.bottom, na.rm = TRUE))



temp.all <- bind_rows(temp.sat, temp.chau, temp.sodus, temp.roch, temp.oswego, temp.noaa) %>% 
  mutate(source = factor(source, ordered = TRUE, levels = c("Chaumont Bay Satellite SWT", "Chaumont Bay Bottom", "Oswego USGS River Gauge", "Sodus Bay Bottom", "Rochester Water Intake", "NOAA")))


ggplot(filter(temp.all, year == 2019), aes(x = date, y = temp.c)) + 
  geom_line(aes(color = source)) + 
  #geom_point(data = temp.bw, aes(y = temp.c.surface), shape = 4, size = 4, stroke = 1) +
  #geom_point(data = temp.bw, aes(y = temp.c.bottom), shape = 3, size = 4, stroke = 1) +
  labs(y = 'Water Temperature (°C)') + 
  #geom_hline(yintercept = 4) +
  scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "black")) +
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%Y-%b-%d") + 
  theme_few() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        legend.title = element_blank(), 
        legend.position = "top",
        legend.text = element_text(size = 12)) #+
  #facet_wrap(~year, scales = "free_x")d

ggsave("figures/lake-ontario/lake-ontario-water-temp-source.png", dpi = 300, width = 12, height = 6)


temp.all <- bind_rows(temp.sat, temp.chau, temp.sodus, temp.roch, temp.oswego) %>% 
  mutate(source = factor(source, ordered = TRUE, levels = c("Chaumont Bay Satellite SWT", "Chaumont Bay Bottom", "Oswego USGS River Gauge", "Sodus Bay Bottom", "Rochester Water Intake")))

temp.all.long <- bind_rows(temp.chau, temp.oswego) %>% 
  select(-year) %>% 
  mutate(source = ifelse(source == 'Chaumont Bay Bottom', 'chaum', 'oswego')) %>% 
  pivot_wider(names_from = "source", values_from = "temp.c") %>% 
  mutate(temp.diff = oswego-chaum) %>% 
  filter(!is.na(temp.diff)) %>% ungroup() %>% 
  summarize(mean.temp.diff = mean(temp.diff))

