#### CLEAR THE ENVIRONMENT FIRST ---------------------------------------------

rm(list = ls(all.names = TRUE))


#### LOAD PACKAGES -----------------------------------------------------------

library(ggplot2)
library(dplyr)
library(readxl)


temp.sat1 <- read_excel("data/LakeOntario_Temp.xlsx", sheet = "2016") %>% 
  mutate(temp.c = ifelse(temp.c < 0, 0.1, temp.c)) %>% 
  select(date, temp.c) %>% 
  mutate(source = "Chaumont SWT", year = "2016")
temp.sat2 <- read_excel("data/LakeOntario_Temp.xlsx", sheet = "2017") %>% 
  mutate(temp.c = ifelse(temp.c < 0, 0.1, temp.c)) %>% 
  select(date, temp.c) %>% 
  mutate(source = "Chaumont SWT", year = "2017")
temp.sat3 <- read_excel("data/LakeOntario_Temp.xlsx", sheet = "2018") %>% 
  mutate(temp.c = ifelse(temp.c < 0, 0.1, temp.c)) %>% 
  select(date, temp.c) %>% 
  mutate(source = "Chaumont SWT", year = "2018")
temp.sat <- bind_rows(temp.sat1, temp.sat2, temp.sat3)

temp.roch1 <- read_excel("raw_data/Lake Ontario/LakeOntario_OffRochester.xlsx", sheet = "2016") %>% 
  mutate(temp.c = ifelse(temp.c < 0, 0.1, temp.c)) %>% 
  mutate(source = "Rochester Intake 10-m", year = "2016")
temp.roch2 <- read_excel("raw_data/Lake Ontario/LakeOntario_OffRochester.xlsx", sheet = "2017") %>% 
  mutate(temp.c = ifelse(temp.c < 0, 0.1, temp.c)) %>% 
  mutate(source = "Rochester Intake 10-m", year = "2017")
temp.roch3 <- read_excel("raw_data/Lake Ontario/LakeOntario_OffRochester.xlsx", sheet = "2018") %>% 
  mutate(temp.c = ifelse(temp.c < 0, 0.1, temp.c)) %>% 
  mutate(source = "Rochester Intake 10-m", year = "2018")
temp.roch <- bind_rows(temp.roch1, temp.roch2, temp.roch3)

temp.oswego1 <- read_excel("raw_data/Lake Ontario/LO-OswegoRiver-USGS-StreamGauge.xlsx", sheet = "2016") %>% 
  mutate(temp.c = ifelse(temp.c < 0, 0.1, temp.c)) %>% 
  group_by(date) %>% 
  summarize(temp.c = mean(temp.c)) %>% 
  mutate(source = "Oswego River Gauge", year = "2016")
temp.oswego2 <- read_excel("raw_data/Lake Ontario/LO-OswegoRiver-USGS-StreamGauge.xlsx", sheet = "2017") %>% 
  mutate(temp.c = ifelse(temp.c < 0, 0.1, temp.c)) %>% 
  group_by(date) %>% 
  summarize(temp.c = mean(temp.c)) %>% 
  mutate(source = "Oswego River Gauge", year = "2017")
temp.oswego3 <- read_excel("raw_data/Lake Ontario/LO-OswegoRiver-USGS-StreamGauge.xlsx", sheet = "2018") %>% 
  mutate(temp.c = ifelse(temp.c < 0, 0.1, temp.c)) %>% 
  group_by(date) %>% 
  summarize(temp.c = mean(temp.c)) %>% 
  mutate(source = "Oswego River Gauge", year = "2018")
temp.oswego <- bind_rows(temp.oswego1, temp.oswego2, temp.oswego3)



temp.all <- bind_rows(temp.sat, temp.roch, temp.oswego) %>% 
  mutate(source = factor(source, ordered = TRUE, levels = c("Chaumont SWT", "Oswego River Gauge", "Rochester Intake 10-m")))


temp.all.composite <- temp.all %>% group_by(year, date) %>% 
  summarize(temp.c = mean(temp.c)) %>% 
  mutate(source = "Composite Average")

temp.all2 <- bind_rows(temp.all, temp.all.composite) %>% 
  mutate(source = factor(source, ordered = TRUE, levels = c("Chaumont SWT", "Oswego River Gauge", "Rochester Intake 10-m", "Composite Average")))


ggplot(temp.all, aes(x = date, y = temp.c, color = source)) + 
  geom_line(alpha = 0.75) + theme_bw() + 
  ylab('Water Temperature (°C)') + 
  geom_hline(yintercept = 4) +
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %d") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(), legend.position = "top") +
  facet_wrap(~year, scales = "free_x")

ggplot(temp.all2, aes(x = date, y = temp.c, color = source, size = source, alpha = source)) + 
  geom_line() + theme_bw() + 
  ylab('Water Temperature (°C)') + 
  geom_hline(yintercept = 4) +
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %d") + 
  scale_size_manual(values = c(0.75, 0.75, 0.75, 1.5)) +
  scale_alpha_manual(values = c(0.5, 0.5, 0.5, 0.9)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(), legend.position = "top") +
  facet_wrap(~year, scales = "free_x")

