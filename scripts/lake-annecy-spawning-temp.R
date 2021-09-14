#### CLEAR THE ENVIRONMENT FIRST ---------------------------------------------

rm(list = ls(all.names = TRUE))


#### LOAD PACKAGES -----------------------------------------------------------

library(dplyr)
library(data.table)
library(readxl)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(stringr)
library(lubridate)
library(lubridateExtras)


#### LOAD TEMPERATURE DATA -----------------------------------------------------------------------

temp.all <- read_excel("data/lake-annecy/lake-annecy-temperature.xlsx", sheet = "temp", skip = 28) %>% 
  mutate(yday = yday(date)) %>% 
  filter(!is.na(temp_c))

## 
ggplot(temp.all, aes(x = date, y = temp_c)) + 
  geom_line() + theme_few() + 
  labs(y = 'Water Temperature (°C)', x = "") + 
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %d") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  facet_wrap(~year, scales = "free_x")


spawn <- read_excel("data/lake-annecy/lake-annecy-spawning.xlsx", sheet = "lake-annecy-spawning", skip = 29)

spawn.all <- do.call(rbind, lapply(unique(spawn$year), function(i) {
  tmp <- spawn %>% filter(year == i)
  
  if(nrow(tmp) == 1) {
    tmp2 <- data.frame(year = i, date = as.Date(tmp$date) + 8, spawn.abundance = NA, total.abundance = NA, percent.abundance = NA)
    tmp <- bind_rows(tmp, tmp2)
  } else { tmp }
}))


spawn.wide <- spawn.all %>% mutate(spawn.group = rep(c("start.date", "end.date"), times = 3)) %>%
  select(year, date, spawn.group) %>% 
  pivot_wider(names_from = spawn.group, values_from = date) %>% 
  mutate(start.yday = yday(start.date),
         start.yday = ifelse(start.yday < 100, 365+start.yday, start.yday),
         end.yday = yday(end.date),
         end.yday = ifelse(end.yday < 100, 365+end.yday, end.yday)) %>% 
  summarize(mean.start.yday = mean(start.yday),
            mean.end.yday = mean(end.yday)) %>% 
  mutate(mean.start.yday = ifelse(mean.start.yday > 365, mean.start.yday-365, mean.start.yday),
         mean.end.yday = ifelse(mean.end.yday > 365, mean.end.yday-365, mean.end.yday))
  
spawn.temp.start <- temp.all %>%
  filter(yday >= spawn.wide$mean.start.yday)
spawn.temp.end <- temp.all %>%
  filter(yday <= spawn.wide$mean.end.yday)
spawn.temp <- bind_rows(spawn.temp.start, spawn.temp.end) %>% arrange(date) %>% 
  group_by(year) %>% 
  filter(row_number() %in% c(1, n())) %>% ungroup() %>% 
  mutate(spawn.group = rep(c("start", "end"), times = 2)) %>% 
  group_by(spawn.group) %>% 
  summarize(temp_c = mean(temp_c))
  
