#### CLEAR THE ENVIRONMENT FIRST -----------------------------------------------------------------

rm(list = ls(all.names = TRUE))


#### LOAD PACKAGES -------------------------------------------------------------------------------

library(dplyr)
library(readxl)


#### LOAD TEMPERATURE DATA -----------------------------------------------------------------------

temp.1 <- read_excel("data/lake-superior-apostle-islands/lake-superior-apostle-islands-temperature.xlsx", sheet = "2016")
temp.2 <- read_excel("data/lake-superior-apostle-islands/lake-superior-apostle-islands-temperature.xlsx", sheet = "2017")
temp.3 <- read_excel("data/lake-superior-apostle-islands/lake-superior-apostle-islands-temperature.xlsx", sheet = "2018")

temp.all <- bind_rows(temp.1, temp.2, temp.3) %>% 
  mutate(yday = yday(date))
rm(temp.1, temp.2, temp.3)

## 
ggplot(temp.all, aes(x = date, y = temp.c)) + 
  geom_line() + theme_few() + 
  ylab('Water Temperature (°C)') + 
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %d") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  + 
  facet_wrap(~year, scales = "free_x")


#### CALCULATE MEAN SPAWNING DATE ----------------------------------------------------------------

mu.spawn <- read_excel("data/lake-superior-apostle-islands/lake-superior-apostle-islands-spawning.xlsx", sheet = "lake-superior-apostle-spawning") %>% 
  group_by(year) %>% 
  summarize(mu.spawn.date = as.Date(weighted.mean(date, prop.ripe), format = "%Y-%m-%d")) %>% 
  mutate(mu.spawn.yday = yday(mu.spawn.date))


#### CALCULATE MEAN HATCHING DATE ----------------------------------------------------------------

mu.hatch <- read_excel("data/lake-superior-apostle-islands/lake-superior-apostle-islands-hatching.xlsx", sheet = "lake-superior-apostle-hatching") %>% 
  select(year, mu.hatch.date = mean.date) %>% 
  mutate(mu.hatch.yday = yday(mu.hatch.date))


## Filter temp profiles by start and end dates
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
  select(EP.date = date, year, EP.temp.c = temp.c)


#### LAKE SUPERIOR CISCO MODEL -------------------------------------------------------------------
model.data.superior <- read_excel("/Users/taylor/SynologyDrive/Cisco-Climate-Change/Coregonine-Temp-Embryo/data/Coregonine-Temperature-Experiment-NA-Hatch.xlsx", sheet = "hatching", skip = 52) %>% 
  mutate(eye = as.numeric(eye),
         hatch = as.numeric(hatch)) %>% 
  filter(population == "superior",
         block != "A" | population != "superior", !is.na(eye), !is.na(hatch), 
         !is.na(dpf), hatch == 1, include_incubation == "y") %>% 
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


model.iteration <- do.call(rbind, lapply(seq(0.5, 200, 0.5), function(i) {
  model.data.superior.alpha <- model.data.superior %>% mutate(log.temp.alpha = log10(temp.c + i))
  
  ## Fit Power-law with Temperature Correction Model
  model.superior <- lm(log.dpf.recip ~ log.temp.alpha, data = model.data.superior.alpha)

  ## Take antilog from daily model output, accumulate across days
  model.ST.perc <- temp.all %>% left_join(mu.spawn) %>% 
    group_by(year) %>% 
    filter(date >= mu.spawn.date) %>% 
    mutate(perc.day = (10^(coef(model.superior)[1] + coef(model.superior)[2] * log10(temp.c + i)))*100,
           perc.cum = cumsum(perc.day)) %>% 
    filter(perc.cum <= 100) %>%
    mutate(ADD = cumsum(temp.c))
  
  model.ST.perc.max <- model.ST.perc %>% group_by(year) %>% 
    filter(perc.cum == max(perc.cum)) %>% 
    select(ST.date = date, year, ST.temp.c = temp.c)
  
  model.hatching.all <- temp.ADD %>% 
    left_join(., model.ST.perc.max) %>% 
    group_by(year) %>% 
    summarize(hatch.date.diff = (as.numeric(EP.date) - as.numeric(ST.date))/86400) %>% 
    mutate(alpha = i)
}))

alpha <- model.iteration %>% mutate(ref = abs(hatch.date.diff-0)) %>% 
  group_by(year) %>% 
  slice(which.min(ref)) 

%>% ungroup() %>% 
  summarize(mean.alpha = round(mean(alpha), 7)) %>% pull()


model.data.superior.alpha <- model.data.superior %>% mutate(log.temp.alpha = log10(temp.c + alpha))

## Fit Power-law with Temperature Correction Model
model.superior <- lm(log.dpf.recip ~ log.temp.alpha, data = model.data.superior.alpha)
summary(model.superior)

