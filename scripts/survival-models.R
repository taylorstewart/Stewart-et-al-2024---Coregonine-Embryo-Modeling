#### CLEAR THE ENVIRONMENT FIRST ---------------------------------------------

rm(list = ls(all.names = TRUE))


#### LOAD PACKAGES -----------------------------------------------------------

library(tidyverse)
library(readxl)
library(lme4)
library(lmerTest)
library(afex)
library(buildmer)
library(ggplot2)
library(gridExtra)
library(grid)
library(cowplot)
library(emmeans)


#### LOAD INCUBATION TEMPERATURE DATA ----------------------------------------

ADD <- read_excel("/Users/taylor/SynologyDrive/Cisco-Climate-Change/Coregonine-Temp-Embryo/data/Coregonine-Temperature-Experiment-NA-Hatch.xlsx", sheet = "temperature", skip = 31) %>% 
  dplyr::select(population, temperature, ADD) %>% 
  group_by(population, temperature) %>% 
  mutate(dpf = 1:n())


#### LOAD HATCHING DATA ------------------------------------------------------

hatch.NA <- read_excel("/Users/taylor/SynologyDrive/Cisco-Climate-Change/Coregonine-Temp-Embryo/data/Coregonine-Temperature-Experiment-NA-Hatch.xlsx", sheet = "hatching", skip = 52) %>% 
  filter(block != "A" | population != "superior") %>% 
  mutate(eye = as.numeric(eye),
         hatch = as.numeric(hatch)) %>% 
  filter(!is.na(eye), !is.na(hatch)) %>% 
  left_join(ADD) %>% 
  dplyr::select(population, latitude, species, male, female, female_tl, female_fm, male_tl, male_fm, block, egg_id, temperature, eye, hatch, dpf, ADD, include_incubation)

hatch.FI <- read_excel("/Users/taylor/SynologyDrive/Cisco-Climate-Change/Coregonine-Temp-Embryo/data/Coregonine-Temperature-Experiment-FI-Hatch.xlsx", sheet = "hatching", skip = 48) %>% 
  mutate(premature = 0) %>% 
  dplyr::select(population, latitude, species, male, female, female_tl, female_fm, male_tl, male_fm, block, egg_id, temperature, eye, hatch, dpf, ADD, include_incubation)

## Combine all populations and years
hatch <- bind_rows(hatch.NA, hatch.FI) %>% 
  mutate(population = factor(population, levels = c("konnevesi", "superior", "ontario"), ordered = TRUE),
         # Create a variable with population and species combined
         group = factor(interaction(population, species), ordered = TRUE,
                        levels = c("konnevesi.albula", "konnevesi.lavaretus", "superior.artedi", "ontario.artedi"),
                        labels = c("LK-Vendace", "LK-Whitefish", "LS-Cisco", "LO-Cisco")))

## Clean up environment
rm(hatch.NA, hatch.FI, ADD)


#### CALCULATE LINEAR REGRESSION BETWEEN EACH TEMP FOR ALL STUDY GROUPS --------------------------

survival.models <- do.call(rbind, lapply(unique(hatch$group), function(i) {
  survival <- hatch %>% filter(eye != 0, group == i) %>% 
    group_by(temperature) %>% 
    summarize(mean.hatch = mean(hatch))
  
  temp <- unique(survival$temperature)
  
  lm1 <- coef(lm(mean.hatch ~ temperature, data = filter(survival, temperature %in% c(temp[1], temp[2]))))
  lm2 <- coef(lm(mean.hatch ~ temperature, data = filter(survival, temperature %in% c(temp[2], temp[3]))))
  lm3 <- coef(lm(mean.hatch ~ temperature, data = filter(survival, temperature %in% c(temp[3], temp[4]))))
  
  lm.all <- data.frame(group = rep(i, 3),
                       start.temp = c(temp[1], temp[2], temp[3]),
                       end.temp = c(temp[2], temp[3], temp[4]),
                       b = as.numeric(rbind(lm1, lm2, lm3)[,1]),
                       m = as.numeric(rbind(lm1, lm2, lm3)[,2]))
}))

write.csv(survival.models, "data/survival-regressions.csv", row.names = FALSE)
