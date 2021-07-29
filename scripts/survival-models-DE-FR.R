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


#### LOAD HATCHING DATA ------------------------------------------------------

hatch <- read_excel("/Users/taylor/SynologyDrive/Cisco-Climate-Change/Coregonine-Temp-Embryo-EuropeanWhitefish/data/Coregonine-Temperature-Experiment-EuropeFrance-Hatch.xlsx", sheet = "hatching") %>% 
  select(population, species_form, family, male, female, female_tl_mm, female_fm_g, male_tl_mm, male_fm_g, block, no, temperature, plate, eye, hatch, dpf, ADD, include.incubation) %>% 
  mutate(temperature = factor(temperature),
         group = factor(interaction(population, species_form)))


#### CALCULATE LINEAR REGRESSION BETWEEN EACH TEMP FOR ALL STUDY GROUPS --------------------------

survival.models <- do.call(rbind, lapply(unique(hatch$group), function(i) {
  survival <- hatch %>% filter(eye != 0, group == i) %>% 
    group_by(temperature) %>% 
    summarize(mean.hatch = mean(hatch))
  
  temp <- unique(survival$temperature)
  
  lm1 <- coef(lm(mean.hatch ~ temperature, data = survival))

  lm.all <- data.frame(group = i,
                       start.temp = temp[1],
                       end.temp = temp[2],
                       b = as.numeric(lm1[1]),
                       m = as.numeric(lm1[2]))
}))

write.csv(survival.models, "data/survival-regressions-whitefish.csv", row.names = FALSE)
