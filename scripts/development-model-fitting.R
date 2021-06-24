#### CLEAR THE ENVIRONMENT FIRST -----------------------------------------------------------------

rm(list = ls(all.names = TRUE))


#### LOAD PACKAGES -------------------------------------------------------------------------------

library(dplyr)
library(readxl)


#### LAKE SUPERIOR CISCO -------------------------------------------------------------------------
model.data.superior <- read_excel("/Users/taylor/SynologyDrive/Cisco-Climate-Change/Coregonine-Temp-Embryo/data/Coregonine-Temperature-Experiment-NA-Hatch.xlsx", sheet = "hatching", skip = 52) %>% 
  filter(population == "superior") %>% 
  mutate(eye = as.numeric(eye),
         hatch = as.numeric(hatch)) %>% 
  filter(!is.na(eye), !is.na(hatch), !is.na(dpf), hatch == 1, include_incubation == "y") %>% 
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

## Fit Semilog Model
model.superior <- lm(log.dpf.recip ~ temp.c + I(temp.c^2), data = model.data.superior)
summary(model.superior)


#### LAKE ONTARIO CISCO --------------------------------------------------------------------------
model.data.ontario <- read_excel("/Users/taylor/SynologyDrive/Cisco-Climate-Change/Coregonine-Temp-Embryo/data/Coregonine-Temperature-Experiment-NA-Hatch.xlsx", sheet = "hatching", skip = 52) %>% 
  filter(population == "ontario") %>% 
  mutate(eye = as.numeric(eye),
         hatch = as.numeric(hatch)) %>% 
  filter(!is.na(eye), !is.na(hatch), !is.na(dpf), hatch == 1, include_incubation == "y") %>% 
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

## Fit Semilog Model
model.ontario <- lm(log.dpf.recip ~ temp.c + I(temp.c^2), data = model.data.ontario)
summary(model.ontario)


#### LAKE KONNEVESI VENDACE ----------------------------------------------------------------------
model.data.konnevesi.vendace <- read_excel("/Users/taylor/SynologyDrive/Cisco-Climate-Change/Coregonine-Temp-Embryo/data/Coregonine-Temperature-Experiment-FI-Hatch.xlsx", sheet = "hatching", skip = 48) %>% 
  mutate(eye = as.numeric(eye),
         hatch = as.numeric(hatch)) %>% 
  filter(species == "albula", !is.na(eye), !is.na(hatch), !is.na(dpf), 
         hatch == 1, include_incubation == "y") %>% 
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

## Fit Semilog Model
model.konnevesi.vendace <- lm(log.dpf.recip ~ temp.c + I(temp.c^2), data = model.data.konnevesi.vendace)
summary(model.konnevesi.vendace)


#### LAKE KONNEVESI EUROPEAN WHITEFISH -----------------------------------------------------------
model.data.konnevesi.whitefish <- read_excel("/Users/taylor/SynologyDrive/Cisco-Climate-Change/Coregonine-Temp-Embryo/data/Coregonine-Temperature-Experiment-FI-Hatch.xlsx", sheet = "hatching", skip = 48) %>% 
  mutate(eye = as.numeric(eye),
         hatch = as.numeric(hatch)) %>% 
  filter(species == "lavaretus", !is.na(eye), !is.na(hatch), !is.na(dpf), 
         hatch == 1, include_incubation == "y") %>% 
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
         log.dpf.recip = log10(dpf.recip)) %>% 
  filter(dpf != 67)

## Fit Semilog Model
model.konnevesi.whitefish <- lm(log.dpf.recip ~ temp.c + I(temp.c^2), data = model.data.konnevesi.whitefish)
summary(model.konnevesi.whitefish)


#### LAKE CONSTANCE EUROPEAN WHITEFISH LITTORAL --------------------------------------------------
model.data.constance.lit <- read_excel("/Users/taylor/SynologyDrive/Cisco-Climate-Change/Coregonine-Temp-Embryo-EuropeanWhitefish/data/Coregonine-Temperature-Experiment-EuropeFrance-Hatch.xlsx", sheet = "hatching") %>% 
  mutate(eye = as.numeric(eye),
         hatch = as.numeric(hatch)) %>% 
  filter(population == "constance", species_form == "littoral",
         !is.na(eye), !is.na(hatch), !is.na(dpf), 
         hatch == 1, include.incubation == "y") %>% 
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

## Fit Semilog Model
model.constance.lit <- lm(log.dpf.recip ~ temp.c + I(temp.c^2), data = model.data.constance.lit)
summary(model.constance.lit)


#### LAKE CONSTANCE EUROPEAN WHITEFISH PELAGIC ---------------------------------------------------
model.data.constance.pel <- read_excel("/Users/taylor/SynologyDrive/Cisco-Climate-Change/Coregonine-Temp-Embryo-EuropeanWhitefish/data/Coregonine-Temperature-Experiment-EuropeFrance-Hatch.xlsx", sheet = "hatching") %>% 
  mutate(eye = as.numeric(eye),
         hatch = as.numeric(hatch)) %>% 
  filter(population == "constance", species_form == "pelagic",
         !is.na(eye), !is.na(hatch), !is.na(dpf), 
         hatch == 1, include.incubation == "y") %>% 
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

## Fit Semilog Model
model.constance.pel <- lm(log.dpf.recip ~ temp.c + I(temp.c^2), data = model.data.constance.pel)
summary(model.constance.pel)


#### LAKE GENEVA EUROPEAN WHITEFISH --------------------------------------------------------------
model.data.geneva <- read_excel("/Users/taylor/SynologyDrive/Cisco-Climate-Change/Coregonine-Temp-Embryo-EuropeanWhitefish/data/Coregonine-Temperature-Experiment-EuropeFrance-Hatch.xlsx", sheet = "hatching") %>% 
  mutate(eye = as.numeric(eye),
         hatch = as.numeric(hatch)) %>% 
  filter(population == "leman", !is.na(eye), !is.na(hatch), !is.na(dpf), 
         hatch == 1, include.incubation == "y") %>% 
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

## Fit Semilog Model
model.geneva <- lm(log.dpf.recip ~ temp.c + I(temp.c^2), data = model.data.geneva)
summary(model.geneva)


#### LAKE BOURGET EUROPEAN WHITEFISH -------------------------------------------------------------
model.data.bourget <- read_excel("/Users/taylor/SynologyDrive/Cisco-Climate-Change/Coregonine-Temp-Embryo-EuropeanWhitefish/data/Coregonine-Temperature-Experiment-EuropeFrance-Hatch.xlsx", sheet = "hatching") %>% 
  mutate(eye = as.numeric(eye),
         hatch = as.numeric(hatch)) %>% 
  filter(population == "bourget", !is.na(eye), !is.na(hatch), !is.na(dpf), 
         hatch == 1, include.incubation == "y") %>% 
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

## Fit Semilog Model
model.bourget <- lm(log.dpf.recip ~ temp.c + I(temp.c^2), data = model.data.bourget)
summary(model.bourget)


