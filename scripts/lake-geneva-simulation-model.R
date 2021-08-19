#### LOAD PACKAGES -----------------------------------------------------------

library(tidyverse)
library(data.table)
library(readxl)
library(ggplot2)
library(ggthemes)
library(stringr)
library(lubridate)


#### CREATE A LIST OF FILES ----------------------------------------------------------------------

simulation.files <- grep(pattern = "(?=.*simstrat)(?=.*watertemp)(?=.*geneva)",
                         x = list.files(path = 'data/climate-simulations', recursive = TRUE, full.names = TRUE), 
                         value = TRUE, perl = TRUE)


#### LOAD SIMULATION DATA ------------------------------------------------------------------------

simulation.data <- do.call(rbind, lapply(simulation.files, function(i) {
  print(i)
  
  model <- str_match(i, "simstrat-\\s*(.*?)\\s*-")[2]
  
  if(model == "gfdl") {
    scenario <- str_match(i, "esm2m-\\s*(.*?)\\s*-watertemp")[2]
  } else if(model == "hadgem2") {
    scenario <- str_match(i, "es-\\s*(.*?)\\s*-watertemp")[2]
  } else if(model == "ipsl") {
    scenario <- str_match(i, "lr-\\s*(.*?)\\s*-watertemp")[2]
  } else if(model == "miroc5") {
    scenario <- str_match(i, "miroc5-\\s*(.*?)\\s*-watertemp")[2]
  }
  
  if(scenario == "historical") {
    lake <- str_match(i, "watertemp-\\s*(.*?)\\s*-1861")[2]
  } else {
    lake <- str_match(i, "watertemp-\\s*(.*?)\\s*-2006")[2]
  }

  if(scenario == "rcp26") {
    scenario.upper <- "RCP 2.6"
  } else if(scenario == "rcp60") {
    scenario.upper <- "RCP 6.0"
  } else if(scenario == "rcp85") {
    scenario.upper <- "RCP 8.5"
  } else if(scenario == "historical") {
    scenario.upper <- "Historical"
  }
  
  data <- fread(i) %>% 
    mutate(lake = toupper(lake),
           scenario = scenario.upper,
           year = year(date),
           month = month(date),
           yday = yday(date)) %>% 
    filter(month %in% c(9, 10, 11, 12, 1, 2, 3, 4, 5, 6)) %>% 
    mutate(year.class = ifelse(yday > 240, year + 1, year)) %>% 
    filter(year.class != 2006, year.class != 2100)
})) %>% 
  filter(year.class >= 1900)


#### LOAD LAKE-SPECIFIC BIOLOGICAL PARAMETERS ----------------------------------------------------

model.locations <- read_excel("data/model-population-parameters.xlsx", sheet = "bio-parameters") %>% 
  filter(population == "Geneva")

model.parameters <- read_excel("data/model-structural-parameters.xlsx", sheet = "coefficients") %>% 
  filter(lake == "Lake Geneva")


#### FILTER SIMULATION TEMPERATURES TO SPAWNING DEPTH --------------------------------------------

simulation.data.filt <- simulation.data %>% 
  filter(depth == model.locations$spawning.depth_m) %>% 
  rename(temp_c = watertemp)


#### AVERAGE DAILY TEMP ACROSS MODELS ------------------------------------------------------------

simulation.data.comb <- simulation.data.filt %>% 
  group_by(lake, scenario, year, year.class, date, month, yday) %>% 
  summarize(temp_c = mean(temp_c)) %>% ungroup()


#### RUN MODEL SIMULATIONS -----------------------------------------------------------------------

simulation.model.hatch.LG <- do.call(rbind, lapply(unique(simulation.data.comb$year.class), function(i) {
  print(i)
  
  ## filter to a single winter
  simulation.data.annual <- simulation.data.comb %>% filter(year.class == i)
  
  ## Calculate a 5-day center moving average to smooth temperature curve
  ## Smoothing prevents issues below trying to find the start and stop from large daily temp deviations
  simulation.data.annual.ma <- simulation.data.annual %>% group_by(scenario) %>% 
    mutate(temp.ma_c = frollmean(temp_c, n = 5, align = "center"))
  
  ## Calculate the start date of spawning period
  spawn.start.date <- simulation.data.annual.ma %>% 
    group_by(scenario) %>% 
    filter(temp.ma_c <= model.locations$start.spawn.temp_c) %>% 
    arrange(date) %>% 
    slice(1) %>% select(scenario, year.class, spawn.start.date = date)
  
  ## Calculate the end date of spawning period
  spawn.end.date <- simulation.data.annual.ma %>% 
    group_by(scenario) %>% 
    filter(temp.ma_c <= model.locations$end.spawn.temp_c) %>% 
    arrange(date) %>% 
    slice(1) %>% 
    select(scenario, year.class, spawn.end.date = date) %>% 
    ## subtract one day to correct for temp less than (spawning ends day of temp threshold)
    mutate(spawn.end.date = as.Date(spawn.end.date)-1)
  
  if(nrow(spawn.end.date) != 4) {
    spawn.end.date <- spawn.start.date %>% left_join(spawn.end.date) %>% 
      mutate(spawn.end.date = as.Date(ifelse(is.na(spawn.end.date) == TRUE, as.Date(spawn.start.date)+7, spawn.end.date), origin = "1970-01-01")) %>% 
      select(-spawn.start.date)
  } else {
    spawn.end.date
  }
  
  ## Combine start and end dates; Filter to each day in spawning period
  spawn.period.temp <- simulation.data.annual.ma %>% 
    left_join(spawn.start.date) %>% 
    left_join(spawn.end.date) %>% 
    mutate(spawn.length_days = as.Date(spawn.end.date) - as.Date(spawn.start.date),
           spawn.end.date = as.Date(ifelse(spawn.length_days > 20, spawn.start.date+20, spawn.end.date), origin = "1970-01-01"),
           spawn.length_days = as.Date(spawn.end.date) - as.Date(spawn.start.date)) %>%
    group_by(scenario) %>% 
    filter(date >= spawn.start.date, date <= spawn.end.date) 
  
  ## Calculate the rate of temperature change and divide number of spawning females propotionally
  spawner.abundance <- spawn.period.temp %>% 
    group_by(scenario) %>% 
    mutate(temp.diff_c = lag(temp_c) - temp_c,
           temp.diff_c = ifelse(is.na(temp.diff_c) == TRUE, model.locations$start.spawn.temp_c - temp_c, temp.diff_c),
           temp.diff.prop = temp.diff_c / sum(temp.diff_c),
           temp.diff.prop = ifelse(temp.diff.prop < 0, 0, temp.diff.prop),
           daily.spawner.abundance = round(500 * temp.diff.prop, 0),
           daily.spawner.abundance = ifelse(daily.spawner.abundance == 0, 1, daily.spawner.abundance),
           daily.eggs = daily.spawner.abundance * 100) %>% 
    select(-temp.diff_c, -temp.diff.prop, -daily.spawner.abundance)
  
  ## Loop across all climate scenarios
  do.call(rbind, lapply(unique(spawner.abundance$scenario), function(j) {
    spawn.period.temp.scenario <- spawner.abundance %>% filter(scenario == j)
    
    ## Run model for each day in the spawning period
    temp.hatch <- do.call(rbind, lapply(unique(spawn.period.temp.scenario$date), function(k) {
      simulation.data.model <- simulation.data.annual %>% filter(scenario == j, date >= k)
      
      spawn.temp_c <- simulation.data.model %>% slice(1) %>% pull(temp_c)
      spawn.length_days <- spawn.period.temp.scenario %>% slice(1) %>% pull(spawn.length_days) %>% as.numeric()
      daily.eggs <- spawn.period.temp.scenario %>% filter(date == k) %>% pull(daily.eggs)
      
      ## Take antilog from daily semilog output, accumulate across days
      simulation.data.model.output <- simulation.data.model %>% 
        mutate(perc.day = (10^(model.parameters$a + model.parameters$b * temp_c))*100,
               perc.cum = cumsum(perc.day),) %>% 
        filter(perc.cum <= 100) %>%
        mutate(ADD = cumsum(temp_c))
      
      ## Extract hatch date
      simulation.data.model.output.max <- simulation.data.model.output %>% 
        slice(which.max(perc.cum)) %>% 
        mutate(spawn.date = k,
               spawn.yday = yday(spawn.date),
               spawn.yday = ifelse(spawn.yday < 100, spawn.yday+365, spawn.yday),
               spawn.length_days = spawn.length_days,
               spawn.temp_c = spawn.temp_c,
               dpf = as.Date(date)-as.Date(k), 
               hatch.yday = yday(date)) %>% 
        select(scenario, year.class, spawn.date, spawn.yday, spawn.length_days, spawn.temp_c, hatch.date = date, hatch.yday, hatch.temp_c = temp_c, dpf, ADD)
      
      simulation.data.model.output.max.rep <- simulation.data.model.output.max %>% slice(rep(1:n(), each = daily.eggs)) %>% 
        mutate(daily.egg.rep = 1:n())
    })) %>% 
      mutate(spawn.peak.date = mean(spawn.date),
             spawn.peak.yday = yday(spawn.peak.date),
             spawn.peak.yday = ifelse(spawn.peak.yday < 100, spawn.peak.yday+365, spawn.peak.yday),
             hatch.peak.date = mean(hatch.date),
             hatch.peak.yday = yday(hatch.peak.date),
             hatch.length_days = length(unique(hatch.yday))) %>% 
      select(1:3, spawn.peak.date, spawn.peak.yday, 4:7, hatch.peak.date, hatch.peak.yday, 8, hatch.length_days, 9:12)
  }))
})) %>% mutate(decade = factor(year(floor_date(hatch.date, years(10)))),
               spawn.yday.plot = as.Date(spawn.yday, origin = "1970-01-01"),
               spawn.peak.yday.plot = as.Date(spawn.peak.yday, origin = "1970-01-01"),
               hatch.yday.plot = as.Date(hatch.yday, origin = "1970-01-01"))


#### CALCULATE ANOMALY ---------------------------------------------------------------------------

## historical means across 1900-2005
simulation.model.hist.mean.LG <- simulation.model.hatch.LG %>% 
  filter(scenario == "Historical", surv == 1) %>% 
  summarize(mean.hist.spawn.yday = mean(spawn.yday),
            mean.hist.hatch.yday = mean(hatch.yday),
            mean.hist.dpf = mean(dpf)) %>% 
  select(mean.hist.spawn.yday, mean.hist.hatch.yday, mean.hist.dpf)

## calculate anomaly
simulation.anomaly.LG <- simulation.model.hatch.LG %>%
  filter(surv == 1) %>% 
  group_by(scenario) %>% 
  distinct(spawn.date, .keep_all = TRUE) %>% 
  mutate(mean.hist.spawn.yday = simulation.model.hist.mean.LG$mean.hist.spawn.yday,
         mean.hist.hatch.yday = simulation.model.hist.mean.LG$mean.hist.hatch.yday,
         mean.hist.dpf = simulation.model.hist.mean.LG$mean.hist.dpf) %>% 
  mutate(spawn.yday.anomaly = spawn.yday - mean.hist.spawn.yday,
         hatch.yday.anomaly = hatch.yday - mean.hist.hatch.yday,
         dpf.anomaly = dpf - mean.hist.dpf) %>% 
  group_by(scenario, year.class) %>% 
  summarize(mean.spawn.yday.anomaly = mean(spawn.yday.anomaly),
            mean.hatch.yday.anomaly = mean(hatch.yday.anomaly),
            mean.dpf.anomaly = as.numeric(mean(dpf.anomaly)))

##
trait.list <- c("mean.spawn.yday.anomaly", "mean.hatch.yday.anomaly", "mean.dpf.anomaly")

simulation.anomaly.slope.LG <- do.call(rbind, lapply(trait.list, function(i) {
  tmp.rcp <- simulation.anomaly.LG %>% select(year.class, scenario, all_of(i)) %>% 
    filter(scenario != "Historical") %>% 
    mutate(scenario = gsub(" ", "_", scenario)) %>% 
    pivot_wider(names_from = scenario, values_from = i)
  
  ## Fit linear regressions
  lm.2.6 <- lm(RCP_2.6 ~ year.class, data = tmp.rcp)
  lm.6.0 <- lm(RCP_6.0 ~ year.class, data = tmp.rcp)
  lm.8.5 <- lm(RCP_8.5 ~ year.class, data = tmp.rcp)
  
  ## extract CI
  ci.2.6.upper <- confint(lm.2.6)[2,2]
  ci.2.6.lower <- confint(lm.2.6)[2,1]
  ci.6.0.upper <- confint(lm.6.0)[2,2]
  ci.6.0.lower <- confint(lm.6.0)[2,1]
  ci.8.5.upper <- confint(lm.8.5)[2,2]
  ci.8.5.lower <- confint(lm.8.5)[2,1]
  
  ## extract intercept
  intercept.2.6 <- coef(lm.2.6)[1]
  intercept.6.0 <- coef(lm.6.0)[1]
  intercept.8.5 <- coef(lm.8.5)[1]
  
  ## extract slopes
  slope.2.6 <- coef(lm.2.6)[2]
  slope.6.0 <- coef(lm.6.0)[2]
  slope.8.5 <- coef(lm.8.5)[2]
  
  ## extract r^2
  R2.2.6 <- summary(lm.2.6)$r.squared
  R2.6.0 <- summary(lm.6.0)$r.squared
  R2.8.5 <- summary(lm.8.5)$r.squared
  
  ##
  slopes <- data.frame(trait = rep(str_split(i, "[.]", simplify = TRUE)[1,2], 3),
                       scenario = c("RCP 2.6", "RCP 6.0", "RCP 8.5"),
                       intercept = round(c(intercept.2.6, intercept.6.0, intercept.8.5), 3),
                       slope = round(c(slope.2.6, slope.6.0, slope.8.5), 3),
                       R2 = round(c(R2.2.6, R2.6.0, R2.8.5), 3),
                       ci.lower = round(c(ci.2.6.lower, ci.6.0.lower, ci.8.5.lower), 3),
                       ci.upper = round(c(ci.2.6.upper, ci.6.0.upper, ci.8.5.upper), 3))
}))

write.csv(simulation.anomaly.slope.LG, "data/anomaly-slopes/lake-geneva-slope.csv", row.names = FALSE)


simulation.anomaly.comp.LG <- do.call(rbind, lapply(trait.list, function(i) {
  tmp.rcp.factor <- simulation.anomaly.LG %>%  select(year.class, scenario, i) %>% 
    filter(scenario != "Historical") %>% 
    mutate(year.class = factor(year.class),
           scenario = factor(scenario))
  
  ## Fit multiple regression for pairwise comparison
  lm.factor <- lm(formula(paste(i, " ~ year.class + scenario")), data = tmp.rcp.factor)
  p.value <- anova(lm.factor)[2,5]
  
  trait.comp <- data.frame(contrast = "overall",
                           estimate = NA,
                           SE = NA,
                           df = NA,
                           t.ratio = NA,
                           p.value = p.value,
                           trait = rep(str_split(i, "[.]", simplify = TRUE)[1,2]))
  
  if(p.value < 0.05) {
    ## Calculate estimated marginal means and pairwise comparisons
    trait.comp.all <- data.frame(pairs(emmeans(lm.factor, ~ scenario), simple = "scenario", adjust = "tukey")) %>% 
      mutate(trait = str_split(i, "[.]", simplify = TRUE)[1,2]) %>% 
      bind_rows(trait.comp)
  } else {
    trait.comp
  }
}))

write.csv(simulation.anomaly.comp.LG, "data/anomaly-slopes/lake-geneva-multComp.csv", row.names = FALSE)


## Clean environment
rm("simulation.files", "simulation.data", "simulation.data.filt", "model.locations", "model.parameters", "simulation.model.hist.mean.LG", "survival.reg")

