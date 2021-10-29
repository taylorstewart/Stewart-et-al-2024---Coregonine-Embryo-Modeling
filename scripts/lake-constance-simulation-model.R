#### LOAD PACKAGES -----------------------------------------------------------

library(tidyverse)
library(data.table)
library(readxl)
library(lubridate)
library(emmeans)


#### LOAD TEMPERATURE DATA -----------------------------------------------------------------------

simulation.data <- fread("data/lake-constance/lake-constance-temperature.csv", skip = 27) %>% 
  mutate(year.class = ifelse(yday > 240, year + 1, year),
         date = as.POSIXct(paste0(year, "-", month, "-", day), format = "%Y-%m-%d")) %>% 
  filter(scenario != "Historical" | year.class != 2006, year.class != 2100)
spawn.temp.data <- fread("data/lake-constance/lake-constance-temperature-spawning.csv", skip = 28) %>% 
  mutate(year.class = ifelse(yday > 240, year + 1, year),
         date = as.POSIXct(paste0(year, "-", month, "-", day), format = "%Y-%m-%d")) %>% 
  filter(scenario != "Historical" | year.class != 2006, year.class != 2100)


#### LOAD LAKE-SPECIFIC BIOLOGICAL PARAMETERS ----------------------------------------------------

model.parameters <- read_excel("data/model-structural-parameters.xlsx", sheet = "coefficients", skip = 32) %>% 
  filter(lake == "Lake Constance", species == "lavaretus wartmanni")

survival.reg <- read_excel("data/survival-regressions.xlsx", sheet = "survival-regressions", skip = 30) %>% 
  filter(lake == "Lake Constance", species == "lavaretus wartmanni")

spawn.dates <- fread("data/lake-constance/lake-constance-scenarios-spawning.csv", skip = 28) %>% 
  mutate(year.class = ifelse(yday > 240, year + 1, year),
         spawn.date = as.Date(round(spawn.yday, 0), origin = paste0(year, "-01-01"))) %>% 
  select(year, year.class, scenario, spawn.date)


#### RUN MODEL SIMULATIONS -----------------------------------------------------------------------

simulation.model.hatch.LC <- do.call(rbind, lapply(unique(simulation.data$year.class), function(i) {
  print(i)
  
  ## filter to a single winter
  simulation.data.annual <- simulation.data %>% filter(year.class == i)
  spawn.temp.data.annual <- spawn.temp.data %>% filter(year.class == i)
  spawn.date <- spawn.dates %>% filter(year.class == i)
  
  ## Calculate the start date of spawning period
  spawn.start.date <- spawn.temp.data.annual %>% 
    left_join(spawn.date) %>% 
    group_by(scenario) %>% 
    filter(date == spawn.date) %>% 
    arrange(date) %>% 
    slice(1) %>% 
    select(scenario, year.class, spawn.start.date = date)
  
  ## Calculate the end date of spawning period
  spawn.end.date <- spawn.temp.data.annual %>% 
    left_join(spawn.date) %>% 
    group_by(scenario) %>% 
    filter(date == spawn.date+9) %>% 
    arrange(date) %>% 
    slice(1) %>% 
    select(scenario, year.class, spawn.end.date = date)
  
  if(i > 2006 & nrow(spawn.end.date) != 3) {
    spawn.end.date <- spawn.start.date %>% left_join(spawn.end.date) %>% 
      mutate(spawn.end.date = as.Date(ifelse(is.na(spawn.end.date) == TRUE, as.Date(spawn.start.date)+20, spawn.end.date), origin = "1970-01-01")) %>% 
      select(-spawn.start.date)
  } else {
    spawn.end.date <- spawn.end.date
  }
  
  ## Combine start and end dates; Filter to each day in spawning period
  spawn.period.temp <- spawn.temp.data.annual %>% 
    left_join(spawn.start.date) %>% 
    left_join(spawn.end.date) %>% 
    mutate(spawn.length_days = as.Date(spawn.end.date) - as.Date(spawn.start.date)) %>%
    group_by(scenario) %>% 
    filter(date >= spawn.start.date, date <= spawn.end.date) %>% 
    arrange(scenario, date)
  
  ## Calculate the rate of temperature change and divide number of spawning females propotionally
  spawner.abundance <- spawn.period.temp %>% 
    group_by(scenario) %>% 
    mutate(temp.diff_c = lag(temp_c) - temp_c,
           temp.diff_c = ifelse(is.na(temp.diff_c) == TRUE, lead(temp.diff_c), temp.diff_c),
           temp.diff_c = ifelse(temp.diff_c < 0, 0, temp.diff_c),
           temp.diff.prop = temp.diff_c / sum(temp.diff_c),
           daily.spawner.abundance = round(500 * temp.diff.prop, 0),
           daily.spawner.abundance = ifelse(daily.spawner.abundance == 0, 1, daily.spawner.abundance),
           daily.eggs = daily.spawner.abundance * 100) %>% 
    select(-temp.diff_c, -temp.diff.prop, -daily.spawner.abundance)
  
  ## Loop across all climate scenarios
  do.call(rbind, lapply(unique(spawner.abundance$scenario), function(j) {
    spawn.period.temp.scenario <- spawner.abundance %>% filter(scenario == j)
    
    ## Run model for each day in the spawning period
    temp.hatch <- do.call(rbind, lapply(unique(spawn.period.temp.scenario$date), function(k) {
      ## Filter by scenario and spawning start date
      simulation.data.model <- simulation.data.annual %>% filter(scenario == j, date >= k)
      
      ## Extract spawning temp, number of spawning days, and number of daily eggs deposited
      spawn.temp_c <- simulation.data.model %>% slice(1) %>% pull(temp_c)
      spawn.length_days <- spawn.period.temp.scenario %>% slice(1) %>% pull(spawn.length_days) %>% as.numeric()
      daily.eggs <- spawn.period.temp.scenario %>% filter(date == k) %>% pull(daily.eggs)
      
      ## Take antilog from daily semilog output, accumulate across days
      simulation.data.model.output <- simulation.data.model %>% 
        mutate(perc.day = (10^(model.parameters$a + model.parameters$b * temp_c + model.parameters$c * temp_c^2))*100,
               perc.cum = cumsum(perc.day),) %>% 
        filter(perc.cum <= 100) %>%
        mutate(ADD = cumsum(temp_c))
      
      ## Calculate mean and median incubation temperatures
      simulation.temp <- simulation.data.model.output %>% 
        summarize(mean.inc.temp_c = mean(temp_c),
                  median.inc.temp_c = median(temp_c))
      simulation.temp.numeric <- simulation.temp %>% pull(mean.inc.temp_c)
      
      ## Calculate survival estimate
      surv.est <- survival.reg %>% mutate(interval = between(simulation.temp.numeric, start.temp, end.temp)) %>% 
        filter(interval == "TRUE") %>% 
        mutate(surv.est = (m * simulation.temp.numeric) + b) %>% pull(surv.est)
      embryo.surv <- floor(daily.eggs * surv.est)
      
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
        select(scenario, year.class, spawn.date, spawn.yday, spawn.length_days, spawn.temp_c, hatch.date = date, hatch.yday, hatch.temp_c = temp_c, dpf, ADD) %>% 
        bind_cols(simulation.temp)
      
      ## Repeat rows to equal cohort size and assign survival
      simulation.data.model.output.max.rep <- simulation.data.model.output.max %>% slice(rep(1:n(), each = daily.eggs)) %>% 
        mutate(daily.egg.rep = 1:n(),
               surv = c(rep(1, embryo.surv), rep(0, n()-embryo.surv)),
               inc.temp_c = simulation.temp.numeric)
    })) %>% 
      mutate(hatch.length_days = length(unique(hatch.yday))) %>% 
      select(1:8, hatch.length_days, 9:16)
  }))
}))


#### CALCULATE ANOMALY ---------------------------------------------------------------------------

## historical means across 1900-2005
simulation.model.hist.mean.LC <- simulation.model.hatch.LC %>% 
  filter(scenario == "Historical", surv == 1) %>% 
  summarize(mean.hist.spawn.yday = mean(spawn.yday),
            mean.hist.hatch.yday = mean(hatch.yday),
            mean.hist.hatch.length = mean(hatch.length_days),
            mean.hist.dpf = mean(dpf)) %>% 
  select(mean.hist.spawn.yday, mean.hist.hatch.yday, mean.hist.hatch.length, mean.hist.dpf)

## calculate anomaly
simulation.anomaly.LC <- simulation.model.hatch.LC %>%
  filter(surv == 1) %>% 
  group_by(scenario) %>% 
  distinct(spawn.date, .keep_all = TRUE) %>% 
  mutate(mean.hist.spawn.yday = simulation.model.hist.mean.LC$mean.hist.spawn.yday,
         mean.hist.hatch.yday = simulation.model.hist.mean.LC$mean.hist.hatch.yday,
         mean.hist.hatch.length = simulation.model.hist.mean.LC$mean.hist.hatch.length,
         mean.hist.dpf = simulation.model.hist.mean.LC$mean.hist.dpf) %>% 
  mutate(spawn.yday.anomaly = spawn.yday - mean.hist.spawn.yday,
         hatch.yday.anomaly = hatch.yday - mean.hist.hatch.yday,
         hatch.length.anomaly = hatch.length_days - mean.hist.hatch.length,
         dpf.anomaly = dpf - mean.hist.dpf) %>% 
  group_by(scenario, year.class) %>% 
  summarize(mean.spawn.yday.anomaly = mean(spawn.yday.anomaly),
            mean.hatch.yday.anomaly = mean(hatch.yday.anomaly),
            mean.hatchdays.length.anomaly = mean(hatch.length.anomaly),
            mean.dpf.anomaly = as.numeric(mean(dpf.anomaly)))

##
trait.list <- c("mean.spawn.yday.anomaly", "mean.hatch.yday.anomaly", "mean.hatchdays.length.anomaly", "mean.dpf.anomaly")

simulation.anomaly.slope.LC <- do.call(rbind, lapply(trait.list, function(i) {
  tmp.rcp <- simulation.anomaly.LC %>%  select(year.class, scenario, all_of(i)) %>% 
    filter(scenario != "Historical") %>% 
    mutate(scenario = gsub(" ", "_", scenario)) %>% 
    pivot_wider(names_from = scenario, values_from = all_of(i))
  
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

write.csv(simulation.anomaly.slope.LC, "data/anomaly-slopes/lake-constance-slope.csv", row.names = FALSE)


simulation.anomaly.comp.LC <- do.call(rbind, lapply(trait.list, function(i) {
  tmp.rcp.factor <- simulation.anomaly.LC %>%  select(year.class, scenario, i) %>% 
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
  
  if(is.na(p.value) == FALSE) {
    if(p.value < 0.05) {
    ## Calculate estimated marginal means and pairwise comparisons
    trait.comp.all <- data.frame(pairs(emmeans(lm.factor, ~ scenario), simple = "scenario", adjust = "tukey")) %>% 
      mutate(trait = str_split(i, "[.]", simplify = TRUE)[1,2]) %>% 
      bind_rows(trait.comp)
  } else {
    trait.comp
  }
    } else {
    trait.comp
  }
}))

write.csv(simulation.anomaly.comp.LC, "data/anomaly-slopes/lake-constance-multComp.csv", row.names = FALSE)

## Clean environment
rm("simulation.data", "spawn.temp.data", "spawn.dates", "model.parameters", "simulation.model.hist.mean.LC", "survival.reg", "trait.list")
