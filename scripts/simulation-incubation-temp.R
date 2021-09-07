#### CLEAR THE ENVIRONMENT FIRST ---------------------------------------------

rm(list = ls(all.names = TRUE))


#### LOAD PACKAGES -------------------------------------------------------------------------------

library(ggplot2)
library(ggthemes)


#### LOAD DATA FROM OTHER SCRIPTS ----------------------------------------------------------------

source("scripts/lake-superior-apostle-islands-simulation-model.R")
source("scripts/lake-ontario-simulation-model.R")
source("scripts/lake-konnevesi-albula-simulation-model.R")
source("scripts/lake-annecy-simulation-model.R")
source("scripts/lake-constance-simulation-model.R")



simulation.inc.temp.LS.APIS <- simulation.model.hatch.LS.APIS %>% group_by(scenario, year.class) %>% 
  summarize(inc.temp_c = mean(inc.temp_c)) %>% 
  mutate(group = "LS")
simulation.inc.temp.LO <- simulation.model.hatch.LO %>% group_by(scenario, year.class) %>% 
  summarize(inc.temp_c = mean(inc.temp_c)) %>% 
  mutate(group = "LO")
simulation.inc.temp.LK <- simulation.model.hatch.LK %>% group_by(scenario, year.class) %>% 
  summarize(inc.temp_c = mean(inc.temp_c)) %>% 
  mutate(group = "LK")
simulation.inc.temp.LA <- simulation.model.hatch.LA %>% group_by(scenario, year.class) %>% 
  summarize(inc.temp_c = mean(inc.temp_c)) %>% 
  mutate(group = "LA")
simulation.inc.temp.LC <- simulation.model.hatch.LC %>% group_by(scenario, year.class) %>% 
  summarize(inc.temp_c = mean(inc.temp_c)) %>% 
  mutate(group = "LC")


simulation.inc.temp <- bind_rows(simulation.inc.temp.LS.APIS, simulation.inc.temp.LO,
                                 simulation.inc.temp.LK, simulation.inc.temp.LA,
                                 simulation.inc.temp.LC) %>% 
  mutate(group = factor(group, ordered = TRUE, levels = c("LA", "LC", "LO", "LS", "LK"),
                        labels = c("Lake Annecy", "Lake Constance", "Lake Ontario", "Lake Superior", "Lake S. Konnevesi")))


simulation.inc.temp.slope <- do.call(rbind, lapply(unique(simulation.inc.temp$group), function(i) {
  
  tmp.group <- simulation.inc.temp %>% 
    filter(scenario != "Historical", group == i) %>% 
    mutate(scenario = gsub(" ", "_", scenario))
  
  tmp <- do.call(rbind, lapply(unique(tmp.group$scenario), function(j) {
    
    tmp.group.scenario <- tmp.group %>% 
      filter(scenario == j)
    
    ## Fit linear regression
    lm <- lm(inc.temp_c ~ year.class, data = tmp.group.scenario)

    ## extract slope
    slope <- coef(lm)[2]

    ##
    slopes <- data.frame(group = i,
                         scenario = j,
                         slope = round(slope, 4),
                         decade.slope = slope * 10)
  }))
}))



lapply(c("Lake Annecy", "Lake Constance"), function(grp) {
  tmp <- simulation.inc.temp.slope %>% filter(group == grp)
  
  # constants
  axis_begin  <- 0
  axis_end    <- 0.4
  total_ticks <- 5
  
  # chart junk data
  tick_frame <- data.frame(ticks = seq(axis_begin, axis_end, length.out = total_ticks))
  lab_frame <- data.frame(lab = seq(axis_begin, axis_end, 0.05))
  
  # plot
  ggplot(tmp, aes(x = decade.slope, y = 1)) +
    # x axis line
    geom_segment(y = 1, yend = 1, x = 0, xend = 0.4, size = 0.5) +
    geom_segment(y = 0.985, yend = 0.985, x = 0, xend = 0.4, size = 0.5) +
    geom_segment(y = 1.015, yend = 1.015, x = 0, xend = 0.4, size = 0.5) +
    # x ticks
    geom_segment(data = tick_frame, aes(x = 0, xend = 0, y = 1.015, yend = 0.985)) +
    geom_segment(data = tick_frame, aes(x = 0.1, xend = 0.1, y = 1.015, yend = 1.01)) +
    geom_segment(data = tick_frame, aes(x = 0.1, xend = 0.1, y = 0.985, yend = 0.99)) +
    geom_segment(data = tick_frame, aes(x = 0.2, xend = 0.2, y = 1.015, yend = 1.01)) +
    geom_segment(data = tick_frame, aes(x = 0.2, xend = 0.2, y = 0.985, yend = 0.99)) +
    geom_segment(data = tick_frame, aes(x = 0.3, xend = 0.3, y = 1.015, yend = 1.01)) +
    geom_segment(data = tick_frame, aes(x = 0.3, xend = 0.3, y = 0.985, yend = 0.99)) +
    geom_segment(data = tick_frame, aes(x = 0.4, xend = 0.4, y = 1.015, yend = 0.985)) +
    # x tick labels
    geom_text(data = lab_frame, aes(x = lab, y = 0.984, label = lab),
              family = 'Helvetica', vjust = 1.5, size = 10) + 
    # data
    geom_point(aes(fill = scenario), size = 10, shape = 21, color = "black") +
    scale_y_continuous(limits = c(0.981, 1.016), expand = c(0, 0)) +
    scale_fill_manual(values = c("#2c7bb6", "#fdae61",  "#d7191c")) +
    labs(x = expression("Temperature Change (°C decade"^-1*")")) +
    theme_void() +
    theme(axis.title.x = element_text(size = 35),
          legend.position = "none",
          plot.margin = unit(c(0, -2, 0, -2), 'cm'))
  
  ggsave(paste0("figures/temperature-change-scalebar/", grp, ".png"), dpi = 300, width = 30, height = 8.5)
})

