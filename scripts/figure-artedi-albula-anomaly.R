#### CLEAR THE ENVIRONMENT FIRST ---------------------------------------------

rm(list = ls(all.names = TRUE))


#### LOAD PACKAGES -------------------------------------------------------------------------------

library(cowplot)
library(ggthemes)
library(ggtext)
library(gridExtra)
library(grid)
library(magick)
library(png)


#### LOAD DATA FROM OTHER SCRIPTS ----------------------------------------------------------------

source("scripts/lake-superior-apostle-islands-simulation-model.R")
source("scripts/lake-ontario-simulation-model.R")
source("scripts/lake-konnevesi-albula-simulation-model.R")


#### PASTE REGRESSION COEFFICIENTS TOGETHER TO MAKE EQUATIONS ------------------------------------

ls.eq <- simulation.anomaly.slope.LS.APIS %>% mutate(eq = paste0("y = ", sprintf("%.2f", slope), "x ", ifelse(intercept >= 0, "+ ", "- "), sprintf("%.2f", abs(intercept))),
                                                     R2 = sprintf("%.2f", R2))
lo.eq <- simulation.anomaly.slope.LO %>% mutate(eq = paste0("y = ", sprintf("%.2f", slope), "x ", ifelse(intercept >= 0, "+ ", "- "), sprintf("%.2f", abs(intercept))),
                                                R2 = sprintf("%.2f", R2))
lk.eq <- simulation.anomaly.slope.LK %>% mutate(eq = paste0("y = ", sprintf("%.2f", slope), "x ", ifelse(intercept >= 0, "+ ", "- "), sprintf("%.2f", abs(intercept))),
                                                R2 = sprintf("%.2f", R2))


#### CALCULATE TEMP SLOPE PER DECADE -------------------------------------------------------------

simulation.inc.temp.LS.APIS <- simulation.model.hatch.LS.APIS %>% group_by(scenario, year.class) %>% 
  summarize(inc.temp_c = mean(inc.temp_c)) %>% 
  mutate(group = "LS")
simulation.inc.temp.LO <- simulation.model.hatch.LO %>% group_by(scenario, year.class) %>% 
  summarize(inc.temp_c = mean(inc.temp_c)) %>% 
  mutate(group = "LO")
simulation.inc.temp.LK <- simulation.model.hatch.LK %>% group_by(scenario, year.class) %>% 
  summarize(inc.temp_c = mean(inc.temp_c)) %>% 
  mutate(group = "LK")

## Combine study groups
simulation.inc.temp <- bind_rows(simulation.inc.temp.LS.APIS, simulation.inc.temp.LO, simulation.inc.temp.LK) %>% 
  mutate(group = factor(group))

## Calculate slope for each climate scenario
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


#### VISUALIZATION - LAKE SUPERIOR (APIS) --------------------------------------------------------

plot.spawn.anomaly.LS.APIS <- ggplot(simulation.anomaly.LS.APIS, aes(x = year.class, y = mean.spawn.yday.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario), size = 1, alpha = 0.5) + 
  geom_smooth(data = filter(simulation.anomaly.LS.APIS, scenario != "Historical"), 
              aes(color = scenario), size = 1, method = "lm", se = FALSE, show.legend = FALSE) +
  #geom_richtext(data = filter(ls.eq, trait == "spawn", scenario == "RCP 2.6"), 
  #              aes(label = paste0("<span style='color:#2c7bb6'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
  #              x = 1905, y = 34.5, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  #geom_richtext(data = filter(ls.eq, trait == "spawn", scenario == "RCP 6.0"), 
  #              aes(label = paste0("<span style='color:#fdae61'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
  #              x = 1905, y = 30.2, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  #geom_richtext(data = filter(ls.eq, trait == "spawn", scenario == "RCP 8.5"), 
  #              aes(label = paste0("<span style='color:#d7191c'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
  #              x = 1905, y = 25.9, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  scale_y_continuous(limits = c(-7, 36), breaks = seq(-5, 35, 5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("gray50","#2c7bb6", "#fdae61",  "#d7191c")) +
  labs(y = "Spawning Date\nAnomaly (Days)") +
  guides(color = guide_legend(override.aes = list(alpha = 1)))+
  theme_few() +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        axis.title.x = element_blank(),
        #axis.title.y = element_text(size = 18, margin = margin(0, 10, 0, 0)),
        axis.title.y = element_blank(),
        #axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        #axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18),
        axis.ticks.length = unit(2, 'mm'),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.key.width = unit(1.5, 'cm'),
        legend.position = "none",
        legend.background = element_rect(fill = "transparent"),
        panel.border = element_rect(size = 1.5),
        plot.margin = unit(c(5.5, 2, 1, 2.5), 'mm'))

plot.dpf.anomaly.LS.APIS <- ggplot(simulation.anomaly.LS.APIS, aes(x = year.class, y = mean.dpf.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario), size = 1, alpha = 0.5) + 
  geom_smooth(data = filter(simulation.anomaly.LS.APIS, scenario != "Historical"), aes(color = scenario), size = 1, method = "lm", se = FALSE, show.legend = FALSE) +
  #geom_richtext(data = filter(ls.eq, trait == "dpf", scenario == "RCP 2.6"), 
  #              aes(label = paste0("<span style='color:#2c7bb6'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
  #              x = 1905, y = -28.7, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  #geom_richtext(data = filter(ls.eq, trait == "dpf", scenario == "RCP 6.0"), 
  #              aes(label = paste0("<span style='color:#fdae61'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
  #              x = 1905, y = -34.9, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  #geom_richtext(data = filter(ls.eq, trait == "dpf", scenario == "RCP 8.5"), 
  #              aes(label = paste0("<span style='color:#d7191c'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
  #              x = 1905, y = -41.1, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  scale_y_continuous(limits = c(-51, 11), breaks = seq(-50, 10, 10), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("gray50","#2c7bb6", "#fdae61",  "#d7191c")) +
  labs(y = "Incubation Duration\nAnomaly (Days)") +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        #axis.title.y = element_text(size = 18, margin = margin(0, 10, 0, 0)),
        axis.title.y = element_blank(),
        #axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        #axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18),
        axis.ticks.length = unit(2, 'mm'),
        legend.position = "none",
        panel.border = element_rect(size = 1.5),
        plot.margin = unit(c(5.5, 2, 1, 1), 'mm'))

plot.hatch.anomaly.LS.APIS <- ggplot(simulation.anomaly.LS.APIS, aes(x = year.class, y = mean.hatch.yday.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario), size = 1, alpha = 0.5) + 
  geom_smooth(data = filter(simulation.anomaly.LS.APIS, scenario != "Historical"), aes(color = scenario), size = 1, method = "lm", se = FALSE, show.legend = FALSE) +
  #geom_richtext(data = filter(ls.eq, trait == "hatch", scenario == "RCP 2.6"), 
  #              aes(label = paste0("<span style='color:#2c7bb6'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
  #              x = 1905, y = -10.5, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  #geom_richtext(data = filter(ls.eq, trait == "hatch", scenario == "RCP 6.0"), 
  #              aes(label = paste0("<span style='color:#fdae61'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
  #              x = 1905, y = -14.0, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  #geom_richtext(data = filter(ls.eq, trait == "hatch", scenario == "RCP 8.5"), 
  #              aes(label = paste0("<span style='color:#d7191c'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>", collapse = "")),
  #              x = 1905, y = -17.5, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  scale_y_continuous(limits = c(-23, 12), breaks = seq(-20, 10, 5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("gray50","#2c7bb6", "#fdae61",  "#d7191c")) +
  labs(y = "Hatching Date\nAnomaly (Days)") +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        #axis.title.y = element_text(size = 18, margin = margin(0, 10, 0, 0)),
        axis.title.y = element_blank(),
        #axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        #axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18),
        axis.ticks.length = unit(2, 'mm'),
        legend.position = "none",
        panel.border = element_rect(size = 1.5),
        plot.margin = unit(c(4.5, 2, 2, 1), 'mm'))


#### VISUALIZATION - LAKE ONTARIO ----------------------------------------------------------------

plot.spawn.anomaly.LO <- ggplot(simulation.anomaly.LO, aes(x = year.class, y = mean.spawn.yday.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario), size = 1, alpha = 0.5) + 
  geom_smooth(data = filter(simulation.anomaly.LO, scenario != "Historical"), aes(color = scenario), size = 1, method = "lm", se = FALSE, show.legend = FALSE) +
  #geom_richtext(data = filter(lo.eq, trait == "spawn", scenario == "RCP 2.6"), 
  #              aes(label = paste0("<span style='color:#2c7bb6'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
  #              x = 1905, y = 34.5, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  #geom_richtext(data = filter(lo.eq, trait == "spawn", scenario == "RCP 6.0"), 
  #              aes(label = paste0("<span style='color:#fdae61'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
  #              x = 1905, y = 30.2, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  #geom_richtext(data = filter(lo.eq, trait == "spawn", scenario == "RCP 8.5"), 
  #              aes(label = paste0("<span style='color:#d7191c'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
  #              x = 1905, y = 25.9, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  scale_y_continuous(limits = c(-7, 36), breaks = seq(-5, 35, 5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("gray50","#2c7bb6", "#fdae61",  "#d7191c")) +
  labs(y = "Spawning Date\nAnomaly (Days)") +
  theme_few() + 
  theme(axis.title = element_blank(),
        #axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        #axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18),
        axis.ticks.length = unit(2, 'mm'),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.position = "none",
        panel.border = element_rect(size = 1.5),
        plot.margin = unit(c(5.5, 3, 1, 13.5), 'mm'))

plot.dpf.anomaly.LO <- ggplot(simulation.anomaly.LO, aes(x = year.class, y = mean.dpf.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario), size = 1, alpha = 0.5) + 
  geom_smooth(data = filter(simulation.anomaly.LO, scenario != "Historical"), aes(color = scenario), size = 1, method = "lm", se = FALSE, show.legend = FALSE) +
  #geom_richtext(data = filter(lo.eq, trait == "dpf", scenario == "RCP 2.6"), 
  #              aes(label = paste0("<span style='color:#2c7bb6'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
  #              x = 1905, y = -28.7, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  #geom_richtext(data = filter(lo.eq, trait == "dpf", scenario == "RCP 6.0"), 
  #              aes(label = paste0("<span style='color:#fdae61'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
  #              x = 1905, y = -34.9, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  #geom_richtext(data = filter(lo.eq, trait == "dpf", scenario == "RCP 8.5"), 
  #              aes(label = paste0("<span style='color:#d7191c'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
  #              x = 1905, y = -41.1, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  scale_y_continuous(limits = c(-51, 11), breaks = seq(-50, 10, 10), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("gray50","#2c7bb6", "#fdae61",  "#d7191c")) +
  labs(y = "Incubation Duration\nAnomaly (Days)") +
  theme_few() + 
  theme(axis.title = element_blank(),
        #axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        #axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18),
        axis.ticks.length = unit(2, 'mm'),
        legend.position = "none",
        panel.border = element_rect(size = 1.5),
        plot.margin = unit(c(5.5, 3, 1, 12), 'mm'))

plot.hatch.anomaly.LO <- ggplot(simulation.anomaly.LO, aes(x = year.class, y = mean.hatch.yday.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario), size = 1, alpha = 0.5) + 
  geom_smooth(data = filter(simulation.anomaly.LO, scenario != "Historical"), aes(color = scenario), size = 1, method = "lm", se = FALSE, show.legend = FALSE) +
  #geom_richtext(data = filter(lo.eq, trait == "hatch", scenario == "RCP 2.6"), 
  #              aes(label = paste0("<span style='color:#2c7bb6'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
  #              x = 1905, y = -10.5, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  #geom_richtext(data = filter(lo.eq, trait == "hatch", scenario == "RCP 6.0"), 
  #              aes(label = paste0("<span style='color:#fdae61'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
  #              x = 1905, y = -14.0, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  #geom_richtext(data = filter(lo.eq, trait == "hatch", scenario == "RCP 8.5"), 
  #              aes(label = paste0("<span style='color:#d7191c'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
  #              x = 1905, y = -17.5, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  scale_y_continuous(limits = c(-23, 12), breaks = seq(-20, 10, 5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("gray50","#2c7bb6", "#fdae61",  "#d7191c")) +
  labs(y = "Hatching Date\nAnomaly (Days)") +
  theme_few() + 
  theme(axis.title = element_blank(),
        #axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        #axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18),
        axis.ticks.length = unit(2, 'mm'),
        legend.position = "none",
        panel.border = element_rect(size = 1.5),
        plot.margin = unit(c(4.5, 3, 2, 12), 'mm'))


#### VISUALIZATION - LAKE KONNEVESI --------------------------------------------------------------

plot.spawn.anomaly.LK <- ggplot(simulation.anomaly.LK, aes(x = year.class, y = mean.spawn.yday.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario), size = 1, alpha = 0.5) + 
  geom_smooth(data = filter(simulation.anomaly.LK, scenario != "Historical"), 
              aes(color = scenario), size = 1, method = "lm", se = FALSE, show.legend = FALSE) +
  #geom_richtext(data = filter(lk.eq, trait == "spawn", scenario == "RCP 2.6"), 
  #              aes(label = paste0("<span style='color:#2c7bb6'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
  #              x = 1905, y = 34.5, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  #geom_richtext(data = filter(lk.eq, trait == "spawn", scenario == "RCP 6.0"), 
  #              aes(label = paste0("<span style='color:#fdae61'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
  #              x = 1905, y = 30.2, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  #geom_richtext(data = filter(lk.eq, trait == "spawn", scenario == "RCP 8.5"), 
  #              aes(label = paste0("<span style='color:#d7191c'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
  #              x = 1905, y = 25.9, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  scale_y_continuous(limits = c(-7, 36), breaks = seq(-5, 35, 5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("gray50","#2c7bb6", "#fdae61",  "#d7191c")) +
  labs(y = "Spawning Date\nAnomaly (Days)") +
  theme_few() + 
  theme(axis.title = element_blank(),
        #axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        #axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18),
        axis.ticks.length = unit(2, 'mm'),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.position = "none",
        panel.border = element_rect(size = 1.5),
        plot.margin = unit(c(5.5, 3, 1, 13.5), 'mm'))

plot.dpf.anomaly.LK <- ggplot(simulation.anomaly.LK, aes(x = year.class, y = mean.dpf.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario), size = 1, alpha = 0.5) + 
  geom_smooth(data = filter(simulation.anomaly.LK, scenario != "Historical"), 
              aes(color = scenario), size = 1, method = "lm", se = FALSE, show.legend = FALSE) +
  #geom_richtext(data = filter(lk.eq, trait == "dpf", scenario == "RCP 2.6"), 
  #              aes(label = paste0("<span style='color:#2c7bb6'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
  #              x = 1905, y = -28.7, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  #geom_richtext(data = filter(lk.eq, trait == "dpf", scenario == "RCP 6.0"), 
  #              aes(label = paste0("<span style='color:#fdae61'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
  #              x = 1905, y = -34.9, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  #geom_richtext(data = filter(lk.eq, trait == "dpf", scenario == "RCP 8.5"), 
  #              aes(label = paste0("<span style='color:#d7191c'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
  #              x = 1905, y = -41.1, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  scale_y_continuous(limits = c(-51, 11), breaks = seq(-50, 10, 10), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("gray50","#2c7bb6", "#fdae61",  "#d7191c")) +
  labs(y = "Incubation Duration\nAnomaly (Days)") +
  theme_few() + 
  theme(axis.title = element_blank(),
        #axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        #axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18),
        axis.ticks.length = unit(2, 'mm'),
        legend.position = "none",
        panel.border = element_rect(size = 1.5),
        plot.margin = unit(c(5.5, 3, 1, 12), 'mm'))

plot.hatch.anomaly.LK <- ggplot(simulation.anomaly.LK, aes(x = year.class, y = mean.hatch.yday.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario), size = 1, alpha = 0.5) + 
  geom_smooth(data = filter(simulation.anomaly.LK, scenario != "Historical"), 
              aes(color = scenario), size = 1, method = "lm", se = FALSE, show.legend = FALSE) +
  #geom_richtext(data = filter(lk.eq, trait == "hatch", scenario == "RCP 2.6"), 
  #              aes(label = paste0("<span style='color:#2c7bb6'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
  #              x = 1905, y = -10.5, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  #geom_richtext(data = filter(lk.eq, trait == "hatch", scenario == "RCP 6.0"), 
  #              aes(label = paste0("<span style='color:#fdae61'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
  #              x = 1905, y = -14.0, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  #geom_richtext(data = filter(lk.eq, trait == "hatch", scenario == "RCP 8.5"), 
  #              aes(label = paste0("<span style='color:#d7191c'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
  #              x = 1905, y = -17.5, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  scale_y_continuous(limits = c(-23, 12), breaks = seq(-20, 10, 5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("gray50","#2c7bb6", "#fdae61",  "#d7191c")) +
  labs(y = "Hatching Date\nAnomaly (Days)") +
  theme_few() + 
  theme(axis.title = element_blank(),
        #axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        #axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18),
        axis.ticks.length = unit(2, 'mm'),
        legend.position = "none",
        panel.border = element_rect(size = 1.5),
        plot.margin = unit(c(4.5, 3, 2, 12), 'mm'))


#### VISUALIZATIONS - TEMP CHANGE SCALE BAR ------------------------------------------------------

## Lake Superior Apostle Islands
temp.change.LS <- ggplot(simulation.inc.temp.slope %>% filter(group == "LS"), aes(x = decade.slope, y = 1)) +
  # x axis line
  geom_segment(y = 1, yend = 1, x = -0.0004, xend = 0.2504, size = 0.5) +
  geom_segment(y = 0.99, yend = 0.99, x = -0.0004, xend = 0.2504, size = 0.5) +
  geom_segment(y = 1.01, yend = 1.01, x = -0.0004, xend = 0.2504, size = 0.5) +
  # x ticks
  geom_segment(data = data.frame(ticks = seq(0, 0.25, length.out = 6)),
               aes(x = 0, xend = 0, y = 1.01, yend = 0.99), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.25, length.out = 6)),
               aes(x = 0.05, xend = 0.05, y = 1.005, yend = 1.01), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.25, length.out = 6)),
               aes(x = 0.05, xend = 0.05, y = 0.995, yend = 0.99), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.25, length.out = 6)),
               aes(x = 0.1, xend = 0.1, y = 1.005, yend = 1.01), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.25, length.out = 6)),
               aes(x = 0.1, xend = 0.1, y = 0.995, yend = 0.99), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.25, length.out = 6)),
               aes(x = 0.15, xend = 0.15, y = 1.005, yend = 1.01), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.25, length.out = 6)),
               aes(x = 0.15, xend = 0.15, y = 0.995, yend = 0.99), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.25, length.out = 6)),
               aes(x = 0.2, xend = 0.2, y = 1.005, yend = 1.01), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.25, length.out = 6)),
               aes(x = 0.2, xend = 0.2, y = 0.995, yend = 0.99), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.25, length.out = 6)),
               aes(x = 0.25, xend = 0.25, y = 1.01, yend = 0.99), size = 0.5) +
  # x tick labels
  geom_text(data = data.frame(lab = seq(0, 0.25, 0.05)),
            aes(x = lab, y = 0.99, label = lab),
            family = 'Helvetica', vjust = 1.5, size = 5) + 
  # data
  geom_point(aes(fill = scenario), size = 5, shape = 21, color = "black") +
  scale_y_continuous(limits = c(0.975, 1.013), expand = c(0, 0)) +
  scale_fill_manual(values = c("#2c7bb6", "#fdae61",  "#d7191c")) +
  labs(x = expression("Temperature Change (°C decade"^-1*")"), title = "Lake Superior - Cisco") +
  theme_void() +
  theme(axis.title.x = element_text(size = 16),
        legend.position = "none",
        plot.title = element_text(size = 18, hjust = 0.5, margin = margin(0, 0, 5, 0)),
        plot.margin = unit(c(0, 0.22, 0, 2.5), 'cm'))

## Lake Ontario
temp.change.LO <- ggplot(simulation.inc.temp.slope %>% filter(group == "LO"), aes(x = decade.slope, y = 1)) +
  # x axis line
  geom_segment(y = 1, yend = 1, x = -0.0004, xend = 0.2504, size = 0.5) +
  geom_segment(y = 0.99, yend = 0.99, x = -0.0004, xend = 0.2504, size = 0.5) +
  geom_segment(y = 1.01, yend = 1.01, x = -0.0004, xend = 0.2504, size = 0.5) +
  # x ticks
  geom_segment(data = data.frame(ticks = seq(0, 0.25, length.out = 6)),
               aes(x = 0, xend = 0, y = 1.01, yend = 0.99), size = 0.75) +
  geom_segment(data = data.frame(ticks = seq(0, 0.25, length.out = 6)),
               aes(x = 0.05, xend = 0.05, y = 1.005, yend = 1.01), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.25, length.out = 6)),
               aes(x = 0.05, xend = 0.05, y = 0.995, yend = 0.99), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.25, length.out = 6)),
               aes(x = 0.1, xend = 0.1, y = 1.005, yend = 1.01), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.25, length.out = 6)),
               aes(x = 0.1, xend = 0.1, y = 0.995, yend = 0.99), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.25, length.out = 6)),
               aes(x = 0.15, xend = 0.15, y = 1.005, yend = 1.01), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.25, length.out = 6)),
               aes(x = 0.15, xend = 0.15, y = 0.995, yend = 0.99), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.25, length.out = 6)),
               aes(x = 0.2, xend = 0.2, y = 1.005, yend = 1.01), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.25, length.out = 6)),
               aes(x = 0.2, xend = 0.2, y = 0.995, yend = 0.99), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.25, length.out = 6)),
               aes(x = 0.25, xend = 0.25, y = 1.01, yend = 0.99), size = 0.75) +
  # x tick labels
  geom_text(data = data.frame(lab = seq(0, 0.25, 0.05)),
            aes(x = lab, y = 0.99, label = lab),
            family = 'Helvetica', vjust = 1.5, size = 5) + 
  # data
  geom_point(aes(fill = scenario), size = 5, shape = 21, color = "black") +
  scale_y_continuous(limits = c(0.975, 1.013), expand = c(0, 0)) +
  scale_fill_manual(values = c("#2c7bb6", "#fdae61",  "#d7191c")) +
  labs(x = expression("Temperature Change (°C decade"^-1*")"), title = "Lake Ontario - Cisco") +
  theme_void() +
  theme(axis.title.x = element_text(size = 16),
        legend.position = "none",
        plot.title = element_text(size = 18, hjust = 0.5, margin = margin(0, 0, 5, 0)),
        plot.margin = unit(c(0, 0.2, 0, 2.2), 'cm'))

## Lake S. Konnevesi
temp.change.LK <- ggplot(simulation.inc.temp.slope %>% filter(group == "LK"), aes(x = decade.slope, y = 1)) +
  # x axis line
  geom_segment(y = 1, yend = 1, x = -0.0004, xend = 0.2504, size = 0.5) +
  geom_segment(y = 0.99, yend = 0.99, x = -0.0004, xend = 0.2504, size = 0.5) +
  geom_segment(y = 1.01, yend = 1.01, x = -0.0004, xend = 0.2504, size = 0.5) +
  # x ticks
  geom_segment(data = data.frame(ticks = seq(0, 0.25, length.out = 6)),
               aes(x = 0, xend = 0, y = 1.01, yend = 0.99), size = 0.75) +
  geom_segment(data = data.frame(ticks = seq(0, 0.25, length.out = 6)),
               aes(x = 0.05, xend = 0.05, y = 1.005, yend = 1.01), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.25, length.out = 6)),
               aes(x = 0.05, xend = 0.05, y = 0.995, yend = 0.99), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.25, length.out = 6)),
               aes(x = 0.1, xend = 0.1, y = 1.005, yend = 1.01), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.25, length.out = 6)),
               aes(x = 0.1, xend = 0.1, y = 0.995, yend = 0.99), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.25, length.out = 6)),
               aes(x = 0.15, xend = 0.15, y = 1.005, yend = 1.01), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.25, length.out = 6)),
               aes(x = 0.15, xend = 0.15, y = 0.995, yend = 0.99), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.25, length.out = 6)),
               aes(x = 0.2, xend = 0.2, y = 1.005, yend = 1.01), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.25, length.out = 6)),
               aes(x = 0.2, xend = 0.2, y = 0.995, yend = 0.99), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.25, length.out = 6)),
               aes(x = 0.25, xend = 0.25, y = 1.01, yend = 0.99), size = 0.75) +
  # x tick labels
  geom_text(data = data.frame(lab = seq(0, 0.25, 0.05)),
            aes(x = lab, y = 0.99, label = lab),
            family = 'Helvetica', vjust = 1.5, size = 5) + 
  # data
  geom_point(aes(fill = scenario), size = 5, shape = 21, color = "black") +
  scale_y_continuous(limits = c(0.975, 1.013), expand = c(0, 0)) +
  scale_fill_manual(values = c("#2c7bb6", "#fdae61",  "#d7191c")) +
  labs(x = expression("Temperature Change (°C decade"^-1*")"), title = "Lake S. Konnevesi - Vendace") +
  theme_void() +
  theme(axis.title.x = element_text(size = 16),
        legend.position = "none",
        plot.title = element_text(size = 18, hjust = 0.5, margin = margin(0, 0, 5, 0)),
        plot.margin = unit(c(0, 0.2, 0, 2.2), 'cm'))


#### VISUALIZATION - COMBINE ALL POPULATIONS -----------------------------------------------------

plot.all <- grid.arrange(
  arrangeGrob(textGrob(""),
              get_legend(plot.spawn.anomaly.LS.APIS + theme(legend.position = "top")),
              ncol = 2,
              widths = c(0.045, 1)
  ),
  arrangeGrob(temp.change.LS, temp.change.LO, temp.change.LK, 
              ncol = 3),
  arrangeGrob(
    arrangeGrob(plot.spawn.anomaly.LS.APIS,
                plot.dpf.anomaly.LS.APIS,
                plot.hatch.anomaly.LS.APIS,
                ncol = 1),
    arrangeGrob(plot.spawn.anomaly.LO,
                plot.dpf.anomaly.LO,
                plot.hatch.anomaly.LO,
                ncol = 1),
    arrangeGrob(plot.spawn.anomaly.LK,
                plot.dpf.anomaly.LK,
                plot.hatch.anomaly.LK,
                ncol = 1),
    ncol = 3,
    widths = c(1, 0.98, 0.98)),
    #bottom = textGrob("Year", x = 0.525, gp = gpar(cex = 1.6, fontfamily = "Arial"))),
  nrow = 3,
  heights = c(0.075, 0.115, 1)
)


ggsave("figures/artedi-albula-simulation-anomaly-ForDefense.png", plot = plot.all, width = 19.5, height = 12.5, dpi = 300)

