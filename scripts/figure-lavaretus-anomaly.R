#### CLEAR THE ENVIRONMENT FIRST ---------------------------------------------

rm(list = ls(all.names = TRUE))


#### LOAD PACKAGES -------------------------------------------------------------------------------

library(cowplot)
library(ggthemes)
library(ggtext)
library(gridExtra)
library(grid)


#### LOAD DATA FROM OTHER SCRIPTS ----------------------------------------------------------------

source("scripts/lake-constance-simulation-model.R")
source("scripts/lake-annecy-simulation-model.R")


#### PASTE REGRESSION COEFFICIENTS TOGETHER TO MAKE EQUATIONS ------------------------------------

lc.eq <- simulation.anomaly.slope.LC %>% mutate(eq = paste0("y = ", sprintf("%.2f", slope), "x ", ifelse(intercept >= 0, "+ ", "- "), sprintf("%.2f", abs(intercept))),
                                                R2 = sprintf("%.2f", R2))
la.eq <- simulation.anomaly.slope.LA %>% mutate(eq = paste0("y = ", sprintf("%.2f", slope), "x ", ifelse(intercept >= 0, "+ ", "- "), sprintf("%.2f", abs(intercept))),
                                                R2 = sprintf("%.2f", R2))


#### CALCULATE TEMP SLOPE PER DECADE -------------------------------------------------------------

simulation.inc.temp.LA <- simulation.model.hatch.LA %>% group_by(scenario, year.class) %>% 
  summarize(inc.temp_c = mean(inc.temp_c)) %>% 
  mutate(group = "LA")
simulation.inc.temp.LC <- simulation.model.hatch.LC %>% group_by(scenario, year.class) %>% 
  summarize(inc.temp_c = mean(inc.temp_c)) %>% 
  mutate(group = "LC")

## Combine study groups
simulation.inc.temp <- bind_rows(simulation.inc.temp.LA, simulation.inc.temp.LC) %>% 
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


#### VISUALIZATION - LAKE CONSTANCE --------------------------------------------------------------

plot.spawn.anomaly.LC <- ggplot(simulation.anomaly.LC, aes(x = year.class, y = mean.spawn.yday.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario), size = 1, alpha = 0.5) + 
  geom_smooth(data = filter(simulation.anomaly.LC, scenario != "Historical"), 
              aes(color = scenario), size = 1, method = "lm", se = FALSE, show.legend = FALSE) +
  geom_richtext(data = filter(lc.eq, trait == "spawn", scenario == "RCP 2.6"), 
                aes(label = paste0("<span style='color:#2c7bb6'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
                x = 1905, y = 62.4, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(lc.eq, trait == "spawn", scenario == "RCP 6.0"), 
                aes(label = paste0("<span style='color:#fdae61'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
                x = 1905, y = 54.4, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(lc.eq, trait == "spawn", scenario == "RCP 8.5"), 
                aes(label = paste0("<span style='color:#d7191c'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
                x = 1905, y = 46.4, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  scale_y_continuous(limits = c(-15, 65), breaks = seq(-10, 60, 10), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("gray50","#2c7bb6", "#fdae61",  "#d7191c")) +
  labs(y = "Spawning Date\nAnomaly (Days)") +
  guides(color = guide_legend(override.aes = list(alpha = 1)))+
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 18, margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 15),
        axis.ticks.length = unit(2, 'mm'),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.key.width = unit(1.5, 'cm'),
        legend.position = "none",
        legend.background = element_rect(fill = "transparent"),
        panel.border = element_rect(size = 1.5),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.margin = unit(c(5.5, 2, 1, 2.5), 'mm'))

plot.dpf.anomaly.LC <- ggplot(simulation.anomaly.LC, aes(x = year.class, y = mean.dpf.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario), size = 1, alpha = 0.5) + 
  geom_smooth(data = filter(simulation.anomaly.LC, scenario != "Historical"), aes(color = scenario), size = 1, method = "lm", se = FALSE, show.legend = FALSE) +
  geom_richtext(data = filter(lc.eq, trait == "dpf", scenario == "RCP 2.6"), 
                aes(label = paste0("<span style='color:#2c7bb6'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
                x = 1905, y = -12.4, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(lc.eq, trait == "dpf", scenario == "RCP 6.0"), 
                aes(label = paste0("<span style='color:#fdae61'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
                x = 1905, y = -16.1, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(lc.eq, trait == "dpf", scenario == "RCP 8.5"), 
                aes(label = paste0("<span style='color:#d7191c'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
                x = 1905, y = -19.8, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  scale_y_continuous(limits = c(-26, 11), breaks = seq(-25, 10, 5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("gray50","#2c7bb6", "#fdae61",  "#d7191c")) +
  labs(y = "Incubation Length\nAnomaly (Days)") +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 18, margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 15),
        axis.ticks.length = unit(2, 'mm'),
        legend.position = "none",
        panel.border = element_rect(size = 1.5),
        plot.margin = unit(c(5.5, 2, 1, 1), 'mm'))

plot.hatch.anomaly.LC <- ggplot(simulation.anomaly.LC, aes(x = year.class, y = mean.hatch.yday.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario), size = 1, alpha = 0.5) + 
  geom_smooth(data = filter(simulation.anomaly.LC, scenario != "Historical"), aes(color = scenario), size = 1, method = "lm", se = FALSE, show.legend = FALSE) +
  geom_richtext(data = filter(lc.eq, trait == "hatch", scenario == "RCP 2.6"), 
                aes(label = paste0("<span style='color:#2c7bb6'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
                x = 1905, y = 39.9, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(lc.eq, trait == "hatch", scenario == "RCP 6.0"), 
                aes(label = paste0("<span style='color:#fdae61'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
                x = 1905, y = 33.7, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(lc.eq, trait == "hatch", scenario == "RCP 8.5"), 
                aes(label = paste0("<span style='color:#d7191c'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
                x = 1905, y = 27.5, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  scale_y_continuous(limits = c(-20, 42), breaks = seq(-20, 40, 10), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("gray50","#2c7bb6", "#fdae61",  "#d7191c")) +
  labs(y = "Hatching Date\nAnomaly (Days)") +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 18, margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 15),
        axis.ticks.length = unit(2, 'mm'),
        legend.position = "none",
        panel.border = element_rect(size = 1.5),
        plot.margin = unit(c(4.5, 2, 2, 1), 'mm'))


#### VISUALIZATION - LAKE ANNECY -----------------------------------------------------------------

plot.spawn.anomaly.LA <- ggplot(simulation.anomaly.LA, aes(x = year.class, y = mean.spawn.yday.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario), size = 1, alpha = 0.5) + 
  geom_smooth(data = filter(simulation.anomaly.LA, scenario != "Historical"), 
              aes(color = scenario), size = 1, method = "lm", se = FALSE, show.legend = FALSE) +
  geom_richtext(data = filter(la.eq, trait == "spawn", scenario == "RCP 2.6"), 
                aes(label = paste0("<span style='color:#2c7bb6'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
                x = 1905, y = 62.4, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(la.eq, trait == "spawn", scenario == "RCP 6.0"), 
                aes(label = paste0("<span style='color:#fdae61'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
                x = 1905, y = 54.4, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(la.eq, trait == "spawn", scenario == "RCP 8.5"), 
                aes(label = paste0("<span style='color:#d7191c'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
                x = 1905, y = 46.4, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  scale_y_continuous(limits = c(-15, 65), breaks = seq(-10, 60, 10), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("gray50","#2c7bb6", "#fdae61",  "#d7191c")) +
  labs(y = "Spawning Date\nAnomaly (Days)") +
  theme_few() + 
  theme(axis.title = element_blank(),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 15),
        axis.ticks.length = unit(2, 'mm'),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.position = "none",
        panel.border = element_rect(size = 1.5),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.margin = unit(c(5.5, 3, 1, 13.5), 'mm'))

plot.dpf.anomaly.LA <- ggplot(simulation.anomaly.LA, aes(x = year.class, y = mean.dpf.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario), size = 1, alpha = 0.5) + 
  geom_smooth(data = filter(simulation.anomaly.LA, scenario != "Historical"), 
              aes(color = scenario), size = 1, method = "lm", se = FALSE, show.legend = FALSE) +
  geom_richtext(data = filter(la.eq, trait == "dpf", scenario == "RCP 2.6"), 
                aes(label = paste0("<span style='color:#2c7bb6'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
                x = 1905, y = -12.4, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(la.eq, trait == "dpf", scenario == "RCP 6.0"), 
                aes(label = paste0("<span style='color:#fdae61'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
                x = 1905, y = -16.1, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(la.eq, trait == "dpf", scenario == "RCP 8.5"), 
                aes(label = paste0("<span style='color:#d7191c'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
                x = 1905, y = -19.8, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  scale_y_continuous(limits = c(-26, 11), breaks = seq(-25, 10, 5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("gray50","#2c7bb6", "#fdae61",  "#d7191c")) +
  labs(y = "Incubation Length\nAnomaly (Days)") +
  theme_few() + 
  theme(axis.title = element_blank(),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 15),
        axis.ticks.length = unit(2, 'mm'),
        legend.position = "none",
        panel.border = element_rect(size = 1.5),
        plot.margin = unit(c(5.5, 3, 1, 12), 'mm'))

plot.hatch.anomaly.LA <- ggplot(simulation.anomaly.LA, aes(x = year.class, y = mean.hatch.yday.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario), size = 1, alpha = 0.5) + 
  geom_smooth(data = filter(simulation.anomaly.LA, scenario != "Historical"), 
              aes(color = scenario), size = 1, method = "lm", se = FALSE, show.legend = FALSE) +
  geom_richtext(data = filter(la.eq, trait == "hatch", scenario == "RCP 2.6"), 
                aes(label = paste0("<span style='color:#2c7bb6'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
                x = 1905, y = 39.9, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(la.eq, trait == "hatch", scenario == "RCP 6.0"), 
                aes(label = paste0("<span style='color:#fdae61'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
                x = 1905, y = 33.7, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(la.eq, trait == "hatch", scenario == "RCP 8.5"), 
                aes(label = paste0("<span style='color:#d7191c'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
                x = 1905, y = 27.5, size = 5.5, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  scale_y_continuous(limits = c(-20, 42), breaks = seq(-20, 40, 10), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("gray50","#2c7bb6", "#fdae61",  "#d7191c")) +
  labs(y = "Hatching Date\nAnomaly (Days)") +
  theme_few() + 
  theme(axis.title = element_blank(),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 15),
        axis.ticks.length = unit(2, 'mm'),
        legend.position = "none",
        panel.border = element_rect(size = 1.5),
        plot.margin = unit(c(4.5, 3, 2, 12), 'mm'))


#### VISUALIZATIONS - TEMP CHANGE SCALE BAR ------------------------------------------------------

## Lake Annecy
temp.change.LA <- ggplot(simulation.inc.temp.slope %>% filter(group == "LA"), aes(x = decade.slope, y = 1)) +
  # x axis line
  geom_segment(y = 1, yend = 1, x = -0.0004, xend = 0.4004, size = 0.5) +
  geom_segment(y = 0.99, yend = 0.99, x = -0.0004, xend = 0.4004, size = 0.5) +
  geom_segment(y = 1.01, yend = 1.01, x = -0.0004, xend = 0.4004, size = 0.5) +
  # x ticks
  geom_segment(data = data.frame(ticks = seq(0, 0.4, length.out = 5)),
               aes(x = 0, xend = 0, y = 1.01, yend = 0.99), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.4, length.out = 5)),
               aes(x = 0.1, xend = 0.1, y = 1.005, yend = 1.01), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.4, length.out = 5)),
               aes(x = 0.1, xend = 0.1, y = 0.995, yend = 0.99), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.4, length.out = 5)),
               aes(x = 0.2, xend = 0.2, y = 1.005, yend = 1.01), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.4, length.out = 5)),
               aes(x = 0.2, xend = 0.2, y = 0.995, yend = 0.99), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.4, length.out = 5)),
               aes(x = 0.3, xend = 0.3, y = 1.005, yend = 1.01), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.4, length.out = 5)),
               aes(x = 0.3, xend = 0.3, y = 0.995, yend = 0.99), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.4, length.out = 5)),
               aes(x = 0.4, xend = 0.4, y = 1.01, yend = 0.99), size = 0.5) +
  # x tick labels
  geom_text(data = data.frame(lab = seq(0, 0.4, 0.1)),
            aes(x = lab, y = 0.99, label = lab),
            family = 'Helvetica', vjust = 1.5, size = 5) + 
  # data
  geom_point(aes(fill = scenario), size = 5, shape = 21, color = "black") +
  scale_y_continuous(limits = c(0.975, 1.013), expand = c(0, 0)) +
  scale_fill_manual(values = c("#2c7bb6", "#fdae61",  "#d7191c")) +
  labs(x = expression("Temperature Change (°C decade"^-1*")"), title = "Lake Annecy - Whitefish") +
  theme_void() +
  theme(axis.title.x = element_text(size = 16),
        legend.position = "none",
        plot.title = element_text(size = 18, hjust = 0.5, margin = margin(0, 0, 5, 0)),
        plot.margin = unit(c(0, 0.22, 0, 2.5), 'cm'))

## Lake Constance
temp.change.LC <- ggplot(simulation.inc.temp.slope %>% filter(group == "LC"), aes(x = decade.slope, y = 1)) +
  # x axis line
  geom_segment(y = 1, yend = 1, x = -0.0004, xend = 0.4004, size = 0.5) +
  geom_segment(y = 0.99, yend = 0.99, x = -0.0004, xend = 0.4004, size = 0.5) +
  geom_segment(y = 1.01, yend = 1.01, x = -0.0004, xend = 0.4004, size = 0.5) +
  # x ticks
  geom_segment(data = data.frame(ticks = seq(0, 0.4, length.out = 5)),
               aes(x = 0, xend = 0, y = 1.01, yend = 0.99), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.4, length.out = 5)),
               aes(x = 0.1, xend = 0.1, y = 1.005, yend = 1.01), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.4, length.out = 5)),
               aes(x = 0.1, xend = 0.1, y = 0.995, yend = 0.99), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.4, length.out = 5)),
               aes(x = 0.2, xend = 0.2, y = 1.005, yend = 1.01), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.4, length.out = 5)),
               aes(x = 0.2, xend = 0.2, y = 0.995, yend = 0.99), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.4, length.out = 5)),
               aes(x = 0.3, xend = 0.3, y = 1.005, yend = 1.01), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.4, length.out = 5)),
               aes(x = 0.3, xend = 0.3, y = 0.995, yend = 0.99), size = 0.5) +
  geom_segment(data = data.frame(ticks = seq(0, 0.4, length.out = 5)),
               aes(x = 0.4, xend = 0.4, y = 1.01, yend = 0.99), size = 0.5) +
  # x tick labels
  geom_text(data = data.frame(lab = seq(0, 0.4, 0.1)),
            aes(x = lab, y = 0.99, label = lab),
            family = 'Helvetica', vjust = 1.5, size = 5) + 
  # data
  geom_point(aes(fill = scenario), size = 5, shape = 21, color = "black") +
  scale_y_continuous(limits = c(0.975, 1.013), expand = c(0, 0)) +
  scale_fill_manual(values = c("#2c7bb6", "#fdae61",  "#d7191c")) +
  labs(x = expression("Temperature Change (°C decade"^-1*")"), title = "Lake Constance - Whitefish") +
  theme_void() +
  theme(axis.title.x = element_text(size = 16),
        legend.position = "none",
        plot.title = element_text(size = 18, hjust = 0.5, margin = margin(0, 0, 5, 0)),
        plot.margin = unit(c(0, 0.22, 0, 2.5), 'cm'))


#### VISUALIZATION - COMBINE ALL POPULATIONS -----------------------------------------------------

plot.all <- grid.arrange(
  arrangeGrob(textGrob(""),
              get_legend(plot.spawn.anomaly.LC + theme(legend.position = "top")),
              ncol = 2,
              widths = c(0.045, 1)
  ),
  arrangeGrob(temp.change.LC, temp.change.LA,
              ncol = 2),
  arrangeGrob(
    arrangeGrob(plot.spawn.anomaly.LC,
                plot.dpf.anomaly.LC,
                plot.hatch.anomaly.LC,
                ncol = 1),
    arrangeGrob(plot.spawn.anomaly.LA,
                plot.dpf.anomaly.LA,
                plot.hatch.anomaly.LA,
                ncol = 1),
    ncol = 2,
    widths = c(1, 0.98),
    bottom = textGrob("Year", x = 0.525, gp = gpar(cex = 1.6, fontfamily = "Arial"))),
  nrow = 3,
  heights = c(0.075, 0.115, 1)
)

ggsave("figures/lavaretus-simulation-anomaly.png", plot = plot.all, width = 13, height = 12.5, dpi = 300)

