#### CLEAR THE ENVIRONMENT FIRST ---------------------------------------------

#rm(list = ls(all.names = TRUE))


source("scripts/lake-superior-apostle-islands/lake-superior-apostle-islands-simulation-model.R")
source("scripts/lake-ontario/lake-ontario-simulation-model.R")


#### VISUALIZATIONS ------------------------------------------------------------------------------

## Lake Superior - Apostle Islands
plot.spawn.anomaly.LS.APIS <- ggplot(simulation.anomaly.LS.APIS, aes(x = year.class, y = mean.spawn.yday.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario, linetype = scenario),
            size = 1, alpha = 0.9) + 
  scale_y_continuous(limits = c(-7, 36), breaks = seq(-5, 35, 5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("gray50","#2c7bb6", "#fdae61",  "#d7191c")) +
  scale_linetype_manual(values = c("solid", "longdash", "twodash", "solid")) +
  labs(y = "Spawning Date\nAnomaly (Days)", title = "Lake Superior (Apostle Islands)") +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 17, margin = margin(0, 10, 0, 0)),
        axis.text = element_text(size = 14),
        axis.ticks.length = unit(2, 'mm'),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.key.width = unit(1.8, 'cm'),
        legend.position = c(0.16, 0.81),
        legend.background = element_rect(fill = "transparent"),
        panel.border = element_rect(size = 1.5),
        plot.title = element_text(hjust = 0.5),
        plot.margin = unit(c(2.5, 5, 2, 2), 'mm'))

plot.dpf.anomaly.LS.APIS <- ggplot(simulation.anomaly.LS.APIS, aes(x = year.class, y = mean.dpf.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario, linetype = scenario),
            size = 1, alpha = 0.9) + 
  scale_y_continuous(limits = c(-51, 11), breaks = seq(-50, 10, 10), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("gray50","#2c7bb6", "#fdae61",  "#d7191c")) +
  scale_linetype_manual(values = c("solid", "longdash", "twodash", "solid")) +
  labs(y = "Incubation Length\nAnomaly (Days)") +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 17, margin = margin(0, 10, 0, 0)),
        axis.text = element_text(size = 14),
        axis.ticks.length = unit(2, 'mm'),
        legend.position = "none",
        panel.border = element_rect(size = 1.5),
        plot.margin = unit(c(5, 5, 5, 2), 'mm'))

plot.hatch.anomaly.LS.APIS <- ggplot(simulation.anomaly.LS.APIS, aes(x = year.class, y = mean.hatch.yday.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario, linetype = scenario),
            size = 1, alpha = 0.9) + 
  scale_y_continuous(limits = c(-21, 12), breaks = seq(-20, 10, 5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("gray50","#2c7bb6", "#fdae61",  "#d7191c")) +
  scale_linetype_manual(values = c("solid", "longdash", "twodash", "solid")) +
  labs(y = "Hatching Date\nAnomaly (Days)") +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 17, margin = margin(0, 10, 0, 0)),
        axis.text = element_text(size = 14),
        axis.ticks.length = unit(2, 'mm'),
        legend.position = "none",
        panel.border = element_rect(size = 1.5),
        plot.margin = unit(c(5, 5, 5, 2), 'mm'))

## Lake Ontario
plot.spawn.anomaly.LO <- ggplot(simulation.anomaly.LO, aes(x = year.class, y = mean.spawn.yday.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario, linetype = scenario),
            size = 1, alpha = 0.9) + 
  scale_y_continuous(limits = c(-7, 36), breaks = seq(-5, 35, 5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("gray50","#2c7bb6", "#fdae61",  "#d7191c")) +
  scale_linetype_manual(values = c("solid", "longdash", "twodash", "solid")) +
  labs(y = "Spawning Date\nAnomaly (Days)", title = "Lake Ontario") +
  theme_few() + 
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 14),
        axis.ticks.length = unit(2, 'mm'),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.position = "none",
        panel.border = element_rect(size = 1.5),
        plot.title = element_text(hjust = 0.5),
        plot.margin = unit(c(2.5, 10, 2, 10), 'mm'))

plot.dpf.anomaly.LO <- ggplot(simulation.anomaly.LO, aes(x = year.class, y = mean.dpf.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario, linetype = scenario),
            size = 1, alpha = 0.9) + 
  scale_y_continuous(limits = c(-51, 11), breaks = seq(-50, 10, 10), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("gray50","#2c7bb6", "#fdae61",  "#d7191c")) +
  scale_linetype_manual(values = c("solid", "longdash", "twodash", "solid")) +
  labs(y = "Incubation Length\nAnomaly (Days)") +
  theme_few() + 
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 14),
        axis.ticks.length = unit(2, 'mm'),
        legend.position = "none",
        panel.border = element_rect(size = 1.5),
        plot.margin = unit(c(5, 10, 5, 10), 'mm'))

plot.hatch.anomaly.LO <- ggplot(simulation.anomaly.LO, aes(x = year.class, y = mean.hatch.yday.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario, linetype = scenario),
            size = 1, alpha = 0.9) + 
  scale_y_continuous(limits = c(-21, 12), breaks = seq(-20, 10, 5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("gray50","#2c7bb6", "#fdae61",  "#d7191c")) +
  scale_linetype_manual(values = c("solid", "longdash", "twodash", "solid")) +
  labs(y = "Hatching Date\nAnomaly (Days)") +
  theme_few() + 
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 14),
        axis.ticks.length = unit(2, 'mm'),
        legend.position = "none",
        panel.border = element_rect(size = 1.5),
        plot.margin = unit(c(5, 10, 5, 10), 'mm'))


## Combine both populations
plot.all <- grid.arrange(
  arrangeGrob(plot.spawn.anomaly.LS.APIS,
              plot.dpf.anomaly.LS.APIS,
              plot.hatch.anomaly.LS.APIS,
              ncol = 1),
  arrangeGrob(plot.spawn.anomaly.LO,
              plot.dpf.anomaly.LO,
              plot.hatch.anomaly.LO,
              ncol = 1),
  ncol = 2,
  widths = c(1, 0.98)
)

ggsave("figures/artedi-simulation-anomaly.png", plot = plot.all, width = 15, height = 12, dpi = 300)






simulation.anomaly.LS.APIS.noHist <- simulation.anomaly.LS.APIS %>% filter(scenario != "Historical")
simulation.anomaly.LO.noHist <- simulation.anomaly.LO %>% filter(scenario != "Historical")

## Lake Superior - Apostle Islands
plot.spawn.anomaly.LS.APIS.noHist <- ggplot(simulation.anomaly.LS.APIS.noHist, aes(x = year.class, y = mean.spawn.yday.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario, linetype = scenario),
            size = 1, alpha = 0.9) + 
  scale_y_continuous(limits = c(-7, 36), breaks = seq(-5, 35, 5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(2007, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("#2c7bb6", "#fdae61",  "#d7191c")) +
  scale_linetype_manual(values = c("twodash", "longdash", "solid")) +
  labs(y = "Spawning Date\nAnomaly (Days)", title = "Lake Superior (Apostle Islands)") +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 17, margin = margin(0, 10, 0, 0)),
        axis.text = element_text(size = 14),
        axis.ticks.length = unit(2, 'mm'),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.key.width = unit(1.8, 'cm'),
        legend.position = c(0.16, 0.84),
        legend.background = element_rect(fill = "transparent"),
        panel.border = element_rect(size = 1.5),
        plot.title = element_text(hjust = 0.5),
        plot.margin = unit(c(2.5, 5, 2, 2), 'mm'))

plot.dpf.anomaly.LS.APIS.noHist <- ggplot(simulation.anomaly.LS.APIS.noHist, aes(x = year.class, y = mean.dpf.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario, linetype = scenario),
            size = 1, alpha = 0.9) + 
  scale_y_continuous(limits = c(-51, 11), breaks = seq(-50, 10, 10), expand = c(0, 0)) +
  scale_x_continuous(limits = c(2007, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("#2c7bb6", "#fdae61",  "#d7191c")) +
  scale_linetype_manual(values = c("twodash", "longdash", "solid")) +
  labs(y = "Incubation Length\nAnomaly (Days)") +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 17, margin = margin(0, 10, 0, 0)),
        axis.text = element_text(size = 14),
        axis.ticks.length = unit(2, 'mm'),
        legend.position = "none",
        panel.border = element_rect(size = 1.5),
        plot.margin = unit(c(5, 5, 5, 2), 'mm'))

plot.hatch.anomaly.LS.APIS.noHist <- ggplot(simulation.anomaly.LS.APIS.noHist, aes(x = year.class, y = mean.hatch.yday.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario, linetype = scenario),
            size = 1, alpha = 0.9) + 
  scale_y_continuous(limits = c(-21, 12), breaks = seq(-20, 10, 5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(2007, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("#2c7bb6", "#fdae61",  "#d7191c")) +
  scale_linetype_manual(values = c("twodash", "longdash", "solid")) +
  labs(y = "Hatching Date\nAnomaly (Days)") +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 17, margin = margin(0, 10, 0, 0)),
        axis.text = element_text(size = 14),
        axis.ticks.length = unit(2, 'mm'),
        legend.position = "none",
        panel.border = element_rect(size = 1.5),
        plot.margin = unit(c(5, 5, 5, 2), 'mm'))

## Lake Ontario
plot.spawn.anomaly.LO.noHist <- ggplot(simulation.anomaly.LO.noHist, aes(x = year.class, y = mean.spawn.yday.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario, linetype = scenario),
            size = 1, alpha = 0.9) + 
  scale_y_continuous(limits = c(-7, 36), breaks = seq(-5, 35, 5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(2007, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("#2c7bb6", "#fdae61",  "#d7191c")) +
  scale_linetype_manual(values = c("twodash", "longdash", "solid")) +
  labs(y = "Spawning Date\nAnomaly (Days)", title = "Lake Ontario") +
  theme_few() + 
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 14),
        axis.ticks.length = unit(2, 'mm'),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.position = "none",
        panel.border = element_rect(size = 1.5),
        plot.title = element_text(hjust = 0.5),
        plot.margin = unit(c(2.5, 10, 2, 10), 'mm'))

plot.dpf.anomaly.LO.noHist <- ggplot(simulation.anomaly.LO.noHist, aes(x = year.class, y = mean.dpf.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario, linetype = scenario),
            size = 1, alpha = 0.9) + 
  scale_y_continuous(limits = c(-51, 11), breaks = seq(-50, 10, 10), expand = c(0, 0)) +
  scale_x_continuous(limits = c(2007, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("#2c7bb6", "#fdae61",  "#d7191c")) +
  scale_linetype_manual(values = c("twodash", "longdash", "solid")) +
  labs(y = "Incubation Length\nAnomaly (Days)") +
  theme_few() + 
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 14),
        axis.ticks.length = unit(2, 'mm'),
        legend.position = "none",
        panel.border = element_rect(size = 1.5),
        plot.margin = unit(c(5, 10, 5, 10), 'mm'))

plot.hatch.anomaly.LO.noHist <- ggplot(simulation.anomaly.LO.noHist, aes(x = year.class, y = mean.hatch.yday.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario, linetype = scenario),
            size = 1, alpha = 0.9) + 
  scale_y_continuous(limits = c(-21, 12), breaks = seq(-20, 10, 5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(2007, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("#2c7bb6", "#fdae61",  "#d7191c")) +
  scale_linetype_manual(values = c("twodash", "longdash", "solid")) +
  labs(y = "Hatching Date\nAnomaly (Days)") +
  theme_few() + 
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 14),
        axis.ticks.length = unit(2, 'mm'),
        legend.position = "none",
        panel.border = element_rect(size = 1.5),
        plot.margin = unit(c(5, 10, 5, 10), 'mm'))


## Combine both populations
plot.all.noHist <- grid.arrange(
  arrangeGrob(plot.spawn.anomaly.LS.APIS.noHist,
              plot.dpf.anomaly.LS.APIS.noHist,
              plot.hatch.anomaly.LS.APIS.noHist,
              ncol = 1),
  arrangeGrob(plot.spawn.anomaly.LO.noHist,
              plot.dpf.anomaly.LO.noHist,
              plot.hatch.anomaly.LO.noHist,
              ncol = 1),
  ncol = 2,
  widths = c(1, 0.98)
)

ggsave("figures/artedi-simulation-anomaly-noHist.png", plot = plot.all.noHist, width = 15, height = 12, dpi = 300)

