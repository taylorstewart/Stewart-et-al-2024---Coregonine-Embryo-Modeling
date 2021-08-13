#### CLEAR THE ENVIRONMENT FIRST ---------------------------------------------

rm(list = ls(all.names = TRUE))


#### LOAD PACKAGES -------------------------------------------------------------------------------

library(cowplot)
library(ggthemes)
library(ggtext)
library(gridExtra)
library(grid)


#### LOAD DATA FROM OTHER SCRIPTS ----------------------------------------------------------------

source("scripts/lake-superior-apostle-islands-simulation-model.R")
source("scripts/lake-ontario-simulation-model.R")
source("scripts/lake-konnevesi-simulation-model.R")
source("scripts/lake-constance-simulation-model.R")


#### PASTE REGRESSION COEFFICIENTS TOGETHER TO MAKE EQUATIONS ------------------------------------

ls.eq <- simulation.anomaly.slope.LS.APIS %>% mutate(eq = paste0("y = ", slope, "x ", ifelse(intercept >= 0, "+ ", "- "), abs(intercept)))
lo.eq <- simulation.anomaly.slope.LO %>% mutate(eq = paste0("y = ", slope, "x ", ifelse(intercept >= 0, "+ ", "- "), abs(intercept)))
lk.eq <- simulation.anomaly.slope.LK %>% mutate(eq = paste0("y = ", slope, "x ", ifelse(intercept >= 0, "+ ", "- "), abs(intercept)))
lc.eq <- simulation.anomaly.slope.LC %>% mutate(eq = paste0("y = ", slope, "x ", ifelse(intercept >= 0, "+ ", "- "), abs(intercept)))


#### VISUALIZATION - LAKE SUPERIOR (APIS) --------------------------------------------------------

plot.spawn.anomaly.LS.APIS <- ggplot(simulation.anomaly.LS.APIS, aes(x = year.class, y = mean.spawn.yday.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario, linetype = scenario), size = 1, alpha = 0.5) + 
  geom_smooth(data = filter(simulation.anomaly.LS.APIS, scenario != "Historical"), 
              aes(color = scenario), size = 1, method = "lm", se = FALSE, show.legend = FALSE) +
  geom_richtext(data = filter(ls.eq, trait == "spawn", scenario == "RCP 2.6"), 
                aes(label = paste0("<span style='color:#2c7bb6'><b>", eq, "; R<sup>2</sup> = ", R2, "</b></span><br>")),
                x = 1905, y = 34.5, size = 5.0, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(ls.eq, trait == "spawn", scenario == "RCP 6.0"), 
                aes(label = paste0("<span style='color:#fdae61'><b>", eq, "; R<sup>2</sup> = ", R2, "</b></span><br>")),
                x = 1905, y = 30.2, size = 5.0, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(ls.eq, trait == "spawn", scenario == "RCP 8.5"), 
                aes(label = paste0("<span style='color:#d7191c'><b>", eq, "; R<sup>2</sup> = ", R2, "</b></span><br>")),
                x = 1905, y = 25.9, size = 5.0, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  scale_y_continuous(limits = c(-7, 36), breaks = seq(-5, 35, 5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("gray50","#2c7bb6", "#fdae61",  "#d7191c")) +
  scale_linetype_manual(values = c("solid", "longdash", "twodash", "solid")) +
  labs(y = "Spawning Date\nAnomaly (Days)", title = "Lake Superior - Cisco") +
  guides(color = guide_legend(override.aes = list(alpha = 1)))+
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 17, margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.ticks.length = unit(2, 'mm'),
        legend.title = element_blank(),
        legend.text = element_text(size = 17),
        legend.key.width = unit(1.5, 'cm'),
        legend.position = "none",
        legend.background = element_rect(fill = "transparent"),
        panel.border = element_rect(size = 1.5),
        plot.title = element_text(size = 17, hjust = 0.5),
        plot.margin = unit(c(1, 2, 0, 2.5), 'mm'))

plot.dpf.anomaly.LS.APIS <- ggplot(simulation.anomaly.LS.APIS, aes(x = year.class, y = mean.dpf.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario, linetype = scenario), size = 1, alpha = 0.5) + 
  geom_smooth(data = filter(simulation.anomaly.LS.APIS, scenario != "Historical"), aes(color = scenario), size = 1, method = "lm", se = FALSE, show.legend = FALSE) +
  geom_richtext(data = filter(ls.eq, trait == "dpf", scenario == "RCP 2.6"), 
                aes(label = paste0("<span style='color:#2c7bb6'><b>", eq, "; R<sup>2</sup> = ", R2, "</b></span><br>")),
                x = 1905, y = -29.7, size = 5.0, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(ls.eq, trait == "dpf", scenario == "RCP 6.0"), 
                aes(label = paste0("<span style='color:#fdae61'><b>", eq, "; R<sup>2</sup> = ", R2, "</b></span><br>")),
                x = 1905, y = -35.9, size = 5.0, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(ls.eq, trait == "dpf", scenario == "RCP 8.5"), 
                aes(label = paste0("<span style='color:#d7191c'><b>", eq, "; R<sup>2</sup> = ", R2, "</b></span><br>")),
                x = 1905, y = -42.1, size = 5.0, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  scale_y_continuous(limits = c(-51, 11), breaks = seq(-50, 10, 10), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("gray50","#2c7bb6", "#fdae61",  "#d7191c")) +
  scale_linetype_manual(values = c("solid", "longdash", "twodash", "solid")) +
  labs(y = "Incubation Length\nAnomaly (Days)") +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 17, margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.ticks.length = unit(2, 'mm'),
        legend.position = "none",
        panel.border = element_rect(size = 1.5),
        plot.margin = unit(c(6.5, 2, 1, 1), 'mm'))

plot.hatch.anomaly.LS.APIS <- ggplot(simulation.anomaly.LS.APIS, aes(x = year.class, y = mean.hatch.yday.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario, linetype = scenario), size = 1, alpha = 0.5) + 
  geom_smooth(data = filter(simulation.anomaly.LS.APIS, scenario != "Historical"), aes(color = scenario), size = 1, method = "lm", se = FALSE, show.legend = FALSE) +
  geom_richtext(data = filter(ls.eq, trait == "hatch", scenario == "RCP 2.6"), 
                aes(label = paste0("<span style='color:#2c7bb6'><b>", eq, "; R<sup>2</sup> = ", R2, "</b></span><br>")),
                x = 1905, y = -11.0, size = 5.0, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(ls.eq, trait == "hatch", scenario == "RCP 6.0"), 
                aes(label = paste0("<span style='color:#fdae61'><b>", eq, "; R<sup>2</sup> = ", R2, "</b></span><br>")),
                x = 1905, y = -14.5, size = 5.0, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(ls.eq, trait == "hatch", scenario == "RCP 8.5"), 
                aes(label = paste0("<span style='color:#d7191c'><b>", eq, "; R<sup>2</sup> = ", R2, "</b></span><br>")),
                x = 1905, y = -18.0, size = 5.0, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  scale_y_continuous(limits = c(-23, 12), breaks = seq(-20, 5, 5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("gray50","#2c7bb6", "#fdae61",  "#d7191c")) +
  scale_linetype_manual(values = c("solid", "longdash", "twodash", "solid")) +
  labs(y = "Hatching Date\nAnomaly (Days)") +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 17, margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.ticks.length = unit(2, 'mm'),
        legend.position = "none",
        panel.border = element_rect(size = 1.5),
        plot.margin = unit(c(5.5, 2, 2, 1), 'mm'))


#### VISUALIZATION - LAKE ONTARIO ----------------------------------------------------------------

plot.spawn.anomaly.LO <- ggplot(simulation.anomaly.LO, aes(x = year.class, y = mean.spawn.yday.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario, linetype = scenario), size = 1, alpha = 0.5) + 
  geom_smooth(data = filter(simulation.anomaly.LO, scenario != "Historical"), aes(color = scenario), size = 1, method = "lm", se = FALSE, show.legend = FALSE) +
  geom_richtext(data = filter(lo.eq, trait == "spawn", scenario == "RCP 2.6"), 
                aes(label = paste0("<span style='color:#2c7bb6'><b>", eq, "; R<sup>2</sup> = ", R2, "</b></span><br>")),
                x = 1905, y = 34.5, size = 5.0, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(lo.eq, trait == "spawn", scenario == "RCP 6.0"), 
                aes(label = paste0("<span style='color:#fdae61'><b>", eq, "; R<sup>2</sup> = ", R2, "</b></span><br>")),
                x = 1905, y = 30.2, size = 5.0, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(lo.eq, trait == "spawn", scenario == "RCP 8.5"), 
                aes(label = paste0("<span style='color:#d7191c'><b>", eq, "; R<sup>2</sup> = ", R2, "</b></span><br>")),
                x = 1905, y = 25.9, size = 5.0, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  scale_y_continuous(limits = c(-7, 36), breaks = seq(-5, 35, 5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("gray50","#2c7bb6", "#fdae61",  "#d7191c")) +
  scale_linetype_manual(values = c("solid", "longdash", "twodash", "solid")) +
  labs(y = "Spawning Date\nAnomaly (Days)", title = "Lake Ontario - Cisco") +
  theme_few() + 
  theme(axis.title = element_blank(),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.ticks.length = unit(2, 'mm'),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.position = "none",
        panel.border = element_rect(size = 1.5),
        plot.title = element_text(size = 17, hjust = 0.5),
        plot.margin = unit(c(1, 3, 0, 13.5), 'mm'))

plot.dpf.anomaly.LO <- ggplot(simulation.anomaly.LO, aes(x = year.class, y = mean.dpf.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario, linetype = scenario), size = 1, alpha = 0.5) + 
  geom_smooth(data = filter(simulation.anomaly.LO, scenario != "Historical"), aes(color = scenario), size = 1, method = "lm", se = FALSE, show.legend = FALSE) +
  geom_richtext(data = filter(lo.eq, trait == "dpf", scenario == "RCP 2.6"), 
                aes(label = paste0("<span style='color:#2c7bb6'><b>", eq, "; R<sup>2</sup> = ", R2, "</b></span><br>")),
                x = 1905, y = -29.7, size = 5.0, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(lo.eq, trait == "dpf", scenario == "RCP 6.0"), 
                aes(label = paste0("<span style='color:#fdae61'><b>", eq, "; R<sup>2</sup> = ", R2, "</b></span><br>")),
                x = 1905, y = -35.9, size = 5.0, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(lo.eq, trait == "dpf", scenario == "RCP 8.5"), 
                aes(label = paste0("<span style='color:#d7191c'><b>", eq, "; R<sup>2</sup> = ", R2, "</b></span><br>")),
                x = 1905, y = -42.1, size = 5.0, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  scale_y_continuous(limits = c(-51, 11), breaks = seq(-50, 10, 10), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("gray50","#2c7bb6", "#fdae61",  "#d7191c")) +
  scale_linetype_manual(values = c("solid", "longdash", "twodash", "solid")) +
  labs(y = "Incubation Length\nAnomaly (Days)") +
  theme_few() + 
  theme(axis.title = element_blank(),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.ticks.length = unit(2, 'mm'),
        legend.position = "none",
        panel.border = element_rect(size = 1.5),
        plot.margin = unit(c(6.5, 3, 1, 12), 'mm'))

plot.hatch.anomaly.LO <- ggplot(simulation.anomaly.LO, aes(x = year.class, y = mean.hatch.yday.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario, linetype = scenario), size = 1, alpha = 0.5) + 
  geom_smooth(data = filter(simulation.anomaly.LO, scenario != "Historical"), aes(color = scenario), size = 1, method = "lm", se = FALSE, show.legend = FALSE) +
  geom_richtext(data = filter(lo.eq, trait == "hatch", scenario == "RCP 2.6"), 
                aes(label = paste0("<span style='color:#2c7bb6'><b>", eq, "; R<sup>2</sup> = ", R2, "</b></span><br>")),
                x = 1905, y = -11.0, size = 5.0, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(lo.eq, trait == "hatch", scenario == "RCP 6.0"), 
                aes(label = paste0("<span style='color:#fdae61'><b>", eq, "; R<sup>2</sup> = ", R2, "</b></span><br>")),
                x = 1905, y = -14.5, size = 5.0, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(lo.eq, trait == "hatch", scenario == "RCP 8.5"), 
                aes(label = paste0("<span style='color:#d7191c'><b>", eq, "; R<sup>2</sup> = ", R2, "</b></span><br>")),
                x = 1905, y = -18.0, size = 5.0, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  scale_y_continuous(limits = c(-23, 12), breaks = seq(-20, 5, 5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("gray50","#2c7bb6", "#fdae61",  "#d7191c")) +
  scale_linetype_manual(values = c("solid", "longdash", "twodash", "solid")) +
  labs(y = "Hatching Date\nAnomaly (Days)") +
  theme_few() + 
  theme(axis.title = element_blank(),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.ticks.length = unit(2, 'mm'),
        legend.position = "none",
        panel.border = element_rect(size = 1.5),
        plot.margin = unit(c(5.5, 3, 2, 12), 'mm'))


#### VISUALIZATION - LAKE KONNEVESI --------------------------------------------------------------

plot.spawn.anomaly.LK <- ggplot(simulation.anomaly.LK, aes(x = year.class, y = mean.spawn.yday.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario, linetype = scenario), size = 1, alpha = 0.5) + 
  geom_smooth(data = filter(simulation.anomaly.LK, scenario != "Historical"), 
              aes(color = scenario), size = 1, method = "lm", se = FALSE, show.legend = FALSE) +
  geom_richtext(data = filter(lk.eq, trait == "spawn", scenario == "RCP 2.6"), 
                aes(label = paste0("<span style='color:#2c7bb6'><b>", eq, "; R<sup>2</sup> = ", R2, "</b></span><br>")),
                x = 1905, y = 34.5, size = 5.0, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(lk.eq, trait == "spawn", scenario == "RCP 6.0"), 
                aes(label = paste0("<span style='color:#fdae61'><b>", eq, "; R<sup>2</sup> = ", R2, "</b></span><br>")),
                x = 1905, y = 30.2, size = 5.0, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(lk.eq, trait == "spawn", scenario == "RCP 8.5"), 
                aes(label = paste0("<span style='color:#d7191c'><b>", eq, "; R<sup>2</sup> = ", R2, "</b></span><br>")),
                x = 1905, y = 25.9, size = 5.0, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  scale_y_continuous(limits = c(-7, 36), breaks = seq(-5, 35, 5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("gray50","#2c7bb6", "#fdae61",  "#d7191c")) +
  scale_linetype_manual(values = c("solid", "longdash", "twodash", "solid")) +
  labs(y = "Spawning Date\nAnomaly (Days)", title = "Lake Konnevesi - Vendace") +
  theme_few() + 
  theme(axis.title = element_blank(),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.ticks.length = unit(2, 'mm'),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.position = "none",
        panel.border = element_rect(size = 1.5),
        plot.title = element_text(size = 17, hjust = 0.5),
        plot.margin = unit(c(1, 3, 0, 13.5), 'mm'))

plot.dpf.anomaly.LK <- ggplot(simulation.anomaly.LK, aes(x = year.class, y = mean.dpf.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario, linetype = scenario), size = 1, alpha = 0.5) + 
  geom_smooth(data = filter(simulation.anomaly.LK, scenario != "Historical"), 
              aes(color = scenario), size = 1, method = "lm", se = FALSE, show.legend = FALSE) +
  geom_richtext(data = filter(lk.eq, trait == "dpf", scenario == "RCP 2.6"), 
                aes(label = paste0("<span style='color:#2c7bb6'><b>", eq, "; R<sup>2</sup> = ", R2, "</b></span><br>")),
                x = 1905, y = -29.7, size = 5.0, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(lk.eq, trait == "dpf", scenario == "RCP 6.0"), 
                aes(label = paste0("<span style='color:#fdae61'><b>", eq, "; R<sup>2</sup> = ", R2, "</b></span><br>")),
                x = 1905, y = -35.9, size = 5.0, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(lk.eq, trait == "dpf", scenario == "RCP 8.5"), 
                aes(label = paste0("<span style='color:#d7191c'><b>", eq, "; R<sup>2</sup> = ", R2, "</b></span><br>")),
                x = 1905, y = -42.1, size = 5.0, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  scale_y_continuous(limits = c(-51, 11), breaks = seq(-50, 10, 10), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("gray50","#2c7bb6", "#fdae61",  "#d7191c")) +
  scale_linetype_manual(values = c("solid", "longdash", "twodash", "solid")) +
  labs(y = "Incubation Length\nAnomaly (Days)") +
  theme_few() + 
  theme(axis.title = element_blank(),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.ticks.length = unit(2, 'mm'),
        legend.position = "none",
        panel.border = element_rect(size = 1.5),
        plot.margin = unit(c(6.5, 3, 1, 12), 'mm'))

plot.hatch.anomaly.LK <- ggplot(simulation.anomaly.LK, aes(x = year.class, y = mean.hatch.yday.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario, linetype = scenario), size = 1, alpha = 0.5) + 
  geom_smooth(data = filter(simulation.anomaly.LK, scenario != "Historical"), 
              aes(color = scenario), size = 1, method = "lm", se = FALSE, show.legend = FALSE) +
  geom_richtext(data = filter(lk.eq, trait == "hatch", scenario == "RCP 2.6"), 
                aes(label = paste0("<span style='color:#2c7bb6'><b>", eq, "; R<sup>2</sup> = ", R2, "</b></span><br>")),
                x = 1905, y = -11.0, size = 5.0, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(lk.eq, trait == "hatch", scenario == "RCP 6.0"), 
                aes(label = paste0("<span style='color:#fdae61'><b>", eq, "; R<sup>2</sup> = ", R2, "</b></span><br>")),
                x = 1905, y = -14.5, size = 5.0, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(lk.eq, trait == "hatch", scenario == "RCP 8.5"), 
                aes(label = paste0("<span style='color:#d7191c'><b>", eq, "; R<sup>2</sup> = ", R2, "</b></span><br>")),
                x = 1905, y = -18.0, size = 5.0, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  scale_y_continuous(limits = c(-23, 12), breaks = seq(-20, 5, 5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("gray50","#2c7bb6", "#fdae61",  "#d7191c")) +
  scale_linetype_manual(values = c("solid", "longdash", "twodash", "solid")) +
  labs(y = "Hatching Date\nAnomaly (Days)") +
  theme_few() + 
  theme(axis.title = element_blank(),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.ticks.length = unit(2, 'mm'),
        legend.position = "none",
        panel.border = element_rect(size = 1.5),
        plot.margin = unit(c(5.5, 3, 2, 12), 'mm'))


#### VISUALIZATION - LAKE CONSTANCE --------------------------------------------------------------

plot.spawn.anomaly.LC <- ggplot(simulation.anomaly.LC, aes(x = year.class, y = mean.spawn.yday.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario, linetype = scenario), size = 1, alpha = 0.5) + 
  geom_smooth(data = filter(simulation.anomaly.LC, scenario != "Historical"), 
              aes(color = scenario), size = 1, method = "lm", se = FALSE, show.legend = FALSE) +
  geom_richtext(data = filter(lc.eq, trait == "spawn", scenario == "RCP 2.6"), 
                aes(label = paste0("<span style='color:#2c7bb6'><b>", eq, "; R<sup>2</sup> = ", R2, "</b></span><br>")),
                x = 1905, y = 34.5, size = 5.0, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(lc.eq, trait == "spawn", scenario == "RCP 6.0"), 
                aes(label = paste0("<span style='color:#fdae61'><b>", eq, "; R<sup>2</sup> = ", R2, "</b></span><br>")),
                x = 1905, y = 30.2, size = 5.0, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(lc.eq, trait == "spawn", scenario == "RCP 8.5"), 
                aes(label = paste0("<span style='color:#d7191c'><b>", eq, "; R<sup>2</sup> = ", R2, "</b></span><br>")),
                x = 1905, y = 25.9, size = 5.0, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  scale_y_continuous(limits = c(-7, 36), breaks = seq(-5, 35, 5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("gray50","#2c7bb6", "#fdae61",  "#d7191c")) +
  scale_linetype_manual(values = c("solid", "longdash", "twodash", "solid")) +
  labs(y = "Spawning Date\nAnomaly (Days)", title = "Lake Constance - Whitefish") +
  theme_few() + 
  theme(axis.title = element_blank(),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.ticks.length = unit(2, 'mm'),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.position = "none",
        panel.border = element_rect(size = 1.5),
        plot.title = element_text(size = 17, hjust = 0.5),
        plot.margin = unit(c(1, 3, 0, 13.5), 'mm'))

plot.dpf.anomaly.LC <- ggplot(simulation.anomaly.LC, aes(x = year.class, y = mean.dpf.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario, linetype = scenario), size = 1, alpha = 0.5) + 
  geom_smooth(data = filter(simulation.anomaly.LC, scenario != "Historical"), 
              aes(color = scenario), size = 1, method = "lm", se = FALSE, show.legend = FALSE) +
  geom_richtext(data = filter(lc.eq, trait == "dpf", scenario == "RCP 2.6"), 
                aes(label = paste0("<span style='color:#2c7bb6'><b>", eq, "; R<sup>2</sup> = ", R2, "</b></span><br>")),
                x = 1905, y = -29.7, size = 5.0, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(lc.eq, trait == "dpf", scenario == "RCP 6.0"), 
                aes(label = paste0("<span style='color:#fdae61'><b>", eq, "; R<sup>2</sup> = ", R2, "</b></span><br>")),
                x = 1905, y = -35.9, size = 5.0, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(lc.eq, trait == "dpf", scenario == "RCP 8.5"), 
                aes(label = paste0("<span style='color:#d7191c'><b>", eq, "; R<sup>2</sup> = ", R2, "</b></span><br>")),
                x = 1905, y = -42.1, size = 5.0, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  scale_y_continuous(limits = c(-51, 11), breaks = seq(-50, 10, 10), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("gray50","#2c7bb6", "#fdae61",  "#d7191c")) +
  scale_linetype_manual(values = c("solid", "longdash", "twodash", "solid")) +
  labs(y = "Incubation Length\nAnomaly (Days)") +
  theme_few() + 
  theme(axis.title = element_blank(),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.ticks.length = unit(2, 'mm'),
        legend.position = "none",
        panel.border = element_rect(size = 1.5),
        plot.margin = unit(c(6.5, 3, 1, 12), 'mm'))

plot.hatch.anomaly.LC <- ggplot(simulation.anomaly.LC, aes(x = year.class, y = mean.hatch.yday.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario, linetype = scenario), size = 1, alpha = 0.5) + 
  geom_smooth(data = filter(simulation.anomaly.LC, scenario != "Historical"), 
              aes(color = scenario), size = 1, method = "lm", se = FALSE, show.legend = FALSE) +
  geom_richtext(data = filter(lc.eq, trait == "hatch", scenario == "RCP 2.6"), 
                aes(label = paste0("<span style='color:#2c7bb6'><b>", eq, "; R<sup>2</sup> = ", R2, "</b></span><br>")),
                x = 1905, y = -11.0, size = 5.0, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(lc.eq, trait == "hatch", scenario == "RCP 6.0"), 
                aes(label = paste0("<span style='color:#fdae61'><b>", eq, "; R<sup>2</sup> = ", R2, "</b></span><br>")),
                x = 1905, y = -14.5, size = 5.0, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(lc.eq, trait == "hatch", scenario == "RCP 8.5"), 
                aes(label = paste0("<span style='color:#d7191c'><b>", eq, "; R<sup>2</sup> = ", R2, "</b></span><br>")),
                x = 1905, y = -18.0, size = 5.0, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  scale_y_continuous(limits = c(-23, 12), breaks = seq(-20, 5, 5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("gray50","#2c7bb6", "#fdae61",  "#d7191c")) +
  scale_linetype_manual(values = c("solid", "longdash", "twodash", "solid")) +
  labs(y = "Hatching Date\nAnomaly (Days)") +
  theme_few() + 
  theme(axis.title = element_blank(),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.ticks.length = unit(2, 'mm'),
        legend.position = "none",
        panel.border = element_rect(size = 1.5),
        plot.margin = unit(c(5.5, 3, 2, 12), 'mm'))


#### VISUALIZATION - COMBINE ALL POPULATIONS -----------------------------------------------------

plot.all <- grid.arrange(
  arrangeGrob(textGrob(""),
              get_legend(plot.spawn.anomaly.LS.APIS + theme(legend.position = "top")),
              ncol = 2,
              widths = c(0.045, 1)
  ),
  arrangeGrob(
    arrangeGrob(plot.spawn.anomaly.LS.APIS + theme(legend.position = "none"),
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
    arrangeGrob(plot.spawn.anomaly.LC,
                plot.dpf.anomaly.LC,
                plot.hatch.anomaly.LC,
                ncol = 1),
    ncol = 4,
    widths = c(1, 0.98, 0.98, 0.98)
  ),
  nrow = 2,
  heights = c(0.075, 1)
)

ggsave("figures/simulation-anomaly.png", plot = plot.all, width = 26, height = 12, dpi = 300)

