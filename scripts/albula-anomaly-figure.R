#### LOAD PACKAGES -------------------------------------------------------------------------------

library(cowplot)
library(ggthemes)
library(ggtext)
library(gridExtra)
library(grid)


#### LOAD DATA FROM OTHER SCRIPTS ----------------------------------------------------------------

source("scripts/lake-konnevesi-simulation-model.R")


#### PASTE REGRESSION COEFFICIENTS TOGETHER TO MAKE EQUATIONS ------------------------------------

lk.eq <- simulation.anomaly.slope.LK %>% mutate(eq = paste0("y = ", slope, "x ", ifelse(intercept >= 0, "+ ", "- "), abs(intercept)))


#### VISUALIZATIONS ------------------------------------------------------------------------------

## Lake Konnevesi
plot.spawn.anomaly.LK <- ggplot(simulation.anomaly.LK, aes(x = year.class, y = mean.spawn.yday.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario, linetype = scenario), size = 1, alpha = 0.5) + 
  geom_smooth(data = filter(simulation.anomaly.LK, scenario != "Historical"), 
              aes(color = scenario), size = 1, method = "lm", se = FALSE, show.legend = FALSE) +
  geom_richtext(data = filter(lk.eq, trait == "spawn", scenario == "RCP 2.6"), 
                aes(label = paste0("<span style='color:#2c7bb6'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
                x = 1905, y = 34.5, size = 4, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(lk.eq, trait == "spawn", scenario == "RCP 6.0"), 
                aes(label = paste0("<span style='color:#fdae61'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
                x = 1905, y = 30.2, size = 4, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(lk.eq, trait == "spawn", scenario == "RCP 8.5"), 
                aes(label = paste0("<span style='color:#d7191c'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
                x = 1905, y = 25.9, size = 4, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  scale_y_continuous(limits = c(-7, 36), breaks = seq(-5, 35, 5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 25), expand = c(0, 0.2)) +
  scale_color_manual(values = c("gray50","#2c7bb6", "#fdae61",  "#d7191c")) +
  scale_linetype_manual(values = c("solid", "longdash", "twodash", "solid")) +
  labs(y = "Spawning Date\nAnomaly (Days)", title = "Lake Konnevesi") +
  theme_few() + 
  theme(axis.title = element_blank(),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.ticks.length = unit(2, 'mm'),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.position = "none",
        panel.border = element_rect(size = 1.5),
        plot.title = element_text(hjust = 0.5),
        plot.margin = unit(c(1.5, 10, 1.5, 7), 'mm'))

plot.dpf.anomaly.LK <- ggplot(simulation.anomaly.LK, aes(x = year.class, y = mean.dpf.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario, linetype = scenario), size = 1, alpha = 0.5) + 
  geom_smooth(data = filter(simulation.anomaly.LK, scenario != "Historical"), 
              aes(color = scenario), size = 1, method = "lm", se = FALSE, show.legend = FALSE) +
  geom_richtext(data = filter(lk.eq, trait == "dpf", scenario == "RCP 2.6"), 
                aes(label = paste0("<span style='color:#2c7bb6'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
                x = 1905, y = -28.6, size = 4, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(lk.eq, trait == "dpf", scenario == "RCP 6.0"), 
                aes(label = paste0("<span style='color:#fdae61'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
                x = 1905, y = -34.8, size = 4, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(lk.eq, trait == "dpf", scenario == "RCP 8.5"), 
                aes(label = paste0("<span style='color:#d7191c'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
                x = 1905, y = -41, size = 4, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
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
        plot.margin = unit(c(6.5, 10, 3, 7), 'mm'))

plot.hatch.anomaly.LK <- ggplot(simulation.anomaly.LK, aes(x = year.class, y = mean.hatch.yday.anomaly, group = scenario)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(aes(color = scenario, linetype = scenario), size = 1, alpha = 0.5) + 
  geom_smooth(data = filter(simulation.anomaly.LK, scenario != "Historical"), 
              aes(color = scenario), size = 1, method = "lm", se = FALSE, show.legend = FALSE) +
  geom_richtext(data = filter(lk.eq, trait == "hatch", scenario == "RCP 2.6"), 
                aes(label = paste0("<span style='color:#2c7bb6'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
                x = 1905, y = -10.5, size = 4, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(lk.eq, trait == "hatch", scenario == "RCP 6.0"), 
                aes(label = paste0("<span style='color:#fdae61'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
                x = 1905, y = -14.0, size = 4, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
  geom_richtext(data = filter(lk.eq, trait == "hatch", scenario == "RCP 8.5"), 
                aes(label = paste0("<span style='color:#d7191c'>", eq, "; R<sup>2</sup> = ", R2, "</span><br>")),
                x = 1905, y = -17.5, size = 4, hjust = 0, vjust = 1, fill = NA, label.color = NA) +
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
        plot.margin = unit(c(6.5, 10, 3, 7), 'mm'))

## Combine both populations
plot.all <- grid.arrange(
  arrangeGrob(textGrob(""),
              get_legend(plot.spawn.anomaly.LK + theme(legend.position = "top")),
              ncol = 2,
              widths = c(0.045, 1)
  ),
  arrangeGrob(plot.spawn.anomaly.LK,
              plot.dpf.anomaly.LK,
              plot.hatch.anomaly.LK,
              ncol = 1),
  ncol = 1,
  nrow = 2,
  heights = c(0.075, 1)
)

ggsave("figures/albula-simulation-anomaly.png", plot = plot.all, width = 7, height = 10, dpi = 300)
