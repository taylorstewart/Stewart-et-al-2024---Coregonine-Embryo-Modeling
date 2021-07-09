#### CLEAR THE ENVIRONMENT FIRST ---------------------------------------------

#rm(list = ls(all.names = TRUE))


source("scripts/lake-superior-apostle-islands/lake-superior-apostle-islands-simulation-model.R")
source("scripts/lake-ontario/lake-ontario-simulation-model.R")


## heatmap
simulation.model.hatch.distinct.LS.APIS <- simulation.model.hatch.LS.APIS %>% group_by(scenario) %>% distinct(spawn.date, .keep_all = TRUE) %>% 
  filter(scenario != "Historical") %>% mutate(lake = "Lake Superior (Apostle Islands)")
simulation.model.hatch.distinct.LO <- simulation.model.hatch.LO %>% group_by(scenario) %>% distinct(spawn.date, .keep_all = TRUE) %>% 
  filter(scenario != "Historical") %>% mutate(lake = "Lake Ontario")
simulation.model.hatch.distinct <- bind_rows(simulation.model.hatch.distinct.LS.APIS, simulation.model.hatch.distinct.LO) %>% 
  mutate(lake = factor(lake, ordered = TRUE, levels = c("Lake Superior (Apostle Islands)", "Lake Ontario")))

ggplot(simulation.model.hatch.distinct, aes(x = year.class, y = spawn.yday.plot)) +
  geom_tile(aes(fill = as.numeric(dpf)), color = "gray70") +
  scale_x_continuous(limits = c(2007, 2100), breaks = seq(2010, 2100, 10), expand = c(0, 0.2)) +
  scale_y_date(limits = c(as.Date("1970-11-04"), as.Date("1971-01-20")),
               date_breaks = "10 days", date_labels =  "%b %d", expand = c(0, 0)) + 
  #scale_y_continuous(limits = c(320, 380), breaks = seq(320, 380, 10), expand = c(0, 0)) +
  scale_fill_gradient2(limits = c(95, 165), breaks = seq(95, 165, 10),
                       low = "#d7191c", mid = "#ffffcc", high = "#2c7bb6", midpoint = 130) +
  labs(x = "Year", y = "Spawning Date", fill = "Incubation\nLength\n(Days)\n") +
  guides(fill = guide_colourbar(ticks.colour = "black", ticks.linewidth = 0.75,
                                frame.colour = "black", frame.linewidth = 1.5)) +
  theme_few() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(color = "Black", size = 18, margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.ticks.length = unit(2, 'mm'),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.key.width = unit(1.0, 'cm'),
        legend.key.height = unit(2.4, 'cm'),
        strip.text = element_text(size = 12),
        panel.spacing = unit(1.5, "lines"),
        panel.border = element_rect(size = 1.5),
        plot.margin = unit(c(2, 2, 2, 5), 'mm')) +
  facet_grid(rows = vars(scenario), cols = vars(lake))

ggsave("figures/artedi-simulation-heatmap.png", width = 8, height = 8, dpi = 300)




ggplot(simulation.model.hatch.distinct.LS.APIS, aes(x = year.class, y = spawn.yday.plot)) +
  geom_tile(aes(fill = as.numeric(dpf)), color = "gray70") +
  scale_x_continuous(limits = c(2007, 2100), breaks = seq(2010, 2100, 10), expand = c(0, 0.2)) +
  scale_y_date(limits = c(as.Date("1970-11-26"), as.Date("1971-01-22")),
               date_breaks = "10 days", date_labels =  "%b %d", expand = c(0, 0)) + 
  #scale_y_continuous(limits = c(320, 380), breaks = seq(320, 380, 10), expand = c(0, 0)) +
  scale_fill_gradient2(limits = c(90, 140), breaks = seq(90, 140, 10),
                       low = "#d7191c", mid = "#ffffcc", high = "#2c7bb6", midpoint = 115) +
  labs(x = "Year", y = "Spawning Date", fill = "Incubation\nLength\n(Days)\n") +
  guides(fill = guide_colourbar(ticks.colour = "black", ticks.linewidth = 0.75,
                                frame.colour = "black", frame.linewidth = 1.5)) +
  theme_few() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(color = "Black", size = 18, margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.ticks.length = unit(2, 'mm'),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.key.width = unit(1.0, 'cm'),
        legend.key.height = unit(2.4, 'cm'),
        strip.text = element_text(size = 12),
        panel.spacing = unit(1.5, "lines"),
        panel.border = element_rect(size = 1.5),
        plot.margin = unit(c(2, 2, 2, 5), 'mm')) +
  facet_grid(rows = vars(scenario), cols = vars(lake))

ggsave("figures/artedi-simulation-heatmap-LS.png", width = 6, height = 8, dpi = 300)




ggplot(simulation.model.hatch.distinct.LO, aes(x = year.class, y = spawn.yday.plot)) +
  geom_tile(aes(fill = as.numeric(dpf)), color = "gray70") +
  scale_x_continuous(limits = c(2007, 2100), breaks = seq(2010, 2100, 10), expand = c(0, 0.2)) +
  scale_y_date(limits = c(as.Date("1970-11-05"), as.Date("1970-12-28")),
               date_breaks = "10 days", date_labels =  "%b %d", expand = c(0, 0)) + 
  #scale_y_continuous(limits = c(320, 380), breaks = seq(320, 380, 10), expand = c(0, 0)) +
  scale_fill_gradient2(limits = c(100, 170), breaks = seq(100, 170, 10),
                       low = "#d7191c", mid = "#ffffcc", high = "#2c7bb6", midpoint = 135) +
  labs(x = "Year", y = "Spawning Date", fill = "Incubation\nLength\n(Days)\n") +
  guides(fill = guide_colourbar(ticks.colour = "black", ticks.linewidth = 0.75,
                                frame.colour = "black", frame.linewidth = 1.5)) +
  theme_few() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(color = "Black", size = 18, margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.ticks.length = unit(2, 'mm'),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.key.width = unit(1.0, 'cm'),
        legend.key.height = unit(2.4, 'cm'),
        strip.text = element_text(size = 12),
        panel.spacing = unit(1.5, "lines"),
        panel.border = element_rect(size = 1.5),
        plot.margin = unit(c(2, 2, 2, 5), 'mm')) +
  facet_grid(rows = vars(scenario), cols = vars(lake))

ggsave("figures/artedi-simulation-heatmap-LO.png", width = 6, height = 8, dpi = 300)



