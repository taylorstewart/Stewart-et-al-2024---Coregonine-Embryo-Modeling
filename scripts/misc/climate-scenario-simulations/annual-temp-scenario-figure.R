data <- climate.simulations.comb %>% filter(year.class %in% c(2007, 2050, 2099), depth == 5)

ggplot(data, aes(x = date, y = watertemp, color = scenario)) + 
  geom_line() + 
  scale_y_continuous(limits = c(4, 26), expand = c(0, 0)) +
  scale_color_manual(values = c("#2c7bb6", "#fdae61",  "#d7191c")) +
  labs(y = "Water Temperature (°C)") +
  theme_few() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 19),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.ticks.length = unit(2, 'mm'),
        legend.title = element_blank(),
        legend.text = element_text(size = 19),
        legend.key.width = unit(1.5, 'cm'),
        legend.position = "top",
        legend.background = element_rect(fill = "transparent"),
        strip.text = element_text(size = 15),
        panel.border = element_rect(size = 1.5),
        plot.margin = unit(c(5, 5, 5, 5), 'mm')) +
facet_wrap(~year.class, scales = "free_x")

ggsave("figures/temp-scenario-annecy.png", width = 15, height = 6, dpi = 300)

