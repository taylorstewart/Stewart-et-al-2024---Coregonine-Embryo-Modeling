library(tidyverse)
library(readxl)
library(ggplot2)
library(ggthemes)


model.parameters <- read_excel("data/model-structural-parameters.xlsx", sheet = "coefficients", skip = 32) %>% 
  filter(lake %in% c("Lake Superior", "Lake Ontario", "Lake Southern Konnevesi", "Lake Geneva", "Lake Constance")) %>% 
  mutate(group = gsub(" ", ".", as.character(interaction(lake, species)))) %>% 
  filter(group != "Lake.Constance.lavaretus", group != "Lake.Constance.lavaretus.macrophthalmus")


dr.data <- do.call(rbind, lapply(unique(model.parameters$group), function(i) {
  model.parameters.filt <- model.parameters %>% filter(group == i)
  
  c.log <- is.na(model.parameters.filt$c)
  
  if(c.log == FALSE) {
    tmp <- do.call(rbind, lapply(seq(0.1, 9.9, 0.1), function(j) {
      perc.day <- round((10^(model.parameters.filt$a + model.parameters.filt$b * j + model.parameters.filt$c * j^2) * 1000), 5)
      data <- data.frame(group = i,
                         temp = j,
                         dr = perc.day)
    }))
  } else {
    tmp <- do.call(rbind, lapply(seq(0.1, 9.9, 0.1), function(j) {
      perc.day <- round((10^(model.parameters.filt$a + model.parameters.filt$b * j) * 1000), 5)
      data <- data.frame(group = i,
                         temp = j,
                         dr = perc.day)
    }))
  }
})) %>% 
  mutate(group = factor(group, ordered = TRUE,
                        levels = c("Lake.Southern.Konnevesi.albula", "Lake.Southern.Konnevesi.lavaretus",
                                   "Lake.Superior.artedi", "Lake.Ontario.artedi" ,
                                   "Lake.Constance.lavaretus.wartmanni", "Lake.Geneva.lavaretus"),
                        labels = c("Lake S. Konnevesi - Vendace  ", "Lake S. Konnevesi - Whitefish  ",
                                   "Lake Superior - Cisco", "Lake Ontario - Cisco",
                                   "Lake Constance - Whitefish (pelagic)", "Lake Geneva - Whitefish")))

dr.data.filt <- dr.data %>% filter(group != "Lake S. Konnevesi - Whitefish  ")


ggplot(dr.data.filt, aes(x = temp, y = dr, color = group, linetype = group)) + 
  geom_line(linewidth = 1) +
  scale_y_continuous(limits = c(3, 36), breaks = seq(5, 35, 5), expand = c(0, 0)) + 
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2.5), expand = c(0, 0)) + 
  scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed", "solid", "dashed", "solid"),
                        guide = guide_legend(ncol = 2)) +
  scale_color_manual(values = c("#1f78b4", "#a6cee3", "#a6cee3", "#33a02c", "#b2df8a"),
  #scale_color_manual(values = c("#c7e9b4", "#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84", "#081d58"),
                     guide = guide_legend(ncol = 2)) +
  labs(x = "Temperature (°C)", y = expression("Daily Proportion of Development x 10"^-3)) +
  theme_few() + 
  theme(axis.title.x = element_text(size = 20, margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(size = 20, margin = margin(0, 10, 0, 0)),
        axis.text = element_text(size = 17),
        axis.ticks.length = unit(2, 'mm'),
        legend.title = element_blank(),
        legend.text = element_text(size = 17),
        legend.key.width = unit(1.15, 'cm'),
        legend.key.height = unit(1, 'cm'),
        legend.background = element_rect(fill = "transparent"),
        legend.position = c(0.41, 0.905),
        panel.border = element_rect(linewidth = 1.5),
        plot.title = element_text(size = 20, hjust = 0.5),
        plot.margin = unit(c(2, 7, 2, 2), 'mm'))

ggsave("figures/IJL-submission/development-rates.tiff", width = 12, height = 8, dpi = 500)


