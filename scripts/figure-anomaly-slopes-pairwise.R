library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(rstatix)

ls.apis.slopes <- read.csv("data/anomaly-slopes/lake-superior-apostle-islands-slope.csv") %>% mutate(population = "Lake Superior - Cisco")
lo.slopes <- read.csv("data/anomaly-slopes/lake-ontario-slope.csv") %>% mutate(population = "Lake Ontario - Cisco")
lk.slopes <- read.csv("data/anomaly-slopes/lake-konnevesi-albula-slope.csv") %>% mutate(population = "Lake Konnevesi - Vendace")
lc.slopes <- read.csv("data/anomaly-slopes/lake-constance-slope.csv") %>% mutate(population = "Lake Constance - Whitefish")
la.slopes <- read.csv("data/anomaly-slopes/lake-annecy-slope.csv") %>% mutate(population = "Lake Annecy - Whitefish")

slopes <- bind_rows(ls.apis.slopes, lo.slopes, lk.slopes, lc.slopes, la.slopes) %>% 
  filter(trait != "hatchdays") %>% 
  mutate(scenario = gsub("RCP ", "", scenario),
         population = factor(population, ordered = TRUE, 
                             levels = c("Lake Konnevesi - Vendace", "Lake Superior - Cisco", "Lake Ontario - Cisco", "Lake Constance - Whitefish", "Lake Annecy - Whitefish"),
                             labels = c("Lake Konnevesi\nVendace", "Lake Superior\nCisco", "Lake Ontario\nCisco", "Lake Constance\nWhitefish", "Lake Annecy\nWhitefish")),
         trait = factor(trait, ordered = TRUE,
                        levels = c("spawn", "dpf", "hatch"),
                        labels = c("Spawning Date", "Incubation Length", "Hatching Date")))

ls.apis.multComp <- read.csv("data/anomaly-slopes/lake-superior-apostle-islands-multComp.csv") %>% mutate(population = "Lake Superior - Cisco")
lo.multComp <- read.csv("data/anomaly-slopes/lake-ontario-multComp.csv") %>% mutate(population = "Lake Ontario - Cisco")
lk.multComp <- read.csv("data/anomaly-slopes/lake-konnevesi-albula-multComp.csv") %>% mutate(population = "Lake Konnevesi - Vendace")
lc.multComp <- read.csv("data/anomaly-slopes/lake-constance-multComp.csv") %>% mutate(population = "Lake Constance - Whitefish")
la.multComp <- read.csv("data/anomaly-slopes/lake-annecy-multComp.csv") %>% mutate(population = "Lake Annecy - Whitefish")

multComp <- bind_rows(ls.apis.multComp, lo.multComp, lk.multComp, lc.multComp, la.multComp) %>% 
  filter(trait != "hatchdays") %>% 
  mutate(population = factor(population, ordered = TRUE, 
                             levels = c("Lake Konnevesi - Vendace", "Lake Superior - Cisco", "Lake Ontario - Cisco", "Lake Constance - Whitefish", "Lake Annecy - Whitefish"),
                             labels = c("Lake Konnevesi\nVendace", "Lake Superior\nCisco", "Lake Ontario\nCisco", "Lake Constance\nWhitefish", "Lake Annecy\nWhitefish")),
         trait = factor(trait, ordered = TRUE,
                        levels = c("spawn", "dpf", "hatch"),
                        labels = c("Spawning Date", "Incubation Length", "Hatching Date")))

multComp.pairwise <- multComp %>% filter(contrast != "overall") %>% 
  select(-estimate, -SE, -df, -t.ratio) %>% 
  mutate(p.value = round(p.value, 4),
         #p.value = ifelse(p.value < 0.001, "< 0.001", p.value),
         #p.value = ifelse(p.value > 0.05, "NS", p.value),
         group1 = gsub("RCP ", "", substr(contrast, 1, 7)),
         group2 = gsub("RCP ", "", substr(contrast, 11, 17)),
         y.position = rep(c(1.15, 1.5, 1.85), times = 14)) %>% 
  select(population, trait, group1, group2, p.value, y.position) %>% 
  add_significance("p.value", cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                   symbols = c("***", "**", "*", "ns"))

ann_text <- slopes %>% filter(trait == "Hatching Date", population == "Lake Superior\nCisco", scenario == "6.0") %>% 
  mutate(slope = 1.65)


ggplot(slopes, aes(x = scenario, y = slope)) + 
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = ci.lower, ymax = ci.upper), width = 0.3) +
  stat_pvalue_manual(data = multComp.pairwise, label = "p.value.signif") +
  geom_text(data = ann_text, label = "ANOVA p-value\n0.9487") + 
  scale_y_continuous(limits = c(-0.6, 2.1), breaks = seq(-0.5, 2.0, 0.5), expand = c(0, 0)) +
  labs(y = "Anomaly Slope", x = "Representative Concentration Pathway Scenarios") +
  theme_few() +
  theme(axis.title.x = element_text(size = 17, margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(size = 17, margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.ticks.length = unit(2, 'mm'),
        strip.text = element_text(size = 14),
        panel.border = element_rect(linewidth = 1.5),
        plot.margin = unit(c(2, 2, 2, 2), 'mm'),
        panel.spacing = unit(4, 'mm')) +
  facet_grid(rows = vars(population), cols = vars(trait))

ggsave("figures/IJL-submission/slope-pairwise.tiff", width = 6.5, height = 10, dpi = 500)


