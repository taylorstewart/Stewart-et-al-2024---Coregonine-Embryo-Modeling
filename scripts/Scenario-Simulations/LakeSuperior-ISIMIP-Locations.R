library(dplyr)
library(data.table)
library(ggplot2)
library(rgdal)
library(ggpolypath)


climate.files.TB <- list.files("data/climate-simulations/lake-superior-thunder-bay", pattern = "rcp85", full.names = TRUE)[1]
climate.files.APIS <- list.files("data/climate-simulations/lake-superior-apostle-islands", pattern = "rcp85", full.names = TRUE)[1]

data.TB <- fread(climate.files.TB)
data.APIS <- fread(climate.files.APIS)
data.coord <- bind_rows(data.TB, data.APIS) %>% 
  group_by(lon, lat) %>% 
  mutate(max.depth = paste0(max(depth), "-m")) %>% 
  distinct(lat, lon, max.depth)


ls_poly <- readOGR(dsn = "data/shapefile/lake-superior.shp", layer = "lake-superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly) %>% mutate(hole = case_when(group == 0.2 ~ "ZZZ",
                                                             group == 0.3 ~ "ZZZ",
                                                             hole == TRUE ~ "Land",
                                                             hole == FALSE ~ "Lake"))



ggplot(data = ls_poly.fort, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = hole), 
               color = "black", size = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c("#e9f3f8", "white", "white")) + 
  geom_point(data = data.coord, aes(x = lon, y = lat), 
             color = "black", fill = "#de2d26", shape = 21, size = 3.25) +
  geom_label(data = data.coord, aes(x = lon, y = lat, label = max.depth), 
             nudge_y = 0.1, fontface = "bold", size = 3) +
  coord_fixed(ratio = 1.4) +
  scale_x_continuous(limits = c(-92.3, -84.3), breaks = seq(-92, -84, 1), expand = c(0, 0),
                     labels =  paste0(seq(-92, -84, 1), "°")) +
  scale_y_continuous(limits = c(46.35, 49.1), breaks = seq(46.5, 49, 0.5), expand = c(0, 0),
                     labels =  paste0(seq(46.5, 49, 0.5), "°")) +
  labs(x = "Longitude", y = "Latitude") +
  theme(axis.text = element_text(size = 13),
        axis.title = element_blank(),
        axis.ticks.length = unit(1.5, 'mm'),
        legend.title = element_blank(), 
        legend.text = element_blank(),
        strip.text = element_text(size = 14),
        strip.background = element_rect(color = "white", fill = "white"),
        panel.background = element_rect(fill = "transparent"), 
        panel.grid = element_line(linetype = 'dashed', color = "#B0B0B080"), 
        panel.border = element_rect(linetype = 'solid', color = "black", fill = "transparent"),
        panel.ontop = TRUE)

ggsave("figures/climate-scenarios/LakeSuperior-ISIMIP-Locations.png", dpi = 300, width = 10, height = 6)
