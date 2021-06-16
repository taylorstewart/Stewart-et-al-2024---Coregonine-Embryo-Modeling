library(dplyr)
library(data.table)
library(ggplot2)
library(rgdal)
library(ggpolypath)


climate.files.CB <- list.files("data/climate-simulations/lake-ontario-chaumont-bay", pattern = "rcp85", full.names = TRUE)[1]

data.CB <- fread(climate.files.CB)
data.coord <- data.CB %>% group_by(lon, lat) %>% 
  mutate(max.depth = paste0(max(depth), "-m")) %>% 
  distinct(lat, lon, max.depth)


lo_poly <- readOGR(dsn = "data/shapefile/lake-ontario.shp", layer = "lake-ontario")
lo_poly <- spTransform(lo_poly, CRS("+proj=longlat"))
lo_poly.fort <- fortify(lo_poly) 


ggplot(data = lo_poly.fort, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = hole), 
               color = "black", size = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c("#e9f3f8", "white")) + 
  geom_point(data = data.coord, aes(x = lon, y = lat), 
             color = "black", fill = "#de2d26", shape = 21, size = 3.25) +
  geom_label(data = data.coord, aes(x = lon, y = lat, label = max.depth), 
             nudge_y = 0.065, fontface = "bold", size = 3) +
  coord_fixed(ratio = 1.4) +
  scale_x_continuous(limits = c(-79.95, -75.85), breaks = seq(-76, -80.0, -0.5), expand = c(0, 0),
                     labels =  paste0(seq(-76, -80.0, -0.5), "°")) +
  scale_y_continuous(limits = c(43.1, 44.5), breaks = seq(43.25, 44.5, 0.5), expand = c(0, 0),
                     labels =  paste0(seq(43.25, 44.5, 0.5), "°")) +
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

ggsave("figures/climate-scenarios/LakeOntario-ISIMIP-Locations.png", dpi = 300, width = 10, height = 6)
