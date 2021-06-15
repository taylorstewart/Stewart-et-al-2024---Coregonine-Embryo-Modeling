library(dplyr)
library(data.table)
library(ggplot2)
library(rgdal)
library(ggpolypath)
library(raster)
library(rgeos)


climate.files <- grep(pattern = "(?=.*simstrat)(?=.*bottemp)(?=.*rcp26)", 
                      x = list.files(path = 'data/climate-simulations', recursive = TRUE, full.names = TRUE), 
                      value = TRUE, perl = TRUE)

data.coord <- do.call(rbind, lapply(climate.files, function(i) {
  data <- fread(i)
  data.coord <- data %>%
    distinct(lake, lat, lon)
}))


poly <- readOGR(dsn = "data/shapefiles/world.shp", layer = "world")
poly <- spTransform(poly, CRS("+proj=longlat"))

bb_poly <- as(extent(c(-180, 100, 20, 80)), "SpatialPolygons")
poly.clipped <- gIntersection(poly, bb_poly)
poly.fort <- fortify(poly.clipped) 


ggplot(data = poly.fort, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = hole), 
               color = "black", size = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c("#e9f3f8", "white")) + 
  geom_point(data = data.coord, aes(x = lon, y = lat), 
             color = "black", fill = "#de2d26", shape = 21, size = 2) +
  ggrepel::geom_label_repel(data = data.coord, aes(x = lon, y = lat, label = lake), 
                            color = "black", size = 3, max.overlaps = 50) +
  #coord_fixed(ratio = 1.4) +
  scale_x_continuous(limits = c(-180, 100), breaks = seq(-180, 100, 40), expand = c(0, 0),
                     labels =  paste0(seq(-180, 100, 40), "°")) +
  scale_y_continuous(limits = c(20, 80), breaks = seq(20, 80, 10), expand = c(0, 0),
                     labels =  paste0(seq(20, 80, 10), "°")) +
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
        plot.margin = unit(c(3, 7, 2, 3), 'mm'),
        panel.ontop = TRUE)

ggsave("figures/climate-scenarios/LocalLake-ISIMIP-Locations.png", dpi = 300, width = 15, height = 7)
