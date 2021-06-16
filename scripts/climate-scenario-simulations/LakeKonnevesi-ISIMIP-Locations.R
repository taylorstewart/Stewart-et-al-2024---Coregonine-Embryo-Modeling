library(dplyr)
library(data.table)
library(ggplot2)
library(rgdal)
library(ggpolypath)
library(raster)
library(ggnewscale)
library(rgeos)


climate.files.LK <- list.files("data/climate-simulations/lake-konnevesi", pattern = "rcp85", full.names = TRUE)[1]

data.LK <- fread(climate.files.LK)
data.coord <- data.LK %>% group_by(lon, lat) %>% 
  mutate(max.depth = paste0(max(depth), "-m")) %>% 
  distinct(lat, lon, max.depth)


lk_poly <- readOGR(dsn = "data/shapefile/lake-konnevesi.shp", layer = "lake-konnevesi")
lk_poly <- spTransform(lk_poly, CRS("+proj=longlat"))
lk_poly.fort <- fortify(lk_poly) 

fin_lakes <- readOGR(dsn = "data/shapefile/finland-11000000-lakes.shp", layer = "finland-11000000-lakes")
fin_lakes <- spTransform(fin_lakes, CRS("+proj=longlat"))

bb_poly <- as(extent(c(26, 27, 62, 63)), "SpatialPolygons")

fin_lakes.clipped <- gIntersection(fin_lakes, bb_poly)


fin_lakes.fort <- fortify(fin_lakes.clipped) 


ggplot(data = fin_lakes.fort, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = hole), 
               color = "black", size = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c("#e9f3f8", "white")) + 
  new_scale_fill() + 
  geom_polygon(data = lk_poly.fort, aes(group = group, fill = hole), 
               color = "black", size = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c("#97CFEB", "white")) + 
  geom_point(data = data.coord, aes(x = lon, y = lat), 
             color = "black", fill = "#de2d26", shape = 21, size = 4) +
  geom_label(data = data.coord, aes(x = lon, y = lat, label = max.depth), 
             nudge_y = 0.03, fontface = "bold", size = 4) +
  coord_fixed(ratio = 1.4) +
  scale_x_continuous(limits = c(26, 27), breaks = seq(26, 27, 0.25), expand = c(0, 0),
                     labels =  paste0(seq(26, 27, 0.25), "°")) +
  scale_y_continuous(limits = c(62, 63), breaks = seq(62, 63, 0.25), expand = c(0, 0),
                     labels =  paste0(seq(62, 63, 0.25), "°")) +
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
        plot.margin = unit(c(3, 5, 2, 3), 'mm'),
        panel.ontop = TRUE)

ggsave("figures/climate-scenarios/LakeKonnevesi-ISIMIP-Locations.png", dpi = 300, width = 6, height = 7.75)
