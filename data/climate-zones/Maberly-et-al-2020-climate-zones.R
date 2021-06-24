library(tidyverse)
library(ggplot2)
library(readxl)
library(sf)
library(nngeo)

load("data/climate-zones/globalpos.Rdata")
load("data/climate-zones/globalclass.Rdata")
load("data/climate-zones/globalflakemat.Rdata")
load("data/climate-zones/uncertall.Rdata")

skip <- which(rowSums(globalflakemat==0)>410)
uall <- round(uncertall[-skip, ],2)

pos <- data.frame(globalpos)
names(pos) <- c("x","y")
gp <- data.frame(cbind(x=globalpos[,1],y=globalpos[,2], group=globalclass ))[!is.na(globalclass),]
gclass <- globalclass[-skip]
post <- rep(NA, length(gclass))
for(i in 1:length(post)){ post[i] <- uall[i, as.numeric(gclass[i])]}

cnames <- c("Northern Frigid",
            "Northern Cool",
            "Northern Temperate",
            "Northern Warm",
            "Northern Hot",
            "Tropical Hot",
            "Southern Hot",
            "Southern Warm",
            "Southern Temperate")

colinfo <- as.data.frame(rbind(c(4,	'NF',	102,	102,	102),
                               c(5,	'NC',	69,   117,	180),
                               c(9,	'NT',	116,	173,	209),
                               c(1,	'NW',	255,	217,	47),
                               c(2,	'NH',	255,	127,	0),
                               c(3,	'TH',	228,	26,	  28),
                               c(6,	'SH',	231,	138,	195),
                               c(8,	'SW',	153,	112,	171), 
                               c(7,	'ST',	84,	  39,	  136)))

colnames(colinfo) <- c("GroupNum", "GroupCode", "R","G","B")

cols <- apply(colinfo, 1, function(x){rgb(red=x[3], green=x[4], blue=x[5],, maxColorValue=255)})
#mc=c("#0000CD", "#1E90FF","#ADD8E6","#EEC900" ,"#FF8C00","#EE0000", "#EEA9B8","#9ACD32", "#228B22")[order(c(4, 5, 9, 1, 2, 3, 6, 8, 7))]
mc=cols[order(c(4, 5, 9, 1, 2, 3, 6, 8, 7))]


codes <- c("NF", "NC", "NT", "NW", "NH", "TH", "SH", "SW", "ST" )
#cols <- c("Dark blue", "Mid blue", "Light blue", "Yellow", "Orange", "Red", "Pink", "Light Green","Dark Green")
dat <- data.frame(cbind("Name"=cnames, "Code"=codes, "Colour"=cols))


sumfun <- function(x){data.frame(round(cbind(min=min(x, na.rm=TRUE), 
                                             max=max(x, na.rm=TRUE), 
                                             mean=mean(x, na.rm=TRUE),
                                             percent.zeros=sum(x==0)*100/length(x)),2))}

lakedat <- do.call(rbind, apply(globalflakemat,1,sumfun))


mp <- NULL
mapWorld <- borders("world", colour="grey80", fill="grey80")
mp <- ggplot() +   mapWorld + labs(x="Longitude", y="Latitude") + theme(legend.title=element_text(size=rel(1.75)) , legend.text=element_text(size=rel(1.75)),axis.text = element_text(size = rel(1.75)), axis.title = element_text(size = rel(1.75)))
gp_ <- gp
gp_$group <- recode(gp_$group, "4"="NF", "5"="NC", "9"="NT",
                    "1"="NW", "2"="NH", "3"="TH",
                    "6"="SH", "8"="SW", "7"="ST") 

mp1 <- mp+ 
  geom_point(data=gp_, aes(x=gp_$x, y=gp_$y, col=factor(gp_$group, levels=c("NW", "NH", "TH", "NF", "NC", "SH", "ST", "SW", "NT"))),
             size=2, shape=15) + 
  labs(color = "Group")  + 
  scale_color_manual(values=mc) + guides(color = guide_legend(override.aes = list(size=5)))
mp1

write.csv(gp_, "data/climate-zones/Maberly-et-al-2020-climate-zones.csv", row.names = FALSE)

cz.points <- st_as_sf(gp_, coords = c("x", "y"))

lake.locations <- read_excel("data/climate-simulations/local-lake-locations.xlsx", sheet = "Sheet1") %>% 
  mutate(lake = toupper(lake))

lake.points <- st_as_sf(lake.locations, coords = c("lon", "lat"))

closest.cz <- st_nn(lake.points, cz.points)
closest.cz.df <- do.call(rbind, closest.cz)

lake.location.cz <- gp_[c(closest.cz.df),] %>% dplyr::bind_cols(lake.locations) %>% 
  select(lake, lat, lon, continent, country, region, lat.group, climate.group = group, max.depth.m, depth.group, include)

write.csv(lake.location.cz, "data/climate-simulations/local-lake-locations.csv", row.names = FALSE)

model.locations <- read_excel("data/model-lake-locations.xlsx", sheet = "coords")

model.points <- st_as_sf(model.locations, coords = c("lon", "lat"))

closest.cz <- st_nn(model.points, cz.points)
closest.cz.df <- do.call(rbind, closest.cz)

model.location.cz <- gp_[c(closest.cz.df),] %>% dplyr::bind_cols(model.locations) 


