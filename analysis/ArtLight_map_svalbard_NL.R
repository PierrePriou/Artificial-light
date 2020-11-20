# Map of Svalbard with cruise tracks or stars to indicate stations
#
# Created: 25/08/2020
# Pierre Priou <pierre.priou@mi.mun.ca># 

library(plyr)
library(tidyverse)
library(marmap)
library(raster)
library(rgdal)
# library(rgeos) # to trim shape file
# library(graticule) # to create graticule and its labels (latitude and longitude lines)
library(lubridate)
library(cmocean) 
library(pals)
source("R/getNOAA.ice.bathy.R") # Download bathy


# Set the default theme for ggplot objects to theme_bw()
theme_set(theme_bw())
theme_update(panel.border = element_rect(fill = NA),
             panel.grid = element_blank(),
             legend.position = "none",
             axis.text = element_text(size = 8), 
             axis.title = element_blank(),
             plot.margin =  unit(c(0.05,0.02,0,0.02), "in")) # t,r,b,l



# Load bathy ----
# Map limits - Svalbard
Sval_lonmin <- 6; Sval_lonmax <- 30
Sval_latmin <- 76; Sval_latmax <- 88

# # Map limits - Newfoundland
# NL_lonmin <- -54; NL_lonmax <- -53.5
# NL_latmin <- 48; NL_latmax <- 48.25
NL_lonmin <- -54; NL_lonmax <- -53.5
NL_latmin <- 46.5; NL_latmax <- 48.5

# Map limits - North Hemisphere
NH_lonmin <- -70; NH_lonmax <- 30
NH_latmin <- 45; NH_latmax <- 85

# Download bathy - Svalbard
Sval_bathy <- getNOAA.ice.bathy(lon1 = Sval_lonmin, lon2 = Sval_lonmax, 
                                lat1 = Sval_latmin, lat2 = Sval_latmax, 
                                resolution = 1,  keep = T, path = "data/")

# Download bathy - NL see https://gadm.org/maps/CAN/newfoundlandandlabrador_2.html
Canada <- getData("GADM", country="CAN", level = 2)
NL <-(Canada[Canada$NAME_1=="Newfoundland and Labrador",]) 
SS_coast <-(NL[NL$NAME_2=="Division No. 7",]) 

# Download bathy - North hemisphere
NH_bathy <- getNOAA.ice.bathy(lon1 = NH_lonmin, lon2 = NH_lonmax, 
                              lat1 = NH_latmin, lat2 = NH_latmax, 
                              resolution = 5,  keep = T, path = "data/")
# # Reproject bathy 
# r1 <- marmap::as.raster(NH_bathy)
# # newproj <- "+proj=aea +lat_1=65 +lon_0=-20 +units=m"
# newproj <- "+proj=moll +lon_0=65"
# r2 <- projectRaster(r1, crs = newproj)
# NH_newproj <- as.bathy(r2)





# Plot Svalbard ggplot ----
map_Svalbard <- autoplot.bathy(Sval_bathy, geom = c("t"), coast=F) +
  # scale_fill_gradientn(colours = rev(cmocean("deep")(256)), limits = c(min(Sval_bathy),0), na.value = "grey") +
  scale_fill_gradientn(colours = rev(brewer.blues(256)), limits = c(min(Sval_bathy),0), na.value = "grey") +
  scale_x_continuous(breaks = seq(10,25,5), 
                     labels = c("10°E", "15°E", "20°E", "25°E"), 
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(76,84),
                     breaks = seq(76,80,2), 
                     labels = c("76°N", "78°N", "80°N"), 
                     expand = c(0,0))
# map_Svalbard
ggsave("plots/map_Svalbard.png", map_Svalbard, 
       width = 2.2, height = 3.2, units = "in", dpi = 300)

# Newfoundland
map_NL <- SS_coast %>%
  ggplot(.) +
  geom_polygon(aes(long,lat, group = group), fill = "grey80") +
  # geom_point(aes(x = -53.667, y = 48.165), cex = 1, col = "black") + # White pelagic
  # geom_point(aes(x = -53.67, y = 48.16), cex = 1, col = "red") + # red pelagic - need to veryify deployment coords!!
  # geom_point(data = st_log, aes(x = Lon, y = Lat, col = event), cex = 2) + 
  coord_map(xlim = c(-54, -53.47), ylim = c(48, 48.27)) +
  scale_x_continuous(breaks = seq(-54, -53.6, 0.2),
                     labels = c("54.0°W", "53.8°W", "53.6°W"),
                     expand = c(0,0)) + 
  scale_y_continuous(breaks = seq(48.0, 48.2, 0.1),
                     labels = c("48.0°N", "48.1°N", "48.2°N"),
                     expand = c(0,0))
map_NL
ggsave("plots/map_Newfoundland.png", map_NL, 
       width = 2, height = 2, units = "in", dpi = 300)

# North hemisphere
map_NH <- autoplot.bathy(NH_bathy, geom = c("t"), coast=F) +
  # scale_fill_gradientn(colours = rev(cmocean("deep")(256)), limits = c(min(Sval_bathy),0), na.value = "grey") +
  scale_fill_gradientn(colours = rev(brewer.blues(256)), limits = c(min(NH_bathy),0), na.value = "grey") +
  scale_x_continuous(expand = c(0,0), breaks = c(0)) +
  scale_y_continuous(expand = c(0,0), breaks = c(45,80)) +
  theme(#axis.text = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.margin =  unit(c(0,0,0,0), "in"))
ggsave("plots/map_North_Hemisphere.png", map_NH, 
       width = 1.2, height = 1.2, units = "in", dpi = 300)

