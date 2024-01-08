#maps

library(tidyverse)
library(sf)
library(deltamapr)
library(lubridate)
library(sfheaders)
library(ggsn)
library(readxl)
library(ggspatial)


mypal = Newregions$colors
load("NewHABregions.RData")

# Map of regions for pwper ####################
# This may need tweaking in illustrator
ggplot() + geom_sf(data = Newregions, aes(fill = Region), alpha = 0.7)+
  geom_sf(data = WW_Delta, fill = "lightblue")+ # + geom_sf(data = HABssf1)+
  scale_fill_manual(values = Newregions$colors, guide = NULL)+
  geom_sf_label(data = Newregions,aes(label = Region))+
  coord_sf(xlim = c(-121.3, -121.9), ylim = c(37.7, 38.6))+
  theme_bw()+
  north(data = Newregions)+
  scalebar(data = Newregions, transform = T, dist = 10, dist_unit = "km", location = "bottomleft")+
  xlab(NULL) + ylab(NULL)

# Map of barrier for pwper ####################
# This may need tweaking in illustrator

barrier = data.frame(Latitude = 38.0576168, Longitude = -121.6712761, label = "Salinity Barrier")
library(ggmap)
basemap = get_stamenmap(bbox = c(-121, 38, -122, 39), messaging = TRUE, maptype = "toner")
ggmap(basemap)
ggplot() + geom_sf(data = Newregions, aes(fill = Region), alpha = 0.4)+
  geom_sf(data = WW_Delta, fill = "lightblue")+ # + geom_sf(data = HABssf1)+
  scale_fill_manual(values = Newregions$colors, guide = NULL)+
  annotate("point", x = -121.6712761, y = 38.0576168, size = 5, shape = 17)+
  annotate("label", x = -121.67, y = 38.07, label = "Barrier", size = 8)+
  coord_sf(xlim = c(-121.5, -121.7), ylim = c(38, 38.15))+
  theme_bw()+
  north(data = Newregions)+
  scalebar(data = Newregions, transform = T, dist = 10, dist_unit = "km", location = "bottomleft")+
  xlab(NULL) + ylab(NULL)




ggplot() + geom_sf(data = WW_Delta, fill = "lightgrey")+
  geom_sf(data = Newregions, aes(fill = Region), alpha = 0.4) +
  scale_fill_manual(values = Newregions$colors, guide = NULL)+
  geom_sf(data = filter(HABssf, Source != "DOP", !Station %in% c("EZ2", "EZ6", "EZ2-SJR", "EZ6-SJR")), aes(shape = Source)) +
  scale_shape_discrete(name = "Visual Index Sites")+
  scale_color_manual(values = c("red", "blue"), name = NULL)+

  coord_sf(xlim = c(-121.9, -121.2), ylim = c(37.7, 38.6))+
  geom_sf_label(data = Newregions, aes(label = Region),
                label.size = 0.05,
                label.padding = unit(0.1, "lines"),
                nudge_y = Newregions$nudge, alpha = 0.8, fontface = "bold")+

  scalebar(dist = 10, dist_unit = "km",
           transform = TRUE, st.dist = .05, x.min = -121.6, x.max = -121.8, y.min = 37.7, y.max = 37.9, height = 0.05) +

  north(data = Newregions, symbol = 2) +
  theme_bw()+ylab("")+xlab("")

ggsave(file = "HABMAP.pdf", device = "pdf", width = 6, height = 6, units = "in")


#OK, now the nutrient statiosn and continous stations

nnuts <- read_csv("hab_nutr_chla_mvi.csv")
nuts = select(nnuts, Source, Station, Latitude, Longitude) %>%
  distinct() %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

continuous = read_excel("continuous stations.xlsx")
cont = continuous %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  filter(Type == "WQ")



ggplot() + geom_sf(data = WW_Delta, fill = "lightgrey")+
  geom_sf(data = Newregions, aes(fill = Region), alpha = 0.4) +
  scale_fill_manual(values = Newregions$colors, guide = NULL)+
  geom_sf(data = nuts, aes(shape = Source), fill = "white", color = "black") +
  scale_shape_manual(values = c(21, 22, 23, 2), name = "Nuritent Sites")+
  geom_sf(data = cont, aes(color = "Continuous WQ"), shape = 21, fill = "red")+
  scale_color_manual(values = "black", name = NULL)+
  coord_sf(xlim = c(-121.9, -121.2), ylim = c(37.7, 38.6))+
  # geom_sf_label(data = Newregions, aes(label = Region),
  #               label.size = 0.05,
  #               label.padding = unit(0.1, "lines"),
  #               nudge_y = Newregions$nudge, alpha = 0.8, fontface = "bold")+
  #
  scalebar(dist = 10, dist_unit = "km",
           transform = TRUE, st.dist = .05, x.min = -121.6, x.max = -121.8, y.min = 37.7, y.max = 37.9, height = 0.05) +

  north(data = Newregions, symbol = 2) +
  theme_bw()+ylab("")+xlab("")

ggsave(file = "HABMAP_continuous.pdf", device = "pdf", width = 6, height = 6, units = "in")

