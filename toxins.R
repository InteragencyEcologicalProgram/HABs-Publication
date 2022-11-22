#I just want toxin data from Franks Tract for the paper. Who collected that?

library(tidyverse)
library(lubridate)
library(sf)
library(deltamapr)

toxins = read_csv("Alltoxindata.csv")
load("NewHABregions.RData")

toxsf = st_as_sf(toxins, coords = c("Latitude", "Longitude"), crs = 4326)

toxsf2 = st_join(toxsf, Newregions) %>%
  mutate(Year = year(Date))

ggplot()+ geom_sf(data = WW_Delta) + geom_sf(data = toxsf)

toxFranks = filter(toxsf2, Region.y %in% c("OMR/Franks", "Lower SJ"), 
                   Station != "DHAB003", Station != "BigBreak", Station != "JPT", Station != "MDM")

ggplot()+ geom_sf(data = WW_Delta) + geom_sf(data = toxFranks, aes(color = Station))+
  #geom_sf_label(data = toxFranks, aes(label = Station))+
  coord_sf(xlim = c(-121.5, -121.7), ylim = c(37.9, 38.1))

toxFranks = filter(toxFranks, Analyte == "Microcystins", Year == 2021)

ggplot(toxFranks, aes(x = Date, y = result)) + geom_point()



#To plot the toxin data, I want to put it in terms of the health
#advisory levels from OEHHA. Here is a dataframe of those levels:
health = data.frame(Analyte = c("Microcystins", "Microcystins", "Microcystins"), Advisory = c(0.8, 6,20),
                    AdviseType = c("Caution\nTier I", "Warning \nTier II","Danger \nTier III")) %>%
  mutate(AdviseType = factor(AdviseType, levels = c("Caution\nTier I", "Warning \nTier II","Danger \nTier III")))



ggplot(toxFranks, aes(x = Date, y = result)) + geom_point(aes(shape = Station))+
  geom_hline(data = filter(health, AdviseType != "Danger \nTier III"), aes(yintercept = Advisory, color = AdviseType))+
  scale_color_manual(values = c("yellow", "orange", "red"), name = "Recreational \nAdvisory")+
  xlab("Month of 2021")+ ylab("Microcystin Concentration ug/L")+
  theme_bw()
