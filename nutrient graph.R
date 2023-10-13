#Nutrient grqaphs
library(DroughtData)
library(sf)
library(tidyverse)
library(lubridate)
load("NewHABregions.RData")
st_write(Newregions, dsn = "outputs/HABRegions.shp")


nut = raw_nutr_2013_2021 %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  select(-Region)

nut2 = st_join(nut, Newregions) %>%
  filter(!is.na(Region)) %>%
  mutate(Month = month(Date)) %>%
  drt_replace_rl(data_var = DissNitrateNitrite, sign_var = DissNitrateNitrite_Sign) %>%
  drt_replace_rl(data_var = DissOrthophos, sign_var = DissOrthophos_Sign) %>%
  drt_replace_rl(data_var = DissAmmonia, sign_var = DissAmmonia_Sign)

nut3 = pivot_longer(nut2, cols = c(DissNitrateNitrite, DissOrthophos, DissAmmonia),
                    values_to = "Value", names_to = "Analyte")

nutsum = group_by(nut3, Month, Analyte, Region) %>%
  summarize(ValueM = mean(Value, na.rm = T), sd = sd(Value, na.rm = T), se = sd(Value, na.rm =T)/sqrt(n()),
            lower = quantile(Value, .25,  na.rm = T), upper = quantile(Value, .75, na.rm = T)) %>%
  mutate(Analyte2 =case_when(Analyte == "DissAmmonia" ~ "Ammonium",
                             Analyte == "DissNitrateNitrite" ~ "Nitrate + Nitrite",
                             Analyte == "DissOrthophos" ~ "Orthophosphate"),
         Region = case_when(Region == "OMR/Franks" ~ "Franks/OMR",
                            TRUE ~ Region))

ggplot(nutsum, aes(x = Month, y = ValueM, fill = Region)) + geom_col()+
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  facet_grid(Analyte2~Region, scales = "free_y")+
  theme_bw() + ylab("Concentration (mg/L)")+
  scale_fill_manual(values = Newregions$colors, guide = NULL) +
  scale_x_continuous(breaks = c(2,5,8,11))

ggsave("plots/nutrientbarplot.tiff", device = "tiff", width = 6, height = 5)
