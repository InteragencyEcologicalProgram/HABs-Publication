#Hab analysis - regional and water year comparisons

library(tidyverse)
library(lubridate)
library(ggplot2)
library(lemon)
library(sf)
library(brms)

#library(StanHeaders)
#library(lme4)
#library(lmerTest)
#library(emmeans)
#library(deltamapr)
#library(visreg)
#library(MASS)
#library(car)
#library(lubridate)


mypath <- "C:/Users/kbouma-gregson/OneDrive - DOI/Documents/DWR/HABs_paper/HABs-Publication/MicrocystisRatingModels_KBG"


#import data with all the visual index data
#load("data/data package/HABs.RData")
load(here::here(mypath,"Data", "HABs.RData"))

# import region data
load(here::here(mypath,"Data", "NewHABregions.RData"))

# Import water year types
wy_SacIndex <- read_csv(here::here(mypath,"Data", "WaterYearAssignments.csv")) %>% 
  filter(Year >= 2007) %>% 
  rename("SacIndex"= Index)
wy_DrSynth <- read_csv(here::here(mypath,"Data", "water_year_type.txt"))


#### DATA FORMAT ####
## Clean up data
HABs2 <- HABs %>% 
  select(Source, Station, Latitude, Longitude, Date, Microcystis, Year, StationID, Month, -Year) %>% 
  filter(!is.na(Microcystis)) %>% 
  filter(`Source` != "DOP") %>% # Remove DOP because they use a different method
  filter(Month >= 5 & Month <= 12) %>%  # limit to June - October
  filter(!is.na(Longitude) | !is.na(Latitude)) %>% 
  rename("mc_rating" = Microcystis) %>% 
  mutate(Year= year(Date),
         YearF= as.factor(Year),
         MonthF= factor(Month, levels = c(5, 6,7,8,9, 10, 11, 12),
                         labels = c("May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))  


## Add water year types (Sacramento Valley Index)
HABs2.wyt <- HABs2 %>% 
 # st_drop_geometry() %>% 
  #select(-colors, -nudge) %>% 
  left_join(., wy_SacIndex)




## Transform original 1-5 scale to None, Low, High
HABs3 <- HABs2.wyt %>%
  mutate(mc_mod= ifelse(mc_rating == 1, "none",
                        ifelse(mc_rating > 1 & mc_rating < 4, "low", "high")),
         mc_mod= factor(mc_mod, ordered= TRUE, levels= c("none", "low", "high")))

## Calculate maximum and minimum MC rating for each month
HABs3.MaxMin <-  HABs3 %>% 
  group_by(Source, Station, Longitude, Latitude, YearF, MonthF, SacIndex, Yr_type, Drought) %>% 
  summarize(mc_max= max(mc_mod),
            mc_min= min(mc_mod)) %>% 
  ungroup()
  

## Spatial join
HABs3.sf <-  HABs3.MaxMin %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(4326))

HABs3.regions <-  st_join(HABs3.sf, Newregions) %>%
  filter(!is.na(Region))

## Plot points
# ggplot() +
#   geom_sf(data= Newregions) +
#   geom_sf(data= HABs2.sf) =
#   coord_sf()
# 
# 
# ggplot() +
#   geom_sf(data= Newregions) +
#   geom_sf(data= HABs2.regions) +
#   coord_sf()

  
## Format for statistical models
HABs.stats <- HABs3.regions %>% 
  st_drop_geometry() %>% 
  select(-colors, -nudge)


#### SUMMARY TABLES ####
unique(HABs.stats$Station)
table(HABs.stats$Source)
HABs.stats %>% 
  select(YearF, Yr_type) %>% 
  distinct() %>% 
  count(Yr_type)


#### STATISTICS ####

fit_max_mc_SI.cumu <- brm(
  formula = mc_max ~ 1 + cs(Yr_type) + MonthF + Region + (1|Station),
  data = HABs.stats,
  family = cumulative("probit"),
  chains= 4,
  iter= 5000,
  warmup= 1000,
  cores= 4,
  backend = "cmdstanr")
  #control = list(adapt_delta = 0.99)

# No difference in output between cumulative() and acat() family
fit_max_mc_SI.acat <- brm(
  formula = mc_max ~ 1 + cs(Yr_type) + MonthF + Region + (1|Station),
  data = HABs.stats,
  family = acat("probit"),
  chains= 4,
  iter= 5000,
  warmup= 1000,
  cores= 4,
  backend = "cmdstanr")
#control = list(adapt_delta = 0.99)

fit_max_mc_SI.region.cumu <- brm(
  formula = mc_max ~ 1 + cs(Yr_type) + Region + (1|Station),
  data = HABs.stats,
  family = cumulative("probit"),
  chains= 4,
  iter= 5000,
  warmup= 1000,
  cores= 4,
  backend = "cmdstanr")
#control = list(adapt_delta = 0.99)



#save(fit_max_mc_SI.1, file= "Data/fit_max_mc1b.Rdata")
#load("Data/fit_max_mc1b.Rdata")
#summary(fit_max_mc_SI.1)
#plot(fit_max_mc_SI.1)

## Extract marginal effects
max_mc1_conditions.cumu <- make_conditions(fit_max_mc_SI.cumu, c("Region", "MonthF"))
max_mc1_effects.cumu <- conditional_effects(fit_max_mc_SI.cumu, "Yr_type", condition= max_mc1_conditions.cumu, categorical= TRUE)$`Yr_type` %>% 
  mutate(Yr_type= factor(Yr_type, ordered= TRUE, levels= c("Critical", "Dry", "Below Normal", "Wet")))

max_mc2_conditions.cumu <- make_conditions(fit_max_mc_SI.cumu, c("Region"))
max_mc2_effects.cumu <- conditional_effects(fit_max_mc_SI.cumu, "Yr_type", condition= max_mc2_conditions.cumu, categorical= TRUE)$`Yr_type` %>% 
  mutate(Yr_type= factor(Yr_type, ordered= TRUE, levels= c("Critical", "Dry", "Below Normal", "Wet")))

max_mc1_conditions.region.cumu <- make_conditions(fit_max_mc_SI.region.cumu, c("Region"))
max_mc1_effects.region.cumu <- conditional_effects(fit_max_mc_SI.region.cumu, "Yr_type", condition= max_mc1_conditions.region.cumu, categorical= TRUE)$`Yr_type` %>% 
  mutate(Yr_type= factor(Yr_type, ordered= TRUE, levels= c("Critical", "Dry", "Below Normal", "Wet")))



max_mc1_conditions.acat <- make_conditions(fit_max_mc_SI.acat, c("Region", "MonthF"))
max_mc1_effects2.acat <- conditional_effects(fit_max_mc_SI.acat, "Yr_type", condition= max_mc1_conditions.acat, categorical= TRUE)$`Yr_type` %>% 
  mutate(Yr_type= factor(Yr_type, ordered= TRUE, levels= c("Critical", "Dry", "Below Normal", "Wet")))



ggplot(max_mc1_effects.cumu, aes(x= cats__, y= estimate__, group= Yr_type)) +
  geom_col(aes(fill= Yr_type), color= "black", position= position_dodge()) +
  geom_errorbar(aes(ymin= lower__, ymax= upper__), width= 0.5, position= position_dodge(0.9)) +
  scale_fill_manual(values= c("firebrick", "darkorange2", "lightgoldenrod", "lightskyblue"), 
                    name= "Water Year") +
  labs(x= expression(paste(italic("Microcystis"), " Rating Level")), y= "Probability", title= "cumu") +
  scale_y_continuous(expand= c(0, 0), limits= c(0, 1)) +
  facet_rep_grid(Region ~ MonthF, repeat.tick.labels = TRUE) 
  ggsave(last_plot(), filename= "MCrating_RegionMonth.png", width= 20, height= 16, dpi= 300,
       path= here::here(mypath, "Figures"))

ggplot(max_mc1_effects.region.cumu, aes(x= cats__, y= estimate__, group= Yr_type)) +
  geom_col(aes(fill= Yr_type), color= "black", position= position_dodge()) +
  geom_errorbar(aes(ymin= lower__, ymax= upper__), width= 0.5, position= position_dodge(0.9)) +
  scale_fill_manual(values= c("firebrick", "darkorange2", "lightgoldenrod", "lightskyblue"), 
                    name= "Water Year") +
  labs(x= expression(paste(italic("Microcystis"), " Rating Level")), y= "Probability") +
  scale_y_continuous(expand= c(0, 0), limits= c(0, 1)) +
  facet_rep_grid(~ Region, repeat.tick.labels = TRUE) 
ggsave(last_plot(), filename= "MCrating_Region.png", width= 10, height= 4, dpi= 300,
       path= here::here(mypath, "Figures"))

ggplot(max_mc1_effects2.acat, aes(x= cats__, y= estimate__, group= Yr_type)) +
    geom_col(aes(fill= Yr_type), color= "black", position= position_dodge()) +
  geom_errorbar(aes(ymin= lower__, ymax= upper__), width= 0.5, position= position_dodge(0.9)) +
  scale_fill_manual(values= c("firebrick", "darkorange2", "lightgoldenrod", "lightskyblue"), 
                    name= "Water Year") +
    labs(x= expression(paste(italic("Microcystis"), " Rating Level")), y= "Probability", title= "acat") +
  scale_y_continuous(expand= c(0, 0), limits= c(0, 1)) +
    facet_rep_grid(Region ~ MonthF, repeat.tick.labels = TRUE)






#### ROSIE'S PREVIOUS CODE BELOW #####################################


unique(Habs2$Year)

#check a few plots for outliers
ggplot(HABs, aes(x = Temperature)) + geom_histogram()
summary(HABs$Temperature)
filter(HABs, Temperature <5)
#missing 120 rows, and some of those are 0s, definitely wrong.
HABs = filter(HABs, Temperature >5)

ggplot(HABs, aes(x = Secchi)) + geom_histogram()
summary(HABs$Secchi)
test =filter(HABs, Secchi <10)

group_by(HABs, Source) %>%
  summarize(secm = min(Secchi, na.rm = T), secM = max(Secchi, na.rm = T))

#Ugh, definitely some more rows where Secchi is in meters, not centemeters. But its not consistent!

HABs = mutate(HABs, Secchi = case_when(Secchi <5 ~Secchi *100,
                                       TRUE ~ Secchi))
summary(HABs$Secchi)
summary(HABs$Temperature)

#convert HAB data to a spatial object and plot it
HABssf = filter(HABs, !is.na(Longitude), !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(4326))

############################################################################
###################################################################
#Now let's do the entire year, by regions
# (but just the regions we're interested in)

Habs2 =   st_join(HABssf, regions) %>%
  st_drop_geometry() %>%
  filter(!is.na(Stratum), !is.na(Microcystis)) %>% 
  mutate(Year = year(Date), Yearf = as.factor(Year),
         Month2 = factor(Month, levels = c(6,7,8,9,10),
                         labels = c("Jun", "Jul", "Aug", "Sep", "Oct")))   

Habs2 =   HABs %>% 
  #st_join(HABssf, regions) %>%
  #st_drop_geometry() %>%
  #filter(!is.na(Stratum), !is.na(Microcystis)) %>% 
  #filter(!is.na(Microcystis)) %>% 
  mutate(Year = year(Date), Yearf = as.factor(Year),
         Month2 = factor(Month, levels = c(6,7,8,9,10),
                         labels = c("Jun", "Jul", "Aug", "Sep", "Oct")))   



####################################################################################
#Models for HAB weed report

#This is the data for table 2-2
effort = group_by(Habs2, Year, Stratum2) %>%
  summarize(N = n()) %>%
  pivot_wider(id_cols = Stratum2, names_from = Year, values_from = N)

write.csv(effort, "outputs/visualindexeffort.csv")



##############################################################
#ordered logistic regression
HABs3 = Habs2

Habs2 = mutate(Habs2, HABord = case_when(
  Microcystis == 1 ~ "Absent",
  Microcystis %in% c(2,3) ~ "Low",
  Microcystis %in% c(4,5) ~ "High")) %>%
  mutate(HABord = factor(HABord, levels = c("Absent", "Low", "High"), ordered = T)) %>%
  filter(Year >2013) %>%
  droplevels()


#now an orgered logistic regression
library(multcomp)
ord2 = polr(HABord ~ Yearf + Stratum2, data = Habs2, Hess = T)
summary(ord2)
Anova(ord2)
pairs = emmeans(ord2, pairwise ~ Yearf)
cont = pairs$contrasts
plot(emmeans(ord2, pairwise ~ Yearf), comparisons = TRUE)
tukcfg = cld(emmeans(ord2, pairwise ~ Yearf), Letters = letters) %>%
  mutate(Year = as.numeric(as.character(Yearf)), 
         Letter = str_trim(.group)) 

tukcfg2 = cld(emmeans(ord2, pairwise ~ Stratum2), Letters = letters) %>%
  mutate( 
    Letter = str_trim(.group)) 

#this is table 2-11
Tuekyresults = bind_rows(tukcfg, tukcfg2)
write.csv(Tuekyresults, "outputs/Pairwise_visualdata_July.csv")

write.csv(pairs, "visualdata_alldelta_July.csv")
pr <- profile(ord2)
confint(pr)
plot(pr)
pairs(pr)

#This is figure 2-27 
#Plot across the whole Delta, just summer/fall
ggplot(HABs3, aes(x = Year, fill = as.factor(Microcystis))) +
  geom_bar(position = "fill", color = "grey")+ 
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), 
                    labels = c("absent", "low", "medium", "high", "very high"),
                    name = "Microcystis")+ ylab("Relative Frequency") +
  geom_text(data = tukcfg, aes(x = Year, y = 0.7, label = Letter), inherit.aes = F)

#ggsave("YearHAB.tiff", device = "tiff", width = 6, height = 5)


#Plot for paper with just three categories
#
ggplot(Habs2, aes(x = Year, fill = HABord)) +
  geom_bar(position = "fill", color = "grey")+ 
  scale_fill_manual(values = c("white", "orange", "red"), 
                    labels = c("absent", "low", "high"),
                    name = "Microcystis")+ ylab("Relative Frequency") +
  geom_text(data = tukcfg, aes(x = Year, y = 0.7, label = Letter), inherit.aes = F)
ggsave("plots/YearHAB_3cat.tiff", device = "tiff", width = 6, height = 5)


(ctable <- coef(summary(ord2)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
#This is table 2-10
(ctable <- cbind(ctable, "p value" = p))
write.csv(ctable, "outputs/Visualindexmodel_july.csv")

(ci <- confint(ord2))
exp(cbind(OR = coef(ord2), ci))
###################################################


#Now we will do a seperate logistic regression for each region
HabMod = nest_by(Habs2, Stratum2) %>%
  mutate(mod = list(polr(HABord ~Yearf, data = data, Hess = T)),
         pairs = list(emmeans(mod, pairwise ~ Yearf)),
         CLD = list(cld(pairs, Letters = letters)))

#pairwise comparisons
RegTuk = summarize(HabMod, broom::tidy(CLD))%>%
  mutate(Year = as.numeric(as.character(Yearf)), 
         Letter = str_trim(.group)) %>%
  rename(emmean = estimate, std.erroremm = std.error)

regMod = summarize(HabMod, broom::tidy(mod)) %>%
  mutate(Yearf = str_sub(term, start = 6, end = 9))

#table of coefficients
ctable <- summarize(HabMod, ctab = coef(summary(mod)),
                    p = pnorm(abs(ctab[, "t value"]), lower.tail = FALSE) * 2)


#Table for appendix A
regMod2 = left_join(regMod, RegTuk) %>%
  bind_cols(ctable)
#write.csv(regMod2, "outputs/regionalresults.csv")


######################################################################################
#plots by year and region

#By Region, just summer/fall
#This is plot 2-28
ggplot(Habs2, aes(x = Year, fill = as.factor(Microcystis))) +
  geom_bar(position = "fill", color = "grey")+ facet_wrap(~Stratum2, nrow = 4)+
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), 
                    labels = c("absent", "low", "medium", "high", "very high"),
                    name = "Microcystis")+ ylab("Relative Frequency") +
  geom_text(data = RegTuk, aes(x = Year, y = 0.9, label = Letter), size = 4, inherit.aes = FALSE)+
  theme_bw()+ theme(legend.position = "top", legend.key = element_rect(color = "black"))

#ggsave("RegionalHAB.tiff", device = "tiff", width = 6, height = 7)

#now with just three categories
ggplot(Habs2, aes(x = Year, fill = HABord)) +
  geom_bar(position = "fill", color = "grey")+ facet_wrap(~Stratum2, nrow = 4)+
  scale_fill_manual(values = c("white", "orange",  "red"), 
                    labels = c("absent", "low", "high"),
                    name = "Microcystis")+ ylab("Relative Frequency") +
  geom_text(data = RegTuk, aes(x = Year, y = 0.9, label = Letter), size = 4, inherit.aes = FALSE)+
  theme_bw()+ theme(legend.position = "top", legend.key = element_rect(color = "black"))