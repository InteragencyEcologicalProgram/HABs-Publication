#Code for models of frequency of Microcystis occurence in the South Delta
#presented in Bouma-Gregson et al, Delta  Blue(green)s : The Impact of
#Drought and Drought Management Actions on Microcystis in the Sacramento- San Joaquin Delta
#
# Data is available here:
# Hartman, R., D. Bosworth, N. Rasmussen, T. Flynn, K. Bouma-Gregson, S. Khanna, and J. Frantzich.
# 2023. Harmful algal bloom and aquatic weeds data from the Sacramento-San Joaquin Delta,
# collected to evaluate the impact of the 2021 Temporary Urgency Change Order and
# Emergency Drought Barrier ver 1. Environmental Data Initiative.
# https://doi.org/10.6073/pasta/9e53e6f97568cacabf5f307a5645efc3 (Accessed 2023-04-13).

library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(lubridate)
library(brms)
library(cmdstanr)
library(corrplot)


#import data with all the visual index data from the edi publication so i know it works
HABs = read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1337.1&entityid=10c5229a3942e631d747c9486d809aea")

Habs2 =   HABs %>%
  filter(!is.na(Microcystis), Month %in% c(6:10), Year >2013) %>%
  mutate(Year = year(Date), Yearf = as.factor(Year),
         Month2 = factor(Month, levels = c(6,7,8,9,10),
                         labels = c("Jun", "Jul", "Aug", "Sep", "Oct")),
           HABord = case_when(
             Microcystis == 1 ~ "Absent",
             Microcystis %in% c(2,3) ~ "Low",
             Microcystis %in% c(4,5) ~ "High")) %>%
           mutate(HABord = factor(HABord, levels = c("Absent", "Low", "High"), ordered = T))

#now get dayflow
DF = read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1337.1&entityid=2368c56990d185597323dba8d859897b")
flowX = mutate(DF, CMS = CFS*0.0283168) %>%
  dplyr::select(Metric, CMS, Date) %>%
  pivot_wider(names_from = Metric, values_from = CMS)

Habs2 = mutate(Habs2, Date = as.Date(Date)) %>%
  left_join(flowX) %>%
  rename(OUT = Outflow, SJR = `San Joaquin Flow`, EXPORTS = `Project Exports`)


#COrrelation tests
Corres = cor(select(Habs2, where(is.numeric)), use = "complete.obs", method = "spearman")
corrplot(Corres, method = "number")

#Note: All of these models take a long time to run.

#Do a bayesian mixed model of HABs in the summer in the south and central delta regions

SoDelta = dplyr::filter(Habs2, Region %in% c("Lower SJ", "Lower Sac", "South Delta", "Franks", "OMR"))


#Scale and center the variables and get ride of values where we have NAs
SoDelta = mutate(SoDelta, day = yday(Date), Outscale = scale(OUT),
                 Exscale = scale(EXPORTS), SJRs = scale(SJR), Tempscale = scale(Temperature), Secchs = scale(Secchi)) %>%
  filter(!is.na(OUT), !is.na(Temperature), !is.na(SJR), !is.na(EXPORTS), !is.na(Secchi), Month %in% c(6:10))

#now let's look at all possible combinations of temperature, outflow, exports, and secchi depth.
#San Joaquin flow is too highly correlated with Outflow to use.

M5.3 = brm(HABord ~ Tempscale + Outscale + Exscale + (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
           iter = 2000,   backend = "cmdstanr", normalize = FALSE,
           control = list(max_treedepth = 15),
           chains = 2, cores=4, threads = threading(2))

M5.41 = brm(HABord ~ Tempscale + Outscale + Secchs+ (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
            iter = 2000,   backend = "cmdstanr", normalize = FALSE,
            control = list(max_treedepth = 15),
            chains = 2, cores=4, threads = threading(2))

M5.5 = brm(HABord ~ Tempscale + Outscale + Exscale+ Secchs + (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
           iter = 2000,   backend = "cmdstanr", normalize = FALSE,
           control = list(max_treedepth = 15),
           chains = 2, cores=4, threads = threading(2))



M5.61 = brm(HABord ~ Tempscale +  Exscale+ Secchs + (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
            iter = 2000,   backend = "cmdstanr", normalize = FALSE,
            control = list(max_treedepth = 15),
            chains = 2, cores=4, threads = threading(2))



M5.71 = brm(HABord ~   Exscale+ Secchs + (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
            iter = 2000,   backend = "cmdstanr", normalize = FALSE,
            control = list(max_treedepth = 15),
            chains = 2, cores=4, threads = threading(2))


M5.8 = brm(HABord ~ Tempscale + (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
           iter = 2000,   backend = "cmdstanr", normalize = FALSE,
           control = list(max_treedepth = 15),
           chains = 2, cores=4, threads = threading(2))



M5.91 = brm(HABord ~  Secchs + (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
            iter = 2000,   backend = "cmdstanr", normalize = FALSE,
            control = list(max_treedepth = 15),
            chains = 2, cores=4, threads = threading(2))


M5.101 = brm(HABord ~  Secchs + Tempscale+ (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
             iter = 2000,   backend = "cmdstanr", normalize = FALSE,
             control = list(max_treedepth = 15),
             chains = 2, cores=4, threads = threading(2))

M5.11 = brm(HABord ~  Outscale+ (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
            iter = 2000,   backend = "cmdstanr", normalize = FALSE,
            control = list(max_treedepth = 15),
            chains = 2, cores=4, threads = threading(2))

M5.12 = brm(HABord ~  Exscale+ (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
            iter = 2000,   backend = "cmdstanr", normalize = FALSE,
            control = list(max_treedepth = 15),
            chains = 2, cores=4, threads = threading(2))

M5.13 = brm(HABord ~  Tempscale + Outscale+ (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
            iter = 2000,   backend = "cmdstanr", normalize = FALSE,
            control = list(max_treedepth = 15),
            chains = 2, cores=4, threads = threading(2))

M5.14 = brm(HABord ~  Secchs + Outscale+ (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
            iter = 2000,   backend = "cmdstanr", normalize = FALSE,
            control = list(max_treedepth = 15),
            chains = 2, cores=4, threads = threading(2))

M5.15 = brm(HABord ~  Exscale + Outscale+ (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
            iter = 2000,   backend = "cmdstanr", normalize = FALSE,
            control = list(max_treedepth = 15),
            chains = 2, cores=4, threads = threading(2))

M5.16 = brm(HABord ~  Exscale + Outscale+ Secchs + (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
            iter = 2000,   backend = "cmdstanr", normalize = FALSE,
            control = list(max_treedepth = 15),
            chains = 2, cores=4, threads = threading(2))


#add infermation criteria to each model
M5.41 = add_criterion(M5.41, "loo")
M5.41 = add_criterion(M5.41, "waic")
M5.3 = add_criterion(M5.3, "loo")
M5.3 = add_criterion(M5.3, "waic")
M5.5 = add_criterion(M5.5, "loo")
M5.5 = add_criterion(M5.5, "waic")
M5.61 = add_criterion(M5.61, "loo")
M5.61 = add_criterion(M5.61, "waic")
M5.71 = add_criterion(M5.71, "loo")
M5.71 = add_criterion(M5.71, "waic")
M5.8 = add_criterion(M5.8, "loo")
M5.8 = add_criterion(M5.8, "waic")
M5.91 = add_criterion(M5.91, "loo")
M5.91 = add_criterion(M5.91, "waic")
M5.101 = add_criterion(M5.101, "loo")
M5.101 = add_criterion(M5.101, "waic")
M5.11 = add_criterion(M5.11, "loo")
M5.11 = add_criterion(M5.11, "waic")
M5.12 = add_criterion(M5.12, "waic")
M5.13 = add_criterion(M5.13, "waic")
M5.14 = add_criterion(M5.14, "waic")
M5.15= add_criterion(M5.15, "waic")
M5.16= add_criterion(M5.16, "waic")


#Compare WAIC scores and LOO scores
testloo = loo_compare(M5.41, M5.3,  M5.5, M5.61, M5.71, M5.8, M5.91, M5.101, M5.11, criterion = "loo")
test = loo_compare(M5.41, M5.3, M5.5, M5.61,  M5.71, M5.8, M5.91, M5.101, M5.11,
                   M5.12, M5.13, M5.15, M5.14, M5.16, criterion = "waic")

#Output table
write.csv(test, "outputs/WAICscores12apr2023.csv")

#Model 5.5 and 6.1 were very close
pp_check(M5.5)
cex5.5 = conditional_effects(M5.5, categorical = TRUE)
cex5.5

pp_check(M5.61)
cex5.61 = conditional_effects(M5.61, categorical = TRUE)
cex5.61


#nice table of effects for the paper
foo = summary(M5.61)
test = bind_rows(foo$fixed, foo$random$Yearf, foo$random$day) %>%
  mutate(Terms = rownames(.), Terms = case_when(Terms == "sd(Intercept)...6" ~ "Year",
                                                Terms == "sd(Intercept)...7" ~ "Day of Year",
                                                TRUE~ Terms))


write.csv(test, "MicroModel.csv")



#save all our work
save(M5.41, M5.3, M5.5, M5.61,M5.71, M5.8, M5.91, M5.101, M5.11, M5.12, M5.13, M5.14, M5.15, M5.16, file = "MCmodels12apr2023")

#######################################################################
#that was all the model code. Here is the code to plot it.


#First lets calculate the mean exports, outflow, temperature, and secchi that
#were actually observed.
SoDeltasum = group_by(SoDelta, Year, Yearf, Month2) %>%
  summarize(Exscale = mean(Exscale), Outscale = mean(Outscale), Export = mean(EXPORTS),
            Outflow = mean(OUT), Tempscale = mean(Tempscale),
            Secchs = mean(Secchs),
            Secchi = mean(Secchi),
            Temperature = mean(Temperature), day = median(day)) %>%
  filter(Yearf %in% c("2021", "2020")) %>%
  droplevels()


#We will need to back=convert the scaled temperature to plot it
temp = cex5.61$`Tempscale:cats__`
lm = lm(Temperature ~Tempscale, data = SoDelta)
foo = as.data.frame(summary(lm)$coefficients)

newdata = data.frame(Tempscale = c(-2,0,2))
newdata = mutate(temp, Temperature = Tempscale*foo$Estimate[2] + foo$Estimate[1])

#Properly formatted x label
xlab <- "Temperature (°C)"

#Plot the effects of temperature on microcystis at each level
ggplot(filter(newdata), aes(x = Temperature, y = estimate__)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = cats__), alpha = 0.3)+
  geom_line(aes(color = cats__))+
  scale_fill_manual(values = c("blue", "orange", "red"),
                    labels = c("Absent", "Low", "High"), name = "Microcystis")+
  scale_color_manual(values = c("blue", "orange", "red"),
                     labels = c("Absent", "Low", "High"), name = "Microcystis")+

  xlab(xlab)+
  ylab("Probability")+
  geom_vline(xintercept = mean(filter(SoDeltasum, Yearf == '2021', Month2 == "Jul")$Temperature),
             linetype = 2)+
  annotate("text", x = 23.4, y = 0.5, angle = 90, label = "Mean Jul 2021")+
  theme_bw()+
  theme(legend.title = element_text(face = "italic"))

ggsave("plots/MicTemp_apr2023.tiff", device = "tiff", width = 6, height = 4, units = "in")


#Plot the effects of exports on microcystis at each level
ex = cex5.61$`Exscale:cats__`
lmE = lm(EXPORTS ~Exscale, data = SoDelta)
fooE = as.data.frame(summary(lmE)$coefficients)
newdataE = mutate(ex, EXPORTS = Exscale*fooE$Estimate[2] + fooE$Estimate[1])

xlabCMS = expression(paste(
  "Project Exports (",
  m^3, "/sec)", sep=""))

ggplot(filter(newdataE), aes(x = EXPORTS, y = estimate__)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = cats__), alpha = 0.3)+
  geom_line(aes(color = cats__))+
  scale_fill_manual(values = c("blue", "orange", "red"),
                    labels = c("Absent", "Low", "High"), name = "Microcystis")+
  scale_color_manual(values = c("blue", "orange", "red"),
                     labels = c("Absent", "Low", "High"), name = "Microcystis")+

  xlab(xlabCMS)+
  ylab("Probability")+
  geom_vline(xintercept = 36.8, linetype = 2)+
  annotate("text", x = 30, y = 0.4, label = "TUCP Export Limit", angle = 90)+
  theme_bw()+
  theme(legend.title = element_text(face = "italic"))
ggsave("plots/MicExports_augCMS.tiff", device = "tiff", width = 6, height = 4, units = "in")


#Now do secchi depth.
turb = cex5.61$`Secchs:cats__`
lmS = lm(Secchi ~Secchs, data = SoDelta)
fooS = as.data.frame(summary(lmS)$coefficients)
newdataS = mutate(turb, Secchi = Secchs*fooS$Estimate[2] + fooS$Estimate[1])


ggplot(filter(newdataS), aes(x = Secchi, y = estimate__)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = cats__), alpha = 0.3)+
  geom_line(aes(color = cats__))+
  scale_fill_manual(values = c("blue", "orange", "red"),
                    labels = c("Absent", "Low", "High"), name = "Microcystis")+
  scale_color_manual(values = c("blue", "orange", "red"),
                     labels = c("Absent", "Low", "High"), name = "Microcystis")+

  xlab("Secchi Depth (cm)")+
  ylab("Probability")+
  geom_vline(xintercept = mean(filter(SoDeltasum, Yearf == '2021', Month2 == "Jul")$Secchi),
             linetype = 2)+
  annotate("text", x = 95, y = 0.5, angle = 90, label = "Mean Jul 2021")+

  theme_bw()+
  theme(legend.title = element_text(face = "italic"))
ggsave("plots/MicSecchi_aug.tiff", device = "tiff", width = 6, height = 4, units = "in")

