#Visual index predictive model
#This pulls in the analyses from the HABsWeeds report and makes graphs for the HABs drought publication


library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(sf)
library(deltamapr)
library(brms)
library(DHARMa)
library(visreg)
library(MASS)
library(car)
library(DroughtData)
library(lubridate)
library(cmdstanr)
library(here)


load("MCmodels8jul2022.RData")
load("SoDelta.RData")

ggplot(SoDelta, aes(x = day, y = Temperature, color = Yearf)) + geom_point()+
  ylab("Water Temperature, degrees C") + xlab("Day of the Year")
ggplot(SoDelta, aes(x = Temperature, y = Outflow, color = Yearf)) + geom_point()
ggplot(SoDelta, aes(x = Temperature, y = Export, color = Yearf)) + geom_point()
ggplot(SoDelta, aes(x = Secchi, y = Outflow, color = Yearf)) + geom_point()
ggplot(SoDelta, aes(x = Secchi, y = Export, color = Yearf)) + geom_point()


summary(M5.61)
pp_check(M5.61)
cex5.61 = conditional_effects(M5.61, categorical = TRUE)
cex5.61

#nice table of effects for the paper
foo = summary(M5.61)
test = bind_rows(foo$fixed, foo$random$Yearf, foo$random$day) %>%
  mutate(Terms = rownames(.), Terms = case_when(Terms == "sd(Intercept)...6" ~ "Year",
                                                Terms == "sd(Intercept)...7" ~ "Day of Year",
                                                TRUE~ Terms))

test2 = mutate(test, Estimate2 = case_when(Terms == "Exscale" ~ Estimate*0.0283168,

                                           TRUE ~ Estimate))

write.csv(test, "MicroModel.csv")
#erg. I may want to unscale the variables and convert CFS to CMS

test = mutate(test, case_when())

# I want prettier plots of the conditional effects
# These are plots 2-31, 2-32, and 2-33
#I should write a function for thsi.
#plot temperature effect
temp = cex5.61$`Tempscale:cats__`
lm = lm(Temperature ~Tempscale, data = SoDelta)
foo = as.data.frame(summary(lm)$coefficients)

newdata = data.frame(Tempscale = c(-2,0,2))
newdata = mutate(temp, Temperature = Tempscale*foo$Estimate[2] + foo$Estimate[1])

xlab <- "Temperature (°C)"

ptemp = ggplot(filter(newdata), aes(x = Temperature, y = estimate__)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = cats__), alpha = 0.3)+
  geom_line(aes(color = cats__))+
  scale_fill_manual(values = c("blue", "orange", "red"),
                    labels = c("Absent", "Low", "High"), name = "Microcystis")+
  scale_color_manual(values = c("blue", "orange", "red"),
                     labels = c("Absent", "Low", "High"), name = "Microcystis")+
  # scale_fill_manual(values = c("orange", "red"),
  #                    labels = c("Low", "High"), name = "Microcystis")+
  #  scale_color_manual(values = c("orange", "red"),
  #                     labels = c("Low", "High"), name = "Microcystis")+

  xlab(xlab)+
  ylab("Probability")+
  geom_vline(xintercept = mean(filter(SoDeltasum, Yearf == '2021', Month2 == "Jul")$Temperature),
             linetype = 2)+
  annotate("text", x = 23.8, y = 0.5, angle = 90, label = "Mean Jul 2021")+
  annotate("text", x = 20.8, y = 0.9, label = "A", size = 15)+

  theme_bw()+
  theme(legend.title = element_text(face = "italic"), legend.position = "top")

#ggsave("plots/MicTemp_aug.tiff", device = "tiff", width = 6, height = 4, units = "in")



ex = cex5.61$`Exscale:cats__`
lmE = lm(EXPORTS ~Exscale, data = SoDelta)
fooE = as.data.frame(summary(lmE)$coefficients)
newdataE = mutate(ex, Exports = Exscale*fooE$Estimate[2] + fooE$Estimate[1])

xlabCMS = expression(paste(
  "Project Exports (",
  m^3, "/sec)", sep=""))

pex = ggplot(filter(newdataE), aes(x = Exports, y = estimate__)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = cats__), alpha = 0.3)+
  geom_line(aes(color = cats__))+
  scale_fill_manual(values = c("blue", "orange", "red"),
                    labels = c("Absent", "Low", "High"), name = "Microcystis")+
  scale_color_manual(values = c("blue", "orange", "red"),
                     labels = c("Absent", "Low", "High"), name = "Microcystis")+
  #scale_fill_manual(values = c("orange", "red"),
  #                  labels = c("Low", "High"), name = "Microcystis")+
  #scale_color_manual(values = c("orange", "red"),
  #                   labels = c("Low", "High"), name = "Microcystis")+
  xlab(xlabCMS)+
  ylab("Probability")+

  coord_cartesian(ylim = c(0,1))+
  geom_vline(xintercept = 36.8, linetype = 2)+
  annotate("text", x = 50, y = 0.9, label = "B", size = 15)+
  annotate("text", x = 30, y = 0.4, label = "TUCP Export Limit", angle = 90)+
  theme_bw()+
  theme(legend.title = element_text(face = "italic"), legend.position = "none")
#ggsave("plots/MicExports_augCMS.tiff", device = "tiff", width = 6, height = 4, units = "in")


turb = cex5.61$`Secchs:cats__`
lmS = lm(Secchi ~Secchs, data = SoDelta)
fooS = as.data.frame(summary(lmS)$coefficients)
newdataS = mutate(turb, Secchi = Secchs*fooS$Estimate[2] + fooS$Estimate[1])


pturb = ggplot(filter(newdataS), aes(x = Secchi, y = estimate__)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = cats__), alpha = 0.3)+
  geom_line(aes(color = cats__))+
  scale_fill_manual(values = c("blue", "orange", "red"),
                    labels = c("Absent", "Low", "High"), name = "Microcystis")+
  scale_color_manual(values = c("blue", "orange", "red"),
                     labels = c("Absent", "Low", "High"), name = "Microcystis")+
  # scale_fill_manual(values = c("orange", "red"),
  #                   labels = c("Low", "High"), name = "Microcystis")+
  # scale_color_manual(values = c("orange", "red"),
  #                    labels = c("Low", "High"), name = "Microcystis")+
  xlab("Secchi Depth (cm)")+
  ylab("Probability")+
  coord_cartesian(ylim = c(0,1))+
  geom_vline(xintercept = mean(filter(SoDeltasum, Yearf == '2021', Month2 == "Jul")$Secchi),
             linetype = 2)+
  annotate("text", x = 95, y = 0.5, angle = 90, label = "Mean Jul 2021")+
  annotate("text", x = 40, y = 0.9, label = "C", size = 15)+

  theme_bw()+
  theme(legend.title = element_text(face = "italic"), legend.position = "none")
#ggsave("plots/MicSecchi_aug.tiff", device = "tiff", width = 6, height = 4, units = "in")


library(gridExtra)
plots = grid.arrange(grobs = list(ptemp, pex, pturb), nrow = 3, heights = c(1.1, 1, 1))
ggsave("plots/CombinedMicPredictions.tiff", plot = plots, device = "tiff", width = 6, height = 12, units = "in")
#########################################################################################
#now let's see how changing exports and stuff will change microcystis



ex2 = data.frame(Scenario = c(1500, 3000, 6000)) %>%
  mutate(scaled = (Scenario-fooE$Estimate[1])/fooE$Estimate[2])

newdata2e = data.frame(Exscale = rep(ex2$scaled[1], 4), Outscale = rep(-.647, 4),
                       Tempscale = filter(SoDeltasum, Yearf == '2021')$Tempscale,
                       Secchs = filter(SoDeltasum, Yearf == '2021')$Secchs,
                       day = c(165,190, 224, 252), Yearf = "2021", Scenario = "1500 CFS")

newdata3e = data.frame(Exscale = rep(ex2$scaled[2], 4), Outscale = rep(-.647, 4),
                       Tempscale = filter(SoDeltasum, Yearf == '2021')$Tempscale,
                       Secchs = filter(SoDeltasum, Yearf == '2021')$Secchs,
                       day = c(165,190, 224, 252), Yearf = "2021", Scenario = "3000 CFS")

newdata4e = data.frame(Exscale = rep(ex2$scaled[3], 4), Outscale = rep(-.647, 4),
                       Tempscale = filter(SoDeltasum, Yearf == '2021')$Tempscale,
                       Secchs = filter(SoDeltasum, Yearf == '2021')$Secchs,
                       day = c(165,190, 224, 252), Yearf = "2021", Scenario = "6000 CFS")
allnewe = bind_rows(newdata2e, newdata3e, newdata4e )


library(tidybayes)

Predictionse = add_epred_draws(allnewe, M5.61)%>%
  median_qi(.epred)


ggplot(filter(Predictionse),  aes(x = as.factor(day), y = .epred, fill = Scenario)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = .lower, ymax = .upper, group = Scenario), position = "dodge")+
  facet_wrap(~.category)+
  scale_fill_manual(values = c("grey", "darkgreen", "lightblue"), name = "Export Scenario")+
  scale_x_discrete(labels = c("June", "July", "August", "Sept"), name = "Month")+
  theme_bw()+ylab("Probability")


diffe = group_by(Predictionse, .category, day) %>%
  summarize(Diff1 = .epred[1]-.epred[2], Diff2 = .epred[2]-.epred[3], Diff3 = .epred[1]-.epred[3])
