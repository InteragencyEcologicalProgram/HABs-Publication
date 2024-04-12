## Graph WQ Values at Franks Tract (FRK)
## Summer 2021

library(tidyverse)
library(lubridate)
library(timetk)
library(cder)
library(rMR)
library(here)

# Import data ------------------------------------------------------------------

# Import QA'd C-EMP WQ data from Andrew Tran
DO <- read_csv(here("WQ-cont-TMF","EMP","FRK_data","FRK_DissolvedOxygen.csv"),
               col_select = c(-...1, -station))

pH <- read_csv(here("WQ-cont-TMF","EMP","FRK_data","FRK_pH.csv"),
               col_select = c(-...1, -station))

Temp <- read_csv(here("WQ-cont-TMF","EMP","FRK_data","FRK_WaterTemperature.csv"),
                 col_select = c(-...1, -station))

SpC <- read_csv(here("WQ-cont-TMF","EMP","FRK_data","FRK_SpC.csv"),
                col_select = c(-...1, -station))

Turb <- read_csv(here("WQ-cont-TMF","EMP","FRK_data","FRK_Turbidity.csv"),
                 col_select = c(-...1, -station))

Chl <- read_csv(here("WQ-cont-TMF","EMP","FRK_data","FRK_Fluorescence.csv"),
                col_select = c(-...1, -station))

## Import QA'd 2021 data from A. Tran
DO.2021 <- read_csv(here("WQ-cont-TMF","EMP","2021","FRK_DissolvedOxygen.csv"),
                    col_select = c(-...1, -station))

pH.2021 <- read_csv(here("WQ-cont-TMF","EMP","2021","FRK_pH.csv"),
                    col_select = c(-...1, -station))

Temp.2021 <- read_csv(here("WQ-cont-TMF","EMP","2021","FRK_WaterTemperature.csv"),
                      col_select = c(-...1, -station))

SpC.2021 <- read_csv(here("WQ-cont-TMF","EMP","2021","FRK_SpC.csv"),
                     col_select = c(-...1, -station))

Turb.2021 <- read_csv(here("WQ-cont-TMF","EMP","2021","FRK_Turbidity.csv"),
                      col_select = c(-...1, -station))

Chl.2021 <- read_csv(here("WQ-cont-TMF","EMP","2021","FRK_Fluorescence.csv"),
                     col_select = c(-...1, -station))


# Combine datasets for each analyte
DO <- dplyr::bind_rows(DO, DO.2021)
pH <- dplyr::bind_rows(pH, pH.2021)
Temp <- dplyr::bind_rows(Temp, Temp.2021)
SpC <- dplyr::bind_rows(SpC, SpC.2021)
Turb <- dplyr::bind_rows(Turb, Turb.2021)
Chl <- dplyr::bind_rows(Chl, Chl.2021)

# Remove duplicates
DO <- DO %>% distinct()
pH <- pH %>% distinct()
Temp <- Temp %>% distinct()
SpC <- SpC %>% distinct()
Turb <- Turb %>% distinct()
Chl <- Chl %>% distinct()

# Import list of dates only
dates <- read_csv(here("WQ-cont-TMF","EMP","FRK_data","FRK_DissolvedOxygen.csv"),
                  col_select = 4)

# Remove X-flagged data
DO <- DO %>% filter(DO$qaqc_flag_id != "X")
pH <- pH %>% filter(pH$qaqc_flag_id != "X")
Temp <- Temp %>% filter(Temp$qaqc_flag_id != "X")
SpC <- SpC %>% filter(SpC$qaqc_flag_id != "X")
Turb <- Turb %>% filter(Turb$qaqc_flag_id != "X")
Chl <- Chl %>% filter(Chl$qaqc_flag_id != "X")

# Combined all data into one
WQ <- left_join(dates, DO[2:3]) %>%
  rename("DO.conc" = "value")

WQ <- left_join(WQ, pH[2:3]) %>%
  rename("pH" = "value")

WQ <- left_join(WQ, Temp[2:3]) %>%
  rename("Temp" = "value")

WQ <- left_join(WQ, SpC[2:3]) %>%
  rename("SpCond" = "value")

WQ <- left_join(WQ, Turb[2:3]) %>%
  rename("Turb" = "value")

WQ <- left_join(WQ, Chl[2:3]) %>%
  rename("Chl.a" = "value")

# Rename date column
WQ <- WQ %>% rename ("DateTime" = "time")

# Calculate DO Saturation using USGS Method
DO.sat <- DO.saturation(WQ$DO.conc,
                        elevation.m = 0,
                        temp.C = WQ$Temp,
                        salinity = WQ$SpCond,
                        salinity.units = "uS")
DO.sat <- as_tibble(DO.sat)

# Convert to %
DO.sat <- DO.sat*100

DO.sat <- DO.sat %>% rename("DO.sat" = "value")
WQ <- cbind(WQ, DO.sat)

# Add column of just the year for highlighting yearly data
# Add Julian date for plotting
WQ <- WQ %>%
  mutate(Year = year(WQ$DateTime)) %>%
  mutate(Julian = yday(WQ$DateTime)) %>%
  mutate(Date = date(WQ$DateTime)) %>%
  mutate(month2 = month(WQ$DateTime))

# Convert month numbers into text months (e.g., Jan, Feb, Mar)
# and append as a column named month3
WQ <- WQ %>%
  mutate(Month = month(WQ$month2, label=TRUE))

# Order month 3 in calendar order rather than (default) alphabetical
WQ$Month = factor(WQ$Month, levels = month.abb)
WQ$month2 <- NULL

# Convert years to characters for plotting
WQ <- WQ %>%
  mutate(Year = as.character(WQ$Year))

# Calculate Daily Means for each parameter -------------------------------------

# Flip data frame to long
WQ.long <- pivot_longer(WQ, names_to = "Parameter", cols = 2:8)

WQ.mean <- WQ.long %>%
  group_by(Date, Parameter) %>%
  summarise(Daily.Mean = mean(value, na.rm = TRUE)) %>%
  ungroup

# Add month/year columns
WQ.mean <- WQ.mean %>%
  mutate(Year = year(WQ.mean$Date)) %>%
  mutate(Julian = yday(WQ.mean$Date)) %>%
  mutate(month2 = month(WQ.mean$Date))

# Convert month numbers into text months (e.g., Jan, Feb, Mar)
# and append as a column named month3
WQ.mean <- WQ.mean %>%
  mutate(Month = month(WQ.mean$month2, label=TRUE))

# Order month 3 in calendar order rather than (default) alphabetical
WQ.mean$Month = factor(WQ.mean$Month, levels = month.abb)
WQ.mean$month2 <- NULL

# Plot Daily Mean in Summer 2020 and 2021
params <- unique(WQ.mean$Parameter)

for (param in params) {
  df_temp <- WQ.mean %>%
    filter(Parameter == param)

  plot.min.max <- ggplot(WQ.mean) +
    geom_point(data = subset(WQ.mean, Parameter == param),
               aes(x = Julian, y = Daily.Mean, color = as.factor(Year)),
               size = 1) +
    scale_x_continuous(breaks = c(1,60,121,182,244,305, 366),
                       labels = c("Jan","Mar","May","Jul","Sep","Nov","Jan")) +
    labs(x = NULL,
         y = paste0(param),
         fill = "Year",
         title = paste0("Daily Mean ",param,", 2015-2021"))

  plot.min.max +
    theme(panel.background = element_rect(fill = "white", linetype = 0)) +
    theme(panel.grid.minor = element_blank()) +
    scale_color_brewer(palette = "Set2", name = "Year") +
    facet_wrap(Year ~ ., ncol = 1, dir = "h")

  ggsave(path="plots",
         filename = paste0("WQ.FRK.",param,".all.pdf"),
         device = "pdf",
         scale=1.0,
         units="in",
         height=9,
         width=6.5,
         dpi="print")

  rm(df_temp)

}

# Calculate Daily Maximum and Minimum for each Parameter------------------------

WQ.min.max <- WQ.long %>%
  group_by(Date, Parameter) %>%
  summarise(Daily.Min = min(value, na.rm = TRUE),
            Daily.Max = max(value, na.rm = TRUE)) %>%
  ungroup

# Remove infinite min values from days with no data
WQ.min.max <- WQ.min.max %>% filter(!is.infinite(Daily.Min))
WQ.min.max <- WQ.min.max %>% filter(!is.infinite(Daily.Max))

# Add month/year columns
WQ.min.max <- WQ.min.max %>%
  mutate(Year = year(WQ.min.max$Date)) %>%
  mutate(Julian = yday(WQ.min.max$Date)) %>%
  mutate(month2 = month(WQ.min.max$Date))

# Convert month numbers into text months (e.g., Jan, Feb, Mar)
# and append as a column named month3
WQ.min.max <- WQ.min.max %>%
  mutate(Month = month(WQ.min.max$month2, label=TRUE))

# Order month 3 in calendar order rather than (default) alphabetical
WQ.min.max$Month = factor(WQ.min.max$Month, levels = month.abb)
WQ.min.max$month2 <- NULL

# Subset to 2020 and 2021 only
WQ.min.max.20s <- WQ.min.max %>%
  filter(Year >= "2020")

WQ.min.max.20s.Summer <- WQ.min.max.20s %>%
  filter(Julian >= 121) %>%
  filter(Julian <= 304)

# Plot Daily Min and Max in Summer 2020 and 2021
theme_set(theme_bw())

params <- unique(WQ.min.max.20s.Summer$Parameter)

for (param in params) {
  df_temp <- WQ.min.max.20s.Summer %>%
    filter(Parameter == param)

  plot.min.max <- ggplot(WQ.min.max.20s.Summer) +
    geom_point(data = subset(WQ.min.max.20s.Summer, Parameter == param),
              aes(x = Julian, y = Daily.Max),
              size = 1,
              color = "blue") +
    geom_point(data = subset(WQ.min.max.20s.Summer, Parameter == param),
              aes(x = Julian, y = Daily.Min),
              size = 1,
              color = "red") +
    scale_x_continuous(breaks = c(121,152,182,213,244,274,305),
                       labels = c("May","Jun","Jul","Aug","Sep","Oct","Nov")) +
    labs(x = NULL,
         y = paste0(param),
         title = paste0("Daily Minimum and Maximum ",param,", May - October"))

  plot.min.max +
    theme(panel.background = element_rect(fill = "white", linetype = 0)) +
    theme(panel.grid.minor = element_blank()) +
    facet_wrap(Year ~ ., ncol = 1, dir = "h")

  ggsave(path="plots",
         filename = paste0("WQ.FRK.",param,".Summer.20s.pdf"),
         device = "pdf",
         scale=1.0,
         units="in",
         height=5,
         width=7,
         dpi="print")

  rm(df_temp)

}

# Make Table of Min Max Values by Month ----------------------------------------
WQ.mon.min.max <- WQ.min.max %>%
  group_by(Year, Month, Parameter) %>%
  summarise(Monthly.Min.Mean = mean(Daily.Min, na.rm = T),
            Monthly.Max.Mean = mean(Daily.Max, na.rm = T),
            Monthly.Min.SD = sd(Daily.Min, na.rm = T),
            Monthly.Max.SD = sd(Daily.Max, na.rm = T)) %>%
  ungroup()

write_csv(WQ.mon.min.max, file = here("WQ-cont-TMF","WQ.mon.min.max.csv"))

# Plot Monthly mean max and min Values
plot.mon.min.max <- ggplot(WQ.mon.min.max) +
  theme(panel.background = element_rect(fill = "white", linetype = 0)) +
  theme(panel.grid.minor = element_blank()) +
  geom_point(data = subset(WQ.mon.min.max, Parameter == "pH"),
             aes(x = Month, y = Monthly.Max.Mean),
             size = 2,
             color = "blue") +
  geom_point(data = subset(WQ.mon.min.max, Parameter == "pH"),
             aes(x = Month, y = Monthly.Min.Mean),
             size = 2,
             color = "red") +
  scale_x_discrete(breaks = c("Jan","Mar","May","Jul","Sep","Nov"))

plot.mon.min.max +
  facet_wrap(Year ~ ., ncol = 4, dir = "h") +
  labs(x = "Month",
       y = "pH")

ggsave(path="plots",
       filename = "WQ.FRK.pH.png",
       device = "png",
       scale=1.0,
       units="in",
       height=5,
       width=8,
       dpi="print")
