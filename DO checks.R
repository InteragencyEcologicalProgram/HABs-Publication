#quick look at the most recent DO data
library(tidyverse)
library(cder)
library(lubridate)

FRK = cdec_query(c("FRK", "FAL", "OSJ", "HOL", "FCT"), sensors = 61,
                 start.date = ymd("2015-01-01"), end.date = today())

FRKsum  = mutate(FRK, Year = year(DateTime), Date = date(DateTime),
                 Month = month(DateTime)) %>%
  filter(Value>0, Value <20, Month %in% c(5:11)) %>%
  group_by(StationID, Year, Date) %>%
  summarize(DO = mean(Value, na.rm =T))

ggplot(FRKsum, aes(x = Date, y = DO, color = StationID))+
  geom_line()+
  facet_wrap(~Year, scales = "free_x")
