## adding NA values 
  ## first is just DO for great bay!!

grbmonth_DO <- monthly %>% filter(station == "grbgbwq") %>% filter(Variable == "DO_mgl") 

# calculate all days
all_days_grb = data.frame(date = seq.Date(from = min(grbmonth_DO$date), to = max(grbmonth_DO$date), by = "day"))

# join to original data
library(dplyr)
grbmonth_complete_DO = left_join(all_days_grb, grbmonth_DO, by = "date")

# now ggplot won't connect lines across missing values
ggplot(grbmonth_complete_DO, aes(date, mean_daily)) +
  geom_point() +
  geom_line()

## my own data
grbmonth_complete_DO %>%
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_line(color = "steelblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  labs(x = "Year", y = "Dissolved Oxygen Concentration (mg/L)", title = "Great Bay DO (mg/L) - Monthly")+
  ecodata::theme_ts()

## waq DO
waqmonth_DO <- monthly %>% filter(station == "wqbmpwq") %>% filter(Variable == "DO_mgl") 

# calculate all days
all_days_waq = data.frame(date = seq.Date(from = min(waqmonth_DO$date), to = max(waqmonth_DO$date), by = "day"))

# join to original data
library(dplyr)
waqmonth_complete_DO = left_join(all_days_waq, waqmonth_DO, by = "date")

# now ggplot won't connect lines across missing values
ggplot(waqmonth_complete_DO, aes(date, mean_daily)) +
  geom_point() +
  geom_line()

## my own data
waqmonth_complete_DO %>%
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_line(color = "steelblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  labs(x = "Year", y = "Dissolved Oxygen Concentration (mg/L)", title = "Waquoit Bay DO (mg/L) - Monthly")+
  ecodata::theme_ts()

## wells DO
welmonth_DO <- monthly %>% filter(station == "welinwq") %>% filter(Variable == "DO_mgl") 

# calculate all days
all_days_wel = data.frame(date = seq.Date(from = min(welmonth_DO$date), to = max(welmonth_DO$date), by = "day"))

# join to original data
#library(dplyr)
welmonth_complete_DO = left_join(all_days_wel, welmonth_DO, by = "date")

# now ggplot won't connect lines across missing values
ggplot(welmonth_complete_DO, aes(date, mean_daily)) +
  geom_point() +
  geom_line()

## my own data
# welmonth_complete_DO %>%
  ggplot(welmonth_complete_DO, aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess")+
  #geom_line(color = "steelblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  labs(x = "Year", y = "Dissolved Oxygen Concentration (mg/L)", title = "Wells Bay DO (mg/L) - Monthly")+
  ecodata::theme_ts()

## Nar DO
narmonth_DO <- monthly %>% filter(station == "nartswq") %>% filter(Variable == "DO_mgl") 

# calculate all days
all_days_nar = data.frame(date = seq.Date(from = min(narmonth_DO$date), to = max(narmonth_DO$date), by = "day"))

# join to original data
#library(dplyr)
narmonth_complete_DO = left_join(all_days_nar, narmonth_DO, by = "date")

# now ggplot won't connect lines across missing values
ggplot(narmonth_complete_DO, aes(date, mean_daily)) +
  geom_point() +
  geom_line()

## my own data
# narmonth_complete_DO %>%
  ggplot(narmonth_complete_DO, aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess")+
  #geom_line(color = "steelblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  labs(x = "Year", y = "Dissolved Oxygen Concentration (mg/L)", title = "Narraganset DO (mg/L) - Monthly")+
  ecodata::theme_ts()
