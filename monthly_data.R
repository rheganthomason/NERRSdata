## Monthly data graphs (water quality: DO, sal, temp)


monthly <- read_csv(here::here("data/water_quality_pep.csv"))
nartsmonth <- monthly %>% filter(station == "nartswq")
waqmonth <- monthly %>% filter(station == "wqbmpwq")
welmonth <- monthly %>% filter(station == "welinwq")
grbmonth <- monthly %>% filter(station == "grbgbwq") %>% 
mutate(date = as.Date(date)) 

#monthly DO (mg/L) for NAR
nartsmonth %>% 
  filter(Variable == "DO_mgl") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  #geom_line(color = "lightblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  labs(x = "Year", y = "Dissolved Oxygen Concentration (mg/L)", title = "Narraganset DO (mg/L) - Monthly")+
  ecodata::theme_ts()

#just 2020
nartsmonth %>% 
  filter(Variable == "DO_mgl") %>% filter(date >= "2020-01-01") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  #geom_line(color = "lightblue")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  labs(x = "Year", y = "Dissolved Oxygen Concentration (mg/L)", title = "Narraganset DO (mg/L)- Monthly (2020)")+
  ecodata::theme_ts()

#% DO NAR monthly
nartsmonth %>% 
  filter(Variable == "DO_Pct") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  #geom_line(color = "lightblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  labs(x = "Year", y = "Dissolved Oxygen Concentration (% saturation)", title = "Narraganset DO (% saturation) - Monthly") +
  ecodata::theme_ts()

#just 2020
nartsmonth %>% 
  filter(Variable == "DO_Pct") %>% filter(date >= "2020-01-01") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  #geom_line(color = "lightblue")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  labs(x = "Year", y = "Dissolved Oxygen Concentration (% saturation)", title = "Narraganset DO (% saturation)- Monthly (2020)")+
  ecodata::theme_ts()

# temp NAR monthly
nartsmonth %>% 
  filter(Variable == "Temp") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  #geom_line(color = "lightblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  labs(x = "Year", y = "Temperature (C)", title = "Narraganset Temperature - Monthly")+
  ecodata::theme_ts()

#just 2020
nartsmonth %>% 
  filter(Variable == "Temp") %>% filter(date >= "2020-01-01") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  #geom_line(color = "lightblue")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  labs(x = "Year", y = "Temperature (C)", title = "Narraganset Temperature - Monthly (2020)")+
  ecodata::theme_ts()

#sal NAR monthly
nartsmonth %>% 
  filter(Variable == "Sal") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  #geom_line(color = "lightblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  labs(x = "Year", y = "Salnity (psu)", title = "Narraganset Salinity - Monthly")+
  ecodata::theme_ts()

#just 2020
nartsmonth %>% 
  filter(Variable == "Sal") %>% filter(date >= "2020-01-01") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  #geom_line(color = "lightblue")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  labs(x = "Year", y = "Salinity (psu)", title = "Narraganset Salinity - Monthly (2020)")+
  ecodata::theme_ts()

## monthly DO (mg/L) for GRB
grbmonth %>% 
  filter(Variable == "DO_mgl") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  #geom_line(color = "lightblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  labs(x = "Year", y = "Dissolved Oxygen Concentration (mg/L)", title = "Great Bay DO (mg/L) - Monthly")+
  ecodata::theme_ts()

#just 2020
grbmonth %>% 
  filter(Variable == "DO_mgl") %>% filter(date >= "2020-01-01") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  #geom_line(color = "lightblue")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  labs(x = "", y = "Dissolved Oxygen Concentration (mg/L)", title = "Great Bay DO (mg/L)- Monthly (2020)")+
  ecodata::theme_ts()

#% DO monthly
grbmonth %>% 
  filter(Variable == "DO_Pct") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  #geom_line(color = "lightblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  labs(x = "Year", y = "Dissolved Oxygen Concentration (% saturation)", title = "Great Bay DO (% saturation) - Monthly") +
  ecodata::theme_ts()

#just 2020
grbmonth %>% 
  filter(Variable == "DO_Pct") %>% filter(date >= "2020-01-01") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  #geom_line(color = "lightblue")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  labs(x = "", y = "Dissolved Oxygen Concentration (% saturation)", title = "Great Bay DO (% saturation)- Monthly (2020)")+
  ecodata::theme_ts()

# temp monthly
grbmonth %>% 
  filter(Variable == "Temp") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  #geom_line(color = "lightblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  labs(x = "Year", y = "Temperature (C)", title = "Great Bay Temperature - Monthly")+
  ecodata::theme_ts()

#just 2020
grbmonth %>% 
  filter(Variable == "Temp") %>% filter(date >= "2020-01-01") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  #geom_line(color = "lightblue")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  labs(x = "", y = "Temperature (C)", title = "Great Bay Temperature - Monthly (2020)")+
  ecodata::theme_ts()

#sal monthly
grbmonth %>% 
  filter(Variable == "Sal") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  #geom_line(color = "lightblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  labs(x = "Year", y = "Salnity (psu)", title = "Narraganset Salinity - Monthly")+
  ecodata::theme_ts()

#just 2020
grbmonth %>% 
  filter(Variable == "Sal") %>% filter(date >= "2020-01-01") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  #geom_line(color = "lightblue")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  labs(x = "", y = "Salinity (psu)", title = "Narraganset Salinity - Monthly (2020)")+
  ecodata::theme_ts()

## waq monthly data
# DO mg/l
waqmonth %>% 
  filter(Variable == "DO_mgl") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  #geom_line(color = "lightblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  labs(x = "Year", y = "Dissolved Oxygen Concentration (mg/L)", title = "Waquoit DO (mg/L) - Monthly")+
  ecodata::theme_ts()

#just 2020
waqmonth %>% 
  filter(Variable == "DO_mgl") %>% filter(date >= "2020-01-01") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  #geom_line(color = "lightblue")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  labs(x = "", y = "Dissolved Oxygen Concentration (mg/L)", title = "Waquoit DO (mg/L)- Monthly (2020)")+
  ecodata::theme_ts()

#% DO monthly
waqmonth %>% 
  filter(Variable == "DO_Pct") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  #geom_line(color = "lightblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  labs(x = "Year", y = "Dissolved Oxygen Concentration (% saturation)", title = "Waquoit DO (% saturation) - Monthly") +
  ecodata::theme_ts()

#just 2020
waqmonth %>% 
  filter(Variable == "DO_Pct") %>% filter(date >= "2020-01-01") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  #geom_line(color = "lightblue")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  labs(x = "", y = "Dissolved Oxygen Concentration (% saturation)", title = "Waquoit DO (% saturation)- Monthly (2020)")+
  ecodata::theme_ts()

# temp monthly
waqmonth %>% 
  filter(Variable == "Temp") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  #geom_line(color = "lightblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  labs(x = "Year", y = "Temperature (C)", title = "Waquoit Temperature - Monthly")+
  ecodata::theme_ts()

#just 2020
waqmonth %>% 
  filter(Variable == "Temp") %>% filter(date >= "2020-01-01") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  #geom_line(color = "lightblue")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  labs(x = "", y = "Temperature (C)", title = "Waquoit Temperature - Monthly (2020)")+
  ecodata::theme_ts()

#sal monthly
waqmonth %>% 
  filter(Variable == "Sal") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  #geom_line(color = "lightblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  labs(x = "Year", y = "Salnity (psu)", title = "Waquoit Salinity - Monthly")+
  ecodata::theme_ts()

#just 2020
waqmonth %>% 
  filter(Variable == "Sal") %>% filter(date >= "2020-01-01") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  #geom_line(color = "lightblue")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  labs(x = "", y = "Salinity (psu)", title = "Waquoit Salinity - Monthly (2020)")+
  ecodata::theme_ts()

## wel monthly data
# DO mg/l
welmonth %>% 
  filter(Variable == "DO_mgl") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  #geom_line(color = "lightblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  labs(x = "Year", y = "Dissolved Oxygen Concentration (mg/L)", title = "Wells DO (mg/L) - Monthly")+
  ecodata::theme_ts()

#just 2020
welmonth %>% 
  filter(Variable == "DO_mgl") %>% filter(date >= "2020-01-01") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  #geom_line(color = "lightblue")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  labs(x = "", y = "Dissolved Oxygen Concentration (mg/L)", title = "Wells DO (mg/L)- Monthly (2020)")+
  ecodata::theme_ts()

#% DO monthly
welmonth %>% 
  filter(Variable == "DO_Pct") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  #geom_line(color = "lightblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  labs(x = "Year", y = "Dissolved Oxygen Concentration (% saturation)", title = "Wells DO (% saturation) - Monthly") +
  ecodata::theme_ts()

#just 2020
welmonth %>% 
  filter(Variable == "DO_Pct") %>% filter(date >= "2020-01-01") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  #geom_line(color = "lightblue")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  labs(x = "", y = "Dissolved Oxygen Concentration (% saturation)", title = "Wells DO (% saturation)- Monthly (2020)")+
  ecodata::theme_ts()

# temp monthly
welmonth %>% 
  filter(Variable == "Temp") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  #geom_line(color = "lightblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  labs(x = "Year", y = "Temperature (C)", title = "Wells Temperature - Monthly")+
  ecodata::theme_ts()

#just 2020
welmonth %>% 
  filter(Variable == "Temp") %>% filter(date >= "2020-01-01") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  #geom_line(color = "lightblue")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  labs(x = "", y = "Temperature (C)", title = "Wells Temperature - Monthly (2020)")+
  ecodata::theme_ts()

#sal monthly
welmonth %>% 
  filter(Variable == "Sal") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  #geom_line(color = "lightblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  labs(x = "Year", y = "Salnity (psu)", title = "Wells Salinity - Monthly")+
  ecodata::theme_ts()

#just 2020
welmonth %>% 
  filter(Variable == "Sal") %>% filter(date >= "2020-01-01") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  #geom_line(color = "lightblue")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  labs(x = "", y = "Salinity (psu)", title = "Wells Salinity - Monthly (2020)")+
  ecodata::theme_ts()
