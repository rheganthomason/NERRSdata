## Monthly data graphs (water quality: DO, sal, temp)

## thresholds
raw.dir <- here::here("data")
daily_csv <-"water_quality_pep.csv"
daily <- read_csv(file.path(raw.dir,daily_csv))
dailyyear <- daily %>% mutate(month = lubridate::month(date, label = TRUE, abbr = TRUE),
                              year = lubridate::year(date))

rmdomgl<-dailyyear %>% filter(Variable == "DO_mgl" & mean_daily > 0 & mean_daily < 15) %>% 
  group_by(month, Variable, station) %>% 
  summarise(mean_month = mean(mean_daily), sd_month = sd(mean_daily)) %>% ungroup()

rmdopct<-dailyyear %>% filter(Variable == "DO_Pct" & mean_daily > 50 & mean_daily < 120 ) %>% 
  group_by(month, Variable, station) %>% 
  summarise(mean_month = mean(mean_daily), sd_month = sd(mean_daily)) %>% ungroup()

rmtemp<-dailyyear %>% filter(Variable == "Temp" & mean_daily > 0 & mean_daily < 30 ) %>% 
  group_by(month, Variable, station) %>% 
  summarise(mean_month = mean(mean_daily), sd_month = sd(mean_daily)) %>% ungroup()

rmsal<-dailyyear %>% filter(Variable == "Sal" & mean_daily > 0 & mean_daily < 32 ) %>% 
  group_by(month, Variable, station) %>% 
  summarise(mean_month = mean(mean_daily), sd_month = sd(mean_daily)) %>% ungroup()

monthly <- rbind(rmdomgl, rmdopct, rmtemp, rmsal)

## only 2020 points
rmdomgl2020 <-dailyyear %>% filter(Variable == "DO_mgl" & mean_daily > 0 & mean_daily < 15) %>% 
  group_by(month, Variable, station, year) %>% 
  summarise(mean_month = mean(mean_daily), sd_month = sd(mean_daily)) %>% filter(year == 2020) %>% ungroup()

rmdopct2020 <-dailyyear %>% filter(Variable == "DO_Pct" & mean_daily > 50 & mean_daily < 120 ) %>% 
  group_by(month, Variable, station, year) %>% 
  summarise(mean_month = mean(mean_daily), sd_month = sd(mean_daily)) %>% filter(year == 2020) %>% ungroup()

rmtemp2020 <-dailyyear %>% filter(Variable == "Temp" & mean_daily > 0 & mean_daily < 30 ) %>% 
  group_by(month, Variable, station, year) %>% 
  summarise(mean_month = mean(mean_daily), sd_month = sd(mean_daily)) %>% filter(year == 2020) %>% ungroup()

rmsal2020 <-dailyyear %>% filter(Variable == "Sal" & mean_daily > 0 & mean_daily < 32 ) %>% 
  group_by(month, Variable, station, year) %>% 
  summarise(mean_month = mean(mean_daily), sd_month = sd(mean_daily)) %>% filter(year == 2020) %>% ungroup()


#filter by station
nartsmonth <- monthly %>% filter(station == "nartswq")
waqmonth <- monthly %>% filter(station == "wqbmpwq")
welmonth <- monthly %>% filter(station == "welinwq")
grbmonth <- monthly %>% filter(station == "grbgbwq") 

#monthly DO (mg/L) for NAR
nartsmonth %>% 
  filter(Variable == "DO_mgl") %>% 
  ggplot(aes(month, mean_month))+
  geom_point(color = "steelblue", size = 2)+
  #geom_smooth(method = "loess", span = 0.01, color = "steelblue")+
  geom_point(data = rmdomgl2020 %>% filter(Variable == "DO_mgl", 
                                           station == "nartswq"), aes(month, mean_month), color = "red", size = 2)+
  ylim(0, 15)+
  labs(x = "", y = "Dissolved Oxygen Concentration (mg/L)", title = "Narraganset DO (mg/L) - Monthly")+
  ecodata::theme_ts()

#just 2020
nartsmonth %>% 
  filter(Variable == "DO_mgl") %>% filter(date >= "2020-01-01") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess", span = 0.05, color = "steelblue")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+ 
  ylim(0, 15)+
  labs(x = "Year", y = "Dissolved Oxygen Concentration (mg/L)", title = "Narraganset DO (mg/L)- Monthly (2020)")+
  ecodata::theme_ts()

#% DO NAR monthly
nartsmonth %>% 
  filter(Variable == "DO_Pct") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess", span = 0.05, color = "steelblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  ylim(50, 120)+
  labs(x = "Year", y = "Dissolved Oxygen Concentration (% saturation)", title = "Narraganset DO (% saturation) - Monthly") +
  ecodata::theme_ts()

#just 2020
nartsmonth %>% 
  filter(Variable == "DO_Pct") %>% filter(date >= "2020-01-01") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess", span = 0.05, color = "steelblue")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  ylim(50, 120)+
  labs(x = "Year", y = "Dissolved Oxygen Concentration (% saturation)", title = "Narraganset DO (% saturation)- Monthly (2020)")+
  ecodata::theme_ts()

# temp NAR monthly
nartsmonth %>% 
  filter(Variable == "Temp") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess", span = 0.05, color = "steelblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  ylim(0, 30)+
  labs(x = "Year", y = "Temperature (C)", title = "Narraganset Temperature - Monthly")+
  ecodata::theme_ts()

#just 2020
nartsmonth %>% 
  filter(Variable == "Temp") %>% filter(date >= "2020-01-01") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess", span = 0.05, color = "steelblue")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  ylim(0, 30)+
  labs(x = "Year", y = "Temperature (C)", title = "Narraganset Temperature - Monthly (2020)")+
  ecodata::theme_ts()

#sal NAR monthly
nartsmonth %>% 
  filter(Variable == "Sal") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess", span = 0.05, color = "steelblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  ylim(0, 32)+
  labs(x = "Year", y = "Salnity (psu)", title = "Narraganset Salinity - Monthly")+
  ecodata::theme_ts()

#just 2020
nartsmonth %>% 
  filter(Variable == "Sal") %>% filter(date >= "2020-01-01") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess", span = 0.05, color = "steelblue")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  ylim(0, 32)+
  labs(x = "Year", y = "Salinity (psu)", title = "Narraganset Salinity - Monthly (2020)")+
  ecodata::theme_ts()

## monthly DO (mg/L) for GRB
grbmonth %>% 
  filter(Variable == "DO_mgl") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess", span = 0.05, color = "steelblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  ylim(0, 15)+
  labs(x = "Year", y = "Dissolved Oxygen Concentration (mg/L)", title = "Great Bay DO (mg/L) - Monthly")+
  ecodata::theme_ts()

#just 2020
grbmonth %>% 
  filter(Variable == "DO_mgl") %>% filter(date >= "2020-01-01") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess", span = 0.05, color = "steelblue")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+ 
  ylim(0, 15)+
  labs(x = "", y = "Dissolved Oxygen Concentration (mg/L)", title = "Great Bay DO (mg/L)- Monthly (2020)")+
  ecodata::theme_ts()

#% DO monthly
grbmonth %>% 
  filter(Variable == "DO_Pct") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess", span = 0.05, color = "steelblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  ylim(50, 120)+
  labs(x = "Year", y = "Dissolved Oxygen Concentration (% saturation)", title = "Great Bay DO (% saturation) - Monthly") +
  ecodata::theme_ts()

#just 2020
grbmonth %>% 
  filter(Variable == "DO_Pct") %>% filter(date >= "2020-01-01") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess", span = 0.05, color = "steelblue")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  ylim(50, 120)+
  labs(x = "", y = "Dissolved Oxygen Concentration (% saturation)", title = "Great Bay DO (% saturation)- Monthly (2020)")+
  ecodata::theme_ts()

# temp monthly
grbmonth %>% 
  filter(Variable == "Temp") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess", span = 0.05, color = "steelblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  ylim(0, 30)+
  labs(x = "Year", y = "Temperature (C)", title = "Great Bay Temperature - Monthly")+
  ecodata::theme_ts()

#just 2020
grbmonth %>% 
  filter(Variable == "Temp") %>% filter(date >= "2020-01-01") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess", span = 0.05, color = "steelblue")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  ylim(0, 30)+
  labs(x = "", y = "Temperature (C)", title = "Great Bay Temperature - Monthly (2020)")+
  ecodata::theme_ts()

#sal monthly
grbmonth %>% 
  filter(Variable == "Sal") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess", span = 0.05, color = "steelblue", se = F)+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  ylim(0, 32)+
  labs(x = "Year", y = "Salnity (psu)", title = "Narraganset Salinity - Monthly")+
  ecodata::theme_ts()

#just 2020
grbmonth %>% 
  filter(Variable == "Sal") %>% filter(date >= "2020-01-01") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess", span = 0.05, color = "steelblue")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  ylim(0, 32)+
  labs(x = "", y = "Salinity (psu)", title = "Narraganset Salinity - Monthly (2020)")+
  ecodata::theme_ts()

## waq monthly data
# DO mg/l
waqmonth %>% 
  filter(Variable == "DO_mgl") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess", span = 0.05, color = "steelblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  ylim(0, 15)+
  labs(x = "Year", y = "Dissolved Oxygen Concentration (mg/L)", title = "Waquoit DO (mg/L) - Monthly")+
  ecodata::theme_ts()

#just 2020
waqmonth %>% 
  filter(Variable == "DO_mgl") %>% filter(date >= "2020-01-01") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess", span = 0.05, color = "steelblue")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+ 
  ylim(0, 15)+
  labs(x = "", y = "Dissolved Oxygen Concentration (mg/L)", title = "Waquoit DO (mg/L)- Monthly (2020)")+
  ecodata::theme_ts()

#% DO monthly
waqmonth %>% 
  filter(Variable == "DO_Pct") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess", span = 0.05, color = "steelblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  ylim(50, 120)+
  labs(x = "Year", y = "Dissolved Oxygen Concentration (% saturation)", title = "Waquoit DO (% saturation) - Monthly") +
  ecodata::theme_ts()

#just 2020
waqmonth %>% 
  filter(Variable == "DO_Pct") %>% filter(date >= "2020-01-01") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess", span = 0.05, color = "steelblue")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  ylim(50, 120)+
  labs(x = "", y = "Dissolved Oxygen Concentration (% saturation)", title = "Waquoit DO (% saturation)- Monthly (2020)")+
  ecodata::theme_ts()

# temp monthly
waqmonth %>% 
  filter(Variable == "Temp") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess", span = 0.05, color = "steelblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  ylim(0, 30)+
  labs(x = "Year", y = "Temperature (C)", title = "Waquoit Temperature - Monthly")+
  ecodata::theme_ts()

#just 2020
waqmonth %>% 
  filter(Variable == "Temp") %>% filter(date >= "2020-01-01") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess", span = 0.05, color = "steelblue")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  ylim(0, 30)+
  labs(x = "", y = "Temperature (C)", title = "Waquoit Temperature - Monthly (2020)")+
  ecodata::theme_ts()

#sal monthly
waqmonth %>% 
  filter(Variable == "Sal") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess", span = 0.05, color = "steelblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  ylim(0, 32)+
  labs(x = "Year", y = "Salnity (psu)", title = "Waquoit Salinity - Monthly")+
  ecodata::theme_ts()

#just 2020
waqmonth %>% 
  filter(Variable == "Sal") %>% filter(date >= "2020-01-01") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess", span = 0.05, color = "steelblue")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  ylim(0, 32)+
  labs(x = "", y = "Salinity (psu)", title = "Waquoit Salinity - Monthly (2020)")+
  ecodata::theme_ts()

## wel monthly data
# DO mg/l
welmonth %>% 
  filter(Variable == "DO_mgl") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess", span = 0.05, color = "steelblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  ylim(0, 15)+
  labs(x = "Year", y = "Dissolved Oxygen Concentration (mg/L)", title = "Wells DO (mg/L) - Monthly")+
  ecodata::theme_ts()

#just 2020
welmonth %>% 
  filter(Variable == "DO_mgl") %>% filter(date >= "2020-01-01") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess", span = 0.05, color = "steelblue")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+ 
  ylim(0, 15)+
  labs(x = "", y = "Dissolved Oxygen Concentration (mg/L)", title = "Wells DO (mg/L)- Monthly (2020)")+
  ecodata::theme_ts()

#% DO monthly
welmonth %>% 
  filter(Variable == "DO_Pct") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess", span = 0.05, color = "steelblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  ylim(50, 120)+
  labs(x = "Year", y = "Dissolved Oxygen Concentration (% saturation)", title = "Wells DO (% saturation) - Monthly") +
  ecodata::theme_ts()

#just 2020
welmonth %>% 
  filter(Variable == "DO_Pct") %>% filter(date >= "2020-01-01") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess", span = 0.05, color = "steelblue")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  ylim(50, 120)+
  labs(x = "", y = "Dissolved Oxygen Concentration (% saturation)", title = "Wells DO (% saturation)- Monthly (2020)")+
  ecodata::theme_ts()

# temp monthly
welmonth %>% 
  filter(Variable == "Temp") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess", span = 0.05, color = "steelblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  ylim(0, 30)+
  labs(x = "Year", y = "Temperature (C)", title = "Wells Temperature - Monthly")+
  ecodata::theme_ts()

#just 2020
welmonth %>% 
  filter(Variable == "Temp") %>% filter(date >= "2020-01-01") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess", span = 0.05, color = "steelblue")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  ylim(0, 30)+
  labs(x = "", y = "Temperature (C)", title = "Wells Temperature - Monthly (2020)")+
  ecodata::theme_ts()

#sal monthly
welmonth %>% 
  filter(Variable == "Sal") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess", span = 0.05, color = "steelblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  ylim(0, 32)+
  labs(x = "Year", y = "Salnity (psu)", title = "Wells Salinity - Monthly")+
  ecodata::theme_ts()

#just 2020
welmonth %>% 
  filter(Variable == "Sal") %>% filter(date >= "2020-01-01") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess", span = 0.05, color = "steelblue")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  ylim(0, 32)+
  labs(x = "", y = "Salinity (psu)", title = "Wells Salinity - Monthly (2020)")+
  ecodata::theme_ts()
