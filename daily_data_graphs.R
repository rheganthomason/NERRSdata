## daily data graph's (DO, Temp, sal)
### index: do_mgl: dissolved oxygen in milligrams per liter, do_pct: dissolved oxygen % saturation, sal: salinity
### grb: great bay, nar: narraganset

obj <- read.csv(here::here("data/daily_wq.csv"))
nartsdaily <- obj %>% filter(station == "NAR")
grbdaily <- obj %>% filter(station == "GRB") %>% 
mutate(date = as.Date(date))

#DO mgl for nar
nartsdaily %>% 
filter(Variable == "do_mgl") %>% filter(year >= 2007) %>% 
ggplot(aes(x = date, y = mean_daily))+
geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), color = "lightblue", alpha = 0.75)+
geom_line(color = "steelblue")+
  labs(x = "Year", y = "Dissolved Oxygen Concentration (mg/L)", title = "Narraganset Dissolved Oxygen") +
scale_x_date(date_labels = "%y", date_breaks = "1 year")+
ecodata::theme_ts()

#DO_pct for nar
nartsdaily %>% filter(Variable == "do_pct") %>% filter(year >= 2007) %>%
ggplot(aes(x = date, y = mean_daily))+
geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), color = "lightblue")+
geom_line(color = "steelblue")+
labs(x = "Year", y = "Dissolved Oxygen Concentration (% saturation)", title = "Narraganset Dissolved Oxygen")+
scale_x_date(date_labels = "%y", date_breaks = "1 year")+
ecodata::theme_ts()

#Temp for nar
nartsdaily %>% filter(Variable == "temp") %>% filter(year >= 2007) %>%
  ggplot(aes(x = date, y = mean_daily))+
  geom_point()+
  geom_line(color = "steelblue")+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), color = "lightblue", alpha = 0.75)+
  labs(x = "Year", y = "Temperature", title = "Narraganset Temperature")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  ecodata::theme_ts()

#sal for nar
nartsdaily %>% 
  filter(Variable == "sal") %>% filter(year >= 2007) %>% 
  ggplot(aes(x = date, y = mean_daily))+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), color = "lightblue", alpha = 0.75)+
  geom_line(color = "steelblue")+
  labs(x = "Year", y = "Salinity", title = "Narraganset Salinity Concentration") +
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  ecodata::theme_ts()

nartsdaily %>% 
  filter(Variable == "sal") %>% filter(year >= 2020) %>% 
  ggplot(aes(x = date, y = mean_daily))+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), color = "lightblue", alpha = 0.75)+
  geom_line(color = "steelblue")+
  labs(x = "Year", y = "Salinity", title = "Narraganset Salinity Concentration") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  ecodata::theme_ts()

## Great Bay graphs
# DOmgl, pct
grbdaily %>% 
  filter(Variable == "do_mgl") %>% filter(year >= 2007) %>% 
  ggplot(aes(x = date, y = mean_daily))+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), color = "lightblue", alpha = 0.75)+
  geom_line(color = "steelblue", na.rm = FALSE)+
  labs(x = "Year", y = "Dissolved Oxygen Concentration (mg/L")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  ecodata::theme_ts()

grbdaily %>% 
  filter(Variable == "do_mgl") %>% filter(year >= 2020) %>% 
  ggplot(aes(x = date, y = mean_daily))+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), color = "lightblue", alpha = 0.75)+
  geom_line(color = "steelblue")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  labs(x = "", y = "Dissolved Oxygen Concentration (mg/L", title = "Great Bay 2020 Dissolved Oxygen")+
  ecodata::theme_ts()

grbdaily %>% filter(Variable == "do_pct") %>% filter(year >= 2007) %>%
  ggplot(aes(x = date, y = mean_daily))+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), color = "lightblue")+
  geom_line(color = "steelblue")+
  labs(x = "Year", y = "Dissolved Oxygen Concentration (% saturation)", title = "Great Bay Dissolved Oxygen")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  ecodata::theme_ts()

grbdaily %>% filter(Variable == "do_pct") %>% filter(year >= 2020) %>%
  ggplot(aes(x = date, y = mean_daily))+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), color = "lightblue")+
  geom_line(color = "steelblue")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  labs(x = "", y = "Dissolved Oxygen Concentration (% saturation)", title = "Great Bay 2020 Dissolved Oxygen")+
  ecodata::theme_ts()

#TEMP for grb
grbdaily %>% filter(Variable == "temp") %>% filter(year >= 2007) %>%
  ggplot(aes(x = date, y = mean_daily))+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), color = "lightblue", alpha = 0.75)+
  geom_line(color = "steelblue")+
  labs(x = "Year", y = "Temperature (°C)")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  ecodata::theme_ts()

grbdaily %>% filter(Variable == "temp") %>% filter(year >= 2020) %>%
  ggplot(aes(x = date, y = mean_daily))+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), color = "lightblue", alpha = 0.75)+
  geom_line(color = "steelblue")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  labs(x = "", y = "Temperature (°C)", title = "2020 Data")+
  ecodata::theme_ts()

#sal for grb 
grbdaily %>% 
  filter(Variable == "sal") %>% filter(year >= 2007) %>% 
  ggplot(aes(x = date, y = mean_daily))+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), color = "lightblue", alpha = 0.75)+
  geom_line(color = "steelblue")+
  labs(x = "Year", y = "Salinity Concentration (psu)", title = "Great Bay Salinity Concentration") +
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  ecodata::theme_ts()

grbdaily %>% 
  filter(Variable == "sal") %>% filter(year >= 2020) %>% 
  ggplot(aes(x = date, y = mean_daily))+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), color = "lightblue", alpha = 0.75)+
  geom_line(color = "steelblue")+
  labs(x = "Year", y = "Salinity Concentration (psu)", title = "Great Bay Salinity Concentration") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  ecodata::theme_ts()
