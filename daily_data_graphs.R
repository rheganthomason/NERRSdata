## daily data graph's (DO, Temp)
obj <- read.csv(here::here("data/daily_wq.csv"))
nartsdaily <- obj %>% filter(station == "NAR")
grbdaily <- obj %>% filter(station == "GRB") %>% 
mutate(date = as.Date(date))

#do_mgl grb
grbdaily %>% 
filter(Variable == "do_mgl") %>% filter(year >= 2007) %>% 
  ggplot(aes(x = date, y = mean_daily))+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), color = "lightblue", alpha = 0.75)+
  geom_line(color = "steelblue")+
  labs(x = "Year", y = "Dissolved Oxygen Concentration (mg/L")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  ecodata::theme_ts()

#DO mgl
nartsdaily %>% 
filter(Variable == "do_mgl") %>% filter(year >= 2007) %>% 
ggplot(aes(x = date, y = mean_daily))+
geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), color = "lightblue", alpha = 0.75)+
#geom_point()+
geom_line(color = "steelblue")+
  labs(x = "Year", y = "Dissolved Oxygen Concentration (mg/L)", title = "Narraganset Dissolved Oxygen") +
scale_x_date(date_labels = "%y", date_breaks = "1 year")+
ecodata::theme_ts()

#DO %
nartsdaily %>% filter(Variable == "do_pct") %>% filter(year >= 2007) %>%
ggplot(aes(x = date, y = mean_daily))+
geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), color = "lightblue")+
geom_line(color = "steelblue")+
labs(x = "Year", y = "Dissolved Oxygen Concentration (% saturation)", title = "Narraganset Dissolved Oxygen")+
scale_x_date(date_labels = "%y", date_breaks = "1 year")+
ecodata::theme_ts()

#TEMP
nartsdaily %>% filter(Variable == "temp") %>% filter(year >= 2007) %>%
  ggplot(aes(x = date, y = mean_daily))+
  geom_point()+
  geom_line(color = "steelblue")+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), color = "lightblue", alpha = 0.75)+
  labs(x = "Year", y = "Temperature", title = "Narraganset Temperature")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  ecodata::theme_ts()

#TURB
nartsdaily %>% filter(Variable == "turb") %>% filter(year >= 2020) %>%
  ggplot(aes(x = date, y = mean_daily))+
  geom_point()+
  geom_line(color = "steelblue")+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), color = "lightblue", alpha = 0.75)+
  labs(x = "Year", y = "Turbidity", title = "Narraganset Turbidity")+
  ecodata::theme_ts()

## Great Bay graphs
# DO mgl, %
grbdaily %>% 
  filter(Variable == "do_mgl") %>% filter(year >= 2007) %>% 
  ggplot(aes(x = date, y = mean_daily))+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), color = "lightblue", alpha = 0.75)+
  geom_line(color = "steelblue")+
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

#TEMP
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
