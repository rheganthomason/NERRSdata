## new new climos - less code, easier method

#import data 
raw.dir <- here::here("data")
daily_csv <-"water_quality_pep.csv"
daily1 <- read_csv(file.path(raw.dir,daily_csv))
dailyyear <- daily1 %>% mutate(month = lubridate::month(date, label = TRUE, abbr = TRUE),
                              year = lubridate::year(date))

#thresholds added (with NA)
 rmdomgl_clim <-dailyyear %>% filter(Variable == "DO_mgl" & mean_daily > 0 & mean_daily < 15 | is.na(mean_daily)) %>% 
  group_by(month, Variable, station) %>% 
  summarise(mean_month = mean(mean_daily), sd_month = sd(mean_daily)) %>% ungroup()

rmdopct_clim <-dailyyear %>% filter(Variable == "DO_Pct" & mean_daily > 50 & mean_daily < 120 | is.na(mean_daily)) %>% 
  group_by(month, Variable, station) %>% 
  summarise(mean_month = mean(mean_daily), sd_month = sd(mean_daily)) %>% ungroup()

rmtemp_clim <-dailyyear %>% filter(Variable == "Temp" & mean_daily > 0 & mean_daily < 30 | is.na(mean_daily)) %>% 
  group_by(month, Variable, station) %>% 
  summarise(mean_month = mean(mean_daily), sd_month = sd(mean_daily)) %>% ungroup()

rmsal_clim <-dailyyear %>% filter(Variable == "Sal" & mean_daily > 0 & mean_daily < 32 | is.na(mean_daily)) %>% 
  group_by(month, Variable, station) %>% 
  summarise(mean_month = mean(mean_daily), sd_month = sd(mean_daily)) %>% ungroup()

monthly_clim <- rbind(rmdomgl_clim, rmdopct_clim, rmtemp_clim, rmsal_clim)

## only 2020 points
rmdomgl2020 <-dailyyear %>% filter(Variable == "DO_mgl" & mean_daily > 0 & mean_daily < 15 | is.na(mean_daily)) %>% 
  group_by(month, Variable, station, year) %>% 
  summarise(mean_month = mean(mean_daily), sd_month = sd(mean_daily)) %>% filter(year == 2020) %>% ungroup()

rmdopct2020 <-dailyyear %>% filter(Variable == "DO_Pct" & mean_daily > 50 & mean_daily < 120 | is.na(mean_daily)) %>% 
  group_by(month, Variable, station, year) %>% 
  summarise(mean_month = mean(mean_daily), sd_month = sd(mean_daily)) %>% filter(year == 2020) %>% ungroup()

rmtemp2020 <-dailyyear %>% filter(Variable == "Temp" & mean_daily > 0 & mean_daily < 30 | is.na(mean_daily)) %>% 
  group_by(month, Variable, station, year) %>% 
  summarise(mean_month = mean(mean_daily), sd_month = sd(mean_daily)) %>% filter(year == 2020) %>% ungroup()

rmsal2020 <-dailyyear %>% filter(Variable == "Sal" & mean_daily > 0 & mean_daily < 32 | is.na(mean_daily)) %>% 
  group_by(month, Variable, station, year) %>% 
  summarise(mean_month = mean(mean_daily), sd_month = sd(mean_daily)) %>% filter(year == 2020) %>% ungroup()

#so you dont have to type out each station
station_names <- as_labeller(
  c('nartswq' = "Narragansett",
    "wqbmpwq"= "Waquoit Bay",
    "welinwq" = "Wells Bay", 
    "grbgbwq" = "Great Bay"))

## monthy clims for all
monthly_clim %>% 
  filter(Variable == "DO_mgl") %>% 
  ggplot(aes(month, mean_month))+
  geom_point(color = "steelblue", size = 2)+
  geom_line(group = "1", color = "steelblue")+
  geom_errorbar(aes(ymin=mean_month-sd_month, ymax=mean_month+sd_month), width=.2,
                position=position_dodge(0.05), color = "steelblue")+
  geom_point(data = rmdomgl2020 %>% filter(Variable == "DO_mgl"), aes(month, mean_month), color = "red", size = 2)+
  geom_line(data = rmdomgl2020, aes(month, mean_month, group = "1"), color = "red")+
  ylim(0, 15)+
  labs(x = "", y = "Dissolved Oxygen Concentration (mg/L)", title = "DO (mg/L) - Monthly")+
  facet_wrap(.~station, labeller = station_names) +
  ecodata::theme_ts()+
  theme(axis.text = element_text(size = 10), axis.title = element_text(size = 10))

monthly_clim %>% 
  filter(Variable == "DO_Pct") %>% 
  ggplot(aes(month, mean_month))+
  geom_point(color = "steelblue", size = 2)+
  geom_line(group = "1", color = "steelblue")+
  geom_errorbar(aes(ymin=mean_month-sd_month, ymax=mean_month+sd_month), width=.2,
                position=position_dodge(0.05), color = "steelblue")+
  geom_point(data = rmdopct2020 %>% filter(Variable == "DO_Pct"), aes(month, mean_month), color = "red", size = 2)+
  geom_line(data = rmdopct2020, aes(month, mean_month, group = "1"), color = "red")+
  ylim(50, 120)+
  labs(x = "", y = "Dissolved Oxygen Concentration (mg/L)", title = "DO (%) - Monthly")+
  facet_wrap(.~station, labeller = station_names) +
  ecodata::theme_ts()+
  theme(axis.text = element_text(size = 10), axis.title = element_text(size = 10))

monthly_clim %>% 
  filter(Variable == "Temp") %>% 
  ggplot(aes(month, mean_month))+
  geom_point(color = "steelblue", size = 2)+
  geom_line(group = "1", color = "steelblue")+
  geom_errorbar(aes(ymin=mean_month-sd_month, ymax=mean_month+sd_month), width=.2,
                position=position_dodge(0.05), color = "steelblue")+
  geom_point(data = rmtemp2020 %>% filter(Variable == "Temp"), aes(month, mean_month), color = "red", size = 2)+
  geom_line(data = rmtemp2020, aes(month, mean_month, group = "1"), color = "red")+
  ylim(0, 30)+
  labs(x = "", y = "Temperature (C)", title = "Temperature - Monthly")+
  facet_wrap(.~station, labeller = station_names) +
  ecodata::theme_ts()+
  theme(axis.text = element_text(size = 10), axis.title = element_text(size = 10))

monthly_clim %>% 
  filter(Variable == "Sal") %>% 
  ggplot(aes(month, mean_month))+
  geom_point(color = "steelblue", size = 2)+
  geom_line(group = "1", color = "steelblue")+
  geom_errorbar(aes(ymin=mean_month-sd_month, ymax=mean_month+sd_month), width=.2,
                position=position_dodge(0.05), color = "steelblue")+
  geom_point(data = rmsal2020 %>% filter(Variable == "Sal"), aes(month, mean_month), color = "red", size = 2)+
  geom_line(data = rmsal2020, aes(month, mean_month, group = "1"), color = "red")+
  ylim(10, 32)+
  labs(x = "", y = "Salinity (psu)", title = "Salinity - Monthly")+
  facet_wrap(.~station, labeller = station_names) +
  ecodata::theme_ts()+
  theme(axis.text = element_text(size = 10), axis.title = element_text(size = 10))
