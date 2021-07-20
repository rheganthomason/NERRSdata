## climatology - updated

## Nar clim start
#seperate out nar station
narclim <- monthly %>% mutate(month = lubridate::month(date, label = TRUE, abbr = TRUE),
                              year = lubridate::year(date)) %>% filter(station == "nartswq") %>% ungroup()

#use 2020 data for red points
allyearnarclim <- narclim %>% 
group_by(month, Variable, station, year) %>% 
summarise(meanmonth = mean(mean_daily),
          meansd = sd(sd_daily)) %>% filter(year == "2020")

#average of all data across all years
onlymonthnarclim <- narclim %>% 
  group_by(month, Variable, station) %>% 
  summarise(meanmonth = mean(mean_daily),
            meansd = sd(sd_daily))

onlymonthnarclim %>% 
  dplyr::filter(Variable == "DO_mgl") %>% 
  ggplot(aes(month, meanmonth))+
  geom_point(color = "lightblue")+
  geom_errorbar(aes(ymin=meanmonth-meansd, ymax=meanmonth+meansd), width=.2,
                position=position_dodge(0.05), color = "steelblue")+
  geom_point(data = allyearnarclim %>% filter(Variable == "DO_mgl"), aes(month, meanmonth), color = "red", size = 2)+
  labs(x = "", y = "Dissolved Oxygen Concentration (mg/L)", title = "Narraganset DO (mg/L) - Climatology")+
  ecodata::theme_ts()

onlymonthnarclim %>% 
  dplyr::filter(Variable == "DO_Pct") %>% 
  ggplot(aes(month, meanmonth))+
  geom_point(color = "lightblue")+
  geom_errorbar(aes(ymin=meanmonth-meansd, ymax=meanmonth+meansd), width=.2,
                position=position_dodge(0.05), color = "steelblue")+
  geom_point(data = allyearnarclim %>% filter(Variable == "DO_Pct"), aes(month, meanmonth), color = "red", size = 2)+
  labs(x = "", y = "Dissolved Oxygen Concentration (%)", title = "Narraganset DO (%) - Climatology")+
  ecodata::theme_ts()

onlymonthnarclim %>% 
  dplyr::filter(Variable == "Sal") %>% 
  ggplot(aes(month, meanmonth))+
  geom_point(color = "lightblue")+
  geom_errorbar(aes(ymin=meanmonth-meansd, ymax=meanmonth+meansd), width=.2,
                position=position_dodge(0.05), color = "steelblue")+
  geom_point(data = allyearnarclim %>% filter(Variable == "Sal"), aes(month, meanmonth), color = "red", size = 2)+
  labs(x = "", y = "Salinity (psu)", title = "Narraganset Salinity - Climatology")+
  ecodata::theme_ts()

onlymonthnarclim %>% 
  dplyr::filter(Variable == "Temp") %>% 
  ggplot(aes(month, meanmonth))+
  geom_point(color = "lightblue")+
  geom_errorbar(aes(ymin=meanmonth-meansd, ymax=meanmonth+meansd), width=.2,
                position=position_dodge(0.05), color = "steelblue")+
  geom_point(data = allyearnarclim %>% filter(Variable == "Temp"), aes(month, meanmonth), color = "red", size = 2)+
  labs(x = "", y = "Temperature (C)", title = "Narraganset Temperature - Climatology")+
  ecodata::theme_ts()

## great bay start 
grbclim <- monthly %>% mutate(month = lubridate::month(date, label = TRUE, abbr = TRUE),
                              year = lubridate::year(date)) %>% filter(station == "grbgbwq") %>% ungroup()

#use 2020 data for red points
allyeargrbclim <- grbclim %>% 
  group_by(month, Variable, station, year) %>% 
  summarise(meanmonth = mean(mean_daily),
            meansd = sd(sd_daily)) %>% filter(year == "2020")

#average of all data across all years
onlymonthgrbclim <- grbclim %>% 
  group_by(month, Variable, station) %>% 
  summarise(meanmonth = mean(mean_daily),
            meansd = sd(sd_daily))

onlymonthgrbclim %>% 
  dplyr::filter(Variable == "DO_mgl") %>% 
  ggplot(aes(month, meanmonth))+
  geom_point(color = "lightblue")+
  geom_errorbar(aes(ymin=meanmonth-meansd, ymax=meanmonth+meansd), width=.2,
                position=position_dodge(0.05), color = "steelblue")+
  geom_point(data = allyeargrbclim %>% filter(Variable == "DO_mgl"), aes(month, meanmonth), color = "red", size = 2)+
  labs(x = "", y = "Dissolved Oxygen Concentration (mg/L)", title = "Great Bay DO (mg/L) - Climatology")+
  ecodata::theme_ts()

onlymonthgrbclim %>% 
  dplyr::filter(Variable == "DO_Pct") %>% 
  ggplot(aes(month, meanmonth))+
  geom_point(color = "lightblue")+
  geom_errorbar(aes(ymin=meanmonth-meansd, ymax=meanmonth+meansd), width=.2,
                position=position_dodge(0.05), color = "steelblue")+
  geom_point(data = allyeargrbclim %>% filter(Variable == "DO_Pct"), aes(month, meanmonth), color = "red", size = 2)+
  labs(x = "", y = "Dissolved Oxygen Concentration (%)", title = "Great Bay DO (%) - Climatology")+
  ecodata::theme_ts()

onlymonthgrbclim %>% 
  dplyr::filter(Variable == "Sal") %>% 
  ggplot(aes(month, meanmonth))+
  geom_point(color = "lightblue")+
  geom_errorbar(aes(ymin=meanmonth-meansd, ymax=meanmonth+meansd), width=.2,
                position=position_dodge(0.05), color = "steelblue")+
  geom_point(data = allyeargrbclim %>% filter(Variable == "Sal"), aes(month, meanmonth), color = "red", size = 2)+
  labs(x = "", y = "Salinity (psu)", title = "Great Bay Salinity - Climatology")+
  ecodata::theme_ts()

onlymonthgrbclim %>% 
  dplyr::filter(Variable == "Temp") %>% 
  ggplot(aes(month, meanmonth))+
  geom_point(color = "lightblue")+
  geom_errorbar(aes(ymin=meanmonth-meansd, ymax=meanmonth+meansd), width=.2,
                position=position_dodge(0.05), color = "steelblue")+
  geom_point(data = allyeargrbclim %>% filter(Variable == "Temp"), aes(month, meanmonth), color = "red", size = 2)+
  labs(x = "", y = "Temperature (C)", title = "Great Bay Temperature - Climatology")+
  ecodata::theme_ts()

## start of waquoit
waqclim <- monthly %>% mutate(month = lubridate::month(date, label = TRUE, abbr = TRUE),
                              year = lubridate::year(date)) %>% filter(station == "wqbmpwq") %>% ungroup()

#use 2020 data for red points
allyearwaqclim <- waqclim %>% 
  group_by(month, Variable, station, year) %>% 
  summarise(meanmonth = mean(mean_daily),
            meansd = sd(sd_daily)) %>% filter(year == "2020")

#average of all data across all years
onlymonthwaqclim <- waqclim %>% 
  group_by(month, Variable, station) %>% 
  summarise(meanmonth = mean(mean_daily),
            meansd = sd(sd_daily))

onlymonthwaqclim %>% 
  dplyr::filter(Variable == "DO_mgl") %>% 
  ggplot(aes(month, meanmonth))+
  geom_point(color = "lightblue")+
  geom_errorbar(aes(ymin=meanmonth-meansd, ymax=meanmonth+meansd), width=.2,
                position=position_dodge(0.05), color = "steelblue")+
  geom_point(data = allyearwaqclim %>% filter(Variable == "DO_mgl"), aes(month, meanmonth), color = "red", size = 2)+
  labs(x = "", y = "Dissolved Oxygen Concentration (mg/L)", title = "Waquoit DO (mg/L) - Climatology")+
  ecodata::theme_ts()

onlymonthwaqclim %>% 
  dplyr::filter(Variable == "DO_Pct") %>% 
  ggplot(aes(month, meanmonth))+
  geom_point(color = "lightblue")+
  geom_errorbar(aes(ymin=meanmonth-meansd, ymax=meanmonth+meansd), width=.2,
                position=position_dodge(0.05), color = "steelblue")+
  geom_point(data = allyearwaqclim %>% filter(Variable == "DO_Pct"), aes(month, meanmonth), color = "red", size = 2)+
  labs(x = "", y = "Dissolved Oxygen Concentration (%)", title = "Waquoit Bay DO (%) - Climatology")+
  ecodata::theme_ts()

onlymonthwaqclim %>% 
  dplyr::filter(Variable == "Sal") %>% 
  ggplot(aes(month, meanmonth))+
  geom_point(color = "lightblue")+
  geom_errorbar(aes(ymin=meanmonth-meansd, ymax=meanmonth+meansd), width=.2,
                position=position_dodge(0.05), color = "steelblue")+
  geom_point(data = allyearwaqclim %>% filter(Variable == "Sal"), aes(month, meanmonth), color = "red", size = 2)+
  labs(x = "", y = "Salinity (psu)", title = "Waquoit Bay Salinity - Climatology")+
  ecodata::theme_ts()

onlymonthwaqclim %>% 
  dplyr::filter(Variable == "Temp") %>% 
  ggplot(aes(month, meanmonth))+
  geom_point(color = "lightblue")+
  geom_errorbar(aes(ymin=meanmonth-meansd, ymax=meanmonth+meansd), width=.2,
                position=position_dodge(0.05), color = "steelblue")+
  geom_point(data = allyearwaqclim %>% filter(Variable == "Temp"), aes(month, meanmonth), color = "red", size = 2)+
  labs(x = "", y = "Temperature (C)", title = "Waquoit Bay Temperature - Climatology")+
  ecodata::theme_ts()

## start of wells
welclim <- monthly %>% mutate(month = lubridate::month(date, label = TRUE, abbr = TRUE),
                              year = lubridate::year(date)) %>% filter(station == "welinwq") %>% ungroup()

#use 2020 data for red points
allyearwelclim <- welclim %>% 
  group_by(month, Variable, station, year) %>% 
  summarise(meanmonth = mean(mean_daily),
            meansd = sd(sd_daily)) %>% filter(year == "2020")

#average of all data across all years
onlymonthwelclim <- welclim %>% 
  group_by(month, Variable, station) %>% 
  summarise(meanmonth = mean(mean_daily),
            meansd = sd(sd_daily))

onlymonthwelclim %>% 
  dplyr::filter(Variable == "DO_mgl") %>% 
  ggplot(aes(month, meanmonth))+
  geom_point(color = "lightblue")+
  geom_errorbar(aes(ymin=meanmonth-meansd, ymax=meanmonth+meansd), width=.2,
                position=position_dodge(0.05), color = "steelblue")+
  geom_point(data = allyearwelclim %>% filter(Variable == "DO_mgl"), aes(month, meanmonth), color = "red", size = 2)+
  labs(x = "", y = "Dissolved Oxygen Concentration (mg/L)", title = "Wells Bay DO (mg/L) - Climatology")+
  ecodata::theme_ts()

onlymonthwelclim %>% 
  dplyr::filter(Variable == "DO_Pct") %>% 
  ggplot(aes(month, meanmonth))+
  geom_point(color = "lightblue")+
  geom_errorbar(aes(ymin=meanmonth-meansd, ymax=meanmonth+meansd), width=.2,
                position=position_dodge(0.05), color = "steelblue")+
  geom_point(data = allyearwelclim %>% filter(Variable == "DO_Pct"), aes(month, meanmonth), color = "red", size = 2)+
  labs(x = "", y = "Dissolved Oxygen Concentration (%)", title = "Wells Bay DO (%) - Climatology")+
  ecodata::theme_ts()

onlymonthwelclim %>% 
  dplyr::filter(Variable == "Sal") %>% 
  ggplot(aes(month, meanmonth))+
  geom_point(color = "lightblue")+
  geom_errorbar(aes(ymin=meanmonth-meansd, ymax=meanmonth+meansd), width=.2,
                position=position_dodge(0.05), color = "steelblue")+
  geom_point(data = allyearwelclim %>% filter(Variable == "Sal"), aes(month, meanmonth), color = "red", size = 2)+
  labs(x = "", y = "Salinity (psu)", title = "Wells Bay Salinity - Climatology")+
  ecodata::theme_ts()

onlymonthwelclim %>% 
  dplyr::filter(Variable == "Temp") %>% 
  ggplot(aes(month, meanmonth))+
  geom_point(color = "lightblue")+
  geom_errorbar(aes(ymin=meanmonth-meansd, ymax=meanmonth+meansd), width=.2,
                position=position_dodge(0.05), color = "steelblue")+
  geom_point(data = allyearwelclim %>% filter(Variable == "Temp"), aes(month, meanmonth), color = "red", size = 2)+
  labs(x = "", y = "Temperature (C)", title = "Wells Bay Temperature - Climatology")+
  ecodata::theme_ts()
