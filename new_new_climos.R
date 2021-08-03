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

rmsal_clim <-dailyyear %>% filter(Variable == "Sal" & mean_daily > 25 & mean_daily < 35 | is.na(mean_daily),
                                  !station == "grbgbwq") %>% 
  group_by(month, Variable, station) %>% 
  summarise(mean_month = mean(mean_daily), sd_month = sd(mean_daily)) %>% ungroup()

##"welinwq","grbgbwq","wqbmpwq","nartswq"

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

rmsal2020 <-dailyyear %>% filter(Variable == "Sal" & mean_daily > 25 & mean_daily < 35 | is.na(mean_daily),
                                 !station == "grbgbwq") %>% 
  group_by(month, Variable, station, year) %>% 
  summarise(mean_month = mean(mean_daily), sd_month = sd(mean_daily)) %>% filter(year == 2020) %>% ungroup()

#for facets
monthly_clim$station <- factor(monthly_clim$station, levels = c("welinwq","grbgbwq","wqbmpwq","nartswq"), 
                            labels = c("Wells Bay", "Great Bay", "Waquoit Bay", "Narragansett"))

rmdomgl2020$station <- factor(rmdomgl2020$station, levels = c("welinwq","grbgbwq","wqbmpwq","nartswq"), 
                      labels = c("Wells Bay", "Great Bay", "Waquoit Bay", "Narragansett"))

rmdopct2020$station <- factor(rmdopct2020$station, levels = c("welinwq","grbgbwq","wqbmpwq","nartswq"), 
                          labels = c("Wells Bay", "Great Bay", "Waquoit Bay", "Narragansett"))

rmtemp2020$station <- factor(rmtemp2020$station, levels = c("welinwq","grbgbwq","wqbmpwq","nartswq"), 
       labels = c("Wells Bay", "Great Bay", "Waquoit Bay", "Narragansett"))

rmsal2020$station <- factor(rmsal2020$station, levels = c("welinwq", "wqbmpwq","nartswq"), 
       labels = c("Wells Bay", "Waquoit Bay", "Narragansett"))

## monthy clims for all
monthly_clim %>% 
  filter(Variable == "DO_mgl") %>% 
  ggplot(aes(month, mean_month, color = "a"))+
  geom_point(size = 2)+
  geom_line(group = "1", color = "steelblue")+
  geom_errorbar(aes(ymin=mean_month-sd_month, ymax=mean_month+sd_month), width=.2,
                position=position_dodge(0.05), color = "steelblue")+
  facet_wrap(.~station) +
  geom_point(data = rmdomgl2020 %>% filter(Variable == "DO_mgl"), aes(month, mean_month, color = "b"), size = 2)+
  geom_line(data = rmdomgl2020, aes(month, mean_month, group = "1"), color = "red")+
  geom_hline(yintercept = 2, color = "darkred", linetype = "dashed")+
  scale_colour_manual(values = c("steelblue", "red"),
                      labels = c("Climatology (2007-2020)","2020"))+
  ylim(0, 15)+
  labs(x = "", y = "Dissolved Oxygen Concentration (mg/L)", title = "")+
  ecodata::theme_ts()+
  theme(axis.text = element_text(size = 10), axis.title = element_text(size = 12), strip.text.x = element_text(size = 12),
        legend.title = element_blank(), legend.text = element_text(size = 12), legend.position = "bottom")

monthly_clim %>% 
  filter(Variable == "DO_Pct") %>% 
  ggplot(aes(month, mean_month, color = "a"))+
  geom_point(size = 2)+
  geom_line(group = "1", color = "steelblue")+
  geom_errorbar(aes(ymin=mean_month-sd_month, ymax=mean_month+sd_month), width=.2,
                position=position_dodge(0.05), color = "steelblue")+
  facet_wrap(.~station) +
  geom_point(data = rmdopct2020 %>% filter(Variable == "DO_Pct"), aes(month, mean_month, color = "b"), size = 2)+
  geom_line(data = rmdopct2020, aes(month, mean_month, group = "1"), color = "red")+
  scale_colour_manual(values = c("steelblue", "red"),
                      labels = c("Climatology (2007-2020)","2020"))+
  ylim(50, 120)+
  labs(x = "", y = "Dissolved Oxygen Concentration (%)", title = "")+
  ecodata::theme_ts()+
  theme(axis.text = element_text(size = 10.5), axis.title = element_text(size = 12), strip.text.x = element_text(size = 12),
        legend.title = element_blank(), legend.text = element_text(size = 12), legend.position = "bottom")

monthly_clim %>% 
  filter(Variable == "Temp") %>% 
  ggplot(aes(month, mean_month, color = "a"))+
  geom_point(size = 2)+
  geom_line(group = "1", color = "steelblue")+
  geom_errorbar(aes(ymin=mean_month-sd_month, ymax=mean_month+sd_month), width=.2,
                position=position_dodge(0.05), color = "steelblue")+
  geom_point(data = rmtemp2020 %>% filter(Variable == "Temp"), aes(month, mean_month, color = "b"), size = 2)+
  geom_line(data = rmtemp2020, aes(month, mean_month, group = "1"), color = "red")+
  scale_colour_manual(values = c("steelblue", "red"),
                      labels = c("Climatology (2007-2020)","2020"))+
  ylim(0, 30)+
  labs(x = "", y = "Temperature (C)", title = "")+
  facet_wrap(.~station) +
  ecodata::theme_ts()+
  theme(axis.text = element_text(size = 10.5), axis.title = element_text(size = 12), strip.text.x = element_text(size = 12),
        legend.title = element_blank(), legend.text = element_text(size = 12), legend.position = "bottom")

monthly_clim %>% 
  filter(Variable == "Sal") %>% 
  ggplot(aes(month, mean_month, color = "a"))+
  geom_point(size = 2)+
  geom_line(group = "1", color = "steelblue")+
  geom_errorbar(aes(ymin=mean_month-sd_month, ymax=mean_month+sd_month), width=.2,
                position=position_dodge(0.05), color = "steelblue")+
  geom_point(data = rmsal2020 %>% filter(Variable == "Sal"), aes(month, mean_month, color = "b"), size = 2)+
  geom_line(data = rmsal2020, aes(month, mean_month, group = "1"), color = "red")+
  scale_colour_manual(values = c("steelblue", "red"),
                      labels = c("Climatology (2007-2020)","2020"))+
  ylim(25, 35)+
  labs(x = "", y = "Salinity (psu)", title = "")+
  facet_wrap(.~station, nrow = (3)) +
  ecodata::theme_ts()+
  theme(axis.text = element_text(size = 10.5), axis.title = element_text(size = 12), strip.text.x = element_text(size = 12),
        legend.title = element_blank(), legend.text = element_text(size = 12), legend.position = "bottom")
