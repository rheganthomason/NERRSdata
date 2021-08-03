## temp spans

##max temp time series
#average of all data across all years
maxtemp_all <- monthly %>% mutate(year = lubridate::year(date)) %>% 
  group_by(Variable, station, year) %>% filter(Variable == "Temp") %>% 
  summarise(maxtemp = max(mean_daily)) %>% ungroup()

## min temp time series
mintemp_all <- monthly %>% mutate(year = lubridate::year(date)) %>% 
  group_by(Variable, station, year) %>% filter(Variable == "Temp") %>% 
  summarise(mintemp = min(mean_daily)) %>% ungroup()

## span time series
spantemp_all <- monthly %>% mutate(year = lubridate::year(date)) %>% 
  group_by(Variable, station, year) %>% filter(Variable == "Temp") %>% 
  summarise(spantemp = max(mean_daily)-min(mean_daily)) %>%  ungroup()

ggplot()+
  geom_point(data = maxtemp_all, aes(year, maxtemp, color = "a"))+
  geom_point(data = mintemp_all, aes(year, mintemp, color = "b"))+
  geom_point(data = spantemp_all, aes(year, spantemp, color = "c"))+
  scale_colour_manual(values = c("green", "steelblue", "palevioletred"),
                      labels = c("Max Temp", "Min Temp", "Span" ))+
  labs(x = "", y = "Temperature (C)")+
  ylim(0, 30)+
  geom_smooth(data = maxtemp_all, aes(year, maxtemp), method = "lm", color = "green", se = F)+
  geom_smooth(data = mintemp_all, aes(year, mintemp), method = "lm", color = "steelblue", se = F)+
  geom_smooth(data = spantemp_all, aes(year, spantemp), method = "lm", color = "palevioletred", se = F)+
  facet_wrap(.~station, labeller = station_names) +
  theme(legend.title = element_blank())+
  ecodata::theme_ts()


# ggplot()+
#   geom_col(data = maxtempnar_all, aes(month, maxtemp, color = "a"), alpha = 0.05)+
#   geom_col(data = mintempnar_all, aes(month, mintemp, color = "b"), alpha = 0.05)+
#   geom_col(data = spantempnar_all, aes(month, spantemp, color = "c"), alpha = 0.05)+
#   scale_colour_manual(values = c("green", "steelblue", "palevioletred"),
#                       labels = c("Max Temp", "Min Temp", "Span" ))+
#   labs(x = "", y = "Temperature (C)", title = "Temperature of Narraganset")+
#   scale_x_discrete(labels = "%y", breaks = "1 year")+
#   theme(legend.title = element_blank())+
#   ecodata::theme_ts()

# ## min temp time series
# mintempnar_all <- monthlytempnar %>% 
#   group_by(month, Variable, station) %>% 
#   summarise(mintemp = min(mean_daily))
# 
# mintempnar_all %>% 
#   ggplot(aes(month, mintemp), group_by("1"))+
#   geom_point(color = "steelblue")+
#   #geom_point(data = maxtempnar_2020, aes(month, maxtemp), color = "red", size = 2)+
#   labs(x = "", y = "Min Temperature (C)", title = "Min Temperature of Narraganset")+
#   ecodata::theme_ts()
# 
# ## span time series
# spantempnar_all <- monthlytempnar %>% 
#   group_by(month, Variable, station) %>% 
#   summarise(spantemp = max(mean_daily)-min(mean_daily))
# 
# spantempnar_all %>% 
# ggplot(aes(month, spantemp), group_by("1"))+
#   geom_point(color = "steelblue")+
#   #geom_point(data = maxtempnar_2020, aes(month, maxtemp), color = "red", size = 2)+
#   labs(x = "", y = "Span of Temperature (C)", title = "Span Temperature of Narraganset")+
#   ecodata::theme_ts()
# 
# #use 2020 data for red points
# maxtemp2020 <- dat_month %>% mutate(year = lubridate::year(date)) %>% 
#   group_by(date, Variable, station, year) %>% filter(Variable == "Temp", 
#                                                      date >= "2020-1-01") %>% 
#   summarise(maxtemp = max(mean_month))  
# 
