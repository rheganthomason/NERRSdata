## monthly data nutrients (chlorophyll)
#no waquoit?

chloro_nutri <- read_csv(here::here("data/nutrient_pep.csv"))
nartsnut <- chloro_nutri %>% filter(station == "nartsnut")
#waqnut <- chloro_nutri %>% filter(station == "wqbmpnut")
welnut <- chloro_nutri %>% filter(station == "welinnut")
grbnut <- chloro_nutri %>% filter(station == "grbgbnut") %>%
  mutate(date = as.Date(date))

#nar chla
nartsnut%>% 
  filter(Variable == "CHLA_N") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "purple", size = 2)+
  geom_smooth(method = "loess", se = F, span = 0.15, color = "steelblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  ylim(0, 10)+
  labs(x = "Year", y = "Chlorophyll concentration (µg/L)", title = "Narraganset Chlorophyll Concentration")+
  ecodata::theme_ts()

#great bay chla
grbnut%>% 
  filter(Variable == "CHLA_N") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_line(color = "steelblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  labs(x = "Year", y = "Chlorophyll concentration (µg/L)", title = "Great Bay Chlorophyll Concentration")+
  ecodata::theme_ts()

#wells chla
welnut%>% 
  filter(Variable == "CHLA_N") %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue")+
  geom_line(color = "steelblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  labs(x = "Year", y = "Chlorophyll concentration (µg/L)", title = "Wells Chlorophyll Concentration")+
  ecodata::theme_ts()
