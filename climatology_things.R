## Time series/Climatology

#for chlorophyll climo
df <- nar2 %>% dplyr::filter(nar2$Variable == "chla_n" & year == 2020)
nar2 %>% dplyr::filter(nar2$Variable == "chla_n") %>% 
  ggplot(aes(x = month, y = Climatology, group_by("1")))+
  ecodata::theme_ts()+
  geom_point()+
  geom_point(data = only2020data, aes(x = month, y = Value), color = "red", size = 2)+
  #geom_line(data = only2020data, aes(x = month, y = Value), color = "red")+
  geom_line()+
  geom_errorbar(aes(ymin=Climatology-clim.sd, ymax=Climatology+clim.sd), width=.2,
                position=position_dodge(0.05), color = "steelblue")+
  labs(x = element_blank(), y = "Chlorophyll (μg/L)")+
  theme(legend.position = "side")+
  ggtitle("Narragansett - Climatology")

#anoms 
nar3help<- nar3 %>% dplyr::filter(Variable == "chla_n") %>% 
  filter(month == "1") %>% group_by(year, station) %>% 
  summarise(anom.mean = mean(anom)) %>% ungroup()
nar3 %>% dplyr::filter(Variable == "chla_n") %>% filter(month == "1") %>% 
  ggplot(aes(x = year, y = anom))+
  ecodata::theme_ts()+
  geom_point()+
  geom_point(nar3help, mapping = aes(x = year, y = anom.mean), 
             color = "red", shape = 3, size = 3)+
  geom_hline(aes(yintercept = 1), color = "steelblue", size = 2)+
  facet_grid(month~station, scales = "free")+
  labs(x = "Year", y = "Chlorophyll Anomaly (ratio)")+
  ggtitle("Narragansett - Chlorophyll")
