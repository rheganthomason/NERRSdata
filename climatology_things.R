## Time series/Climatology

#for chlorophyll climo
nar2 %>% dplyr::filter(nar2$Variable == "chla_n") %>% 
  ggplot(aes(x = month, y = Climatology))+
  geom_point()+
  geom_line()+
  facet_grid(~station, scales = "free")+
  ggtitle("Narragansett - Chlorophyll")

#anoms 
nar3help<- nar3 %>% dplyr::filter(Variable == "chla_n") %>% 
  filter(month == "1") %>% group_by(year, station) %>% 
  summarise(anom.mean = mean(anom)) %>% ungroup()
nar3 %>% dplyr::filter(Variable == "chla_n") %>% filter(month == "1") %>% 
  ggplot(aes(x = year, y = anom))+
  geom_point()+
  geom_point(nar3help, mapping = aes(x = year, y = anom.mean), 
             color = "red", shape = 3, size = 3)+
  facet_grid(month~station, scales = "free")+
  ggtitle("Narragansett - Chlorophyll")