## Time series/Climatology

#for chlorophyll
nar2 %>% dplyr::filter(nar2$Variable == "chla_n") %>% 
  ggplot(aes(x = month, y = Climatology))+
  geom_point()+
  geom_line()+
  facet_grid(Variable~station, scales = "free")+
  ggtitle("Narragansett - Chlorophyll")

#
nar3 %>% dplyr::filter(nar3$Variable == "chla_n") %>% filter(month == "1") %>% 
  ggplot(aes(x = year, y = anom))+
  geom_point()+
  geom_point(data = nar3 %>%
                      group_by(year, station) %>% 
                      summarise(anom.mean = mean(anom)) %>% ungroup(), 
                      mapping = aes(y = year, x = anom.mean, color = "red"))+
  facet_grid(month~station, scales = "free")+
  ggtitle("Narragansett - Chlorophyll")

