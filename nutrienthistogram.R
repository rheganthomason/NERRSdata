## Nutrient histograms (norm, log)

library(tidyverse)

# Wrangle data
narts2 <- narts %>%  mutate_if(is.character,as.numeric) %>% 
  tidyr::pivot_longer(!datetimestamp, names_to = "Variable", values_to = "Value") %>% 
  tidyr::drop_na(Value) %>% 
  mutate(station = c("TWharfSurface"))

nar <- rbind(narts2)

#chla normal
nar %>% dplyr::filter(nar$Variable == "chla_n") %>%
  ggplot(aes(x = Value))+
  geom_histogram()+
  facet_grid(Variable~station, scales = "free")+
  ggtitle("Narragansett - Chlorophyll")

#chla log 
nar %>% dplyr::filter(nar$Variable == "chla_n") %>% 
  dplyr::mutate(logvalue = log(Value)) %>%
  ggplot(aes(x = logvalue))+
  geom_histogram()+
  facet_grid(Variable~station, scales = "free")+
  ggtitle("Narragansett - logChlorophyll")

#po4f norm
nar %>% dplyr::filter(nar$Variable == "po4f") %>%
  ggplot(aes(x = Value))+
  geom_histogram()+
  facet_grid(Variable~station, scales = "free")+
  ggtitle("Narragansett - Orthophosphate")

#po4f log
nar %>% dplyr::filter(nar$Variable == "po4f") %>% 
  dplyr::mutate(logvalue = log(Value)) %>%
  ggplot(aes(x = logvalue))+
  geom_histogram()+
  facet_grid(Variable~station, scales = "free")+
  ggtitle("Narragansett - logOrthophosphate")

#nh4f norm
nar %>% dplyr::filter(nar$Variable == "nh4f") %>%
  ggplot(aes(x = Value))+
  geom_histogram()+
  facet_grid(Variable~station, scales = "free")+
  ggtitle("Narragansett - Ammonium")

#nh4f log
nar %>% dplyr::filter(nar$Variable == "nh4f") %>% 
  dplyr::mutate(logvalue = log(Value)) %>%
  ggplot(aes(x = logvalue))+
  geom_histogram()+
  facet_grid(Variable~station, scales = "free")+
  ggtitle("Narragansett - logAmmonium")

#no2f norm
nar %>% dplyr::filter(nar$Variable == "no2f") %>%
  ggplot(aes(x = Value))+
  geom_histogram()+
  facet_grid(Variable~station, scales = "free")+
  ggtitle("Narragansett - Nitrite")

#no2f log
nar %>% dplyr::filter(nar$Variable == "no2f") %>% 
  dplyr::mutate(logvalue = log(Value)) %>%
  ggplot(aes(x = logvalue))+
  geom_histogram()+
  facet_grid(Variable~station, scales = "free")+
  ggtitle("Narragansett - logNitrite")

#no3f norm
nar %>% dplyr::filter(nar$Variable == "no3f") %>%
  ggplot(aes(x = Value))+
  geom_histogram()+
  facet_grid(Variable~station, scales = "free")+
  ggtitle("Narragansett - Nitrate")

#no3f log
nar %>% dplyr::filter(nar$Variable == "no3f") %>% 
  dplyr::mutate(logvalue = log(Value)) %>%
  ggplot(aes(x = logvalue))+
  geom_histogram()+
  facet_grid(Variable~station, scales = "free")+
  ggtitle("Narragansett - logNitrate")

#no23f norm
nar %>% dplyr::filter(nar$Variable == "no23f") %>%
  ggplot(aes(x = Value))+
  geom_histogram()+
  facet_grid(Variable~station, scales = "free")+
  ggtitle("Narragansett - Nitrite + Nitrate")

#no23f log
nar %>% dplyr::filter(nar$Variable == "no23f") %>% 
  dplyr::mutate(logvalue = log(Value)) %>%
  ggplot(aes(x = logvalue))+
  geom_histogram()+
  facet_grid(Variable~station, scales = "free")+
  ggtitle("Narragansett - logNitriteNitrate")

## great bay nutri histos
nutrient_pep <- read_csv(here::here("data/nutrient_pep.csv"))

nutrient_pep_grb <- nutrient_pep %>% mutate(station = c("grbgnut")) %>% mutate(year =)

# chloro hist for great bay
nutrient_pep_grb %>% dplyr::filter(nutrient_pep$Variable == "CHLA_N") %>% 
  ggplot(aes(x = mean_daily))+
  geom_histogram()+
  facet_grid(Variable~station, scales = "free")+
  ggtitle("Great Bay - Chlorophyll")

nutrient_pep_grb %>% dplyr::filter(nutrient_pep$Variable == "PO4F") %>% 
  ggplot(aes(x = date, y = mean_daily))+
  geom_point()+
  facet_grid(Variable~station, scales = "free")+
  ggtitle("Great Bay - Orthophosphate")

nutrient_pep_grb %>% dplyr::filter(nutrient_pep$Variable == "CHLA_N") %>% 
  ggplot(aes(x = mean_daily))+
  geom_histogram()+
  facet_grid(Variable~station, scales = "free")+
  ggtitle("")

nutrient_pep_grb %>% dplyr::filter(nutrient_pep$Variable == "CHLA_N") %>% 
  ggplot(aes(x = mean_daily))+
  geom_histogram()+
  facet_grid(Variable~station, scales = "free")+
  ggtitle("")

nutrient_pep_grb %>% dplyr::filter(nutrient_pep$Variable == "CHLA_N") %>% 
  ggplot(aes(x = mean_daily))+
  geom_histogram()+
  facet_grid(Variable~station, scales = "free")+
  ggtitle("")


