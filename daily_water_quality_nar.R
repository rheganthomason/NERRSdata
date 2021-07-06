## Data pull

narpc <- all_params_dtrng("narpcwq", c('01/01/2015', '12/31/2020')) #Not yet
narts <- all_params_dtrng("nartswq", c('08/01/2002', '12/31/2020'))
narnc <- all_params_dtrng("narncwq", c('04/01/2002', '12/31/2020'))

# # Wrangle data
# -5 Outside high sensor range
# -4 Outside low sensor range
# -3 Data rejected due to QAQC
# -2 Missing data
# -1 Optional parameter not collected
# 0 Passed initial QAQC checks
# 1 Suspect data
# 2 Reserved for future use
# 3 Calculated data: non-vented depth/level sensor correction for changes in barometric pressure
# 4 Historical: Pre-auto QAQC
# 5 Corrected data


narpc2 <- narpc %>%  mutate_if(is.character,as.numeric) %>% 
  tidyr::pivot_longer(!datetimestamp, names_to = "Variable", values_to = "Value") %>% 
  mutate(date = as.Date(datetimestamp)) %>% 
  dplyr::filter(!Variable == str_detect(Variable, "f_") &  !Value == c(0, 1,2, 3, 4, 5) ) %>% 
  tidyr::drop_na(Value) %>% 
  group_by(date, Variable) %>% 
  summarise(mean = mean(Value, na.rm=TRUE), 
            sd = sd(Value)) %>% 
  filter(!str_detect(Variable, "f_")) %>% 
  dplyr::mutate(station = c("PottersCove"))
  

narpc2 <- narpc %>%  mutate_if(is.character,as.numeric) %>% 
  tidyr::pivot_longer(!datetimestamp, names_to = "Variable", values_to = "Value") %>% 
  mutate(date = as.Date(datetimestamp)) %>% 
  dplyr::filter(!Variable == str_detect(Variable, "f_") &  !Value == c(0, 1,2, 3, 4, 5) ) %>% 
  tidyr::drop_na(Value) %>% 
  group_by(date, Variable) %>% 
  summarise(mean_daily = mean(Value, na.rm=TRUE), 
            sd_daily  = sd(Value)) %>% 
  filter(!str_detect(Variable, "f_")) %>% 
  dplyr::mutate(station = c("TWsurface"))

nar<- rbind(narpc2, narts2)

narpc2 %>% filter(Variable == "temp") %>% 
  ggplot(aes(x = datetimestamp, y = Value))+
  geom_point()