## NERRs data inventory

# Install package - comment aka hastag out this once the package is installed
install.packages('SWMPr')

# Library packages
library(SWMPr)
library(tidyverse)

# get list of site codes 
codes <- SWMPr::site_codes()


# filter for relevant states
codes <- codes %>% 
  dplyr::filter(reserve_name == "Narragansett Bay")

# download data
#narpc <- all_params_dtrng("narpcnut", c('01/01/2010', '12/31/2020'))
narts <- all_params_dtrng("nartsnut", c('01/01/2010', '12/31/2020'))
#narnc <- all_params_dtrng("narncnut", c('01/01/2010', '12/31/2020'))

# Wrangle data
# narpc2 <- narpc %>%  mutate_if(is.character,as.numeric) %>% 
#   tidyr::pivot_longer(!datetimestamp, names_to = "Variable", values_to = "Value") %>% 
#   tidyr::drop_na(Value) %>% 
#   mutate(station = c("PottersCove"))

narts2 <- narts %>%  mutate_if(is.character,as.numeric) %>% 
  tidyr::pivot_longer(!datetimestamp, names_to = "Variable", values_to = "Value") %>% 
  tidyr::drop_na(Value) %>% 
  mutate(station = c("TWharfSurface"))

# narnc2 <- narnc %>%  mutate_if(is.character,as.numeric) %>% 
#   tidyr::pivot_longer(!datetimestamp, names_to = "Variable", values_to = "Value") %>% 
#   tidyr::drop_na(Value) %>% 
#   mutate(station = c("NagCreek"))

nar<- narts2

# dirty plot
nar %>% 
  ggplot(aes(x = datetimestamp, y = Value))+
  geom_point()+
  facet_grid(Variable~station, scales = "free")+
  ggtitle("Narragansett Stations")

nar2 <- nar %>% mutate(month = lubridate::month(datetimestamp, label =TRUE, abbr=TRUE)) %>%
  group_by(month, Variable, station) %>% summarise(Climatology = mean(Value)) %>% 
  mutate(clim.sd = sd(Value)) %>% 
  ungroup() 

nar3 <- nar %>% mutate(month = lubridate::month(datetimestamp),
                       year = lubridate::year(datetimestamp)) %>%
  group_by(month, Variable, station) %>% mutate(Climatology = mean(Value)) %>%
  mutate(anom = exp(log(Value)-log(Climatology))) %>% ungroup()
