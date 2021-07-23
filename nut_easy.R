## nutrient - easy graphs

library(tidyverse)

chloro_nutri <- read_csv(here::here("data/nutrient_pep.csv"))
dailynut <- chloro_nutri %>% 
  group_by(station, Variable, date) %>%
  summarize(mean_daily = mean(mean_daily, na.rm = TRUE),
            mean_daily = ifelse(is.nan(mean_daily),
                                NA_real_,
                                mean_daily))

rmchla <-dailynut %>% group_by(station) %>%
  filter(Variable == "CHLA_N" & mean_daily > 0 & mean_daily < 60 | is.na(mean_daily))

#so each station is added in one code chunk
station_names <- as_labeller(
  c('nartsnut' = "Narragansett",
    "welinnut" = "Wells Bay", 
    "grbgbnut" = "Great Bay"))

rmchla %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "darkblue", size = 2)+
  #geom_smooth(method = "loess", span = 0.08, color = "steelblue", se = F)+
  geom_line(group = "1", color = "steelblue")+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  ylim(0, 15)+
  labs(x = "Year", y = "Chlorophyll Concentration (µg/L)") +
  facet_wrap(.~station, labeller = station_names) + 
  ecodata::theme_ts()+
  theme(axis.text = element_text(size = 10), axis.title = element_text(size = 10))
