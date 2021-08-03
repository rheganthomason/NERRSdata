## nutrient - easy graphs

library(tidyverse)

nutrients_pep <- read_csv("data/nutrients_pep.csv")

dailynut <- nutrients_pep %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date, label = TRUE, abbr = TRUE)) %>% 
  group_by(station, Variable, date, month) %>%
  summarize(mean_daily = mean(mean_daily, na.rm = TRUE),
            mean_daily = ifelse(is.nan(mean_daily),
                                NA_real_,
                                mean_daily))

dailynut1 <- dailynut %>%
  mutate(month_year = format(as.Date(date), "%Y-%m")) %>%
  group_by(station, month_year, Variable, grp = with(rle(!is.na(mean_daily)), 
                                                     rep(seq_along(lengths),
                                                         lengths))) %>%
  mutate(counter = 1:n()) %>%
  ungroup() %>%
  select(-grp) %>%
  group_by(station, month_year, Variable) %>%
  mutate(mean_month = ifelse(max(counter) >= 60,
                             mean(mean_daily, na.rm = TRUE),
                             NA_real_),
         mean_month = ifelse(is.nan(mean_month),
                             NA_real_,
                             mean_month),
         date = as.Date(paste0(month_year, "-1"))) %>% ## Create a dummy date variable for the first of each month
  select(month_year, date, station, Variable, mean_month) %>%
  distinct(.keep_all = TRUE)

## filter out chlorophyll 
rmchla <- dailynut %>% group_by(station) %>%
  filter(Variable == "chla_n" & mean_daily > 0 & mean_daily < 60 | is.na(mean_daily))

## for facet_grid, reordering
rmchla$station <- factor(rmchla$station, levels = c("welinwq","grbgbwq","wqbmpwq","nartswq"), 
                            labels = c("Wells Bay", "Great Bay", "Waquoit Bay", "Narraganset"))

rmchla %>% 
  ggplot(aes(date, mean_daily))+
  geom_point(color = "lightblue", size = 2)+
  #geom_smooth(method = "loess", span = 0.08, color = "steelblue", se = F, na.rm=FALSE)+
  geom_line(color = "steelblue", size = 1.25)+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  ylim(0, 15)+
  labs(x = "Year", y = "Chlorophyll Concentration (µg/L)") +
  facet_grid(.~station) + 
  ecodata::theme_ts()+
  theme(axis.text = element_text(size = 10), axis.title = element_text(size = 12), strip.text.x = element_text(size = 12))
